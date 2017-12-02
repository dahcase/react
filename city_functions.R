#'
#' @param modis_grid sf object. Grid of modis tiles
#' @param city_shape sf object. Shapefile of the city
#' @param start_date date time. Start of the time series
#' @param end_date date time. End of the time series
#' @param base_folder file path. Location of a folder with processed tif files (potentially in subfiles)
#' @param modis_product character. name of the modis product
#' @param project character. CRS string of of an output projection
#' @param mask logical. Should the brick be masked to the outlines
#' @param cores numeric. Number of cores to be used
#' @return raster brick
build_city_brick = function(modis_grid, city_shape, start_date, end_date, md_start, md_end, bydays = '8 day', base_folder, modis_product, variable, mask, project = NULL, cores = 1){
  
  #find which modis titles need extraction
  cs = st_transform(city_shape, st_crs(modis_grid))
  target_tiles = st_intersection(modis_grid, cs)
  target_tiles$geometry = NULL
  target_tiles = data.frame(target_tiles)[, c('h','v')]
  
  #generate the times
  st <- as.Date(start_date, format="%Y.%m.%d") #start date
  en <- as.Date(end_date, format="%Y.%m.%d")
  times = do.call(c, lapply(year(st):year(en), function(x) expand_yearlist(md_start,md_end, x, bydays = bydays)))
  times = times[times >= st & times<= en]
  times = data.frame(dates = times)
  
  target_slices = expand.grid.df(target_tiles, times)
  
  #convert to the modis date time
  target_slices$modis_dt = format(target_slices$dates,"%Y%j")
  
  #for each unique date, return a mosaiced raster
  uniq_dates = unique(target_slices$modis_dt)
  city_brick = parallel::mclapply(uniq_dates, function(y) load_raster(df = target_slices[target_slices$modis_dt==y,],
                                                            modis_product = modis_product,
                                                            variable = variable,
                                                            base_folder = base_folder,
                                                            city_shape = city_shape,
                                                            mask = mask), mc.cores = cores)
  if(length(city_brick)>1){
    city_brick = raster::brick(city_brick)
  } else {
    city_brick = city_brick[[1]]
  }
  names(city_brick) = uniq_dates
  
  #project things
  if(!is.null(project)){
    city_brick = projectRaster(city_brick, crs = project)
  }
  
  return(city_brick)
  
}

load_raster = function(df, modis_product, variable, base_folder, city_shape, mask){
  
  tile = paste0('h',sprintf("%02d", df$h),'v',sprintf("%02d", df$v))
  #patt = paste0('^(?=.*\\b',modis_product,'\\b)(?=.*\\b',tile, '\\b).*$')
  
  patt = paste0(variable, "_", modis_product,'\\.A', df$modis_dt,'\\.',tile,'.*tif$')
  
  #get a list of files
  target_files = unlist(lapply(patt, function(x) list.files(path = base_folder, pattern = x, recursive = T, full.names = T)))
  
  #load the rasters
  ras = lapply(file.path(target_files), raster::raster)
  
  #load the files
  if(length(target_files) > 1){
    ras$fun = mean
    #merge them together
    ras = do.call(raster::mosaic, ras)
  }else{
    ras = ras[[1]]
  }
  #mask and crop?
  cs = st_transform(city_shape, as.character(crs(ras)))
  
  if(inherits(cs,'sf')) cs = as(st_zm(cs), 'Spatial')

  ras = raster::crop(ras, cs)

  if(mask) ras = raster::mask(ras, cs)

  return(ras)
  
}

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
