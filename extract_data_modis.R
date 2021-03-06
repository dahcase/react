###
# A refactor of Benoit's script

#load libraries and functions
library('sf'); library('MODIS');library('httr'); library('rvest'); library('raster'); library('data.table'); library('pryr')

#load functions
code.dir = '/home/dan/Documents/code/react/'
for(ddd in c('MODIS_and_raster_processing_functions.R','download_modis_fun.R', 'extract_tiles_fun.R', 'setup_processing.R', 'city_functions.R')) source(paste0(code.dir,ddd))


###Major run parameters
s1 = F #download data
s2 = F #extract and format for QA
s3 = T #save as a raster brick
######SETUP#######
ppp = data.frame(modis_base = c('MOD11A2','MOD11A2', 'MYD11A2','MYD11A2', 'MOD13A2'),var = c('lst','lst','lst','lst','ndvi'), alt = c(T, F,T,F,F))
ppp = ppp[4,]
for(q in 1:nrow(ppp)){
  print(ppp[q,])
  setup(ppp$var[q], ppp$alt[q])
  {
    modis_base = ppp[q,'modis_base']
    
    #load directories 
    work.dir = '/media/dan/react_data/post_proc/'
    
    #load runtime info
    MODIS_product <- paste0(modis_base, ".006")
    
    start_date <- "2004.01.01"
    end_date <- "2016.12.31"
    
    num_cores = 5
    
    #geometries
    infile_modis_grid <- "/home/dan/Documents/react_data/modis_grid/modis_sinusoidal_grid_world.shp" #param11
    infile_reg_outline<- "/home/dan/Documents/react_data/Cities_React/Boundaries.shp" #param9 '/home/dan/Documents/react_data/Cities_React/roi_1.shp' #
    CRS_reg <-"+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0" #Station coords WGS84
    
    #lpdaac settings
    up = '/home/dan/Documents/react_data/lpdaac.Rdata'
    load(up)
    
    #create working directory
    out_dir = file.path(work.dir, paste0('output_', modis_base))
    dir.create(out_dir, recursive = T)
  }
  
  ####Get tiles
  list_tiles_modis <- get_modis_tiles_list(infile_modis_grid,
                                           reg_outline=infile_reg_outline)
  list_tiles_modis <- unique(unlist(strsplit(list_tiles_modis,",")))
  
  
  if(s1){
    mod_links = get_modis_links(modis_product = MODIS_product, tile_list = list_tiles_modis, start_date = start_date, end_date = end_date, 
                                extensions = c('.hdf','.xml'), read_local = T, verbose = F, terra = substr(modis_base, 1,2) == 'MO')
    gc()
    #download files
    download_modis(urls = mod_links, output_folder = out_dir, redownload = F)
  }
  
  #generate a data frame to goven the next section
  step2start = Sys.time()
  if(s2){
    modis_files = list.files(file.path(out_dir, list_tiles_modis), pattern = '.hdf$', full.names = T)
    params =data.table(hdf_file = modis_files)
    params[, output_file := file.path(out_dir, basename(dirname(hdf_file)), paste0(variable, '_', basename(hdf_file)))]
    params[, output_file := paste0(substr(output_file, 1,nchar(output_file)-4),'.tif')]
    
    dat = parallel::mclapply(1:nrow(params), function(x) process_image(hdf_file = params[x,hdf_file],
                                                       val_layer_id = val_layer,
                                                       qa_layer_id = qa_layer,
                                                       qa_mask = qa_codes,
                                                       inverse = T,
                                                       scaling_factors = scaling_factors,
                                                       output_projection = NULL,
                                                       output_filepath = params[x,output_file],
                                                       return_raster = F), mc.cores = num_cores)
    # dat = lapply(1:10, function(x) process_image(hdf_file = params[x,hdf_file],
    #                                                                    val_layer_id = val_layer,
    #                                                                    qa_layer_id = qa_layer,
    #                                                                    qa_mask = qa_codes,
    #                                                                    inverse = T,
    #                                                                    scaling_factors = scaling_factors,
    #                                                                    output_projection = NULL,
    #                                                                    output_filepath = params[x,output_file],
    #                                                                    return_raster = F))
  }
  step2end = Sys.time()
  #make bricks per city
  step3start = Sys.time()
  if(s3){
    #get a list of geotiffs
    cities = sf::st_read(infile_reg_outline)
    if(is.null(cities$Name)){
      cities$Name = 'ROI'
    } 
    cities$Name = as.character(cities$Name)
    modis_grid = sf::st_read(infile_modis_grid)
  
    #for each city, build a raster brick
    #but do it by year
    sy = as.numeric(substr(start_date, 1, 4))
    ey = as.numeric(substr(end_date, 1, 4))
    for(yyy in 2011:ey){
      print(yyy)
      sd = paste0(yyy,'.01.01')
      ed = paste0(yyy,'.12.31')
      
      city_bricks <- lapply(unique(cities$Name), function(x) build_city_brick(modis_grid = modis_grid,
                                                                           city_shape = cities[cities$Name%in%x,],
                                                                           start_date = sd,
                                                                           end_date = ed,
                                                                           md_start = md_start,
                                                                           md_end = md_end,
                                                                           bydays = temporal_resolution,
                                                                           base_folder = out_dir,
                                                                           modis_product = modis_base,
                                                                           variable = variable,
                                                                           mask = F,
                                                                           project = st_crs(cities)$proj4string,
                                                                           cores = num_cores))
      names(city_bricks) = unique(cities$Name)
      
      #save objects to rdata file
      rasdir = '/media/dan/react_data/post_proc/roi_list/'
      writeRaster(city_bricks[[1]], paste0(rasdir, variable,'_',modis_base,'_',yyy,'_roi.tif'), overwrite = T)
      #saveRDS(city_bricks, file.path(out_dir, paste0(variable,'_',modis_base,'_',yyy,'_roi.rds')))
      rm(city_bricks)
    }
  }
  step3end = Sys.time()
}
