#Make summaries and NA statistics for the cities
library('sf'); library('httr'); library('rvest'); library('raster'); library('data.table'); library('MODIS')

code.dir = '/home/dan/Documents/code/react/'
for(ddd in c('MODIS_and_raster_processing_functions.R','download_modis_fun.R', 'extract_tiles_fun.R', 'setup_processing.R', 'city_functions.R')) source(paste0(code.dir,ddd))

setup('lst', T)

work.dir = '/media/dan/react_data/post_proc/'
out_dir = file.path(work.dir, paste0('output_', modis_base))

start_date <- "2004.01.01"
end_date <- "2016.12.31"

city_shapes = st_read("/home/dan/Documents/react_data/Cities_React/Boundaries.shp")
city_shapes$Name = as.character(city_shapes$Name)
city_bricks = readRDS(file.path(out_dir, paste0(variable, 'city_bricks.rds')))



#' @param citybrick rasterbrick 
city_summaries = function(city, city_shape, start_date, end_date){
  print(city_shape$Name)
  #convert to dates
  st <- as.Date(start_date,format="%Y.%m.%d") #start date
  en <- as.Date(end_date,format="%Y.%m.%d") #end date
  city_dates = as.Date(substr(names(city), 2,100), format='%Y%j')
  
  keepers = which(city_dates>=st & city_dates<=en)
  
  city = city[[keepers]]
  
  #if city shape doesn ot match the raster projection, match it
  city_shape = st_transform(city_shape, as.character(crs(city)))
  
  ras_shape = raster::rasterize(as(st_zm(city_shape), 'Spatial'), city)
  
  dat = data.table(date = city_dates, total_valid_cells_in_city = sum(!is.na(as.vector(ras_shape))))
  
  #for each date, get the number of valid cells
  dat[, valid_cells := as.vector(lapply(1:dim(city)[3], function(x) sum(!is.na(as.vector(city[[x]]))))) ]
  
  #for each date, get the mean, range, upper, lower, median
  dat[, mean := as.vector(lapply(1:dim(city)[3], function(x) mean(as.vector(city[[x]]),na.rm = T))) ]
  dat[, range := as.vector(lapply(1:dim(city)[3], function(x) range(as.vector(city[[x]]),na.rm = T))) ]
  dat[, median := as.vector(lapply(1:dim(city)[3], function(x) median(as.vector(city[[x]]),na.rm = T))) ]
  dat[, lower := as.vector(lapply(1:dim(city)[3], function(x) quantile(as.vector(city[[x]]),probs = .025,na.rm = T))) ]
  dat[, upper := as.vector(lapply(1:dim(city)[3], function(x) quantile(as.vector(city[[x]]),probs = .975,na.rm = T))) ]
  dat[, sd := as.vector(lapply(1:dim(city)[3], function(x) sd(as.vector(city[[x]]),na.rm = T)))]
  
  #make a series of maps showing things on a cell basis
  #make a map to show which areas have the highest coverage
  coverage = sum(!is.na(city))
  mean_ras = mean(city, na.rm = T)
  range_ras = range(city, na.rm = T)
  med_ras = calc(city, fun = function(x) median(x, na.rm = T))
  lower_ras = calc(city, fun= function(x) quantile(x, probs = .025, na.rm = T))
  upper_ras = calc(city, fun= function(x) quantile(x, probs = .975, na.rm = T))
  sd_ras = calc(city, fun= function(x) sd(x,na.rm = T))
  pos_cells = sd_ras
  pos_cells[] = dim(city)[3]
  
  #brick the outputs
  ret_brik = brick(coverage, mean_ras, range_ras, med_ras, lower_ras, upper_ras, sd_ras, pos_cells)
  names(ret_brik) = c('coverage', 'mean', 'min','max', 'med', 'lower', 'upper', 'sd', 'possible_cells')
  
  return(list(dat, ret_brik))
  
}

#for each city, generate summary stats
summ_stats = lapply(unique(names(city_bricks)), function(x) city_summaries(city_bricks[[x]],
                                                                           city_shapes[city_shapes$Name==x,],
                                                                           start_date,
                                                                           end_date) )

names(summ_stats) = unique(names(city_bricks))
#save output
saveRDS(summ_stats, file.path(out_dir, paste0(variable, 'city_brick_summaries.rds')))
