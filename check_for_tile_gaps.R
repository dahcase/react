#check gaps

#load libraries and code
library('data.table')

code.dir = '/home/dan/Documents/code/react/'
for(ddd in c('MODIS_and_raster_processing_functions.R','download_modis_fun.R', 'extract_tiles_fun.R', 'setup_processing.R')) source(paste0(code.dir,ddd))

setup('ndvi')

#Boundaries and modis tile
infile_modis_grid <- "/home/dan/Documents/react_data/modis_grid/modis_sinusoidal_grid_world.shp" #param11
infile_reg_outline<- "/home/dan/Documents/react_data/Cities_React/Boundaries.shp" #param9

#out_dir
work.dir = '/media/dan/react_data/post_proc/'
out_dir = file.path(work.dir, paste0('output_', modis_base))

#dates
start_date <- "2004.01.01"
end_date <- "2016.12.31"

#figure out what tiles we are using
list_tiles_modis <- get_modis_tiles_list(infile_modis_grid,
                                         reg_outline=infile_reg_outline)
list_tiles_modis <- unique(unlist(strsplit(list_tiles_modis,",")))

check_for_gaps = function(tile, base_folder, modis_product, start_date, end_date, md_start, md_end, bydays = '8 day',file_format = 'tif'){
  
  print(tile)
  
  #Convert to date objects
  st <- as.Date(start_date,format="%Y.%m.%d") #start date
  en <- as.Date(end_date,format="%Y.%m.%d") #end date
  start_year = year(st)
  end_year = year(en)
  years = start_year:end_year
  
  #get the list of downloaded tiles
  patt = paste0(modis_product,'.*',tile,'.*',file_format,'$')
  
  #get a list of files
  target_files = list.files(path = base_folder, pattern = patt, recursive = T)
  
  stopifnot(length(target_files)>=1)
  bnames = basename(target_files)
  
  #extract the date and time of the tiles
  timedates = regexpr(pattern = 'A[0-9][0-9][0-9][0-9][0-9][0-9][0-9]', bnames)
  timedates = substr(bnames, timedates+1, timedates + 7 )
  timedates = strptime(timedates, format="%Y%j", tz = 'UTC')
  timedates = as.Date(timedates)
  
  timedates = timedates[timedates<= en & timedates>=st]
  
  possible_dates = do.call(c, lapply(years, function(x) expand_yearlist(md_start,md_end, x,bydays = bydays)))
  
  #check if missing
  mis_tiles = possible_dates[!possible_dates %in% timedates]
  
  return(mis_tiles)
}

completeness = lapply(list_tiles_modis, function(x) check_for_gaps(x, out_dir, modis_base, start_date, end_date, md_start, md_end,'16 day', 'hdf'))
