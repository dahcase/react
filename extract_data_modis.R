###
# A refactor of Benoit's script

#load libraries and functions
library('sf'); library('httr'); library('rvest'); library('raster'); library('data.table')

#load functions
code.dir = '/home/dan/Documents/code/react/'
for(ddd in c('MODIS_and_raster_processing_functions.R','download_modis_fun.R', 'extract_tiles_fun.R', 'setup_processing.R')) source(paste0(code.dir,ddd))


###Major run parameters
s1 = F #download data
s2 = T #extract and format for QA

######SETUP#######
setup_lstday()
{
  #load directories
  work.dir = '/media/dan/react_data/post_proc/'
  
  #load runtime info
  MODIS_product <- paste0(modis_base, ".006")
  
  start_date <- "2004.01.01"
  end_date <- "2016.12.31"
  
  num_cores = 7
  
  #geometries
  infile_modis_grid <- "/home/dan/Documents/react_data/modis_grid/modis_sinusoidal_grid_world.shp" #param11
  infile_reg_outline<- "/home/dan/Documents/react_data/Cities_React/Boundaries.shp" #param9
  CRS_reg <-"+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0" #Station coords WGS84
  
  #lpdaac settings
  up = '/home/dan/Documents/react_data/lpdaac.Rdata'
  load(up)
  
  #create working directory
  out_dir = file.path(work.dir, paste0('output_', modis_base))
  dir.create(out_dir, recursive = T)
}

####Get tiles
list_tiles_modis <- get_modis_tiles_list(modis_grid,
                                         reg_outline=infile_reg_outline)
list_tiles_modis <- unique(unlist(strsplit(list_tiles_modis,",")))


if(s1){
  mod_links = get_modis_links(modis_product = MODIS_product, tile_list = list_tiles_modis, start_date = start_date, end_date = end_date, 
                              extensions = c('.hdf','.xml'), read_local = T, verbose = F)
  gc()
  #download files
  download_modis(urls = mod_links, output_folder = out_dir, redownload = F)
}

#generate a data frame to goven the next section
if(s2){
  modis_files = list.files(file.path(out_dir, list_tiles_modis), pattern = '.hdf$', full.names = T)
  params =data.table(hdf_file = modis_files)
  params[, output_file := file.path(out_dir, list_tiles_modis, paste0(variable, '_', basename(hdf_file)))]
  params[, output_file := paste0(substr(output_file, 1,nchar(output_file)-4),'.tif')]
  
  timer = system.time(mclapply(1:nrow(params), function(x) process_image(hdf_file = params[x,hdf_file],
                                                     val_layer_id = val_layer,
                                                     qa_layer_id = qa_layer,
                                                     qa_mask = qa_codes,
                                                     inverse = T, 
                                                     scaling_factors = scaling_factors,
                                                     output_projection = CRS_reg,
                                                     output_filepath = params[x,output_file],
                                                     return_raster = F), mc.cores = num_cores))
}
