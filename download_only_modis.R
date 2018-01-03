###
# A refactor of Benoit's script

#load libraries and functions
library('sf'); library('MODIS');library('httr'); library('rvest'); library('raster'); library('data.table'); library('pryr')

#load functions
code.dir = '/home/dan/Documents/code/react/'
for(ddd in c('MODIS_and_raster_processing_functions.R','download_modis_fun.R', 'extract_tiles_fun.R', 'setup_processing.R', 'city_functions.R')) source(paste0(code.dir,ddd))

modis_prods = c('MOD09A1','MYD09A1')
for(modis_base in modis_prods){
  print(modis_base)
  {
    #modis_base = ppp[q,'modis_base']
    
    #load directories 
    work.dir = '/media/dan/react_data/post_proc/'
    
    #load runtime info
    MODIS_product <- paste0(modis_base, ".006")
    
    start_date <- "2004.01.01"
    end_date <- "2016.12.31"
    
    num_cores = 5
    
    #geometries
    infile_modis_grid <- "/home/dan/Documents/react_data/modis_grid/modis_sinusoidal_grid_world.shp" #param11
    infile_reg_outline<- "/home/dan/Documents/react_data/Cities_React/Boundaries.shp" #'/home/dan/Documents/react_data/Cities_React/roi_1.shp' # #param9
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
  
  mod_links = get_modis_links(modis_product = MODIS_product, tile_list = list_tiles_modis, start_date = start_date, end_date = end_date, 
                              extensions = c('.hdf','.xml'), read_local = T, verbose = F, terra = substr(modis_base, 1,2) == 'MO')
  gc()
  #download files
  download_modis(urls = mod_links, output_folder = out_dir, redownload = F)
}