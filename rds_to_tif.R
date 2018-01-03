#RDS to geotiff
library('raster')
library('rgdal')
library('data.table')

code.dir = '/home/dan/Documents/code/react/'
for(ddd in c('MODIS_and_raster_processing_functions.R','download_modis_fun.R', 'extract_tiles_fun.R', 'setup_processing.R', 'city_functions.R')) source(paste0(code.dir,ddd))


ppp = data.frame(modis_base = c('MOD11A2','MOD11A2', 'MYD11A2','MYD11A2', 'MOD13A2'),var = c('lst','lst','lst','lst','ndvi'), alt = c(T, F,T,F,F))
setDT(ppp)
ppp = ppp[var == 'lst',]

out_dir = '/media/dan/react_data/post_proc/roi_list'
dir.create(paste0(out_dir,'/tmp/'))

rasterOptions(tmpdir = paste0(out_dir,'/tmp/'))
for(q in 1:nrow(ppp)){
  setup(ppp$var[q], ppp$alt[q])
  modis_base = ppp[q,modis_base]
  for(yyy in 2004:2016){
    infile  =  file.path('/media/dan/react_data/post_proc',paste0('output_',modis_base,'/',variable,'_',modis_base,'_',yyy,'_roi.rds'))
    outfile = paste0(out_dir, '/', substr(basename(infile),1,nchar(basename(infile))-4),'.tif')
    
    if(!file.exists(outfile)){
      ras = readRDS(infile)[[1]]
      writeRaster(ras, outfile, overwrite = T)
    }
    
  }
}
