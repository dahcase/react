library('raster')
library('gapfill')
library('sf')
library('lubridate')

ras = brick('/media/dan/processed//MOD13A1/latlong/Bamako_MOD13A1_006_NDVI_2001_2016.tif')
nnn = read.delim('/media/dan/earth_engine/MODIS_006_MOD13A1.txt', header = F, stringsAsFactors = F)[,1]
city_shape = st_read("/home/dan/Documents/react_data/Cities_React/study_areas.shp", stringsAsFactors = F)
bam = city_shape[city_shape$Name=='Bamako',]

#layer 200 is a good example

ras = crop(ras, as(bam, 'Spatial'))

if(!all(grepl('_', nnn, fixed = T))){
  nnn = paste(substr(nnn, 1,4), substr(nnn, 5,6), substr(nnn, 7,8), sep = '_')
}

#convert to date paths
nnn = as.Date(nnn, '%Y_%m_%d')

#set dimensions
out = ras
dimvect = c(dim(ras)[1:2],length(unique(yday(nnn))),length(unique(year(nnn))))
ras = array(ras, dim=dimvect)

#apply gapfill
doParallel::registerDoParallel(6)
output <- Gapfill(data=ras, dopar = T, iMax = 100)

#apply results
out[] <- output$fill
