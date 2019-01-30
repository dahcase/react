library('raster')
library('gapfill')
library('sf')

ras = brick('/media/dan/processed//MOD13A1/latlong/Bamako_MOD13A1_006_NDVI_2001_2016.tif')
nnn = read.delim('/media/dan/earth_engine/MODIS_006_MOD13A1.txt', header = F, stringsAsFactors = F)[,1]
city_shape = st_read("/home/dan/Documents/react_data/Cities_React/study_areas.shp", stringsAsFactors = F)
bam = city_shape[city_shape$Name=='Bamako',]

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
out = ras
out[] <- output$fill


# 
# a = ras[[which(year(nnn) %in% 2009:2010)]]
# b = nnn[which(year(nnn) %in% 2009:2010)]
# 
# dimvect = c(dim(a)[1:2],length(unique(yday(b))),length(unique(year(b))))
# a1 = array(a, dim=dimvect)
# 
# doParallel::registerDoParallel(4)
# oot <- Gapfill(data=a1, dopar = T, iMax = 10000)
# a2 = a
# a2[] = oot$fill

## https://gis.stackexchange.com/questions/214144/r-package-gapfill-how-to-convert-r-raster-stack-to-4-dimensional-array-and-then
## create array and predict missing values with gapfill()

#ras <- aperm(ras, c(2,1,3,4))



## convert back
out = ras
out[] <- array(output$fill, c(dimvect[1:2], dimvect[3] * dimvect[4]))[]


#one strategy could be to run a gam with the dataset being its pixels and neighbors.
#Or just run a temporal model and assume constant autocorrelation
#spde still seems like the right choice though
#Annoying to implement
#or move the gapfilling to earth engine