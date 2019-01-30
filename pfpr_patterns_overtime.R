library('malariaAtlas')
library('raster')
library('sf')
library('data.table')
library('ggplot2')
library('openxlsx')


city_shape = st_read("/home/dan/Documents/react_data/Cities_React/study_areas.shp")
listofcities = as.character(city_shape$Name)

dat = read.xlsx('/media/dan/maldat/REACT SSA Cities PR data (240717)_Final.xlsx')
setDT(dat)
dat[, pfpr := `PfPR2-10`/100]

dat = dat[, .(Lat, Long, pfpr, year = as.numeric(YEAR))]
dat = st_as_sf(dat, coords =c('Long', 'Lat'), crs = 4326)
dat = st_intersection(dat, city_shape)
dat$city = dat$Name

ras <- lapply(1:nrow(city_shape), function(x) getRaster(surface = "Plasmodium falciparum PR2-10", shp = as(city_shape[x,], 'Spatial'), year = 2000:2015))
ras = lapply(ras, cellStats, stat = 'mean')

pfpr = expand.grid(city = listofcities, year = 2000:2015, stringsAsFactors = F)
setorder(pfpr, +city, +year)
setDT(pfpr)

pfpr[, pfpr:= unlist(ras)]

dat = dat[,c('city', 'year', 'pfpr'), drop = T]

pdf('/media/dan/pfpr.pdf', height = 8, width = 13)
a = ggplot() + geom_point(data = dat, aes(x = year, y = pfpr), alpha = .4) + geom_line(data = pfpr, aes(x = year, y = pfpr))  +  xlim(2000, 2015) +
  facet_wrap(~city) + theme_bw() + xlab('Year') + ylab('PfPR 2 - 10') + ggtitle('PfPR 2 - 10 in select African cities')

plot(a)
dev.off()
