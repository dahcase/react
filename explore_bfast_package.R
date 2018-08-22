library('bfast')
library('bfastSpatial')
library(data.table)
library('ggplot2')

ras = brick('/media/dan/ee_res/indices/ndwi_nirswi_Dakar_1_16.tif')

dat = as.data.frame(ras)
setDT(dat)

dat[, id:=.I]

dat = melt(dat, id.vars = 'id', variable.factor = F)

#seperate out everything past the .
dat[, time_point := as.numeric(substr(variable, 24, 100))]

#in% sample(unique(id), 100)
g = ggplot(dat[id == 6877, ], aes(x = time_point, y = value, group = id)) + geom_line(alpha = .2)
plot(g)

#make bfast
most_px = dat[, sum(is.na(value)), by = 'id']
most_px[which.min(V1),id]

bf = ts(data = dat[id ==6877,value], start =c(2000, 1), end = c(2016,12), deltat = 16/365)

#fill in time series
bff = na.approx(bf)

a = bfast(bff,h = .1, season = 'harmonic', max.iter = 1000, breaks = 10)
plot(a)
