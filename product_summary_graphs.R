#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(data.table)
library('raster')
library('sp')
library('ggplot2')

#create product options
cities = c("Kinshasa", "dar es salaam", "Bamako", "Kigali", "Kampala", 
           "Lagos", "Mombasa", "Lome", "Dakar", "Accra", "Conacry", "Maputo", 
           "N Djamena", "Ouagadougou", "Freetown", "Aba", "Lusaka", "Nairobi", 
           "Mbuji-Mayi", "Kumasi", "Kananga", "Monrovia", "Yaounde", "Douala", 
           "Lulumbashi", "Abidjan", "Ibadan", "Kisangani")
products = data.table(modis_product = c('MOD13A2','MOD11A2','MOD11A2'), subdataset = c('NDVI', 'LST_Day', "LST_Night"))
raster_metrics = c("coverage", "mean", "min", "max", "med", "lower", "upper", 
                   "sd")
date_metrics = c("valid_cells", "mean", "median", "lower", "upper", "sd")

#metric definitions
date_met_def = c('Percent of cells not masked for QA', 'Mean of all valid cells',
                 'Median of all valid cells', '2.5th percentile of all valid cells',
                 '97.5th percentile of all valid cells', 'Standard Dev. of all valid cells')
raster_met_def = c('Number of valid cells', 'Mean',
                   'Minimum', 'Maximum',
                   'Median', '2.5th %tile',
                   '97.5th %tile', 'Standard deviation')

#work_dir
work.dir = '/media/dan/react_data/post_proc/'


for(mp in unique(products[,modis_product])){
  for(sp in unique(products[modis_product == mp,subdataset])){
    da <- readRDS(paste0(work.dir,'output_', mp,'/',sp,'city_brick_summaries.rds'))
    
    #start pdf of charts
    pdf(paste0('/media/dan/react_data/variable_plots_', mp,'_',sp,'.pdf'), height = 10, width = 10)
    
    #for each city
    for(city in names(da)){
      cit = da[[city]][[1]]
      
      #for each date metric
      for(ddd in date_metrics){
        
        cit[, output:= as.numeric(get(ddd))]
        
        if(ddd == 'valid_cells'){
          cit[,output:=output/total_valid_cells_in_city]
        }
        
        g = ggplot(cit, aes(x = date, y = output)) + geom_line() + theme_bw() + 
          labs(title = date_met_def[which(date_metrics == ddd)], subtitle = paste(city, '|', mp, '|', sp )) 
        
        if(ddd == 'valid_cells'){
          g = g + ylim(0,1)
        }
        
        plot(g)
        
      }
      
    }
    
    dev.off()
    
  }
}

#dataset summary
data_sum = function(mp, sp){
  da <- readRDS(paste0(work.dir,'output_', mp,'/',sp,'city_brick_summaries.rds'))
  
  #pull out data table for each city
  da = lapply(names(da), function(x) da[[x]][[1]][,city:= x])
  
  #combine them
  da = rbindlist(da)
  da[, (date_metrics) := lapply(date_metrics, function(x) as.numeric(get(x)))]
  
  #summarize things by city
  da = da[, list(num_slices = .N, valid_cells_in_city = median(total_valid_cells_in_city), avg_daily_valid_cells = mean(valid_cells)), by = 'city']
  
  da[, modis_product := mp]
  da[, modis_variable := sp]
  
  #get the sizes of the folders
  return(da)
  
}

summ = rbindlist(lapply(1:nrow(products), function(x) data_sum(products[x,modis_product], products[x,subdataset])))

write.csv(summ, paste0(work.dir, 'high_level_data_summary.csv'))

#Data sizes

