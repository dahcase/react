#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library('raster')
library('sp')
library('leaflet')
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


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("React City Visualiztion"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("modis_product", "MODIS Product:", 
                    choices = unique(products[,modis_product])),
        
        #a second drop down based on the selection of the first
        uiOutput("subproduct"),
        
        
        #options to load cities
        selectInput("city", "City:", choices = cities),
        
        #conditional panel for City tab
        conditionalPanel(condition = 'input.tabselected == 2', uiOutput('ras_met_out')),
        
        #conditional panel for time series panel
        conditionalPanel(condition = 'input.tabselected == 1', uiOutput('ts_met_out'), uiOutput('date_selection')),
      
        #a button to load the data
        actionButton('dataload', "Load Data")
        
      ),

      
      # Show a plot of the generated distribution
       mainPanel(
         
         #tabset
         tabsetPanel(type = "tabs",
                   tabPanel("Time Series", plotOutput("timeseries"), value = 1),
                   tabPanel("City", leafletOutput("leafmap"), value = 2),
                   tabPanel('Values', dataTableOutput('city_data'), value = 3),id = 'tabselected')
         
         #Subheading
         
                 
                 ), 
      
      position = 'left'
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #modis sub products
  output$subproduct <-
    renderUI({
      selectInput("subproduct", 'Variable', unique(products[modis_product == input$modis_product, subdataset]))
    })
  
  output$ras_met_out <- renderUI({ selectInput('ras_met', 'Metrics', choices = raster_metrics) })
  
  output$ts_met_out <- renderUI({ selectInput('ts_met', 'Metrics', choices = date_metrics) })
  
  output$date_selection <- renderUI({ dateRangeInput('ts_date', 'Date', start = '2004-01-01', end = '2016-12-31',
                                                     min = '2004-01-01', max = '2016-12-31')})
  
  output$city_data <- renderDataTable({
    validate(need(input$dataload>0, message = 'Please load some data'))
    ppp = dat()[[input$city]][[1]]
    return(ppp)
  })
  
  #load the data
  dat <- reactive({
    
    #only load data upon command
    isolate({
      da <- readRDS(paste0(work.dir,'output_', input$modis_product,'/',input$subproduct,'city_brick_summaries.rds'))
      
      return(da)
      #return('loaded')
    })
    
  }, label = 'data_load')
  
  ras <- reactive({

    validate(need(input$dataload>0, message = 'Please load some data'), need(!is.null(input$ts_met), 'Please select the raster view'))

    ras = dat()[[input$city]][[2]][[input$ras_met]]
    return(ras)
  }, label = 'raster_load')
  
  #load the proper city brick 
   output$leafmap <- renderLeaflet({
    
    validate(need(input$dataload>0, message = 'Please load some data'))
    
     #start up parameter
     isolate({
       ext <- extent(ras())
       rasvals = values(ras())
       rasvals[rasvals == -Inf] <- NA
       rasvals[rasvals == Inf] <- NA
       pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), rasvals,
                           na.color = "transparent")
       
       
    })
     
     
    leaflet() %>% addTiles() %>% fitBounds(ext@xmin, ext@ymin, ext@xmax, ext@ymax) %>% 
      addRasterImage(ras(), colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = rasvals,
                title = paste0(raster_met_def[which(raster_metrics == input$ras_met)]))
   })
   
   
   #redraw when there is a new metric
   observe({
    
     validate(need(input$dataload>0, message = 'Please load some data'))
      
     rasvals = values(ras())
     rasvals[rasvals == -Inf] <- NA
     rasvals[rasvals == Inf] <- NA
     pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), rasvals,
                         na.color = "transparent")
     
     prox <- leafletProxy('leafmap') %>% clearControls %>% 
       addRasterImage(ras(), colors = pal, opacity = 0.8) %>%
       addLegend(pal = pal, values = rasvals,
                 title = paste0(raster_met_def[which(raster_metrics == input$ras_met)]))
     
   })
   
   
   output$timeseries <-renderPlot({
     validate(need(input$dataload>0, message = 'Please load some data'))
     
     gra = dat()[[input$city]][[1]]
     
     #subset by time selection
     gra = gra[date>=input$ts_date[1] & date<= input$ts_date[2],]
     
     if(input$ts_met == 'valid_cells'){
       gra[,valid_cells := as.numeric(valid_cells)/total_valid_cells_in_city]
     }
     gra[, output := as.numeric(get(input$ts_met))]
     
     g = ggplot(gra, aes(x = date, y = output)) + geom_line() + theme_bw() + 
       ggtitle(date_met_def[which(date_metrics == input$ts_met)]) 
     
     if(input$ts_met == 'valid_cells'){
       g = g + ylim(0,1)
     }
     
     plot(g)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

