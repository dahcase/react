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
                   "sd", "possible_cells")
date_metrics = c("valid_cells", "mean", "median", "lower", "upper", "sd")


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
       mainPanel(tabsetPanel(type = "tabs",
                   tabPanel("Time Series", plotOutput("timeseries"), value = 1),
                   tabPanel("City", leafletOutput("leafmap"), value = 2), id = 'tabselected'
       )), 
      
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
  
  #load the data
  dat <- reactive({
    
    #if clicked, run things, but not the first time
    if(input$dataload == 0 ){
      #make a minimum output
      oot = lapply(cities, function(x) list(NA, list(1), 'No Data Loaded'))
      names(oot) = cities
      return(oot)
      
    }
    
    #only load data upon command
    isolate({
      da <- readRDS(paste0(work.dir,'output_', input$modis_product,'/',input$subproduct,'city_brick_summaries.rds'))
      
      return(da)
      #return('loaded')
    })
    
  })
  
  #load the proper city brick 
   output$leafmap <- renderLeaflet({
    
    validate(need(input$dataload>0, message = 'Please load some data'))
    
    #trigger reactivity
    the_city = input$city
    the_metric = input$ras_met
    ras = dat()[[the_city]][[2]][[the_metric]]
    
    #determine color
    #colors
    rasvals = values(ras)
    rasvals[rasvals == -Inf] <- NA
    rasvals[rasvals == Inf] <- NA
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), rasvals,
                        na.color = "transparent")
    
    
    leaflet() %>% addTiles() %>%
      addRasterImage(ras, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = rasvals,
                title = paste0(the_metric, ', ', the_city))
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
       ggtitle(paste(input$ts_met, 'for',input$subproduct, 'in', input$city))
     plot(g)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

