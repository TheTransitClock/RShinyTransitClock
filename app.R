#
# This is a Shiny web application to display some of the underlying data in TheTransitClock. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#
# Find out more about TheTransitClock here:
#
# www.transitclock.org.
#
# Author: Sean Óg Crudden
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(DBI)
library(pool)
library(ggplot2)
library("RMariaDB")
library(plotly)
source("src/R/GetSQL.R")
source("src/R/TransitClock.R")

pw <- {
  "transitimemn"
}

pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  user = 'transitime',
  port = 33306 ,
  password = pw,
  dbname = 'transitime_mnrt',
  host = '127.0.0.1'
)

onStop(function() {
  poolClose(pool)
})


routes = transitclock.getRoutes(pool)

ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = "TransitClock dashboard"),
  dashboardSidebar(
    selectInput("route", "Route:",
                routes),
    
    selectInput("direction", "Direction:",
                c("1" = "1",
                  "0" = "0")),
    dateInput("startdate", "Start Date:", value = "2019-10-14"),
    dateInput("enddate", "End Date:", value = "2019-10-15"),
    actionButton("plot", "Update Map")
    
    
  ),
  dashboardBody(
    fluidRow(
      tabBox(
        width = "100%",
        height = "auto",
        
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabsetmap",
        tabPanel("Events", leafletOutput("eventmap", height = "800")),
        tabPanel("Predictions", leafletOutput("predictionmap", height =
                                                "800")),
        #tabPanel("Arrivals and Departures",  plotOutput("arrivaldepartureplot",height = "600")),
        tabPanel("Arrivals and Departures",  plotlyOutput("arrivaldepartureplotly",height = "600"), 
                 DT::dataTableOutput("adtable")
                 )
      )
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$eventmap <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  output$predictionmap <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  observe({
    input$my_tabsetPanel
    
    tab1 <- leafletProxy('eventmap') %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~ lon,
        lat = ~ lat,
        data = points(),
        stroke = FALSE,
        fillOpacity = 1,
        radius = 10 ,
        label = ~ label,
        color = ~ pal(color)
      ) %>%
      addCircleMarkers(
        lng = ~ lon,
        lat = ~ lat,
        data = stops(),
        stroke = FALSE,
        fillOpacity = 1,
        radius = 6 ,
        label = ~ label,
        color = 'black'
      ) %>%
      addLegend("bottomright", opacity="1", colors= c("red", "green","orange","blue","#cd8500", "#9400d3"), labels=c("No match","Predictable","Delayed","No Progress","Not leaving terminal","Other"), title="Event")
    
    
    tab2 <- leafletProxy('predictionmap') %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~ lon,
        lat = ~ lat,
        data = accurracy(),
        stroke = FALSE,
        fillOpacity = 0.5,
        radius = ( ~ measure * 100),
        label = ~ label,
        color = ~ pal(color)
      )  %>%
      addCircleMarkers(
        lng = ~ lon,
        lat = ~ lat,
        data = stops(),
        stroke = FALSE,
        fillOpacity = 1,
        radius = 6 ,
        label = ~ label,
        color = 'black'
      ) 
      
    
    
  })
  
  output$arrivaldepartureplot <- renderPlot({
    ggplot(data = arrivalsdepartures(), 
           aes(time,gtfsStopSeq)) + geom_point(colour = "black", size = 2) + ggtitle("Arrivals and Departures")
  })
  
  
  output$arrivaldepartureplotly <- renderPlotly({
    plot_ly(arrivalsdepartures(),x=~time, y=~gtfsStopSeq, hoverinfo=~vehicleId) 
  })
  
  output$adtable <- DT::renderDataTable({
    DT::datatable(arrivalsdepartures(), options = list(orderClasses = TRUE))
  })
  
  points = eventReactive(input$plot, {
    
    
      return(transitclock.getEventData(pool,
                 input$route,
                 input$direction,
                 input$startdate,
                 input$enddate))
    
  }, ignoreNULL = FALSE)
  
  stops = eventReactive(input$plot, {
  
    return (transitclock.getStopData(pool, input$route, input$direction))
  
  }, ignoreNULL = FALSE)
  

  accurracy = eventReactive(input$plot, {
    
      return (transitclock.getAccuracyData(pool,
                    input$route,
                    input$direction,
                    input$startdate,
                    input$enddate))
    
   
  }, ignoreNULL = FALSE)
  
  arrivalsdepartures=eventReactive(input$plot,{
    
    result=transitclock.getArrivalsDepartures(pool,
                                              input$route,
                                              input$direction,
                                              input$startdate,
                                              input$enddate)
    return (result)
    
  }, ignoreNULL = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
