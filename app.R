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
library(shinyTime)
library(shinydashboard)
library(leaflet)
library(DBI)
library(pool)
library(ggplot2)
library(ggpubr)
library("RMariaDB")
library(plotly)
source("src/R/GetSQL.R")
source("src/R/TransitClock.R")
print(getwd())
pw <- {
  "transitimemn"
}

pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  user = 'transitime',
  port = 33309,
  password = pw,
  dbname = 'transitime_mnrt',
  host = '127.0.0.1'
)

onStop(function() {
  poolClose(pool)
})

pal <-
  colorFactor(
    palette = c("blue", "green", "red", "orange", "darkviolet", "orange3"),
    levels = c("blue", "green", "red", "orange", "darkviolet", "orange3")
  )

routes = transitclock.getRoutes(pool)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "TransitClock dashboard"),
  dashboardSidebar(
    menuItem("Routes Analysis", tabName = "routes", icon = icon("dashboard")),
    menuItem("Prediction Quality", tabName = "quality", icon = icon("dashboard"))
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName="quality",
      fluidRow(
        tabBox(id="taba",
               side = "left",
               tabPanel("Error distribution", plotOutput("errordistribution"))
               
        ),
        box(
          side = "right",
          numericInput("min_horizon", "Range min horizon:", 0, min = 0, max = 1500),
          numericInput("max_horizon", "Range max horizon:", 300, min = 1, max = 1500),
          dateInput("startdate", "Start Date:", value = "2019-10-24"),
          dateInput("enddate", "End Date:", value = "2019-10-25"),
          textInput("samplestarttime", "Sart Time:", value = "15:00"),
          textInput("sampleendtime", "End Time:", value = "20:00"),
          actionButton("graph", "Update graph")   
        )
      )
    ),
    tabItem(
      tabName = "routes",
      fluidRow(
        tabBox(
          side = "left",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabsetmap",
          tabPanel("Events", leafletOutput("eventmap", height = "800"),  
            actionButton("updateevents", "Update events"),
            DT::dataTableOutput("eventsbyroutetable")
          ),
          tabPanel("Predictions", leafletOutput("predictionmap", height =
                                                  "800")),
          #tabPanel("Arrivals and Departures",  plotOutput("arrivaldepartureplot",height = "600")),
          tabPanel(
            "Arrivals and Departures",
            plotlyOutput("arrivaldepartureplotly"),
            DT::dataTableOutput("adtable")
          )
          #tabPanel("Prediction scatter plot",plotlyOutput("predictionploty"), DT::dataTableOutput("predictiontable"))
        ),
        box(
          side = "right",
          
          selectInput("route", "Route:",
                      routes),
          
          selectInput("direction", "Direction:",
                      c("1" = "1",
                        "0" = "0")),
          dateInput("startdate", "Start Date:", value = "2019-10-14"),
          timeInput(
            "starttime",
            "Start time",
            seconds = FALSE,
            value = strptime("00:00", "%T")
          ),
          dateInput("enddate", "End Date:", value = "2019-10-15"),
          timeInput(
            "endtime",
            "End time",
            seconds = FALSE ,
            value = strptime("23:59", "%T")
          ),
          actionButton("plot", "Update Map")
          
        )
      
      )
    )))
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
  output$errordistribution <- renderPlot({
    
    input$graph
    
    ggdensity(errordistribution(), x = "error", 
              fill = "source", color = "source",palette = c("blue", "red"),
              add = "mean", rug = TRUE)
    }
  )
  
  observe({
   
    
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
      addLegend(
        "bottomright",
        opacity = "1",
        colors = c("red", "green", "orange", "blue", "#cd8500", "#9400d3"),
        labels = c(
          "No match",
          "Predictable",
          "Delayed",
          "No Progress",
          "Not leaving terminal",
          "Other"
        ),
        title = "Event"
      )
    
    
    tab2 <- leafletProxy('predictionmap') %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~ lon,
        lat = ~ lat,
        data = accurracy(),
        stroke = FALSE,
        fillOpacity = 0.5,
        radius = (~ measure * 100),
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
  
  
  output$predictionplotly <- renderPlotly({
    plot_ly(
      predictions(),
      x =  ~ error,
      y =  ~ horizon,
      hoverinfo =  ~ vehicleId
    )
  })
  
  
  
  output$adtable <- DT::renderDataTable({
    DT::datatable(arrivalsdepartures(), options = list(orderClasses = TRUE))
  })
  
  output$predictiontable <- DT::renderDataTable({
    DT::datatable(predictions(), options = list(orderClasses = TRUE))
  })
  
  output$eventsbyroutetable <- DT::renderDataTable({
    DT::datatable(numbereventsbyroute(), options = list(orderClasses = TRUE))
  })
  
  
  errordistribution = eventReactive(input$graph, {
    result = transitclock.getErrors(pool,
                                    input$startdate,
                                    input$enddate, input$min_horizon, input$max_horizon, input$samplestarttime, input$sampleendtime)
    
   
    
    return (result)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  points = eventReactive(input$plot, {
    return(
      transitclock.getEventData(
        pool,
        input$route,
        input$direction,
        input$startdate,
        input$enddate
      )
    )
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  stops = eventReactive(input$plot, {
    return (transitclock.getStopData(pool, input$route, input$direction))
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  accurracy = eventReactive(input$plot, {
    return (
      transitclock.getAccuracyData(
        pool,
        input$route,
        input$direction,
        input$startdate,
        input$enddate
      )
    )
    
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  arrivalsdepartures = eventReactive(input$plot, {
    result = transitclock.getArrivalsDepartures(pool,
                                                input$route,
                                                input$direction,
                                                input$startdate,
                                                input$enddate)
    
    output$arrivaldepartureplot <- renderPlot({
      ggplot(data = result,
             aes(time, gtfsStopSeq)) + geom_point(colour = "black", size = 2) + ggtitle("Arrivals and Departures")
    })
    
    
    output$arrivaldepartureplotly <- renderPlotly({
      plot_ly(
        result,
        x =  ~ time,
        y =  ~ gtfsStopSeq,
        hoverinfo =  ~ vehicleId
      )
    })
    
    return (result)
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  predictions = eventReactive(input$plot, {
    result = transitclock.getPredictions(pool,
                                         input$route,
                                         input$direction,
                                         input$startdate,
                                         input$enddate)
    return (result)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  numbereventsbyroute = eventReactive(input$updateevents, {
    result = transitclock.getNumberEventsByRoute(pool,
                                                 input$startdate,
                                                 input$enddate)
    return (result)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  
  
}
# Run the application
shinyApp(ui = ui, server = server)
