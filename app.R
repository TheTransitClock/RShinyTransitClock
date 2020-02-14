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
library("RPostgres")
library(plotly)
library(data.table)
source("src/R/GetSQL.R")
source("src/R/TransitClock.R")
source("src/R/TransitClockDraw.R")
print(getwd())
pw <- {
  "transitclock"
}

pool <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  user = 'postgres',
  port = 5432,
  password = pw,
  dbname = 'CAPMETRO',
  host = '127.0.0.1'
)

onStop(function() {
  poolClose(pool)
})

routes = transitclock.getRoutes(pool)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "TransitClock dashboard"),
  dashboardSidebar(sidebarMenu(
    id = "sidebar",
    menuItem("Summary",
             tabName = "Summary",
             icon = icon("dashboard")),
    menuItem(
      "Routes Analysis",
      tabName = "Routes",
      icon = icon("dashboard")
    ),
    menuItem(
      "Predictions",
      tabName = "Predictions",
      icon = icon("dashboard")
    )
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "Summary",
            fluidRow(
              tabBox(side = "left",
                     tabPanel(
                       "Total events by route",
                       DT::dataTableOutput("eventsbyroutetable")
                     )),
              box(
                side = "right",
                dateInput("startdate", "Start Date:", value = "2020-02-10"),
                dateInput("enddate", "End Date:", value = "2020-02-12"),
                actionButton("go3", "Go")
              )
              
            )),
    tabItem(tabName = "Predictions",
            fluidRow(
              tabBox(
                id = "tabsetquality",
                side = "left",
                tabPanel("Error distribution", plotOutput("errordistribution")),
                tabPanel("Interval chart", plotOutput("intervalchart"))
              ),
              box(
                side = "right",
                numericInput(
                  "min_horizon",
                  "Range min horizon:",
                  0,
                  min = 0,
                  max = 1500
                ),
                numericInput(
                  "max_horizon",
                  "Range max horizon:",
                  300,
                  min = 1,
                  max = 1500
                ),
                dateInput("startdate", "Start Date:", value = "2020-02-10"),
                dateInput("enddate", "End Date:", value = "2020-02-12"),
                textInput("samplestarttime", "Sart Time:", value = "15:00"),
                textInput("sampleendtime", "End Time:", value = "20:00"),
                actionButton("go1", "Go")
              )
            )),
    tabItem(tabName = "Routes",
            fluidRow(
              tabBox(
                side = "left",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabsetroute",
                tabPanel("Events",
                         leafletOutput("eventmap", height = "800")),
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
                dateInput("startdate", "Start Date:", value = "2020-02-10"),
                timeInput(
                  "starttime",
                  "Start time",
                  seconds = FALSE,
                  value = strptime("00:00", "%T")
                ),
                dateInput("enddate", "End Date:", value = "2020-02-12"),
                timeInput(
                  "endtime",
                  "End time",
                  seconds = FALSE ,
                  value = strptime("23:59", "%T")
                ),
                actionButton("go2", "Go")
                
              )
              
            ))
    
  ))
)
# Define server logic required to draw a histogram

server <- function(input, output) {
  
 

  
  v <- reactiveValues(doPlot = FALSE)
  
  points <- reactiveValues()
  
  observeEvent(c(input$go1,
                 input$go2,
                 input$go3), {
                   v$doPlot = input$go
                   
                   
                   if (input$sidebar == "Summary")
                   {
                     print (input$sidebar)
                     print (input$tabsetroute)
                     transitclock.drawEventsSummary(output, pool,
                                                    input$startdate,
                                                    input$enddate)
                   }
                   
                   
                   if (input$sidebar == "Routes")
                   {
                     print (input$sidebar)
                     print (input$tabsetroute)
                     if (input$tabsetroute == "Events")
                     {
                       transitclock.drawEvents(output, pool,
                                               input$route,
                                               input$direction,
                                               input$startdate,
                                               input$enddate)
                     }
                     
                     if (input$tabsetroute == "Predictions")
                     {
                       transitclock.drawPredictionQuality(output, pool,
                                                          input$route,
                                                          input$direction,
                                                          input$startdate,
                                                          input$enddate)
                     }
                     if(input$tabsetroute == "Arrivals and Departures")
                     {
                       transitclock.drawArrivalDepartures(output, pool,  input$route,
                                                          input$direction,
                                                          input$startdate,
                                                          input$enddate)
                     }
                     
                   }
                   if (input$sidebar == "Predictions")
                   {
                     print (input$sidebar)
                     print (input$tabsetquality)
                     if (input$tabsetquality == "Error distribution")
                     {
                       transitclock.drawErrorDistribution(
                         output,
                         pool,
                         input$startdate,
                         input$enddate,
                         input$min_horizon,
                         input$max_horizon,
                         input$samplestarttime,
                         input$sampleendtime
                       )
                     }
                     if(input$tabsetquality=="Interval chart")
                     {
                       transitclock.drawInterval(output,
                                                   pool,
                                                   input$startdate,
                                                   input$enddate,
                                                    "TransitClock")
                     }
                   }
                 })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)
