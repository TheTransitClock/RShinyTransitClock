pal <-
  colorFactor(
    palette = c("blue", "green", "red", "orange", "darkviolet", "orange3"),
    levels = c("blue", "green", "red", "orange", "darkviolet", "orange3")
  )

transitclock.drawArrivalDepartures <- function(output, con, route,
                                   direction,
                                   startdate,
                                   enddate)
{
  
  result = transitclock.getArrivalsDepartures(con,
                                              route,
                                              direction,
                                              startdate,
                                              enddate)
  
  output$arrivaldepartureplot <- renderPlot({
    ggplot(data = result,
           aes(time, gtfsstopseq)) + geom_point(colour = "black", size = 2) + ggtitle("Arrivals and Departures")
  })
  
  
  output$arrivaldepartureplotly <- renderPlotly({
    plot_ly(
      result,
      x =  ~ time,
      y =  ~ gtfsstopseq,
      hoverinfo =  ~ vehicleid
    )
  })
  
  return()
}
transitclock.drawEventsSummary <- function(output, con, startdate,
                                           enddate )
{
  result = transitclock.getNumberEventsByRoute(con,
                                               startdate,
                                               enddate)
  
  output$eventsbyroutetable <- DT::renderDataTable({
    DT::datatable(result, options = list(orderClasses = TRUE))
  })
  return()
}
transitclock.drawErrorDistribution <- function(output, con,
                                  startdate,
                                  enddate,
                                  min_horizon,
                                  max_horizon,
                                  samplestarttime,
                                  sampleendtime)
{
  result = transitclock.getErrors(
    con,
    startdate,
    enddate,
    min_horizon,
    max_horizon,
    samplestarttime,
    sampleendtime)
  
  output$errordistribution <- renderPlot({
    ggdensity(
      result,
      x = "error",
      fill = "source",
      color = "source",
      palette = c("blue", "red"),
      add = "mean",
      rug = TRUE
    )
  })
  return()
}
transitclock.drawPredictionQuality <- function(con,route,direction,startdate,enddate)
{
  stops = transitclock.getStopData(con, route, direction)
  
  result = transitclock.getAccuracyData(con,
                                        route,
                                        direction,
                                        startdate,
                                        enddate)
  
  
  tab2 <- leafletProxy('predictionmap') %>%
    clearMarkers() %>%
    clearControls() %>%
    addCircleMarkers(
      lng = ~ lon,
      lat = ~ lat,
      data = result,
      stroke = FALSE,
      fillOpacity = 0.5,
      radius = ( ~ measure * 100),
      label = ~ label,
      color = ~ pal(color)
    )  %>%
    addCircleMarkers(
      lng = ~ lon,
      lat = ~ lat,
      data = stops,
      stroke = FALSE,
      fillOpacity = 1,
      radius = 6 ,
      label = ~ label,
      color = 'black'
    )
  
  return()
}
transitclock.drawEvents <- function(con, route, direction, startdate, enddate)
{
  stops = transitclock.getStopData(con, route, direction)
  
  points = transitclock.getEventData(
    con,
    route,
    direction,
    startdate,
    enddate
  )
  
  tab1 <- leafletProxy('eventmap') %>%
    clearMarkers() %>%
    clearControls() %>%
    addCircleMarkers(
      lng = ~ lon,
      lat = ~ lat,
      data = points,
      stroke = FALSE,
      fillOpacity = 1,
      radius = 10 ,
      label = ~ label,
      color = ~ pal(color)
    ) %>%
    addCircleMarkers(
      lng = ~ lon,
      lat = ~ lat,
      data = stops,
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
  return()
}