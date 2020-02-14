pal <-
  colorFactor(
    palette = c("blue", "green", "red", "orange", "darkviolet", "orange3"),
    levels = c("blue", "green", "red", "orange", "darkviolet", "orange3")
  )
transitclock.drawInterval <- function(output, con,startdate,
                                      enddate, source)
{
  result=transitclock.getIntervalData(con,startdate,enddate, source)
  
  print(result);
  
  data_quantile=do.call("rbind",tapply(result$error,result$horizon, quantile, c(0.05, 0.125, 0.5, 0.875, 0.95)))
  
  pred <- setDT(as.data.frame(data_quantile), keep.rownames = TRUE)[]
  colnames(pred) <- c('horizon', 'P_05', 'P_15', 'P_50', 'P_85', 'P_95')
  pred$horizon <- as.numeric(pred$horizon)
  
  output$intervalchart <-renderPlot({ggplot() +
    geom_line(data = pred, aes(y=P_05, x=horizon, color="TheTransitClock"), size=0) +
    geom_line(data = pred, aes(y=P_15, x=horizon, color="TheTransitClock"), size=0)+
    geom_line(data = pred, aes(y=P_50, x=horizon, color="TheTransitClock"), size=0.5)+
    geom_line(data = pred, aes(y=P_85, x=horizon, color="TheTransitClock"), size=0)+
    geom_line(data = pred, aes(y=P_95, x=horizon, color="TheTransitClock"), size=0)+
    geom_ribbon(data = pred, aes(ymin=P_05,ymax=P_15, x=horizon, fill="TheTransitClock", alpha="90 Percent"))+
    geom_ribbon(data = pred, aes(ymin=P_15,ymax=P_50, x=horizon, fill="TheTransitClock", alpha="75 Percent"))+
    geom_ribbon(data = pred, aes(ymin=P_50,ymax=P_85, x=horizon, fill="TheTransitClock", alpha="75 Percent"))+
    geom_ribbon(data = pred, aes(ymin=P_85,ymax=P_95, x=horizon, fill="TheTransitClock", alpha="90 Percent")) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"), legend.position="bottom",plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) + 
    ggtitle("Prediction Error Quantiles Against Horizon") +
    ylab("Prediction Error (Actual-Predicted) in Seconds") + xlab("Horizon (Actual-Prediction Time) in Seconds")+
    scale_x_continuous(expand = c(0, 00), limits = c(0,1200))+
    #scale_fill_manual(name = "", values = c("NexTrip" = "red", "TheTransitClock" = "blue")) +
    scale_alpha_manual(name = "", values = c("75 Percent" = 0.4, "90 Percent" = 0.2)) +
    labs(fill = "Quantile:") + 
    labs(color = " Median:")
  })
}
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
transitclock.drawPredictionQuality <- function(output, con,route,direction,startdate,enddate)
{
  stops = transitclock.getStopData(con, route, direction)
  
  result = transitclock.getAccuracyData(con,
                                        route,
                                        direction,
                                        startdate,
                                        enddate)
  
  output$predictionmap <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(-97.733330, 30.266666, zoom = 10)
  })
  
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
transitclock.drawEvents <- function(output, con, route, direction, startdate, enddate)
{
  stops = transitclock.getStopData(con, route, direction)
  
  points = transitclock.getEventData(
    con,
    route,
    direction,
    startdate,
    enddate
  )

  output$eventmap <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(-97.733330, 30.266666, zoom = 10)
    
  })
    
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