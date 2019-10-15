transitclock.getRoutes <- function(con)
{
  routes_query <-
    getSQL("./src/sql/Routes.sql")
  
  result <- dbGetQuery(con, routes_query)
  
  return(result)
}

transitclock.getStopData <- function(con, route, direction)
{
  routes_stops <-
    getSQL("./src/sql/StopsByRoute.sql")
  
  routes_stops <- gsub(":route", route, routes_stops)
  
  routes_stops <- gsub(":direction", direction, routes_stops)
  
  result <- dbGetQuery(con, routes_stops)
  
  return(result)
  
}
transitclock.getAccuracyData <-
  function(con, route, direction, startdate, enddate)
  {
    query <-
      getSQL("./src/sql/ByStopQuality.sql")
    
    query <- gsub(":route", route, query)
    
    query <- gsub(":direction", direction, query)
    
    query <- gsub(":startdate", startdate, query)
    
    query <- gsub(":enddate", enddate, query)
    
    result <- dbGetQuery(con, query)
    
    return(result)
    
  }
transitclock.getArrivalsDepartures <- function(con, route, direction, startdate, enddate)
{
  query_events <-
    getSQL("./src/sql/ArrivalsDepartures.sql")
  
  query_events <- gsub(":route", route, query_events)
  
  query_events <- gsub(":direction", direction, query_events)
  
  query_events <- gsub(":startdate", startdate, query_events)
  
  query_events <- gsub(":enddate", enddate, query_events)
  
  result <- dbGetQuery(con, query_events)
  
  return(result)  
}

transitclock.getEventData <- function(con, route, direction, startdate, enddate)
{
  query_events <-
    getSQL("./src/sql/Events.sql")
  
  query_events <- gsub(":route", route, query_events)
  
  query_events <- gsub(":direction", direction, query_events)
  
  query_events <- gsub(":startdate", startdate, query_events)
  
  query_events <- gsub(":enddate", enddate, query_events)
  
  result <- dbGetQuery(con, query_events)
  
  return(result)  
}