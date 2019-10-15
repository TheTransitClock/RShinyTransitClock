transitclock.getRoutes <- function(con)
{
  routes_query <-
    getSQL("./src/SQL/Routes.sql")
  
  result <- dbGetQuery(con, routes_query)
  
  return(result)
}
transitclock.getArrivalsDepartures <- function(con, route, direction, startdate, enddate)
{
  print("Calling transitclock.getArrivalsDepartures()")
  query_events <-
    getSQL("./src/SQL/ArrivalsDepartures.sql")
  
  query_events <- gsub(":route", route, query_events)
  
  query_events <- gsub(":direction", direction, query_events)
  
  query_events <- gsub(":startdate", startdate, query_events)
  
  query_events <- gsub(":enddate", enddate, query_events)
  
  result <- dbGetQuery(con, query_events)
  print("Finished transitclock.getArrivalsDepartures()")
  return(result)  
}


transitclock.getStopData <- function(con, route, direction)
{
  print("Calling transitclock.getStopData()")
  routes_stops <-
    getSQL("./src/SQL/StopsByRoute.sql")
  
  routes_stops <- gsub(":route", route, routes_stops)
  
  routes_stops <- gsub(":direction", direction, routes_stops)
  
  result <- dbGetQuery(con, routes_stops)
  print("Finished transitclock.getStopData()")
  return(result)
  
}
transitclock.getAccuracyData <-
  function(con, route, direction, startdate, enddate)
  {
    print("Calling transitclock.getAccuracyData()")
    query <-
      getSQL("./src/SQL/bystopquality.sql")
    
    query <- gsub(":route", route, query)
    
    query <- gsub(":direction", direction, query)
    
    query <- gsub(":startdate", startdate, query)
    
    query <- gsub(":enddate", enddate, query)
    
    result <- dbGetQuery(con, query)
    print("Finished transitclock.getAccuracyData()")
    return(result)
    
  }

transitclock.getEventData <- function(con, route, direction, startdate, enddate)
{
  print("Calling transitclock.getEventData()")
  query_events <-
    getSQL("./src/SQL/Events.sql")
  
  query_events <- gsub(":route", route, query_events)
  
  query_events <- gsub(":direction", direction, query_events)
  
  query_events <- gsub(":startdate", startdate, query_events)
  
  query_events <- gsub(":enddate", enddate, query_events)
  
  result <- dbGetQuery(con, query_events)
  print("Finished transitclock.getEventData()")
  return(result)  
}