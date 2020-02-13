select ve.lon, ve.lat, ve.vehicleId, concat(ve.avlTime,':', ve.vehicleId,':', ve.eventType, ': ', ve.description) as label,  
CASE WHEN ve.eventType = 'No Match' THEN 'red'
     WHEN ve.eventType = 'Predictable' THEN 'green'
     WHEN ve.eventType = 'Delayed'  THEN 'orange'
     WHEN ve.eventType = 'No progress' THEN 'blue'
     WHEN ve.eventType = 'Not leaving terminal' THEN 'orange3'
     else 'darkviolet'
 END
 as color,
 t.directionId as direction
from VehicleEvents ve  
left join Trips t on  ve.tripId =t.tripId and t.configRev=(select configRev from ActiveRevisions)
where time BETWEEN ':startdate' AND ':enddate'
and ve.routeShortname=':route' 
and t.directionId=':direction'
order by ve.tripid, ve.time;
