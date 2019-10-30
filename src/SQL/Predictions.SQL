
SELECT 	pa.tripid as 'tripid',
    pa.routeid as 'routeid',
    pa.vehicleid as 'vehicleid',
    pa.affectedbywaitstop as 'affectedbywaitstop', 
    pa.stopid as 'stopid',
    pa.directionid as 'directionid',
	TIMESTAMPDIFF(MICROSECOND,  pa.predictionReadTime, pa.arrivalDepartureTime) / 1000000 as 'horizon',
    TIMESTAMPDIFF(MICROSECOND, pa.arrivalDepartureTime, pa.predictedTime) / 1000000 as 'error',
    pa.predictionReadTime as 'creationtime',
    pa.arrivalDepartureTime as 'time',
    pa.predictedTime as 'predictiontime'
from PredictionAccuracy pa    
where 
pa.routeid=(select distinct(r.id) from Routes r where r.configRev = (select max(configRev) from ActiveRevisions) and r.shortName=':route') 
and pa.predictionReadTime BETWEEN ':startdate' AND ':enddate' 
and pa.directionid=':direction' order by time
