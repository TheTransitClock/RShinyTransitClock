SELECT * FROM transitime_mnrt.ArrivalsDepartures ad where 
ad.routeid=(select distinct(r.id) from Routes r where r.configRev = (select max(configRev) from ActiveRevisions) and r.shortName=':route') 
and time BETWEEN ':startdate' AND ':enddate' 
and directionid=':direction' order by time
