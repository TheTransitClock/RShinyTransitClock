select s.lon as lon, s.lat as lat, concat(s.id,':',s.name) as label from Routes r 
left join StopPaths sp on sp.routeId=r.id and sp.configRev=(select configRev from ActiveRevisions) 
left join Stops s on sp.stopId=s.Id and s.configRev=(select configRev from ActiveRevisions) 
where 
r.configRev=(select configRev from ActiveRevisions) and
r.shortname=':route';

