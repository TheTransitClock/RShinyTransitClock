select 
        sum(abs((error)/(horizon)))/count(*) as measure, 
        count(*), 
        concat( q.routeid,':',directionid,':',q.stopid ) as label,
        q.stopid,
        s.lat, s.lon , 
        q.routeid, 
        directionid,
        sp.layoverStop,
        sp.waitStop,
        CASE WHEN sp.waitStop = TRUE THEN 'blue'
                WHEN sp.waitStop = FALSE and  sp.layoverStop=FALSE THEN 'green'
                WHEN sp.layoverStop=TRUE then 'orange'
                ELSE 'red'
        END
                as color 
from 
        transitime_mnrt.pdqv q 
        left join Stops s on s.id = q.stopid and s.configRev = (select max(configRev) from ActiveRevisions)
		left join StopPaths sp on sp.stopId = s.id and sp.gtfsStopSeq=q.gtfsStopSeq and sp.routeid=q.routeid and  sp.configRev = (select configRev from ActiveRevisions)
where 
        q.time  BETWEEN ':startdate' AND ':enddate'
        and q.routeid=(select distinct(r.id) from Routes r where r.configRev = (select max(configRev) from ActiveRevisions) and r.shortName=':route')
        and q.gtfsstopseq is not null and directionid=':direction' 
        and q.configRev = (select max(configRev) from ConfigRevision)
group by 
stopid, s.lat, s.lon,  routeid, directionid, s.timepointstop 
order by 
measure desc
