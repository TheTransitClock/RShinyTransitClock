SELECT count(*) as Count, routeid as Route FROM VehicleEvents  where
time BETWEEN ':startdate' AND ':enddate' group by routeid
order by count(*) desc