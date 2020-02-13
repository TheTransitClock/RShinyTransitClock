SELECT
  abs(DateDiff('second',arrivalDepartureTime::time,predictionReadTime::time)) as horizon,
  abs(DateDiff('second',arrivalDepartureTime::time,predictedTime::time)) as error,
predictionsource
  FROM PredictionAccuracy
where
mod(abs(DateDiff('second',arrivalDepartureTime::time,predictionReadTime::time)) , 30)=0
and abs(DateDiff('second',arrivalDepartureTime::time,predictionReadTime::time)) BETWEEN 29 AND 1201
and arrivalDepartureTime BETWEEN ':start_date' AND ':end_date' 
and predictionSource = ':source'
ORDER BY Horizon, predictionSource, error
limit 2000000;
