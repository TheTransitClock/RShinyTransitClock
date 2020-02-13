SELECT 
        DateDiff('second', 
            pa.predictionReadTime::time,
            pa.arrivalDepartureTime::time)  AS horizon,
        DateDiff('second', 
            pa.arrivalDepartureTime::time,
            pa.predictedTime::time) AS error,
        pa.predictionSource as source
FROM PredictionAccuracy pa where arrivalDepartureTime between  ':start_date' AND ':end_date' and mod(id,10)=0 
and  DateDiff('second', 
            pa.predictionReadTime::time,
            pa.arrivalDepartureTime::time) < :max_horizon 
and DateDiff('second', 
            pa.predictionReadTime::time,
            pa.arrivalDepartureTime::time)  > :min_horizon
and abs(DateDiff('second', 
            pa.arrivalDepartureTime::time,
            pa.predictedTime::time))<600 
and pa.arrivalDepartureTime::time > ':start_time'
and pa.arrivalDepartureTime::time < ':end_time'