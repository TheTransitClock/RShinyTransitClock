SELECT 
        (TIMESTAMPDIFF(MICROSECOND,
            pa.predictionReadTime,
            pa.arrivalDepartureTime) / 1000000) AS horizon,
        (TIMESTAMPDIFF(MICROSECOND,
            pa.arrivalDepartureTime,
            pa.predictedTime) / 1000000) AS error,
        pa.predictionSource as source
FROM transitime_mnrt.PredictionAccuracy pa where arrivalDepartureTime between  ':start_date' AND ':end_date' and mod(id,10)=0 
and  (TIMESTAMPDIFF(MICROSECOND,
            pa.predictionReadTime,
            pa.arrivalDepartureTime) / 1000000) < :max_horizon 
and (TIMESTAMPDIFF(MICROSECOND,
            pa.predictionReadTime,
            pa.arrivalDepartureTime) / 1000000) > :min_horizon
and abs((TIMESTAMPDIFF(MICROSECOND,
            pa.arrivalDepartureTime,
            pa.predictedTime) / 1000000))<600 
and TIME(pa.arrivalDepartureTime) > ':start_time'
and TIME(pa.arrivalDepartureTime) < ':end_time'