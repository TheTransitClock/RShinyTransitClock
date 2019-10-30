
install.packages("RMySQL")
install.packages("ggplot2")
library(RMySQL)
library(ggplot2)


mydb = dbConnect(MySQL(), user='transitime', password='transitimemn', dbname='transitime_mnrt', host='127.0.0.1', port = 33309)

fetch_db = dbSendQuery(mydb, "select 
routeshortname, predictionsource, count(*) as sample_size,
avg(time_to_sec(timediff(arrivalDepartureTime,predictedTime))) as mean_error, 
stddev(time_to_sec(timediff(arrivalDepartureTime,predictedTime))) as std_error  
FROM transitime_mnrt.PredictionAccuracy
where arrivalDepartureTime > '2019-10-28'
and time_to_sec(timediff(arrivalDepartureTime,predictionReadTime)) BETWEEN 590 and 610
group by predictionsource, routeshortname")

Ten_Min_By_Route = fetch(fetch_db, n=-1)

# One Option consists in summarizing everything first: https://rcompanion.org/handbook/C_04.html

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
#                               Comparison of Std. Error on 
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
Cum_Ten_Min_By_Route <- Ten_Min_By_Route
Cum_Ten_Min_By_Route$routeshortname <- factor(Cum_Ten_Min_By_Route$routeshortname, levels = Cum_Ten_Min_By_Route$routeshortname[order(subset(Cum_Ten_Min_By_Route, predictionsource == 'GTFS-rt')$std_error)])

ggplot(subset(Cum_Ten_Min_By_Route, sample_size >20),                ### The data frame to use.
       aes(x     = routeshortname,
           y     = std_error,
           color = predictionsource)) +
  geom_point(shape = 15,
             size  = 2) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), legend.position="bottom") + ggtitle("Prediction Error Comparison By Route on 600 Second Horizon") +
  ylab("Prediction Std. Error in Seconds")+ xlab("Route Number")



# ---------------------------------------------------------------------------------------------------------------------------------------------------------
#                               Comparison of Std. Error on 
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

#Ten_Min_By_Route$routeshortname <- factor(Ten_Min_By_Route$routeshortname, levels = unique(as.numeric(Ten_Min_By_Route$routeshortname)))
pd = position_dodge(.4)    ### How much to jitter the points on the plot

ggplot(subset(Ten_Min_By_Route, sample_size > 50 & as.numeric(routeshortname) > 100),                ### The data frame to use.
       aes(x     = routeshortname,
           y     = mean_error,
           color = predictionsource)) +
  geom_point(shape = 15,
             size  = 2,
             position = pd) +
   geom_errorbar(aes(ymin  = mean_error - std_error,
                    ymax  =  mean_error + std_error),
               width = 0.2,
              size  = 1.0,
              #size = sample_size/100,
             position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), legend.position="bottom") + ggtitle("Prediction Distribution Comparison By Route on 600 Second Horizon") +
  ylab("Prediction Distribution (Mean ± Std. Error) in Seconds") + xlab("Route Number")+ labs(color = "Prediction Source:")
