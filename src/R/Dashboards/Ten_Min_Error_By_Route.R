
install.packages("RMySQL")
install.packages("ggplot2")
library(RMySQL)
library(ggplot2)

# ----- LOAD AND PROCESS MT DATA FROM PLAYBACK

mydb_PB = dbConnect(MySQL(), user='transitime', password='transitimemn', dbname='transitime_mnrt', host='127.0.0.1', port = 33309)

Ten_Min_By_Route_PB = dbGetQuery(mydb_PB, "select 
routeshortname, predictionsource, count(*) as sample_size,
avg(time_to_sec(timediff(arrivalDepartureTime,predictedTime))) as mean_error, 
stddev(time_to_sec(timediff(arrivalDepartureTime,predictedTime))) as std_error  
FROM transitime_mnrt.PredictionAccuracy
where arrivalDepartureTime BETWEEN '2019-10-28' AND '2019-11-02'
and predictionsource = 'GTFS-rt'
and time_to_sec(timediff(arrivalDepartureTime,predictionReadTime))  BETWEEN 590 AND 610
group by predictionsource, routeshortname",n=-1)


# ----- LOAD AND PROCESS TTC DATA FROM DEV
mydb_Dev = dbConnect(MySQL(), user='transitime', password='transitimemn', dbname='transitime_mnrt', host='127.0.0.1', port = 33312)

Ten_Min_By_Route_Dev = dbGetQuery(mydb_Dev, "select 
routeshortname, predictionsource, count(*) as sample_size,
avg(time_to_sec(timediff(arrivalDepartureTime,predictedTime))) as mean_error, 
stddev(time_to_sec(timediff(arrivalDepartureTime,predictedTime))) as std_error  
FROM transitime_mnrt.PredictionAccuracy
where arrivalDepartureTime BETWEEN '2019-10-28' AND '2019-11-02'
and predictionsource = 'TransitClock'
and time_to_sec(timediff(arrivalDepartureTime,predictionReadTime))  BETWEEN 590 AND 610
group by routeshortname",n=-1)

# ----- COMBINING 

Ten_Min_By_Route_PB$predictionsource <- 'NexTrip'
Ten_Min_By_Route_Dev$predictionsource <- 'TheTransitClock'
Ten_Min_By_Route <- rbind(Ten_Min_By_Route_PB, Ten_Min_By_Route_Dev)


# One Option consists in summarizing everything first: https://rcompanion.org/handbook/C_04.html
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
#                               Comparison of Std. Error on 
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
Cum_Ten_Min_By_Route <- Ten_Min_By_Route
Cum_Ten_Min_By_Route$routeshortname <- factor(Cum_Ten_Min_By_Route$routeshortname, levels = Cum_Ten_Min_By_Route$routeshortname[order(subset(Cum_Ten_Min_By_Route, predictionsource == 'NexTrip')$std_error)])
Cum_Ten_Min_By_Route <- Cum_Ten_Min_By_Route[order(Cum_Ten_Min_By_Route$routeshortname),]
Cum_Ten_Min_By_Route$routeshortname <- rep(1:(nrow(Cum_Ten_Min_By_Route)/2), each=2)
  


ggsave(filename = "Cum_Ten_Min_By_Route_Oct28_Nov1.png", path = '/Users/simonberrebi2/Dropbox/KF_Paper/Figures/Cumulative_Error',
ggplot(subset(Cum_Ten_Min_By_Route, sample_size >50),                ### The data frame to use.
       aes(x     = routeshortname,
           y     = std_error,
           color = predictionsource)) +
  geom_point(shape = 15,
             size  = 2) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), legend.position="bottom") + ggtitle("Prediction Error Comparison By Route on 600 Second Horizon") +
  ylab("Prediction Std. Error in Seconds")+ xlab("Number of Routes")+ labs(color = "Prediction Source:")+
  scale_x_continuous(expand = c(0, 00), limits = c(0,180)), width = 10, height = 4)



# ---------------------------------------------------------------------------------------------------------------------------------------------------------
#                               Comparison of Std. Error on 
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

#Ten_Min_By_Route$routeshortname <- factor(Ten_Min_By_Route$routeshortname, levels = unique(as.numeric(Ten_Min_By_Route$routeshortname)))
pd = position_dodge(.4)    ### How much to jitter the points on the plot

ggsave(filename = "Ten_Min_By_Route_Oct28_Nov1_Normal.png", path = '/Users/simonberrebi2/Dropbox/KF_Paper/Figures/Distribution_By_Route',
ggplot(subset(Ten_Min_By_Route, sample_size > 50 & as.numeric(routeshortname) <  100),                ### The data frame to use.
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
  ylab("Prediction Distribution (Mean ± Std. Error) in Seconds") + xlab("Route Number")+ labs(color = "Prediction Source:"), width = 15, height = 5)

#scale_color_manual(name = "", values = c("GTFS-rt" = "red", "TransitClock" = "blue"))



