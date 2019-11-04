
install.packages("RMySQL")
install.packages("ggplot2")
install.packages("ddply")
install.packages("data.table")
library(RMySQL)
library(ggplot2)
library(ddply)
library(plyr)
library(data.table)

# ----- LOAD AND PROCESS MT DATA FROM PLAYBACK

#PlayBack
mydb_PB = dbConnect(MySQL(), user='transitime', password='transitimemn', dbname='transitime_mnrt', host='127.0.0.1', port = 33309)

fetch_db_PB = dbGetQuery(mydb_PB, "
SELECT
time_to_sec(timediff(arrivalDepartureTime,predictionReadTime)) as Horizon,
time_to_sec(timediff(arrivalDepartureTime,predictedTime)) as Error,
predictionSource
#, PredictionAccuracy.* 
  FROM transitime_mnrt.PredictionAccuracy
where time_to_sec(timediff(arrivalDepartureTime,predictionReadTime)) mod 30 = 0
and time_to_sec(timediff(arrivalDepartureTime,predictionReadTime)) BETWEEN 29 AND 1201
and arrivalDepartureTime BETWEEN '2019-10-28' AND '2019-11-02' 
and predictionSource = 'GTFS-rt'
ORDER BY Horizon, predictionSource, error
limit 2000000;
", n=1000000)

Data_MT_Quantile <- do.call("rbind", tapply(fetch_db_PB$Error, fetch_db_PB$Horizon, quantile, c(0.05, 0.125, 0.5, 0.875, 0.95)))
MT_Pred <- setDT(as.data.frame(Data_MT_Quantile), keep.rownames = TRUE)[]
colnames(MT_Pred) <- c('Horizon', 'P_05', 'P_15', 'P_50', 'P_85', 'P_95')
MT_Pred$Horizon <- as.numeric(MT_Pred$Horizon)

# ----- LOAD AND PROCESS TTC DATA FROM DEV

# Dev
mydb_Dev = dbConnect(MySQL(), user='transitime', password='transitimemn', dbname='transitime_mnrt', host='127.0.0.1', port = 33312)

fetch_db_Dev = dbGetQuery(mydb_Dev, "
SELECT
time_to_sec(timediff(arrivalDepartureTime,predictionReadTime)) as Horizon,
time_to_sec(timediff(arrivalDepartureTime,predictedTime)) as Error,
predictionSource
#, PredictionAccuracy.* 
  FROM transitime_mnrt.PredictionAccuracy
where time_to_sec(timediff(arrivalDepartureTime,predictionReadTime)) mod 30 = 0
and time_to_sec(timediff(arrivalDepartureTime,predictionReadTime)) BETWEEN 29 AND 1201
and arrivalDepartureTime BETWEEN '2019-10-28' AND '2019-11-02' 
and predictionSource = 'TransitClock'
ORDER BY Horizon, predictionSource, error
limit 2000000;
", n=1000000)

Data_TTC_Quantile <- do.call("rbind", tapply(fetch_db_Dev$Error, fetch_db_Dev$Horizon, quantile, c(0.05, 0.125, 0.5, 0.875, 0.95)))
TTC_Pred <- setDT(as.data.frame(Data_TTC_Quantile), keep.rownames = TRUE)[]
colnames(TTC_Pred) <- c('Horizon', 'P_05', 'P_15', 'P_50', 'P_85', 'P_95')
TTC_Pred$Horizon <- as.numeric(TTC_Pred$Horizon)

# ----- PLOT

ggsave(filename = "Interval_Oct28_Nov1.png", path = '/Users/simonberrebi2/Dropbox/KF_Paper/Figures/Intervals',
ggplot() +
  geom_line(data = MT_Pred, aes(y=P_05, x=Horizon, color="NexTrip"), size=0) +
  geom_line(data = MT_Pred, aes(y=P_15, x=Horizon, color="NexTrip"), size=0)+
  geom_line(data = MT_Pred, aes(y=P_50, x=Horizon, color="NexTrip"), size=0.5)+
  geom_line(data = MT_Pred, aes(y=P_85, x=Horizon, color="NexTrip"), size=0)+
  geom_line(data = MT_Pred, aes(y=P_95, x=Horizon, color="NexTrip"), size=0)+
  geom_ribbon(data = MT_Pred, aes(ymin=P_05,ymax=P_15, x=Horizon, fill="NexTrip", alpha="90 Percent"))+
  geom_ribbon(data = MT_Pred, aes(ymin=P_15,ymax=P_50, x=Horizon, fill="NexTrip", alpha="75 Percent"))+
  geom_ribbon(data = MT_Pred, aes(ymin=P_50,ymax=P_85, x=Horizon, fill="NexTrip", alpha="75 Percent"))+
  geom_ribbon(data = MT_Pred, aes(ymin=P_85,ymax=P_95, x=Horizon, fill="NexTrip", alpha="90 Percent")) +
  geom_line(data = TTC_Pred, aes(y=P_05, x=Horizon, color="TheTransitClock"), size=0) +
  geom_line(data = TTC_Pred, aes(y=P_15, x=Horizon, color="TheTransitClock"), size=0)+
  geom_line(data = TTC_Pred, aes(y=P_50, x=Horizon, color="TheTransitClock"), size=0.5)+
  geom_line(data = TTC_Pred, aes(y=P_85, x=Horizon, color="TheTransitClock"), size=0)+
  geom_line(data = TTC_Pred, aes(y=P_95, x=Horizon, color="TheTransitClock"), size=0)+
  geom_ribbon(data = TTC_Pred, aes(ymin=P_05,ymax=P_15, x=Horizon, fill="TheTransitClock", alpha="90 Percent"))+
  geom_ribbon(data = TTC_Pred, aes(ymin=P_15,ymax=P_50, x=Horizon, fill="TheTransitClock", alpha="75 Percent"))+
  geom_ribbon(data = TTC_Pred, aes(ymin=P_50,ymax=P_85, x=Horizon, fill="TheTransitClock", alpha="75 Percent"))+
  geom_ribbon(data = TTC_Pred, aes(ymin=P_85,ymax=P_95, x=Horizon, fill="TheTransitClock", alpha="90 Percent")) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), legend.position="bottom",plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  ggtitle("Prediction Error Quantiles Against Horizon 10/28-11/1 2019") +
  ylab("Prediction Error (Actual-Predicted) in Seconds") + xlab("Horizon (Actual-Prediction Time) in Seconds")+
  scale_x_continuous(expand = c(0, 00), limits = c(0,1200))+
  #scale_fill_manual(name = "", values = c("NexTrip" = "red", "TheTransitClock" = "blue")) +
  scale_alpha_manual(name = "", values = c("75 Percent" = 0.4, "90 Percent" = 0.2)) +
  #scale_color_manual(name = "", values = c("NexTrip" = "red", "TheTransitClock" = "blue"))+
  #scale_size_manual(name = "", values = c("Median" = 0.5))+
   labs(fill = "Quantile:") + labs(color = " Median:"),
width = 10, height = 5)



