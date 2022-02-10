library(ggplot2)

#Set working directory
setwd("G:/Shared drives/UGS_Groundwater/Projects/WRI/Tintic/Soil Moisture/Tintic_SM_RScripts/SMandPrecip")

#import daily average soil moisture and daily total precip files
VSM <- read.csv("G:/Shared drives/UGS_Groundwater/Projects/WRI/Tintic/Soil Moisture/RExports//T27SouthDeathDailyAveVSM.csv")
Precip <- read.csv("G:/Shared drives/UGS_Groundwater/Projects/WRI/Tintic/Precipitation_Data/RExports//MSRU1DailyPrecip.csv")

VSM$Day <- as.Date(VSM$Day)
VSM$Ave_VSM <- as.numeric(VSM$Ave_VSM)
Precip$Day <- as.Date(Precip$Day)
Precip$total_precip_in <- as.numeric(Precip$total_precip_in)

daily <- merge(Precip, VSM, by = "Day")


ggplot(daily, aes(x = Day)) +
  geom_line(aes(y = Ave_VSM, color = Depth)) +
  geom_line(color="skyblue3", aes(y = total_precip_in)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Total Daily Precipitation (in.)")) +
  labs(title = "T27 South Death", x = "Date", y = "Average Soil Moisture m3/m3 VWC") +
  scale_color_discrete(breaks = c("Port_1", "Port_2", "Port_3", "Precipitation"), 
                       labels = c("1.5 ft", "5 ft", "15 ft", "Precipitation"),
                       name = "") +
  scale_x_date(date_breaks = "6 months") +
  theme(axis.text.y.right=element_text(colour= "skyblue3"),
        axis.ticks.y.right=element_line(colour= "skyblue3"),
        axis.title.y.right=element_text(colour= "skyblue3"),
        legend.position = "left")
