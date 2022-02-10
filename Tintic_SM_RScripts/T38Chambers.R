#Set working directory to google drive folder containing soil moisture data
setwd("G:/Shared drives/UGS_Groundwater/Projects/WRI/Tintic/Soil Moisture/T38 Chambers Sp/RawData")

# install tidyverse, magrittr, dplyr, readxl, openxlsx, data.table, ggplot2, reshape2, zoo
library(tidyverse)
library(magrittr)
library(dplyr)
library(readxl)
library(openxlsx)
library(data.table)
library(ggplot2)
library(reshape2)
library(zoo)


#upload files from working directory into list
file.list <- list.files(pattern = '*.xls', recursive = TRUE)
df.list <- lapply(file.list, read_excel)

#fix column names - two parts
colnames <- c("DateTime", "Port_1", "Port_2", "Port_3", "Port_4", "Port_5")
df.list2 <- lapply(df.list, setNames, colnames)

# Remove first two rows from all dfs in list
df.list3 <- lapply(df.list2, function(x) x[-1, ])
df.list4 <- lapply(df.list3, function(x) x[-1, ])

#combine list into single data.frame
SM_df <- rbindlist(df.list4) 

#remove NA rows
SM_df_clean <- SM_df[!(SM_df$Port_1 == "NA" & SM_df$Port_2 == "NA" & SM_df$Port_3 == "NA" & SM_df$Port_4 == "NA" & SM_df$Port_5 == "NA")]

#change Excel number to datetime (needs work - puts in central time?)
SM_df_full <- SM_df_clean %>% mutate(DateTime = convertToDateTime(SM_df_clean$DateTime, origin = "1899-12-30", "%Y-%m-%d %H:%M:%S", tz = "MST"))

#change columns to numeric values
SM_df_numeric <- SM_df_full %>% mutate(Port_1 = as.numeric(SM_df_full$Port_1), Port_2 = as.numeric(SM_df_full$Port_2), Port_3 = as.numeric(SM_df_full$Port_3), Port_4 = as.numeric(SM_df_full$Port_4), Port_5 = as.numeric(SM_df_full$Port_5))

#tidy
SM_df_tidy <- SM_df_numeric %>% gather("Port_1", "Port_2", "Port_3", "Port_4", "Port_5", key = "Depth", value = "VSM")

#flag/remove soil moisture out of range (<0, >0.57)
##view
negitives <- SM_df_tidy[SM_df_tidy$VSM < 0, ]
big <- SM_df_tidy[SM_df_tidy$VSM > 0.57, ]
##change VSM values less than 0 or greater than 0.57 (measurement range of 10HS) to NA
SM_df_tidy2 <- SM_df_tidy %>% mutate(VSM = replace(VSM, VSM < 0, NA)) %>% mutate(VSM = replace(VSM, VSM > 0.57, NA))

#plot - use scale_color_discrete to only display proper depths, edit titles if necessary
ggplot(SM_df_tidy2, aes(DateTime, VSM)) +
  geom_line(aes(color = Depth)) +
  labs(title = "T38 Chambers Spring", x = "Date", y = "Soil Moisture m3/m3 VWC") +
  ylim(0, 0.6) +
  scale_color_discrete(breaks = c("Port_1", "Port_2"), labels = c("1 ft", "3 ft"), name = "Sensor Depth")


#Other statistics
SM_df_tidy2$Day <- as.Date(SM_df_tidy2$DateTime)

Daily_VSM <- SM_df_tidy2 %>%
  group_by(Day, Depth) %>%
  summarise(Ave_VSM = mean(VSM))

write.csv(Daily_VSM, "G:/Shared drives/UGS_Groundwater/Projects/WRI/Tintic/Soil Moisture/RExports//T38ChambersDailyAveVSM.csv")

##monthly mean 
SM_df_tidy2$Month_Yr <- format(as.Date(SM_df_tidy2$Date), "%Y-%m")

Monthly_Mean <- SM_df_tidy2 %>%
  group_by(Month_Yr, Depth) %>%
  summarise(mean_VSM = mean(VSM), sd_VSM = sd(VSM))

##Monthly mean plot
ggplot(Monthly_Mean, aes(Month_Yr, mean_VSM)) +
  geom_point(aes(color = Depth)) +
  labs(title = "T38 Chambers Spring Monthly Average", x = "Month", y = "Average VSM (m3/m3)") +
  scale_color_discrete(breaks = c("Port_1", "Port_2")) +
  theme(axis.text.x = element_text(angle=45))

##Monthly mean plot with error bars
ggplot(Monthly_Mean, aes(Month_Yr, mean_VSM)) +
  geom_point(aes(color = Depth)) +
  geom_errorbar(aes(ymin = mean_VSM - sd_VSM, ymax = mean_VSM + sd_VSM), width=.2,
                position=position_dodge(0.05)) +
  labs(title = "T38 Chambers Spring Monthly Average", x = "Month", y = "Average VSM (m3/m3)") +
  scale_color_discrete(breaks = c("Port_1", "Port_2")) +
  theme(axis.text.x = element_text(angle=45))