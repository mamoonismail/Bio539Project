# BIO539 project
# Real case groundwater data (quality, water levels, abstractions)
# Data exists originally in Excel format (separate files)
# Dates was separated into Year, Month, and Day
############################################
# Load library
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(scales)
install.packages("ggthemes")
library(ggthemes)
############################################
# Read data files
quality <- read.csv("data/AmmQuality.csv", stringsAsFactors = FALSE)
waterlevel <- read.csv("data/AmmWaterlevel.csv", stringsAsFactors = FALSE)
waterabstr <- read.csv("data/AmmAbstraction.csv", stringsAsFactors = FALSE, check.names=F)
############################################

#########################################################################################
####                      =====> QUALITY DATA ANALYSIS <=====                    ####
#########################################################################################

# guality dataset has 91511 opservations and 10 variables (add inline commands for number of rows and cols)
glimpse(quality)

# clean our data set by removing na's
qualitycleaned <- quality %>% filter(!is.na(Value), Station.ID != "", Parameter != "")

# The parametres that have been analyzed and the number of the analysis carried out for each is:
# it shows that Electrical conuctivity and pH are the highest parametres analyzed (make it plot)
par_count <- qualitycleaned %>% 
  group_by(Parameter) %>%
  summarise(analysis_count = n()) %>%
  arrange(desc(analysis_count))
# PLOT
ggbarplot(par_count, x = "Parameter", y = "analysis_count", fill = "steelblue", 
          title = "Number of Analysis Carried") +
          theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("figs/Number_of_Analysis.png", width = 6, dpi=300)


### SOME SUMMARY STATISTICS
#Summarize for annual mean, max, min, and number of analysis for (Nitrate) for each well
#then we will exclude those who has less than 2 analysis per year, this is very important summary table.

qualitycleaned %>%
  group_by(Station.ID, Year, Parameter) %>%
  summarise(analysis_count = n(),
            mean = mean(Value),
            min = min(Value),
            max = max(Value)) %>%
  filter(analysis_count > 2, Parameter == "Nitrate") %>%
  arrange(desc(max))

# This is a summary for EC as above for Nitrate
qualitycleaned %>%
  group_by(Station.ID, Year, Parameter) %>%
  summarise(analysis_count = n(),
            mean = mean(Value),
            min = min(Value),
            max = max(Value)) %>%
  filter(analysis_count > 2, Parameter == "Electrical Conductivity") %>%
  arrange(desc(max))


# This is to check the above command
qualitycleaned%>% select(Station.ID, Year, Parameter)%>%
  filter(Parameter == "Nitrate", Station.ID == "AL2691", Year == 1997)

# We are interested more in looking at Electrical conductivity and Nitarte, since they are a primary
# keys for drinking water suitability
Ec_Nitrate <- qualitycleaned %>%
  filter(Parameter == "Electrical Conductivity" | Parameter == "Nitrate")


###                 ---------------------- EC ANALYSIS ----------------------


# Assuming that EC analysis is conducting monthly and we are interested in wells that have more than 5
# years of record (which means exclude those have less than 60 analysis record)\\\ plot this
EC_count <- Ec_Nitrate %>%
  filter(Parameter == "Electrical Conductivity") %>%
  group_by(Station.ID) %>%
  summarise(analysis_count = n())%>%
  filter(analysis_count > 60)%>%
  arrange(desc(analysis_count))
# PLOT
ggbarplot(EC_count, x = "Station.ID", y = "analysis_count", fill = "green", 
          title = "Number of EC Analysis For Each Well") +
          theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("figs/Number_of_EC_Analysis_Well.png", width = 6, dpi=300)


# now we found our target wells, let us understand their analysis results
# calculate the average EC for each well
# let us first calculate the avearge EC for all wells then we will join both tables (EC_count & EC_mean)
EC_mean <- Ec_Nitrate%>%
  group_by(Station.ID, Parameter)%>%
  summarise(meanEC = mean(Value, na.rm=TRUE))%>%
  filter(Parameter == "Electrical Conductivity")%>%
  arrange(desc(meanEC))


# let us join the two tables (EC_count & EC_mean) to find the mean EC for the target wells\\ plot this
# plot the threshold line (1500) those below the threshold are suitable for drinking
EC_join <- left_join(EC_count, EC_mean)
#PLOT
ggbarplot(EC_join, x = "Station.ID", y = "meanEC", fill = "grey", 
          title = "Average EC For Each Well") +
          theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_hline (yintercept = 1500, linetype = "dashed", color = "red", size = 1)
ggsave("figs/Average_EC_Analysis_Well.png", width = 6, dpi=300)



# Let us draw an EC time series for our target wells
# 1- join target wells file (EC_count) with the time series analysis table
EC_all <- qualitycleaned %>%
  filter(Parameter == "Electrical Conductivity")
EC_joinseries <- left_join(EC_count, EC_all)
# 2- combine date
EC_joinseries$date <- ymd(paste(EC_joinseries$Year,EC_joinseries$Month,EC_joinseries$Day,sep="-"))
# 3- Draw a facet for all the wells
ggplot(EC_joinseries, aes(x=date, y=Value)) +
  geom_line()+facet_wrap(~Station.ID) + theme_cleveland() +
  theme(panel.grid = element_blank()) + xlab('Date') + ylab('EC (Us/cm)') +
  theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5))
ggsave("figs/Facet_EC_Analysis_AllWell.png", width = 6, dpi=300)


# Let us plot the time series of the well that has the longest record of EC analysis
# 1- find the well with the longest EC record
qualitycleaned %>%
  filter(Parameter == "Electrical Conductivity") %>%
  group_by(Station.ID) %>%
  summarise(analysis_count = n())%>%
  arrange(desc(analysis_count))%>%
  filter(analysis_count == max(analysis_count))
# 2- write the record of that well into an object  
LongestEC <- qualitycleaned %>% select(Station.ID, Parameter, Year, Month, Day, Value) %>%
  filter(Station.ID == 'AL2691', Parameter == 'Electrical Conductivity') %>%
  arrange(Year)
# 3- combine date
LongestEC$date <- ymd(paste(LongestEC$Year,LongestEC$Month,LongestEC$Day,sep="-"))
# 4- ggplot to see general pattern, trend, and outliers and to customize
ggplot(LongestEC, aes(date, Value)) +
  geom_point(color="blue", size=2, shape=19) +
  ggtitle("Electrical Conductivity for Well AL2691 (1995-2005)") +
  xlab("Date") + ylab("EC (Us/cm)") +
  (scale_x_date(breaks=date_breaks("2 years"), labels=date_format("%b %y"))) +
  theme_cleveland() + geom_smooth(method = "lm")
ggsave("figs/EC_Timeseries_AL2691.png", width = 6, dpi=300)



###                 ---------------------- NO3 ANALYSIS ----------------------

# FOR Nitarte
# Assuming that NO3 analysis is conducting monthly and we are interested in wells that have more than 5
# years of record (which means exclude those have less than 60 analysis record)\\\ plot this
N_count <- Ec_Nitrate %>%
  filter(Parameter == "Nitrate") %>%
  group_by(Station.ID) %>%
  summarise(analysis_count = n())%>%
  filter(analysis_count > 60)%>%
  arrange(desc(analysis_count))
# PLOT
ggbarplot(N_count, x = "Station.ID", y = "analysis_count", fill = "green", 
          title = "Number of Nitrate Analysis For Each Well") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("figs/Number_of_N_Analysis_Well.png", width = 6, dpi=300)


# now we found our target wells, let us understand their analysis results
# calculate the average NO3 for each well
# let us first calculate the avearge NO3 for all wells then we will join both tables (N_count & N_mean)
N_mean <- Ec_Nitrate%>%
  group_by(Station.ID, Parameter)%>%
  summarise(mean_N = mean(Value, na.rm=TRUE))%>%
  filter(Parameter == "Nitrate")%>%
  arrange(desc(mean_N))


# let us join the two tables (N_count & N_mean) to find the mean NO3 for the target wells\\ plot this
N_join <- left_join(N_count, N_mean)

# EPA standard for Nitrate in drinking water is 10 ppm. while the Jordanian permissible
# level is 70 ppm. 5 wells have average N greater than 70 and not adequate for drinking
# and 14 wells have average N equal or less than 70 and are adequate for drinking.
# based on the EPA standards onle 1 well has No3 below standsrd and the rest are above the permissible level
N_join %>% filter (mean_N > 70) %>% tally()
N_join %>% filter (mean_N <= 70) %>% tally()
N_join %>% filter (mean_N <= 10) %>% tally()
#PLOT
ggbarplot(N_join, x = "Station.ID", y = "mean_N", fill = "grey", 
          title = "Average Nitrate For Each Well") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_hline (yintercept = 70, linetype = "dashed", color = "red", size = 1) +
  geom_hline (yintercept = 10, linetype = "dashed", color = "blue", size = 1)
ggsave("figs/Average_N_Analysis_Well.png", width = 6, dpi=300)


# Let us draw an N time series for our target wells
# 1- join target wells file (N_count) with the time series analysis table
N_all <- qualitycleaned %>%
  filter(Parameter == "Nitrate")
N_joinseries <- left_join(N_count, N_all)
# 2- combine date
N_joinseries$date <- ymd(paste(N_joinseries$Year,N_joinseries$Month,N_joinseries$Day,sep="-"))
# 3- Draw a facet for all the wells
ggplot(N_joinseries, aes(x=date, y=Value)) +
  geom_line()+facet_wrap(~Station.ID) + theme_cleveland() +
  theme(panel.grid = element_blank()) + xlab('Date') + ylab('NO3 (ppm)') +
  theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5))
ggsave("figs/Facet_N_Analysis_AllWell.png", width = 6, dpi=300)


# Let us plot the time series of the well that has the longest record of NO3 analysis
# 1- find the well with the longest EC record
N_joinseries %>%
  group_by(Station.ID) %>%
  summarise(analysis_count = n())%>%
  arrange(desc(analysis_count))%>%
  filter(analysis_count == max(analysis_count))
# 2- write the record of that well into an object  
LongestN <- N_joinseries %>% select(Station.ID, Parameter, Value, date) %>%
  filter(Station.ID == 'AL2691') %>%
  arrange(date)
# 3- ggplot to see general pattern, trend, and outliers and to customize
ggplot(LongestN, aes(date, Value)) +
  geom_point(color="blue", size=2, shape=19 ) +
  ggtitle("Nitrate for Well AL2691 (1995-2005)") +
  xlab("Date") + ylab("NO3 (ppm)") +
  (scale_x_date(breaks=date_breaks("2 years"), labels=date_format("%b %y"))) +
  theme_cleveland() + geom_smooth(method = "lm")
ggsave("figs/N_Timeseries_AL2691.png", width = 6, dpi=300)


# NO3 time series for any other well just change well ID
N_joinseries %>% filter (Station.ID == "AL2715") %>%
  ggplot(aes(x=date, y=Value)) +
  geom_point(color="blue", size=2, shape=19 ) +
  ggtitle("Nitrate for Well AL2715 (1985-2005)") +
  xlab('Date') + ylab('NO3 (ppm)') +
  (scale_x_date(breaks=date_breaks("2 years"), labels=date_format("%b %y"))) +
  theme_cleveland() + geom_smooth(method = "lm")
ggsave("figs/N_Timeseries_AL2715.png", width = 6, dpi=300)

#########################################################################################
####                      =====> WATER_LEVEL DATA ANALYSIS <=====                    ####
#########################################################################################

# Water-Level dataset has 11,353 opservations and 13 variables (add inline commands for number of rows and cols)
glimpse(waterlevel)

# clean our data set by removing na's
wlcleaned <- waterlevel %>% filter(!is.na(DEPTH_TO_WATER), STATION_ID != "")


# Assuming that water-level is measured monthly and we are interested in wells that have more than 10
# years of record (which means exclude those have less than 120 water-level records)\\\ plot this
# Initially there are 84 wells monitored for water level, and 36 after applying the filtering 
wl_count <- wlcleaned %>%
  group_by(STATION_ID) %>%
  summarise(analysis_count = n())%>%
  filter(analysis_count > 120)

# Let us have a quick look on the water-level data
wlcleaned %>% 
  group_by(STATION_ID) %>%
  summarise(minwl = min(DEPTH_TO_WATER, na.rm=TRUE),
            maxwl = max(DEPTH_TO_WATER, na.rm=TRUE),
            meanwl = mean(DEPTH_TO_WATER, na.rm=TRUE))

# PLOT
ggbarplot(wl_count, x = "STATION_ID", y = "analysis_count", fill = "green", 
          title = "Number of Water-level Measurements For Each Well") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("figs/Number_of_Water-level Measurements_Well.png", width = 6, dpi=300)


# Let us draw an water-level time series for our target wells
# 1- join target wells file (wl_count) with the time series measurements table
wl_join <- left_join(wl_count, wlcleaned)
# 2- combine date
wl_join$date <- ymd(paste(wl_join$Year,wl_join$Month,wl_join$Day,sep="-"))
# 3- Draw a facet for all the wells
ggplot(wl_join, aes(x=date, y=DEPTH_TO_WATER)) +
  geom_line()+facet_wrap(~STATION_ID) + theme_cleveland() +
  theme(panel.grid = element_blank()) + xlab('Date') + ylab('Depth to water (m)') +
  theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5)) +
  scale_y_reverse()
ggsave("figs/Facet_WL_AllWell.png", width = 6, dpi=300)


# Let us plot the time series of the well that has the longest record of WL measurements
# 1- find the well with the longest WL record
wlcleaned %>%
  group_by(STATION_ID) %>%
  summarise(analysis_count = n())%>%
  arrange(desc(analysis_count))%>%
  filter(analysis_count == max(analysis_count))
# 2- write the record of that well into an object  
WL_longest <- wlcleaned %>% select(STATION_ID, DEPTH_TO_WATER, WATER_LEVEL, Year, Month, Day) %>%
  filter(STATION_ID == 'AL1043') %>%
  arrange(Year)
# 3- combine date
WL_longest$date <- ymd(paste(WL_longest$Year,WL_longest$Month,WL_longest$Day,sep="-"))
# 4- ggplot to see general pattern, trend, and outliers and to customize
ggplot(WL_longest, aes(date, DEPTH_TO_WATER)) +
  geom_point(color="blue", size=2, shape=19 ) +
  ggtitle("Water-Level for Well AL1043 (1968-2016)") +
  xlab('Date') + ylab('Depth to Water (m)') +
  (scale_x_date(breaks=date_breaks("5 years"), position = "top", labels=date_format("%b %y"))) +
  theme_cleveland() + geom_smooth() +
  scale_y_reverse()
ggsave("figs/WL_Timeseries_AL1043.png", width = 6, dpi=300)

# 5- ggplot for any other wells
## AL1926
wlcleaned$date <- ymd(paste(wlcleaned$Year,wlcleaned$Month,wlcleaned$Day,sep="-"))
wlcleaned %>% filter (STATION_ID == "AL1926") %>%
  ggplot(aes(x=date, y=DEPTH_TO_WATER)) +
  geom_point(color="blue", size=2, shape=19 ) +
  ggtitle("Water-Level for Well AL1926") +
  xlab('Date') + ylab('Depth to Water (m)') +
  (scale_x_date(breaks=date_breaks("5 years"), position = "top", labels=date_format("%b %y"))) +
  theme_cleveland() + geom_smooth() + scale_y_reverse()
## AL1040
wlcleaned$date <- ymd(paste(wlcleaned$Year,wlcleaned$Month,wlcleaned$Day,sep="-"))
wlcleaned %>% filter (STATION_ID == "AL1043") %>%
  ggplot(aes(x=date, y=DEPTH_TO_WATER)) +
  geom_point(color="blue", size=2, shape=19 ) +
  ggtitle("Water-Level for Well AL1043") +
  xlab('Date') + ylab('Depth to Water (m)') +
  (scale_x_date(breaks=date_breaks("5 years"), position = "top", labels=date_format("%b %y"))) +
  theme_cleveland() + geom_smooth() + scale_y_reverse()

### Make a subset of the plot (this could not be knitted??)
sset <- subset(wlcleaned, date > as.Date("2007-1-1"))

sset %>% filter (STATION_ID == "AL1043") %>%
  ggplot(aes(x = date, y = DEPTH_TO_WATER))+
  geom_line(color = "#00AFBB", size = 2)+
  ggtitle("Water-Level for Well AL1043") +
  xlab('Date') + ylab('Depth to Water (m)') +
  (scale_x_date(breaks=date_breaks("1 years"), position = "top", labels=date_format("%b %y"))) +
  theme_cleveland() + geom_smooth() + scale_y_reverse()
ggsave("figs/WL_subset_AL1043.png", width = 6, dpi=300)


## plot changes in water levels (this could not be knitted??)
WL_change <- wlcleaned %>%
  group_by(STATION_ID) %>% mutate(change = DEPTH_TO_WATER - lag(DEPTH_TO_WATER)) %>%
  select (STATION_ID, date, DEPTH_TO_WATER, change)


ggbarplot(WL_change, x = "date", y = "change", fill = "green", 
          title = "water level change",xlab = "Date", ylab = "change(m)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


ggplot(data = WL_change, aes(x = date, y = change))+
  geom_line(color = "#00AFBB", size = 2)

WL_change %>% filter (STATION_ID == "AL1043", !is.na(change)) %>%
  ggplot(aes(x=date, y=change, na.rm = TRUE)) +
  geom_point(color="blue", size=2, shape=19 ) +
  ggtitle("Water-Level changes for Well AL1926") +
  xlab('Date') + ylab('Water-Level changes (m)') +
  (scale_x_date(breaks=date_breaks("5 years"),labels=date_format("%b %y"))) +
  theme_cleveland() + geom_smooth()
ggsave("figs/WL_changes_AL1043.png", width = 6, dpi=300)




#########################################################################################
####                   =====> ABSTRACTIONS DATA ANALYSIS <=====                      ####
#########################################################################################



# Waterabstr dataset is spreaded and needs to be in gathered and cleaned
abstr_gather <- waterabstr %>% gather('1995':'2013', key = "year", value = "abstraction", na.rm = TRUE, convert = TRUE)

#the file has  14,534 opservations and 4 variables (add inline commands for number of rows and cols)
glimpse(abstr_gather)

# Let us calculate the annual abstraction for all wells, number of pumping wells,
# and summary statistics
annual_abstr <- abstr_gather %>% 
  group_by(year) %>%
  summarise(wells_count = n(),
            minabstr = min(abstraction, na.rm=TRUE),
            maxabstr = max(abstraction, na.rm=TRUE),
            meanabstr = mean(abstraction, na.rm=TRUE),
            sumabstr = sum(abstraction, na.rm=TRUE))
# Add a column for abstractions in MCM
annual_abstrMCM <- annual_abstr %>% mutate(abstrMCM = sumabstr/1000000)

# Plot annual abstractions and assign it to variable
abstraction <- ggbarplot(annual_abstrMCM, x = "year", y = "abstrMCM", fill = "green", 
          title = "Annual Abstraction Rates (MCM)",xlab = "Date", ylab = "Abstractions(mcm)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + rotate()

# Plot number of abstraction wells over time and assign to a variable
abstr_wells <- ggbarplot(annual_abstrMCM, x = "year", y = "wells_count", fill = "green", 
          title = "Number of Abstraction Wells Yearly", xlab = "Date", ylab = "Number of Wells") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + rotate()

# combine both two plots
combo_plot <- grid.arrange(abstraction,abstr_wells, 
                           ncol=2, widths=c(4,6))
ggsave("figs/combined abstraction wells.png", combo_plot, width = 10, dpi=300)


# Plot the relationship between number of abstraction wells and their abstraction rates
ggscatter(annual_abstrMCM, x= "wells_count", y= "abstrMCM", color="blue", size=2, shape=19,
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of wells", ylab = "Abstraction rates (MCM)")
Corr <- cor.test(annual_abstrMCM$wells_count,annual_abstrMCM$abstrMCM)

ggsave("figs/Scatter abstraction wells.png", width = 8, dpi=300)




