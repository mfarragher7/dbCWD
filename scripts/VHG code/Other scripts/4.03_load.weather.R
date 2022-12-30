#weather station data
#2020-12-09

#libraries
library(dplyr)
library(plyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggpubr)






#TO-DO
#make figs nicer obv
#load surface par here too?






# LOAD DATA ############
weather = read.csv("dbBuoy/highfreqNPS/4200iSIC_2020_UTC.csv",header=T)
str(weather)
#cut NA
weather = weather %>% filter(DateTime > "06-05-2020 19:30:00")
summary(weather)
#split datetime
weather = weather %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
weather$DateTime = mdy_hms(weather$DateTime)
weather$Date = as.Date(weather$Date, format="%m-%d-%Y")
str(weather)

#daily averages
dailyweather = ddply(weather, .(Date), summarize,
                     air_temp_c_mean=mean(air_temp_c), air_temp_c_sd=sd(air_temp_c),
                     precip_mm_mean=mean(precip_mm), precip_mm_sd=sd(precip_mm),
                     wind_speed_mph_mean=NA, wind_speed_mph_sd=NA,
                     baro_pressure_mmhg_mean=mean(baro_pressure_mmhg), baro_pressure_mmhg_sd=sd(baro_pressure_mmhg))

#get avg wind speed from mean of min/max                    
dates = dailyweather$Date
#mean wind speed
for (i in 1:length(dates)){ #for each date,
  temp = weather[weather$Date == dates[i], ] #temp dataframe that subsets weather for Date i
  temp$temp_mean_windspeed = ((temp$min_wind_speed_mph + temp$max_wind_speed_mph) / 2) #average together min and max wind speeds for each datetime
  mean_windspeed = mean(temp$temp_mean_windspeed) #average of all mean_windspeeds for Date i
  sd_windspeed = sd(temp$temp_mean_windspeed) #sd of all mean_windspeeds
  dailyweather[i,6] = mean_windspeed[1] #paste avg wind speed for date i in col 6
  dailyweather[i,7] = sd_windspeed[1] } #paste sd wind speed for date i in col 7


# FIGURES ############
airtemp = 
  ggplot(dailyweather,aes(x=as.Date(Date),y=air_temp_c_mean)) +
  geom_point() +
  #geom_errorbar(aes(ymin=chl_mean-chl_sd,ymax=chl_mean+chl_sd),width=5) +
  geom_line(linetype=3) +
  scale_x_date(breaks='1 month',date_labels="%d-%b") +
  labs(title='Air Temperature (C)',x='Date',y='Air Temp C') +
  theme_classic(); airtemp


precip =
  ggplot(dailyweather,aes(x=as.Date(Date),y=precip_mm_mean)) +
  geom_point() +
  #geom_errorbar(aes(ymin=chl_mean-chl_sd,ymax=chl_mean+chl_sd),width=5) +
  geom_line(linetype=3) +
  scale_x_date(breaks='1 month',date_labels="%d-%b") +
  labs(title='Precipitation (mm)',x='Date',y='Precip (mm)') +
  theme_classic(); precip


windsp =   
  ggplot(dailyweather,aes(x=as.Date(Date),y=wind_speed_mph_mean)) +
  geom_point() +
  #geom_errorbar(aes(ymin=chl_mean-chl_sd,ymax=chl_mean+chl_sd),width=5) +
  geom_line(linetype=3) +
  scale_x_date(breaks='1 month',date_labels="%d-%b") +
  labs(title='Mean Wind Speed (mph)',x='Date',y='Wind Speed (mm)') +
  theme_classic(); windsp


baro = 
  ggplot(dailyweather,aes(x=as.Date(Date),y=baro_pressure_mmhg_mean)) +
  geom_point() +
  #geom_errorbar(aes(ymin=chl_mean-chl_sd,ymax=chl_mean+chl_sd),width=5) +
  geom_line(linetype=3) +
  scale_x_date(breaks='1 month',date_labels="%d-%b") +
  labs(title='Barometric Pressure (mmHg)',x='Date',y='Barometric Pressure (mm)') +
  theme_classic(); windsp


p.weather = ggarrange(airtemp,precip,windsp,baro,nrow=4,ncol=1); p.weather




















