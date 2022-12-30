#Maine DEP Hobo data for RMN lakes
#load and manipulate data for figure generation in script 02
#created 2022-06-13


#Libraries ####
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)
library(zoo)
library(RCurl)



#load temp data
bb.hobo = read.csv("~/Ecology/MaineDEP/data/Bubble_temper_ALL.csv")
ls.hobo = read.csv("~/Ecology/MaineDEP/data/LSB_temper_ALL.csv")
wd.hobo = read.csv("~/Ecology/MaineDEP/data/Wood_temper_ALL.csv")
el.hobo = read.csv("~/Ecology/MaineDEP/data/Ellis_temper_ALL.csv")

#coerce date/time
bb.hobo = bb.hobo %>% 
  set_names(~ str_to_lower(.)) %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=F) %>% 
  mutate(datetime=as_datetime(datetime, format="%Y-%m-%d %H:%M:%S")) %>% 
  mutate(date=as.Date(date))

ls.hobo = ls.hobo %>% 
  set_names(~ str_to_lower(.)) %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=F) %>% 
  mutate(datetime=as_datetime(datetime, format="%Y-%m-%d %H:%M:%S")) %>% 
  mutate(date=as.Date(date))

wd.hobo = wd.hobo %>% 
  set_names(~ str_to_lower(.)) %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=F) %>% 
  mutate(datetime=as_datetime(datetime, format="%Y-%m-%d %H:%M:%S")) %>% 
  mutate(date=as.Date(date))

el.hobo = el.hobo %>% 
  set_names(~ str_to_lower(.)) %>%
  add_column(lake='Ellis') %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=F) %>% 
  mutate(datetime=as_datetime(datetime, format="%Y-%m-%d %H:%M:%S")) %>% 
  mutate(date=as.Date(date)) %>% 
  add_column(midas=4086)

str(bb.hobo)
str(ls.hobo)
str(wd.hobo)
str(el.hobo)



#combine all lakes

#transpose for ggplot
bbl = bb.hobo %>% 
  pivot_longer(cols=starts_with('wtr'), names_to="depth", values_to="temp") %>% 
  mutate(depth = str_replace(depth, "wtr_", "")) %>% 
  mutate(depth=as.numeric(depth)) 
str(bbl)

lsl = ls.hobo %>% 
  pivot_longer(cols=starts_with('wtr'), names_to="depth", values_to="temp") %>% 
  mutate(depth = str_replace(depth, "wtr_", "")) %>% 
  mutate(depth=as.numeric(depth))
str(lsl)

ell = el.hobo %>% 
  pivot_longer(cols=starts_with('wtr'), names_to="depth", values_to="temp") %>% 
  mutate(depth = str_replace(depth, "wtr_", "")) %>% 
  mutate(depth=as.numeric(depth)) 
str(ell)    

wdl = wd.hobo %>% 
  pivot_longer(cols=starts_with('wtr'), names_to="depth", values_to="temp") %>% 
  mutate(depth = str_replace(depth, "wtr_", "")) %>% 
  mutate(depth=as.numeric(depth))
str(wdl)

#combine
lakes = rbind(bbl, ell, lsl, wdl)
plyr::count(is.na(lakes$temp)) #261229 NA for temp
lakes.na = lakes #save df with NAs for interpolation
lakes = na.omit(lakes) #dropped only NA temps
str(lakes)

#add ID cols, and cols to fill in
lakes = lakes %>% 
  mutate(lakedate=paste(lake, date, sep="_")) %>% 
  mutate(lakehr=paste(lake, date, time, sep="_"))







#Total summary ########
#summarize entire water column, ignore depths

#* hourly ####

lakes.hr = ddply(lakes, .(lakehr, lake, date, time), summarize,
                 temp.min=min(temp), 
                 temp.max=max(temp), 
                 temp.mean=mean(temp), 
                 temp.sd=sd(temp), 
                 thermo.depth = NA,
                 meta.top=NA,
                 meta.bottom=NA,
                 schmidt=NA)


#get thermocline depth for each hour
length(unique(lakes.hr$lakehr))
lh = unique(lakes.hr$lakehr)

for (i in 1:length(lh)){ #for every unique lakehr,
  td = lakes[lakes$lakehr == lh[i], ] #temp dataframe that subsets lakes df by each lakehour
  td = td[!duplicated(td$depth), ] #remove duplicate depths
  thermo = thermo.depth(td$temp, td$depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  lakes.hr[i,9] = thermo[1] }    #paste thermocline depth in 10th column of row i*j == i in lakes.hr df


#get meta depths
#test
test = lakes[lakes$lakehr == 'Bubble_2019-08-01_12:00:00', ] 
meta.depths(test$temp, test$depth, slope=0.1, seasonal=F)

for (i in 1:length(lh)){ #for every unique lakehr,
  td = lakes[lakes$lakehr == lh[i], ] #temp dataframe that subsets lakes df by each lakehour
  td = td[!duplicated(td$depth), ] #remove duplicate depths
  meta = meta.depths(td$temp, td$depth, slope=0.1, seasonal=F, mixed.cutoff=1) 
  lakes.hr[i,10] = meta[1] #top
  lakes.hr[i,11] = meta[2] #bottom
}    


#save dataframe
write.csv(lakes.hr, "library/summary.hourly.temp.csv", row.names=FALSE)


#test plot
ggplot(lakes.hr, aes(x=date, y=thermo.depth)) +
  geom_point(shape=1, alpha=0.25) + 
  # stat_smooth(method="loess", size=1, span=0.35, se=F, show.legend=F) +
  facet_wrap(~lake, nrow=3, ncol=2, scales='free') +
  # scale_x_date(limits=as.Date(c('2021-04-01','2021-12-01')), date_labels="%b") +
  scale_y_reverse() +
  labs(x='Date', y='Thermocline Depth (m)')







#* daily ######
#get daily averages from hourly summary

lakes.d = ddply(lakes.hr, .(lake, date), summarize,  
                #temp - min, max, mean, sd, thermocline depth, meta top and bottom, schidt stability
                temp.min = min(temp.min), 
                temp.max = max(temp.max), 
                temp.mean = mean(temp.mean), 
                temp.sd = sd(temp.mean), 
                thermo.depth = mean(thermo.depth),
                meta.top = mean(meta.top),
                meta.bottom = mean(meta.bottom),
                shmidt = NA) 

#pull out year
lakes.d$year = lubridate::year(lakes.d$date)
#get julian date
lakes.d$jday = yday(lakes.d$date)



# save daily temp
write.csv(lakes.d, "library/summary.daily.temp.csv", row.names=FALSE)



#test plots
ggplot(filter(lakes.d, lake=='Bubble'),
       aes(x=date, y=thermo.depth)) +
  geom_point(shape=1, alpha=0.25) + 
  # stat_smooth(method="loess", size=1, span=0.35, se=F, show.legend=F) +
  facet_wrap(~year, nrow=3, ncol=2, scales='free') +
  # scale_x_date(limits=as.Date(c('2021-04-01','2021-12-01')), date_labels="%b") +
  scale_y_reverse() +
  labs(x='Date', y='Thermocline Depth (m)')



#make meta top and bottom factors
lakes.meta = lakes.d %>% 
  pivot_longer(cols=c(meta.top,meta.bottom,thermo.depth),
               names_to="layers", values_to="depth")

lakes.meta$meta = as.factor(lakes.meta$meta)


#BUBBLE
ggplot(filter(lakes.meta, lake=='Bubble'), 
             aes(x=jday, y=depth, color=layers)) +
  geom_point(shape=19, alpha=0.75) + 
  facet_wrap(~year, nrow=3, ncol=2, scales='free') +
  scale_x_continuous(limits=c(0,365),
                     breaks=c(32, 121, 213, 305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  # scale_x_date(date_labels="%b") +
  scale_y_reverse(limits=c(12,0), n.breaks=4) +
  scale_color_manual(limits=c("meta.top.d", "meta.bottom.d",'thermo.depth.d'),
                     labels=c("Meta Top", "Meta Bottom", 'Thermocline'),
                     values=c("turquoise1", 'coral', 'gray30')) +
  labs(title='Bubble Pond',
       x='Date', 
       y='Metalimnion depths (m)') +
  theme_bw() +
  theme(title=element_text(size=10),
        legend.title=element_blank())


#ELLIS
ggplot(filter(lakes.meta, lake=='Ellis'), 
       aes(x=jday, y=depth, color=layers)) +
  geom_point(shape=19, alpha=0.75) + 
  facet_wrap(~year, nrow=3, ncol=2, scales='free') +
  scale_x_continuous(limits=c(0,365),
                     breaks=c(32, 121, 213, 305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  # scale_x_date(date_labels="%b") +
  scale_y_reverse(limits=c(12,0), n.breaks=4) +
  scale_color_manual(limits=c("meta.top.d", "meta.bottom.d",'thermo.depth.d'),
                     labels=c("Meta Top", "Meta Bottom", 'Thermocline'),
                     values=c("turquoise1", 'coral', 'gray30')) +
  labs(title='Ellis Pond',
       x='Date', 
       y='Metalimnion depths (m)') +
  theme_bw() +
  theme(title=element_text(size=10),
        legend.title=element_blank())


#LSB
ggplot(filter(lakes.meta, lake=='Lower South Branch'), 
       aes(x=jday, y=depth, color=layers)) +
  geom_point(shape=19, alpha=0.75) + 
  facet_wrap(~year, nrow=3, ncol=2, scales='free') +
  scale_x_continuous(limits=c(5,365),
                     breaks=c(32, 121, 213, 305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  # scale_x_date(date_labels="%b") +
  scale_y_reverse(limits=c(18,0), n.breaks=6) +
  scale_color_manual(limits=c("meta.top.d", "meta.bottom.d",'thermo.depth.d'),
                     labels=c("Meta Top", "Meta Bottom", 'Thermocline'),
                     values=c("turquoise1", 'coral', 'gray30')) +
  labs(title='Lower South Branch Pond',
       x='Date', 
       y='Metalimnion depths (m)') +
  theme_bw() +
  theme(title=element_text(size=10),
        legend.title=element_blank())






#Depth summary ########
#* daily ####

#get daily averages for each depth
daily.temp = ddply(lakes, .(lake, date, depth), summarize, 
                   temp.mean = mean(temp))

#pull out year and julian day
daily.temp$year = lubridate::year(daily.temp$date)
daily.temp$jday = yday(daily.temp$date)


write.csv(daily.temp, 'library/summary.daily.depth.temp.csv',row.names=F)


#* interpolation ######
#interpolated depth values using zoo package

#drop last NA, then interpolate
lakes.int = lakes.na %>% 
  slice(-n()) %>% 
  mutate(temp.int = na.approx(temp))

#get daily values for each depth
daily.temp.int = ddply(lakes.int, .(lake, date, depth), summarize, 
                       temp.mean = mean(temp.int))
#year and jday
daily.temp.int$year = lubridate::year(daily.temp.int$date)
daily.temp.int$jday = yday(daily.temp.int$date)


write.csv(daily.temp.int, 'library/summary.daily.depth.temp.int.csv',row.names=F)







#Dissolved Oxygen ########
#load DO data 

#Bubble
bb.do = read.csv("~/Ecology/MaineDEP/data/Bubble_DO_ALL.csv")
names(bb.do)
bb.do = bb.do %>% 
  mutate(Lake=replace(Lake, grepl('Bubble',Lake), 'Bubble'))
#coerce dates
bb.do = bb.do %>%
  set_names(~ str_to_lower(.)) %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=F) %>% 
  mutate(datetime=as_datetime(datetime, format="%Y-%m-%d %H:%M:%S")) %>% 
  mutate(date=as.Date(date))
#longer
bb.do2 = bb.do %>% 
  select(lake,midas,datetime,date,time,do_01m,do_07m,do_11m) %>% 
  pivot_longer(cols=starts_with('do'), names_to="depth", values_to="do.mg") %>% 
  mutate(depth=replace(depth,grepl('01',depth),1)) %>% 
  mutate(depth=replace(depth,grepl('07',depth),7)) %>% 
  mutate(depth=replace(depth,grepl('11',depth),11)) %>% 
  mutate(depth=as.numeric(depth)) 
str(bb.do2)


#LSB
lsb.do = read.csv("~/Ecology/MaineDEP/data/LSB_DO_ALL.csv")
names(lsb.do)
lsb.do$lake = 'Lower South Branch'
lsb.do$midas = 4222
#coerce dates
lsb.do = lsb.do %>%
  set_names(~ str_to_lower(.)) %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=F) %>% 
  mutate(datetime=as_datetime(datetime, format="%Y-%m-%d %H:%M:%S")) %>% 
  mutate(date=as.Date(date))
#longer
lsb.do2 = lsb.do %>% 
  select(lake,midas,datetime,date,time,do_01m,do_10m,do_17m) %>% 
  pivot_longer(cols=starts_with('do'), names_to="depth", values_to="do.mg") %>% 
  mutate(depth=replace(depth,grepl('01',depth),1)) %>% 
  mutate(depth=replace(depth,grepl('10',depth),10)) %>% 
  mutate(depth=replace(depth,grepl('17',depth),17)) %>% 
  mutate(depth=as.numeric(depth)) 
str(lsb.do2)


#Combine
do = rbind(bb.do2, lsb.do2)
plyr::count(is.na(do$do.mg))
do = na.omit(do)
str(do)

#add ID cols, and cols to fill in
do = do %>% 
  mutate(lakedate=paste(lake, date, sep="_")) %>% 
  mutate(lakehr=paste(lake, date, time, sep="_"))

write.csv(do, 'library/do.combined.csv')

#DO summary ######
#daily summary
do.daily = ddply(do, .(lake,date,depth), summarize,
                 do.mean = mean(do.mg))
#pull out year and jday
do.daily$year = lubridate::year(do.daily$date)
do.daily$jday = lubridate::yday(do.daily$date)


write.csv(do.daily, 'library/summary.do.daily.csv',row.names=F)






#Weather #######
#using NOAA dataset
#Greenville station for Ellis
#Millinocket for LSB
#Bar Harbor for Bubble and Wood

w = read.csv("~/Ecology/MaineDEP/data/Weather_2018-2022.csv")
str(w)
names(w)

#AWND - avg daily wind speed m/s
#PGTM - peak gust time
#PRCP - precipitation mm
#PSUN - Daily percent of possible sunshine
#TAVG - temp avg C
#TMAX - temp max c
#TMIN - temp min c
#TSUN - daily total sunshine, minutes
#WDF2 - direction of fastest 2 min wind (degrees)
#WDF5 - 
#WSF2 - fastest wind speeds
#WSF5 - 
#WTs - various other weather types. see documentation, fog, hail, ice, etc

wr = w %>% select(NAME,DATE,AWND,PRCP,TAVG,TMIN,TMAX)
colnames(wr) = c('name','date','wind.mean','precip','temp.mean','temp.min','temp.max')
wr = wr %>% 
  mutate(lake=ifelse(grepl('MILLINOCKET',name),'Lower South Branch',NA)) %>% 
  mutate(lake=replace(lake,grepl('GREENVILLE',name),'Ellis')) %>% 
  mutate(lake=replace(lake,grepl('MCFARLAND',name),'Bubble and Wood')) 

#pull out year
wr$year = lubridate::year(wr$date)
#get julian date
wr$jday = yday(wr$date)


#save
write.csv(wr, "library/weather.csv", row.names=F)


#pull out wind speed for Bubble and Wood. From Bar Harbor station
#https://www.ndbc.noaa.gov/histsearch.php?station=atgm1&year=2018&f1=wspd&t1a=gt&v1a=0&t1b=gt&v1b=0&c1=&f2=&t2a=&v2a=&t2b=&v2b=&c2=&f3=&t3a=&v3a=&t3b=&v3b=
#no 2022 data available

wind21 = read.delim("~/Ecology/MaineDEP/data/wind2021.txt", sep=' ', skip=3)
colnames(wind21) = c('year','month','day','hour','min','wind.dir','X','wind.speed')
wind21 = wind21 %>% select(year,month,day,wind.speed)
wind21 = wind21 %>% mutate(date=paste(year, month, day, sep='-'))
wind21$date = as.Date(wind21$date)

wind20 = read.delim("~/Ecology/MaineDEP/data/wind2020.txt", sep=' ', skip=3)
colnames(wind20) = c('year','month','day','X','X1','X2','X3','wind.speed')
wind20 = wind20 %>% select(year,month,day,wind.speed)
wind20 = wind20 %>% mutate(date=paste(year, month, day, sep='-'))
wind20$date = as.Date(wind20$date)

wind19 = read.delim("~/Ecology/MaineDEP/data/wind2019.txt", sep=' ', skip=3)
colnames(wind19) = c('year','month','day','X','X1','X2','X3','wind.speed')
wind19 = wind19 %>% select(year,month,day,wind.speed)
wind19 = wind19 %>% mutate(date=paste(year, month, day, sep='-'))
wind19$date = as.Date(wind19$date)

wind18 = read.delim("~/Ecology/MaineDEP/data/wind2018.txt", sep=' ', skip=3)
colnames(wind18) = c('year','month','day','X','X1','X2','X3','wind.speed')
wind18 = wind18 %>% select(year,month,day,wind.speed)
wind18 = wind18 %>% mutate(date=paste(year, month, day, sep='-'))
wind18$date = as.Date(wind18$date)

#combine
bbwd.wind = rbind(wind18,wind19,wind20,wind21)
str(bbwd.wind)
#get daily averages
bbwd.summary = bbwd.wind %>% 
  group_by(date) %>% 
  summarise(wind=mean(wind.speed,na.rm=T))
  
#pull out year
bbwd.summary$year = lubridate::year(bbwd.summary$date)
#get julian date
bbwd.summary$jday = yday(bbwd.summary$date)

write.csv(bbwd.summary, "library/bb.wd.wind.csv", row.names=F)








#Water Level #####################
#load water level data











