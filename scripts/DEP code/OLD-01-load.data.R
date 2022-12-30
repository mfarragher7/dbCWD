#Maine DEP Hobo data
#created 2022-06-13


#libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)
library(zoo)


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
#lakes = na.omit(lakes) #dropped only NA temps
str(lakes)

#add ID cols, and cols to fill in
lakes = lakes %>% 
  mutate(lakedate=paste(lake, date, sep="_")) %>% 
  mutate(lakehr=paste(lake, date, time, sep="_"))





#SUMMARIZE ########

#hourly summary

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


#meta depths
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



p1 = ggplot(lakes.hr, aes(x=date, y=thermo.depth)) +
  geom_point(shape=1, alpha=0.25) + 
  # stat_smooth(method="loess", size=1, span=0.35, se=F, show.legend=F) +
  facet_wrap(~lake, nrow=2, ncol=2, scales='free') +
  # scale_x_date(limits=as.Date(c('2021-04-01','2021-12-01')), date_labels="%b") +
  scale_y_reverse() +
  labs(x='Date', y='Thermocline Depth (m)')
p1



theme_bw() + 
  theme(strip.background=element_rect(fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.title=element_blank()); p1





#Daily ######
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




p2 = ggplot(filter(lakes.d, lake=='Bubble'),
            aes(x=date, y=thermo.depth.d)) +
  geom_point(shape=1, alpha=0.25) + 
  # stat_smooth(method="loess", size=1, span=0.35, se=F, show.legend=F) +
  facet_wrap(~year, nrow=2, ncol=2, scales='free') +
  # scale_x_date(limits=as.Date(c('2021-04-01','2021-12-01')), date_labels="%b") +
  scale_y_reverse() +
  labs(x='Date', y='Thermocline Depth (m)')
p2





#Meta plots ######

#plot ideas. separate years for each lake

#make meta top and bottom factors
lakes.meta = lakes.d %>% 
  pivot_longer(cols=c(meta.top.d,meta.bottom.d,thermo.depth.d),
               names_to="layers", values_to="depth")

lakes.meta$meta = as.factor(lakes.meta$meta)


#BUBBLE
bb1 = ggplot(filter(lakes.meta, lake=='Bubble'), 
             aes(x=jday, y=depth, color=layers)) +
  geom_point(shape=19, alpha=0.75) + 
  facet_wrap(~year, nrow=2, ncol=2, scales='free') +
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
        legend.title=element_blank()); bb1


#ELLIS
el1 = ggplot(filter(lakes.meta, lake=='Ellis'), 
             aes(x=jday, y=depth, color=layers)) +
  geom_point(shape=19, alpha=0.75) + 
  facet_wrap(~year, nrow=2, ncol=2, scales='free') +
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
        legend.title=element_blank()); el1



#LSB
ls1 = ggplot(filter(lakes.meta, lake=='Lower South Branch'), 
             aes(x=jday, y=depth, color=layers)) +
  geom_point(shape=19, alpha=0.75) + 
  facet_wrap(~year, nrow=2, ncol=2, scales='free') +
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
        legend.title=element_blank()); ls1








#Thermocline only

p1 = ggplot(lakes.d, aes(x=date, y=thermo.depth)) +
  geom_point(shape=1, alpha=0.5) + 
  # stat_smooth(method="loess", size=1, span=0.35, se=F, show.legend=F) +
  facet_wrap(~lake, nrow=2, ncol=2, scales='free') +
  # scale_x_date(limits=as.Date(c('2021-04-01','2021-12-01')), date_labels="%b") +
  scale_y_reverse() +
  #scale_color_manual(labels=c("Power consumed", "Power generated"), values=c("red", "blue")) +
  labs(x='Date',
       y='Thermocline Depth (m)') +
  #guides(color=guide_legend(override.aes=list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(strip.background=element_rect(fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.title=element_blank()); p1



ggsave(plot=p1, 
       filename="figures/complete.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)




#temp profiles

#Depth values ########
#get daily averages for each depth
daily.temp = ddply(lakes, .(lake, date, depth), summarize, 
                   temp.mean = mean(temp))



daily.temp$year = lubridate::year(daily.temp$date)
daily.temp$jday = yday(daily.temp$date)


write.csv(daily.temp, 'library/summary.daily.depth.temp.csv',row.names=F)






#interpolated depth values using zoo package

#drop last NA, then interpolate
lakes.int = lakes %>% 
  slice(-n()) %>% 
  mutate(temp.int = na.approx(temp))



daily.temp.int = ddply(lakes.int, .(lake, date, depth), summarize, 
                   temp.mean = mean(temp.int))



daily.temp.int$year = lubridate::year(daily.temp.int$date)
daily.temp.int$jday = yday(daily.temp.int$date)


write.csv(daily.temp.int, 'library/summary.daily.depth.temp.int.csv',row.names=F)

  
  
  
  
  





cols = c('#2E0769','#07766F','#59A64D','#E9CE28','#F94343')

str(daily.temp)


#BuBBle
ggplot(filter(daily.temp, lake=='Bubble'),
       aes(x=jday,
           y=depth,
           color=temp.mean)) +
  geom_tile() +
  facet_wrap(~year,ncol=2,nrow=2) +
  scale_y_reverse(limits=c(12,0), n.breaks=4) +
  scale_x_continuous(limits=c(0,365),
                     breaks=c(32, 121, 213, 305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  scale_color_gradientn(colours=cols,
                        limits=c(0,30),
                        na.value="white") +
  labs(title='Bubble Pond',
       color="Temp C",
       x="Date",
       y="Depth (m)") +
  theme_bw() +
  theme(title=element_text(size=10),
        legend.title=element_blank())



#Wood
ggplot(filter(daily.temp, lake=='Wood'),
       aes(x=jday,
           y=depth,
           color=temp.mean)) +
  geom_tile() +
  facet_wrap(~year,ncol=2,nrow=2) +
  scale_y_reverse(limits=c(3.5,0)) +
  scale_x_continuous(limits=c(0,365),
                     breaks=c(32, 121, 213, 305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  scale_color_gradientn(colours=cols,
                        limits=c(0,30),
                        na.value="white") +
  labs(title='Wood Pond',
       color="Temp C",
       x="Date",
       y="Depth (m)") +
  theme_bw() +
  theme(title=element_text(size=10),
        legend.title=element_blank())


#LSB
ggplot(filter(daily.temp, lake=='Lower South Branch'),
       aes(x=jday,
           y=depth,
           color=temp.mean)) +
  geom_tile() +
  facet_wrap(~year,ncol=2,nrow=2) +
  scale_y_reverse(limits=c(18,0)) +
  scale_x_continuous(limits=c(0,365),
                     breaks=c(32, 121, 213, 305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  scale_color_gradientn(colours=cols,
                        limits=c(0,30),
                        na.value="white") +
  labs(title='LSB Pond',
       color="Temp C",
       x="Date",
       y="Depth (m)") +
  theme_bw() +
  theme(title=element_text(size=10),
        legend.title=element_blank())


#Ellis
ggplot(filter(daily.temp, lake=='Ellis'),
       aes(x=jday,
           y=depth,
           color=temp.mean)) +
  geom_tile() +
  facet_wrap(~year,ncol=2,nrow=2) +
  scale_y_reverse(limits=c(12,0), n.breaks=4) +
  scale_x_continuous(limits=c(0,365),
                     breaks=c(32, 121, 213, 305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  scale_color_gradientn(colours=cols,
                        limits=c(0,30),
                        na.value="white") +
  labs(title='Ellis Pond',
       color="Temp C",
       x="Date",
       y="Depth (m)") +
  theme_bw() +
  theme(title=element_text(size=10),
        legend.title=element_blank())








#try with individual lakes

# Bubble ###############

#transpose for ggplot
bbl = bb.hobo %>% 
  pivot_longer(cols=starts_with('wtr'), names_to="depth", values_to="temp") %>% 
  mutate(depth = str_replace(depth, "wtr_", "")) %>% 
  mutate(depth=as.numeric(depth)) 
str(bbl)

#drop NA temps
bbl = na.omit(bbl) 

#add ID cols, and cols to fill in
bbl = bbl %>% 
  mutate(lakedate=paste(lake, date, sep="_")) %>% 
  mutate(lakehr=paste(lake, date, time, sep="_")) %>% 
  mutate(thermo.depth = NA) %>% 
  mutate(meta.top = NA) %>% 
  mutate(meta.bottom = NA) %>% 
  mutate(schmidt = NA)

length(unique(bbl$lakehr))
lh = unique(bbl$lakehr)


for (i in 1:length(lh)){ #for every unique lakehr,
  td = bbl[bbl$lakehr == lh[i], ] #temp dataframe that subsets lakes df by each lakehour
  thermo = thermo.depth(td$temp, td$depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  lakes[i,10] = thermo[1] }    #paste thermocline depth in 10th column of row i*j == i 



#hourly
hr = ddply(lakes, .(lakehr, lake, date, time), summarize,
           thermo.depth = thermo.depth)











#basic figs


#LakeAnalyzer figs ######

#heatmap test
el = el.hobo %>% 
  select(-date, -time, -midas, -lake) %>% 
  rename_with(~paste0(., ".0"), starts_with('wtr')) %>% 
  rename('wtr_10.5' = 'wtr_10.5.0')

str(el)
wtr.heat.map(el)






#Dissolved Oxygen ########

#load DO data 

#Bubble
bb.do = read.csv("~/Ecology/MaineDEP/data/Bubble_DO_ALL.csv")
names(bb.do)
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



#daily summary
do.daily = ddply(do, .(lake,date,depth), summarize,
                 do.mean = mean(do.mg))

#pull out year
do.daily$year = lubridate::year(do.daily$date)
#get julian date
do.daily$jday = lubridate::yday(do.daily$date)


write.csv(do.daily, 'library/summary.do.daily.csv',row.names=F)





#load water level data














#OLD STUFF ####

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
lakes = na.omit(lakes) #dropped only NA temps
str(lakes)



#not sure my laptop can handle 1 mil rows so doing individual lakes first....


#add lakedate ID column, lakehour ID column....
#....and other stuff to fill in from rLakeAnalyzer with for loops

#hourly values
lakes = lakes %>% 
  mutate(lakedate=paste(lake, date, sep="_")) %>% 
  mutate(lakehr=paste(lake, date, time, sep="_")) %>% 
  mutate(thermo.depth = NA) %>% 
  mutate(meta.top = NA) %>% 
  mutate(meta.bottom = NA) %>% 
  mutate(schmidt = NA)
#check
length(unique(lakes$lakehr))
lh = unique(lakes$lakehr)


# get thermocline depths for each hour
test = lakes[lakes$lakehr == 'Bubble_2019-07-03_16:00:00', ] 
thermo.depth(test$temp, test$depth, seasonal=F, index=F, mixed.cutoff=1)




for (i in 1:length(lh)){ #for every unique lakehr,
  td = lakes[lakes$lakehr == lh[i], ] #temp dataframe that subsets lakes df by each lakehour
  thermo = thermo.depth(td$temp, td$depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  lakes[i,10] = thermo[1] }    #paste thermocline depth in 10th column of row i*j == i 



#hourly
hr = ddply(lakes, .(lakehr, lake, date, time), summarize,
           thermo.depth = thermo.depth)




#daily summary ############
daily = ddply(lakes, .(lakehr, lake, date), summarize,  
              #temp - min, max, mean, sd, thermocline depth, meta top and bottom, schidt stability
              temp.min=min(temp), 
              temp.max=max(temp), 
              temp.mean=mean(temp), 
              temp.sd=sd(temp), 
              thermo.depth=mean(thermo.depth),
              thermo.min=min(thermo.depth), 
              thermo.max=max(thermo.depth), 
              thermo.sd=sd(thermo.depth), 
              temp.sd=sd(thermo.depth), 
              meta.top=NA,
              meta.bottom=NA,
              schmidt=NA)





