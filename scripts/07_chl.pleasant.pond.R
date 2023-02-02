#work through chl for pleasant pond
#created 2023-02-02-02-02

#load libraries
library(plyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(ggplot2)

#load tp #####
chl = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/chl.1974-2021.csv',header=T)
chl = chl %>% filter(lake=='pleasant')
chl = chl %>% filter(station==1)
chl = chl %>% filter(type=='c')
length(unique(chl$sampID))
length(unique(chl$date))
check.date = plyr::count(chl$date)
plyr::count(chl$r)
#a few reps, a few dep/cwd dups or side by side sampling, a bunch of duplicated data. 
#average by date should solve that

chlr = ddply(chl, .(sampID, station, date, agency, depth), summarize,
             chl = mean(chla))

length(unique(chlr$sampID))
length(unique(chlr$date))
check.date = plyr::count(chlr$date)

#2013-08-21 pick cwd value
chlr = chlr %>% 
  mutate(chl = replace(chl, (date=='2013-08-21' & agency=='ei'), NA)) %>% 
  filter(!is.na(chl)) %>% 
  mutate(date = as.Date(date))



#plot all sample dates
ggplot(chlr, aes(x=date, y=chl)) + 
  geom_point() +
  scale_x_date(date_labels="%Y")


#a mess!
#get yearly mean
chlr$year = lubridate::year(chlr$date)
chlyr = ddply(chlr, .(year), summarize, 
              n = length(unique(sampID)), 
              chl.mean = mean(chl),
              chl.min = min(chl),
              chl.max = max(chl), 
              chl.sd = sd(chl))




#plot yearly mean
temp = chlyr %>% 
  tidyr::pivot_longer(
    cols = c(chl.min, chl.mean, chl.max), 
    names_to = 'Value',
    values_to = 'chl.val') %>% 
  mutate(Value = replace(Value, grepl('chl.min',Value),'Min')) %>% 
  mutate(Value = replace(Value, grepl('chl.mean',Value),'Mean')) %>% 
  mutate(Value = replace(Value, grepl('chl.max',Value),'Max'))


ggplot(temp, aes(x=year, y=chl.val, color=Value)) + 
  geom_point() + 
  stat_smooth(method="loess", 
              linewidth=0.5,
              se=T, 
              show.legend=F,
              span = 0.9,
              alpha = 0.1) + 
  scale_x_continuous(limits=c(1999.5,2022)) +
  scale_color_manual(values=c('red','blue','green')) +
  labs(title='Chlorophyll concentration 2000-2021',
       y='Chl a (ug/L)',
       x='Date') +
  theme_bw() +
  theme(title=element_text(size=10))





mean(chlyr$chl.mean)
sd(chlyr$chl.mean)

mean(chlyr$chl.max)

gr8r = chlr %>% filter(chl > 8, year>1999)





         