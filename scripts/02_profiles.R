# exploratory profile plots
#2022-12-01

#libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)
library(ggpubr)


pro = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/profiles.dep.summary.csv", header=T)


#format dates
pro = pro %>% mutate(dm = format(as.Date(date), "%m-%d"))
str(pro)

pro = pro %>% 
  mutate(mm = ifelse(month==1, 'Jan', NA)) %>%
  mutate(mm = replace(mm, month==2, 'Feb')) %>%
  mutate(mm = replace(mm, month==3, 'Mar')) %>%
  mutate(mm = replace(mm, month==4, 'Apr')) %>%
  mutate(mm = replace(mm, month==5, 'May')) %>%
  mutate(mm = replace(mm, month==6, 'Jun')) %>%
  mutate(mm = replace(mm, month==7, 'Jul')) %>%
  mutate(mm = replace(mm, month==8, 'Aug')) %>%
  mutate(mm = replace(mm, month==9, 'Sep')) %>% 
  mutate(mm = replace(mm, month==10, 'Oct')) %>%
  mutate(mm = replace(mm, month==11, 'Nov')) %>%
  mutate(mm = replace(mm, month==12, 'Dec'))
  
pro$mm = factor(pro$mm, levels=c('Jan',
                                 'Feb',
                                 'Mar',
                                 'Apr',
                                 'May',
                                 'Jun',
                                 'Jul',
                                 'Aug',
                                 'Sep',
                                 'Oct',
                                 'Nov',
                                 'Dec'))
str(pro)

#check for dups
temp = plyr::count(pro$sampleid)



#Wilson pond ########

wilspro = filter(pro, lake=='wilson' & (dm > '06-01' & dm < '09-15'))
str(wilspro)
wilspro$date = as.Date(wilspro$date)


#mean whole lake epi temperature in the summer
temp = plyr::count(wilspro$date)


#remove duplicate
wilspro = wilspro %>% filter(!sampleid=='3832_1_1988-06-02_ei')

#plot epi temps
ggplot(wilspro, aes(x=date, y=temp.epi)) + 
  geom_point() +
  geom_line(linetype=3) +
  facet_wrap(~mm, nrow=2, ncol=2, scales='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(limits=c(10,30)) +
  labs(title='Wilson Pond Epi temp')


#try with just max temp
ggplot(wilspro, aes(x=date, y=temp.max)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~mm, nrow=2, ncol=2, scales='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(limits=c(10,30)) +
  labs(title='Wilson Pond Max Temp')

#min temp
ggplot(wilspro, aes(x=date, y=temp.min)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~mm, nrow=2, ncol=2, scales='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(limits=c(0,15)) +
  labs(title='Wilson Pond Min Temp')

#plot thermocline depths
ggplot(wilspro, aes(x=date, y=thermo.depth)) + 
  geom_point() +
  facet_wrap(~mm, nrow=2, ncol=2, scales='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_reverse(limits=c(10,0)) +
  #scale_y_continuous(limits=c(10,30)) +
  labs(title='Wilson Pond Thermocline depth') 

#thermo, top and bottom of meta
wlayers = wilspro %>% 
  pivot_longer(cols = c(thermo.depth,meta.top,meta.bottom),
               names_to = 'layer',
               values_to = 'depth') %>% 
  mutate(layer=replace(layer,layer=='thermo.depth','Thermocline')) %>% 
  mutate(layer=replace(layer,layer=='meta.top','Meta top')) %>% 
  mutate(layer=replace(layer,layer=='meta.bottom','Meta bottom'))

wlayers$layer = factor(wlayers$layer, levels=c('Thermocline','Meta top','Meta bottom'))

#plot
ggplot(wlayers, aes(x=date, y=depth, color=layer)) +
  geom_point() +
  geom_line(linetype=3, show.legend=F) +
  facet_wrap(~mm, ncol=1, nrow=4, scale='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_reverse(limits=c(13,0)) +
  #scale_y_reverse(limits=c(12,0), n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Wilson Pond',
       x="Date",
       y="Depth (m)") 





#pleasant pond ######

ppp = filter(pro, lake=='pleasant' & (dm > '06-01' & dm < '09-15'))
str(ppp)
ppp$date = as.Date(ppp$date)

temp = plyr::count(ppp$sampleid)
temp = plyr::count(ppp$date)

unique(ppp$station)
ppp$station = factor(ppp$station, levels=c(1,2,3))
                                 



#mean whole lake epi temperature in the summer



#plot epi temps
ggplot(ppp, aes(x=date, y=temp.epi, color=station)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~mm, nrow=2, ncol=2, scales='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(limits=c(10,30)) +
  labs(title='Pleasant Pond Epi temp')


#try with just max temp
ggplot(ppp, aes(x=date, y=temp.max, color=station)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~mm, nrow=2, ncol=2, scales='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(limits=c(10,30)) +
  labs(title='Pleasant Pond Max Temp')

#min temp
ggplot(ppp, aes(x=date, y=temp.min, color=station)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~mm, nrow=2, ncol=2, scales='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(limits=c(0,15)) +
  labs(title='Pleasant Pond Min Temp')


#min and maxes
ppminmax = ppp %>% 
  pivot_longer(cols = c(temp.min, temp.max, temp.mean),
               names_to = 'temp.param',
               values_to = 'temp.value') 


ppminmax$temp.param = factor(ppminmax$temp.param, levels=c('temp.min', 'temp.mean', 'temp.max'))


#plot
ggplot(ppminmax, aes(x=date, y=temp.value, color=temp.param)) +
  geom_point() +
  #geom_line(linetype=3, show.legend=F) +
  stat_smooth(method="loess", size=0.75, se=F, show.legend=F) + 
  facet_wrap(~mm, ncol=1, nrow=4, scale='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Pleasant Pond',
       x="Date",
       y="Depth (m)") 




#epi and hypo
ppepihypo = ppp %>% 
  pivot_longer(cols = c(temp.epi, temp.hypo),
               names_to = 'temp.layer',
               values_to = 'temp.value') 

ppepihypo$temp.layer = factor(ppepihypo$temp.layer, 
                         levels=c('temp.epi','temp.hypo'))

ggplot(ppepihypo, aes(x=date, y=temp.value, color=temp.layer)) +
  geom_point() +
  #geom_line(linetype=3, show.legend=F) +
  stat_smooth(method="loess", size=0.75, se=F, show.legend=F) + 
  facet_wrap(~mm, ncol=1, nrow=4, scale='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_color_manual(values=c('red3','blue'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Pleasant Pond',
       x="Date",
       y="Depth (m)") 





#plot thermocline, meta depths
ggplot(filter(ppp, station==1),
       aes(x=date, y=thermo.depth)) + 
  geom_point() +
  facet_wrap(~mm, nrow=2, ncol=2, scales='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_reverse(limits=c(10,0)) +
  #scale_y_continuous(limits=c(10,30)) +
  labs(title='Pleasant Pond Thermocline depth') 



#thermo, top and bottom of meta
pplayers = ppp %>% 
  pivot_longer(cols = c(thermo.depth,meta.top,meta.bottom),
               names_to = 'layer',
               values_to = 'depth') %>% 
  mutate(layer=replace(layer,layer=='thermo.depth','Thermocline')) %>% 
  mutate(layer=replace(layer,layer=='meta.top','Meta top')) %>% 
  mutate(layer=replace(layer,layer=='meta.bottom','Meta bottom'))

pplayers$layer = factor(pplayers$layer, levels=c('Thermocline','Meta top','Meta bottom'))

#plot
ggplot(pplayers, aes(x=date, y=depth, color=layer)) +
  geom_point() +
  #geom_line(linetype=3, show.legend=F) +
  stat_smooth(method="loess", size=0.75, se=F, show.legend=F) + 
  facet_wrap(~mm, ncol=1, nrow=4, scale='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_reverse(limits=c(13,0)) +
  #scale_y_reverse(limits=c(12,0), n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Pleasant Pond',
       x="Date",
       y="Depth (m)") 




















       
       
       
       





