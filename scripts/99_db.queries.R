#database queries
#created 10-01-2023


#libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)





#PLEASANT ########
#1985 in particular

#tp from DEP
tp.dep = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/tp.dep.csv',header=T)
tpdep.pleas = tp.dep %>% 
  filter(lake=='pleasant' & year==1985)

#chl from DEP
chl.dep = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/chlorophyll.dep.csv',header=T)
chldeppleas = chl.dep %>% 
  filter(lake=='pleasant' & year==1985)


#survey
surveydep = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/survey.dep.csv',h=T)
sdeppleas = surveydep %>% 
  filter(lake=='pleasant' & year==1985)


#profiles
profilesdep = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/profiles.dep.csv',h=T)
prodeppleas = profilesdep %>% 
  filter(lake=='pleasant' & year==1985)

























#DB Q's #########
#dbCWD queries :)

dexterp = phos %>% filter(lake=='dexter')
dpro = profiles %>% filter(lake=='dexter')
dg = general %>% filter(lake=='dexter')

mg = general %>% filter(lake=='maranacook' & year==1996)
mpro = profiles %>% filter(lake=='maranacook' & year==1996)

wilsp = phos %>% filter(lake=='wilson' & year==2011)

chg = general %>% filter(lake=='cochnewagon' & year==1988)
chpro = profiles %>% filter(lake=='cochnewagon' & year==1988)
chphos = phos %>% filter(lake=='cochnewagon' & year==1988)
chchl = chl %>% filter(lake=='cochnewagon' & year==1988)


ag = general %>% filter(lake=='annabessacook' & year==1987)
apro = profiles %>% filter(lake=='annabessacook' & year==1987)
aphos = phos %>% filter(lake=='annabessacook' & year==1987)
achl = chl %>% filter(lake=='annabessacook' & year==1987)


cg = general %>% filter(lake=='cobbossee')
cpro = profiles %>% filter(lake=='cobbossee' & year==1979)
cphos = phos %>% filter(lake=='cobbossee' & year==1979)
cchl = chl %>% filter(lake=='cobbossee' & year==1979)





#wilson pond TP figure for layers
wtp = read.csv("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/dbCWD/tp.wilson2021.csv")
wtp$date = as.Date(wtp$date, format="%m/%d/%Y")
colnames(wtp) = c('date','0-6m','6-12m')

#melt
wtp1 = wtp %>% 
  pivot_longer(cols = c('0-6m','6-12m'),
               names_to = 'layer',
               values_to = 'tp.kg')

ggplot(wtp1, aes(x=date, y=tp.kg, color=layer)) +
  geom_point(size=4) +
  geom_line(linetype=2) +
  labs(title='Wilson Pond Total Phosphorus kilograms 2021',
       x='Date',
       y="TP (kg)",
       fill="Layer") +
  theme_bw()

#make layers and dates factors
wtp1$layer = factor(wtp1$layer, levels=c("6-12m","0-6m"))
wtp1 = wtp1 %>% mutate(dm = format(as.Date(date), "%d %b"))
ds = unique(wtp1$dm)
wtp1$dm = factor(wtp1$dm, levels=ds)

#normal dates, hypo on top
ggplot(wtp1, aes(x=date, y=tp.kg, fill=layer)) +
  geom_bar(position='stack',stat='identity') +
  # scale_y_reverse() + 
  labs(title='Wilson Pond TP 2021',
       x='Date',
       y="TP (kg)",
       fill="Layer") +
  theme_bw()

#factor dates
ggplot(wtp1, aes(x=dm, y=tp.kg, fill=layer)) +
  geom_bar(position='stack',stat='identity') +
  labs(title='Wilson Pond Total Phosphorus kilograms 2021',
       x='Date',
       y="TP (kg)",
       fill="Layer") +
  theme_bw()


#wilson pond alkalinity

walk = chem2 %>% filter(lake == 'wilson')
unique(walk$date) #2000 to 2020
summary(walk$alk)
str(walk)
walk$date = as.Date(walk$date)

ggplot(walk, aes(x=date, y=alk)) + 
  geom_point() +
  geom_line() +
  scale_x_date(date_labels='%Y')




#check dep db wilson alk
walkdep = chem %>% filter(lake == 'wilson')

wsub = walkdep %>% filter(year<2000)


ggplot(filter(year > 1989), aes(x=date, y=alk)) + 
  geom_point() +
  geom_line() +
  scale_x_date(date_labels='%Y')




















