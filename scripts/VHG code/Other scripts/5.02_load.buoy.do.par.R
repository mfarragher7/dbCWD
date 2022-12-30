#Load DO and PAR buoy data
#updated 2020-12-01

#load libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)




#DO ####
#JP 
#deployed Feb21 @ 15:00, download Jun13 @ 11:30, download again Oct12 at 2pm.
#DO top
jp.top = read.delim('dbBuoy/buoyJP/DOtop/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
jp.top = jp.top %>%  
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>% #UTC dateTime isn't the correct one
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='JPtop') %>% #add ID column
  slice(-1,) #delete 'units' row
#DO bottom
jp.bot = read.delim('dbBuoy/buoyJP/DObottom/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
jp.bot = jp.bot %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='JPbottom') %>% 
  slice(-1,) #delete 'units' row
#combine
jp.do = rbind(jp.top,jp.bot)
str(jp.do)
jp.do$DOmgpl = as.numeric(jp.do$DOmgpl)
jp.do$DOsat = as.numeric(jp.do$DOsat)
jp.do$Temp = as.numeric(jp.do$Temp)
#remove data download times, split Date and Time
jp.do = jp.do %>% 
  mutate(DOsat=replace(DOsat,(DateTime>=ymd_hms("2020-06-13 10:00:00")&DateTime<=ymd_hms("2020-06-13 16:00:00")),NA)) %>% 
  mutate(DOmgpl=replace(DOmgpl,(DateTime>=ymd_hms("2020-06-13 10:00:00")&DateTime<=ymd_hms("2020-06-13 16:00:00")),NA))
#trim leading whitespace
jp.do$DateTime = trimws(jp.do$DateTime, "l")
#split into date and time
jp.do = jp.do %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
jp.do$Date = as.Date(jp.do$Date)
str(jp.do)


#SC
#deployed Feb21 @ 11:00, download Jun14 @ 14:00, download again Oct15 @ 11
#DO top
sc.top = read.delim('dbBuoy/buoySC/DOtop/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
sc.top = sc.top %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='SCtop') %>% 
  slice(-1,) 
#DO bottom
sc.bot = read.delim('dbBuoy/buoySC/DObottom/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
sc.bot = sc.bot %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='SCbottom') %>% 
  slice(-1,) 
#combine
sc.do = rbind(sc.top,sc.bot)
str(sc.do)
sc.do$DOmgpl = as.numeric(sc.do$DOmgpl)
sc.do$DOsat = as.numeric(sc.do$DOsat)
sc.do$Temp = as.numeric(sc.do$Temp)
#remove download times
sc.do = sc.do %>% 
  mutate(DOsat=replace(DOsat,(DateTime>=ymd_hms("2020-06-14 12:00:00")&DateTime<=ymd_hms("2020-06-14 20:00:00")),NA)) %>% 
  mutate(DOmgpl=replace(DOmgpl,(DateTime>=ymd_hms("2020-06-14 12:00:00")&DateTime<=ymd_hms("2020-06-14 20:00:00")),NA))
#trim leading whitespace
sc.do$DateTime = trimws(sc.do$DateTime, "l")
#split into date and time
sc.do = sc.do %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
sc.do$Date = as.Date(sc.do$Date)
str(sc.do)



#WH
#deployed Feb25 @ 15:00, download Jun13 @ 16:00
#DO top
wh.top = read.delim('dbBuoy/buoyWH/DOtop/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
wh.top = wh.top %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='WHtop') %>% 
  slice(-1,) 
#DO bottom
wh.bot = read.delim('dbBuoy/buoyWH/DObottom/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
wh.bot = wh.bot %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='WHbottom') %>% 
  slice(-1,) 
#combine
wh.do = rbind(wh.top,wh.bot)
str(wh.do)
wh.do$DOmgpl = as.numeric(wh.do$DOmgpl)
wh.do$DOsat = as.numeric(wh.do$DOsat)
wh.do$Temp = as.numeric(wh.do$Temp)
#remove download times
wh.do = wh.do %>% 
  mutate(DOsat=replace(DOsat,(DateTime>=ymd_hms("2020-06-13 14:00:00")&DateTime<=ymd_hms("2020-06-13 22:00:00")),NA)) %>% 
  mutate(DOmgpl=replace(DOmgpl,(DateTime>=ymd_hms("2020-06-13 14:00:00")&DateTime<=ymd_hms("2020-06-13 22:00:00")),NA)) 
#trim leading whitespace
wh.do$DateTime = trimws(wh.do$DateTime, "l")
#split into date and time
wh.do = wh.do %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
wh.do$Date = as.Date(wh.do$Date)
str(wh.do)



#BB
bb.do = read.csv('sampling/buoyBB/Bubb_DO_full.csv')
bb.do = bb.do %>% 
  select(-Lake,-MIDAS,-ID)
#separate top
bb.do.top = bb.do %>% 
  select(-2,-5,-6,-7,-8,-9,-10,-11,-12,-13) %>% 
  dplyr::rename('DOmgpl'='DO_1m') %>% 
  dplyr::rename('Temp'='Temp_1m') %>% 
  mutate(ID ='BBtop')
#separate middle
bb.do.mid = bb.do %>% 
  select(-2,-3,-4,-5,-6,-9,-10,-11,-12,-13) %>% 
  dplyr::rename('DOmgpl'='DO_7m') %>% 
  dplyr::rename('Temp'='Temp_7m') %>% 
  mutate(ID ='BBmid')
#separate top
bb.do.bot = bb.do %>% 
  select(-2,-3,-4,-5,-6,-7,-8,-9,-10,-13) %>% 
  dplyr::rename('DOmgpl'='DO_11m') %>% 
  dplyr::rename('Temp'='Temp_11m') %>% 
  mutate(ID ='BBbot')
#merge
bb.do2 = rbind(bb.do.top,bb.do.mid,bb.do.bot)








#### PAR ###############################################################

#JP
jp.par = read.delim('dbBuoy/buoyJP/PAR/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
jp.par = jp.par %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Acceleration.X,-Acceleration.Y,-Acceleration.Z) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='JPpar') %>% 
  slice(-1,)
#coerce
str(jp.par)
jp.par$PAR = as.numeric(jp.par$PAR)
jp.par$Temp = as.numeric(jp.par$Temp)
#remove data download times
jp.par = jp.par %>%
  mutate(PAR=replace(PAR,(DateTime>=ymd_hms("2020-06-13 10:00:00")&DateTime<=ymd_hms("2020-06-13 16:00:00")),NA))
#trim leading whitespace
jp.par$DateTime = trimws(jp.par$DateTime, "l")
#split into date and time
jp.par = jp.par %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
jp.par$Date = as.Date(jp.par$Date)
str(jp.par)
#label 'before/after demonic intrusion'\
jp.par = jp.par %>% mutate(BDI = ifelse(DateTime >= "2020-06-25 18:00:00",'y','n'))
#select only time range 10am - 2pm
#jp.par = jp.par %>% mutate(PAR=replace(PAR,Time < "10:00:00" | Time > "14:00:00",NA))


#SC
sc.par = read.delim('dbBuoy/buoySC/PAR/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
sc.par = sc.par %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Acceleration.X,-Acceleration.Y,-Acceleration.Z) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='SCpar') %>% 
  slice(-1,)
#coerce
str(sc.par)
sc.par$PAR = as.numeric(sc.par$PAR)
sc.par$Temp = as.numeric(sc.par$Temp)
#remove download times
sc.par = sc.par %>%
  mutate(PAR=replace(PAR,(DateTime>=ymd_hms("2020-06-14 12:00:00")&DateTime<=ymd_hms("2020-06-14 20:00:00")),NA)) 
#trim leading whitespace
sc.par$DateTime = trimws(sc.par$DateTime, "l")
#split into date and time
sc.par = sc.par %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
sc.par$Date = as.Date(sc.par$Date)
str(sc.par)
#select only time range 10am - 2pm
#sc.par = sc.par %>% mutate(PAR=replace(PAR,Time < "10:00:00" | Time > "14:00:00",NA))


#WH
wh.par = read.delim('dbBuoy/buoyWH/PAR/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
wh.par = wh.par %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Acceleration.X,-Acceleration.Y,-Acceleration.Z) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='SCpar') %>% 
  slice(-1,)
#coerce
str(wh.par)
wh.par$PAR = as.numeric(wh.par$PAR)
wh.par$Temp = as.numeric(wh.par$Temp)
#remove download times
wh.par = wh.par %>% 
  mutate(PAR=replace(PAR,(DateTime>=ymd_hms("2020-06-13 14:00:00")&DateTime<=ymd_hms("2020-06-13 22:00:00")),NA))
#trim leading whitespace
wh.par$DateTime = trimws(wh.par$DateTime, "l")
#split into date and time
wh.par = wh.par %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
wh.par$Date = as.Date(wh.par$Date)
str(wh.par)
#select only time range 10am - 2pm
#wh.par = wh.par %>% mutate(PAR=replace(PAR,Time < "10:00:00" | Time > "14:00:00",NA))








