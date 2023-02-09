#load phosphorus datas
#2023-01-24

#libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)


#load cwd data
tp9820 = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.cwd/cwd.tp.98-2020.csv',header=T)
tp21 = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.cwd/cwd.tp.2021.csv',header=T)
str(tp9820)
str(tp21)
tp21 = tp21 %>% select(-UNIT, -QUAL)
tp21$SAMPDATE = as.Date(tp21$SAMPDATE, format='%m/%d/%Y')
#combine
phos = rbind(tp21, tp9820)
sum(is.na(phos$TP_PPM))



#lowercase everything and rename cols
phos = phos %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(date=sampdate) %>% 
  dplyr::rename(station=basin) %>% 
  dplyr::rename(tp.ppm=tp_ppm)
  
#format date
phos$date = as.Date(phos$date)
length(unique(phos$date)) #number of unique sample dates

#rename lakes
unique(phos$lake)
#check midas first
phos = phos %>% mutate(midascheck = paste(midas,lake))
unique(phos$midascheck)
plyr::count(phos$lake)
plyr::count(phos$midascheck)

temp1 = plyr::count(phos$lake)

#rename
phos = phos %>% 
  mutate(lake = replace(lake, midas==9961,'annabessacook')) %>% 
  mutate(lake = replace(lake, midas==3828,'berry')) %>% 
  mutate(lake = replace(lake, midas==5242,'buker')) %>% 
  mutate(lake = replace(lake, midas==5310,'carlton')) %>% 
  mutate(lake = replace(lake, midas==5236,'cobbossee')) %>% 
  mutate(lake = replace(lake, midas==8065,'little cobbossee')) %>% 
  mutate(lake = replace(lake, midas==3814,'cochnewagon')) %>% 
  mutate(lake = replace(lake, midas==5265, 'desert')) %>%
  mutate(lake = replace(lake, midas==3830,'dexter')) %>% 
  mutate(lake = replace(lake, midas==5252,'horseshoe')) %>% 
  mutate(lake = replace(lake, midas==5304,'hutchinson')) %>% 
  mutate(lake = replace(lake, midas==5302,'jamies')) %>% 
  mutate(lake = replace(lake, midas==5244,'jimmy')) %>% 
  mutate(lake = replace(lake, midas==5316,'kezar')) %>%   
  mutate(lake = replace(lake, midas==5246,'loon')) %>% 
  mutate(lake = replace(lake, midas==5312,'maranacook')) %>% 
  mutate(lake = replace(lake, midas==98,'narrows upper')) %>% 
  mutate(lake = replace(lake, midas==103,'narrows lower')) %>% 
  mutate(lake = replace(lake, midas==5254,'pleasant')) %>% 
  mutate(lake = replace(lake, midas==5250,'little purgatory')) %>% 
  mutate(lake = replace(lake, midas==5238,'sand')) %>% 
  mutate(lake = replace(lake, midas==5300,'shed')) %>% 
  mutate(lake = replace(lake, midas==5307,'torsey')) %>% 
  mutate(lake = replace(lake, midas==3832,'wilson')) %>% 
  mutate(lake = replace(lake, midas==5240,'woodbury'))

temp2 = plyr::count(phos$lake)

sum(temp1$freq)
sum(temp2$freq)
unique(phos$lake)



#* QC ########
names(phos)
str(phos)
phos$station = as.numeric(phos$station)
unique(phos$type) 
phos = phos %>% mutate(type = na_if(type,''))
#depth
phos$depth = as.numeric(phos$depth)
unique(phos$depth)
plyr::count(phos$depth)
phos = phos %>% filter(depth <100) #6639 to 6523
#rep
unique(phos$r)
phos$r = as.numeric(phos$r)
#PhospHorus ppm
phos$tp.ppm = as.numeric(phos$tp.ppm)
sum(is.na(phos$tp.ppm)) #none..
plyr::count(phos$tp.ppm)
phos = phos %>% 
  mutate(tp.ppm = replace(tp.ppm, tp.ppm > 99, NA)) %>% # two 99s
  drop_na(tp.ppm)

summary(phos$tp.ppm)


# year and month
phos$year = lubridate::year(phos$date)
phos$month = lubridate::month(phos$date)
phos = phos %>% select(-midascheck)
phos = phos %>% mutate(sampID=paste(midas, station, date, agency, sep="_"))

#save

write.csv(phos, "C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/tp.cwd.1998-2021.csv", row.names = F)

