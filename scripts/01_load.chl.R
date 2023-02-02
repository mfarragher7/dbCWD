# chlorophyll
#load and merge chl data from dep, cwd db, cwd 2021, etc etc
#created 2023-02-02


#libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)



#DEP ####################
chl = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.dep/CWD_CHLORO.csv",header=T)
str(chl)

#lowercase everything and rename cols
chl = chl %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(lake=laknam) %>% 
  dplyr::rename(datetime=sampdate) %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=T) %>% 
  mutate(date = as.Date(date))

length(unique(chl$date)) #number of unique sample dates

#rename lakes
unique(chl$lake)
#check midas first
chl = chl %>% mutate(midascheck = paste(midas,lake))
unique(chl$midascheck)
plyr::count(chl$lake)
plyr::count(chl$midascheck)
temp1 = plyr::count(chl$lake)
#rename
chl = chl %>% 
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

temp2 = plyr::count(chl$lake)
sum(temp1$freq)
sum(temp2$freq)
unique(chl$lake)


#* QC ########
names(chl)
str(chl)
unique(chl$agency)
unique(chl$project)
unique(chl$depth)
chl$depth = as.numeric(chl$depth)
unique(chl$type)
chl = chl %>% mutate(type = na_if(type,'')) #NA where blank
#chla
unique(chl$chla)
chl$chla = as.numeric(chl$chla)
summary(chl$chla) 
unique(chl$r) # d??? might mean "DEP" for lab
chl = chl %>% mutate(r = na_if(r,'')) #NA where blank
unique(chl$l)
unique(chl$flag) #empty
chl = chl %>% select(-midascheck,-flag,-l,-time,-project)
chl = chl %>% mutate(db='dep')
chl = chl %>% mutate(sampID=paste(midas, station, date, agency, sep="_"))





#CWD 19-20 ##############
chl.cwd = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.cwd/cwd.chl.98-2020.csv",header=T)


#lowercase everything and rename cols
chl.cwd = chl.cwd %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(station=basin) %>% 
  dplyr::rename(date=sampdate) %>% 
  mutate(date = as.Date(date))
length(unique(chl.cwd$date)) #number of unique sample dates

#rename lakes
unique(chl.cwd$lake)
#check midas first
chl.cwd = chl.cwd %>% mutate(midascheck = paste(midas,lake))
unique(chl.cwd$midascheck)
plyr::count(chl.cwd$lake)
plyr::count(chl.cwd$midascheck)
temp1 = plyr::count(chl.cwd$lake)
#rename
chl.cwd = chl.cwd %>% 
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
temp2 = plyr::count(chl.cwd$lake)
sum(temp1$freq)
sum(temp2$freq)
unique(chl.cwd$lake)



#* QC ########
names(chl.cwd)
str(chl.cwd)
unique(chl.cwd$agency)
unique(chl.cwd$project)
#depth
unique(chl.cwd$depth)
chl.cwd$depth = as.numeric(chl.cwd$depth)
chl.cwd = chl.cwd %>% filter(depth <50)
unique(chl.cwd$type)
#chl.cwda
unique(chl.cwd$chla)
chl.cwd$chla = as.numeric(chl.cwd$chla)
summary(chl.cwd$chla) 
chl.cwd = chl.cwd %>% filter(depth <90)
unique(chl.cwd$r) # d??? might mean "DEP" for lab
chl.cwd = chl.cwd %>% select(-midascheck)
chl.cwd = chl.cwd %>% mutate(db='cwd')
chl.cwd = chl.cwd %>% mutate(sampID=paste(midas, station, date, agency, sep="_"))



#db comparison ###########
check.cwd = chl.cwd %>% filter(date > '1998-01-01' & date < '2018-12-31')
check.dep = chl %>% filter(date > '1998-01-01' & date < '2018-12-31')
cwdID = unique(check.cwd$sampID)
depID = unique(check.dep$sampID)
#identify IDs that exist in cwd db only
cwdonly = setdiff(cwdID, depID)
cwdonly
deponly = setdiff(depID, cwdID)
deponly
#merge
checkdf = rbind(check.cwd, check.dep)
#combine uniqueID vectors
uniqueID = c(cwdonly, deponly)
checksub = checkdf[checkdf$sampID %in% uniqueID, ]
#check
unique(checksub$sampID) #67 total 

#subset of cwd 98-18 IDs NOT in dep df, to be added to dep df
cwdpre2019unique = checkdf[checkdf$sampID %in% cwdonly, ]

#subset cwd 2019/20
cwd1920 = chl.cwd %>% 
  filter(date > '2019-01-01' & date < '2020-12-31')

#merge
names(cwdpre2019unique)
names(cwd1920)
names(chl)
chl.thru2020 = rbind(chl, cwdpre2019unique)
chl.thru2020 = rbind(chl.thru2020, cwd1920)
#hopefully that's everything!


#cwd 2021 ###############
chl21 = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.cwd/cwd.chl.2021.csv",header=T)


#lowercase everything and rename cols
chl21 = chl21 %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(station=basin)%>% 
  dplyr::rename(date=sampdate)%>% 
  mutate(date = as.Date(date, format='%m/%d/%Y'))
length(unique(chl21$date)) #number of unique sample dates

#rename lakes
unique(chl21$lake)
#check midas first
chl21 = chl21 %>% mutate(midascheck = paste(midas,lake))
unique(chl21$midascheck)
plyr::count(chl21$lake)
plyr::count(chl21$midascheck)
temp1 = plyr::count(chl21$lake)
#rename
chl21 = chl21 %>% 
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
temp2 = plyr::count(chl21$lake)
sum(temp1$freq)
sum(temp2$freq)
unique(chl21$lake)

#* QC ########
names(chl21)
str(chl21)
unique(chl21$agency)
#depth
unique(chl21$depth)
chl21$depth = as.numeric(chl21$depth)
unique(chl21$type)
chl21 = chl21 %>% mutate(type = na_if(type,'')) #NA where blank
#chl
unique(chl21$chla)
chl21$chla = as.numeric(chl21$chla)
summary(chl21$chla) 
unique(chl21$r) 
chl21$r = as.numeric(chl21$r)
chl21 = chl21 %>% select(-midascheck)
chl21 = chl21 %>% mutate(db='cwd21')
chl21 = chl21 %>% mutate(sampID=paste(midas, station, date, agency, sep="_"))



#merge ############
names(chl21)
names(chl.thru2020)
chl.full = rbind(chl.thru2020, chl21)

#get year and month
chl.full$year = lubridate::year(chl.full$date)
chl.full$month = lubridate::month(chl.full$date)
str(chl.full)



write.csv(chl.full, "C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/chl.1974-2021.csv", row.names = F)











