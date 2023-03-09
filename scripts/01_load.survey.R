#load survey data
# general, secchi, personnel, etc
#created 2023-03-08

#contains: 
#  DEP data 1970s through 2018
#  CWD data 2019-20, plus additional pre-2019 data
#  CWD data from 2021
#  2022 and onward coming next

#cleaned data stored here: library/survey.1974-2021.csv 


#libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)



#DEP ####
#load dep data, 1971-2018
depsurv = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.dep/CWD_GENERAL.csv",header=T)

#lowercase and rename cols
depsurv = depsurv %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(lake=laknam) %>% 
  dplyr::rename(date=sampdate) %>% 
  mutate(date = as.Date(date))
length(unique(depsurv$date)) #number of unique sample dates

#rename lakes
unique(depsurv$lake)
#check midas first
depsurv = depsurv %>% mutate(midascheck = paste(midas,lake))
unique(depsurv$midascheck)
#save
temp1 = plyr::count(depsurv$lake)
temp = plyr::count(depsurv$midascheck)
depsurv = depsurv %>% 
  mutate(lake = replace(lake, midas==9961, 'annabessacook')) %>% 
  mutate(lake = replace(lake, midas==3828, 'berry')) %>% 
  mutate(lake = replace(lake, midas==5242, 'buker')) %>% 
  mutate(lake = replace(lake, midas==5310, 'carlton')) %>% 
  mutate(lake = replace(lake, midas==5236, 'cobbossee')) %>% 
  mutate(lake = replace(lake, midas==8065, 'little cobbossee')) %>% 
  mutate(lake = replace(lake, midas==3814, 'cochnewagon')) %>% 
  mutate(lake = replace(lake, midas==5265, 'desert')) %>%
  mutate(lake = replace(lake, midas==3830, 'dexter')) %>% 
  mutate(lake = replace(lake, midas==5252, 'horseshoe')) %>% 
  mutate(lake = replace(lake, midas==5304, 'hutchinson')) %>% 
  mutate(lake = replace(lake, midas==5302, 'jamies')) %>% 
  mutate(lake = replace(lake, midas==5244, 'jimmy')) %>% 
  mutate(lake = replace(lake, midas==5316, 'kezar')) %>% 
  mutate(lake = replace(lake, midas==5246, 'loon')) %>% 
  mutate(lake = replace(lake, midas==5312, 'maranacook')) %>% 
  mutate(lake = replace(lake, midas==98,'narrows upper')) %>% 
  mutate(lake = replace(lake, midas==103,'narrows lower')) %>% 
  mutate(lake = replace(lake, midas==5254,'pleasant')) %>% 
  mutate(lake = replace(lake, midas==5250,'little purgatory')) %>% 
  mutate(lake = replace(lake, midas==5238,'sand')) %>% 
  mutate(lake = replace(lake, midas==5300,'shed')) %>% 
  mutate(lake = replace(lake, midas==5307,'torsey')) %>% 
  mutate(lake = replace(lake, midas==3832,'wilson')) %>% 
  mutate(lake = replace(lake, midas==5240,'woodbury'))
temp2 = plyr::count(depsurv$lake)
#check for same number. yup
sum(temp1$freq)
sum(temp2$freq)

#check some stuff
unique(depsurv$agency)
unique(depsurv$project)

#create sample id, fix col names
depsurv = depsurv %>% 
  mutate(sampID=paste(midas, station, date, agency, sep="_")) %>% 
  mutate(surveyors = paste(surveyor1,surveyor2,surveyor3,surveyor4, sep=',')) %>% 
  select(-surveyor1,-surveyor2,-surveyor3,-surveyor4,-midascheck,-project) %>% 
  mutate(lat=NA) %>% 
  mutate(long=NA) %>% 
  mutate(db='dep')




#CWD #######
cwdsurv = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.cwd/cwd.secchi.98-2020.csv",header=T)

#lowercase and rename cols
cwdsurv = cwdsurv %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(date=sampdate) %>% 
  dplyr::rename(station=basin) %>% 
  mutate(date = as.Date(date, format="%m/%d/%Y"))
length(unique(cwdsurv$date)) #number of unique sample dates

#rename lakes
unique(cwdsurv$lake)
#check midas first
cwdsurv = cwdsurv %>% mutate(midascheck = paste(midas,lake))
unique(cwdsurv$midascheck)
#save
temp1 = plyr::count(cwdsurv$lake)
temp = plyr::count(cwdsurv$midascheck)
cwdsurv = cwdsurv %>% 
  mutate(lake = replace(lake, midas==9961, 'annabessacook')) %>% 
  mutate(lake = replace(lake, midas==3828, 'berry')) %>% 
  mutate(lake = replace(lake, midas==5242, 'buker')) %>% 
  mutate(lake = replace(lake, midas==5310, 'carlton')) %>% 
  mutate(lake = replace(lake, midas==5236, 'cobbossee')) %>% 
  mutate(lake = replace(lake, midas==8065, 'little cobbossee')) %>% 
  mutate(lake = replace(lake, midas==3814, 'cochnewagon')) %>% 
  mutate(lake = replace(lake, midas==5265, 'desert')) %>%
  mutate(lake = replace(lake, midas==3830, 'dexter')) %>% 
  mutate(lake = replace(lake, midas==5252, 'horseshoe')) %>% 
  mutate(lake = replace(lake, midas==5304, 'hutchinson')) %>% 
  mutate(lake = replace(lake, midas==5302, 'jamies')) %>% 
  mutate(lake = replace(lake, midas==5244, 'jimmy')) %>% 
  mutate(lake = replace(lake, midas==5316, 'kezar')) %>% 
  mutate(lake = replace(lake, midas==5246, 'loon')) %>% 
  mutate(lake = replace(lake, midas==5312, 'maranacook')) %>% 
  mutate(lake = replace(lake, midas==98,'narrows upper')) %>% 
  mutate(lake = replace(lake, midas==103,'narrows lower')) %>% 
  mutate(lake = replace(lake, midas==5254,'pleasant')) %>% 
  mutate(lake = replace(lake, midas==5250,'little purgatory')) %>% 
  mutate(lake = replace(lake, midas==5238,'sand')) %>% 
  mutate(lake = replace(lake, midas==5300,'shed')) %>% 
  mutate(lake = replace(lake, midas==5307,'torsey')) %>% 
  mutate(lake = replace(lake, midas==3832,'wilson')) %>% 
  mutate(lake = replace(lake, midas==5240,'woodbury'))
temp2 = plyr::count(cwdsurv$lake)
#check for same number. yup
sum(temp1$freq)
sum(temp2$freq)

#create sample id
cwdsurv = cwdsurv %>% 
  mutate(sampID=paste(midas, station, date, agency, sep="_"))

#check stuff
unique(cwdsurv$station)
unique(cwdsurv$agency)
#fix columns
cwdsurv = cwdsurv %>% 
  mutate(agency = replace(agency, grepl('e',agency),'ei')) %>% 
  mutate(time=NA) %>% 
  mutate(comments=NA) %>% 
  mutate(gloeo=NA) %>% 
  rename(scope=scopetype) %>% 
  rename(seccbot=bottom) %>% 
  rename(secchi=depth) %>% 
  rename(qa_cert=qacert)%>% 
  rename(surveyors=surveyor) %>% 
  select(-midascheck) %>% 
  mutate(db='cwd98-20')
  



#* db compare #############
names(cwdsurv)
names(depsurv)

#subset 98-18. will be pulling out unique profiles (CWD only) from sampIDs to add to 'complete' db
cwd98to18 = cwdsurv %>% 
  filter(date > '1998-01-01' & date < '2018-12-31') %>% 
  mutate(secID = paste(midas, station, date, sep='_')) 

#subset same timeframe. wont be using this subsetted db in 'complete' db 
dep98to18 = depsurv %>% 
  filter(date > '1998-01-01' & date < '2018-12-31') %>% 
  mutate(secID = paste(midas, station, date, sep='_'))


#if I use midas-station-date and leave off agency, I should still get unique samples from each db

#get sampIDs as vector
cwdID = unique(cwd98to18$secID)
depID = unique(dep98to18$secID)

#identify IDs that exist in cwd db only
cwdonly = setdiff(cwdID, depID)
cwdonly
#identify IDs that exist in dep db only
deponly = setdiff(depID, cwdID)
deponly

#rbind
check = rbind(cwd98to18, dep98to18)
#combine uniqueID vectors
uniqueID = c(cwdonly, deponly)

#subset of all profiles that are in either cwd or dep dbs but NOT BOTH
checksub = check[check$secID %in% uniqueID, ]
#check
unique(checksub$secID) #209 total 

#subset of cwd 98-18 profiles NOT in dep df, to be added to dep df
cwdpre2019unique = cwd98to18[cwd98to18$secID %in% cwdonly, ]

#subset cwd 2019/20
cwd1920 = cwdsurv %>% 
  filter(date > '2019-01-01' & date < '2020-12-31')

#merge dfs together so far....
names(depsurv)
names(cwdpre2019unique)
names(cwd1920)
cwdpre2019unique = cwdpre2019unique %>% select(-secID)
surveythru2020 = rbind(depsurv, cwdpre2019unique)
surveythru2020 = rbind(surveythru2020, cwd1920)
names(surveythru2020)




#CWD 21 ########

cwd21 = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.cwd/cwd.secchi.2021.csv",header=T)

#lowercase and rename cols
cwd21 = cwd21 %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(station=basin) %>% 
  dplyr::rename(date=sampdate) %>% 
  mutate(date = as.Date(date, format='%m/%d/%Y'))
length(unique(cwd21$date)) #number of unique sample dates

#rename lakes
unique(cwd21$lake)
#check midas first
cwd21 = cwd21 %>% mutate(midascheck = paste(midas,lake))
unique(cwd21$midascheck)
#save
temp1 = plyr::count(cwd21$lake)
temp = plyr::count(cwd21$midascheck)
cwd21 = cwd21 %>% 
  mutate(lake = replace(lake, midas==9961, 'annabessacook')) %>% 
  mutate(lake = replace(lake, midas==3828, 'berry')) %>% 
  mutate(lake = replace(lake, midas==5242, 'buker')) %>% 
  mutate(lake = replace(lake, midas==5310, 'carlton')) %>% 
  mutate(lake = replace(lake, midas==5236, 'cobbossee')) %>% 
  mutate(lake = replace(lake, midas==8065, 'little cobbossee')) %>% 
  mutate(lake = replace(lake, midas==3814, 'cochnewagon')) %>% 
  mutate(lake = replace(lake, midas==5265, 'desert')) %>%
  mutate(lake = replace(lake, midas==3830, 'dexter')) %>% 
  mutate(lake = replace(lake, midas==5252, 'horseshoe')) %>% 
  mutate(lake = replace(lake, midas==5304, 'hutchinson')) %>% 
  mutate(lake = replace(lake, midas==5302, 'jamies')) %>% 
  mutate(lake = replace(lake, midas==5244, 'jimmy')) %>% 
  mutate(lake = replace(lake, midas==5316, 'kezar')) %>% 
  mutate(lake = replace(lake, midas==5246, 'loon')) %>% 
  mutate(lake = replace(lake, midas==5312, 'maranacook')) %>% 
  mutate(lake = replace(lake, midas==98,'narrows upper')) %>% 
  mutate(lake = replace(lake, midas==103,'narrows lower')) %>% 
  mutate(lake = replace(lake, midas==5254,'pleasant')) %>% 
  mutate(lake = replace(lake, midas==5250,'little purgatory')) %>% 
  mutate(lake = replace(lake, midas==5238,'sand')) %>% 
  mutate(lake = replace(lake, midas==5300,'shed')) %>% 
  mutate(lake = replace(lake, midas==5307,'torsey')) %>% 
  mutate(lake = replace(lake, midas==3832,'wilson')) %>% 
  mutate(lake = replace(lake, midas==5240,'woodbury'))
temp2 = plyr::count(cwd21$lake)
#check for same number. yup
sum(temp1$freq)
sum(temp2$freq)

#check stuff
unique(cwd21$station)
unique(cwd21$agency)
names(cwd21)

#fix columns
cwd21 = cwd21 %>% 
  mutate(sampID=paste(midas, station, date, agency, sep="_")) %>% 
  mutate(time=NA) %>% 
  mutate(comments=NA) %>% 
  rename(scope=scopetype) %>% 
  rename(seccbot=bottom) %>% 
  rename(secchi=depth) %>% 
  rename(qa_cert=qacert)%>% 
  mutate(surveyors = paste(surveyor1,surveyor2,surveyor3,surveyor4, sep=',')) %>% 
  select(-surveyor1,-surveyor2,-surveyor3,-surveyor4,-midascheck,-project) %>% 
  mutate(db='cwd21')
  

names(cwd21)
names(surveythru2020)

survey.full = rbind(cwd21,surveythru2020)

survey.full = survey.full %>% 
  select(sampID, midas, lake, station, 
         date, time, agency, db, 
         secchi, rep, seccbot, scope, 
         surveyors, qa_cert, 
         windvel, winddir, cloudcvr, 
         lat, long, gloeo, comments)




#* QC ######
names(survey.full)

#wind velocity
str(survey.full$windvel)
temp1 = plyr::count(survey.full$windvel)
#change to numeric and compare
survey.full$windvel = as.numeric(survey.full$windvel)
temp2 = plyr::count(survey.full$windvel)
survey.full = survey.full %>% filter(windvel <50) #remove 99s
summary(survey.full$windvel)

#wind direction.
str(survey.full$winddir)
temp1 = plyr::count(survey.full$winddir)
#change to numeric and compare
survey.full$winddir = as.numeric(survey.full$winddir)
temp2 = plyr::count(survey.full$winddir)
summary(survey.full$winddir)

#clouds
str(survey.full$cloudcvr)
temp1 = plyr::count(survey.full$cloudcvr)
unique(survey.full$cloudcvr)
#NA where blank
survey.full = survey.full %>% mutate(cloudcvr = na_if(cloudcvr,''))
#numeric and character... hmm

#secchi
str(survey.full$secchi)
length(survey.full$secchi)
temp1 = plyr::count(survey.full$secchi)
survey.full$secchi = as.numeric(survey.full$secchi)
summary(survey.full$secchi)

#secchi bottom
str(survey.full$seccbot)
unique(survey.full$seccbot)
temp1 = plyr::count(survey.full$seccbot)
#NA where blank
survey.full = survey.full %>% mutate(seccbot = na_if(seccbot,''))
#change 'b' to 'y'
survey.full = survey.full %>% mutate(seccbot = replace(seccbot, seccbot=='b', 'y'))
temp2 = plyr::count(survey.full$seccbot)
#scope
str(survey.full$scope)
unique(survey.full$scope)
#seems fine!
#rep
str(survey.full$rep)
unique(survey.full$rep)
#qacert
str(survey.full$qa_cert)
unique(survey.full$qa_cert)
survey.full = survey.full %>% mutate(qa_cert = na_if(qa_cert,''))
#gloeo?
str(survey.full$gloeo)
unique(survey.full$gloeo)
#comments
unique(survey.full$comments)
#check for issues and mark resolved


#save in library
write.csv(survey.full, "C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/survey.1974-2021.csv", row.names = F)

