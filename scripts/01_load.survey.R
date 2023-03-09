#load survey data
# general, secchi, personnel, etc
#created 2023-03-08

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
  select(-surveyor1,-surveyor2,-surveyor3,-surveyor4,midascheck,-project) %>% 
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
  rename(surveyors=surveyor)



#* db compare #############
names(cwdsurv)
names(depsurv)

#subset 98-18. will be pulling out unique profiles (CWD only) from sampIDs to add to 'complete' db
cwd98to18 = cwdsurv %>% 
  filter(date > '1998-01-01' & date < '2018-12-31') %>% 
  mutate(db='cwd') %>% 
  mutate(secID = paste(midas, station, date, sep='_')) 

#subset same timeframe. wont be using this subsetted db in 'complete' db 
dep98to18 = depsurv %>% 
  filter(date > '1998-01-01' & date < '2018-12-31') %>% 
  mutate(db='dep') %>% 
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
  filter(date > '2019-01-01' & date < '2020-12-31') %>% 
  mutate(db='cwd19-20')

#merge dfs together so far....
names(depsurv)
names(cwdpre2019unique)
names(cwd1920)
cwdpre2019unique = cwdpre2019unique %>% select(-secID)
surveythru2020 = rbind(depsurv, cwdpre2019unique)
surveythru2020 = rbind(surveythru2020, cwd1920)
names(surveythru2020)




#* CWD 21 ########









































#* QC ######
names(depsurv)

#wind velocity
str(depsurv$windvel)
temp1 = plyr::count(depsurv$windvel)
#change to numeric and compare
depsurv$windvel = as.numeric(depsurv$windvel)
temp2 = plyr::count(depsurv$windvel)
summary(depsurv$windvel)

#wind direction. look up codes...
str(depsurv$winddir)
temp1 = plyr::count(depsurv$winddir)
#change to numeric and compare
depsurv$winddir = as.numeric(depsurv$winddir)
temp2 = plyr::count(depsurv$winddir)
summary(depsurv$winddir)

#clouds
str(depsurv$cloudcvr)
temp1 = plyr::count(depsurv$cloudcvr)
unique(depsurv$cloudcvr)
#NA where blank
depsurv = depsurv %>% mutate(cloudcvr = na_if(cloudcvr,''))

#secchi
str(depsurv$secchi)
length(depsurv$secchi)
temp1 = plyr::count(depsurv$secchi)
depsurv$secchi = as.numeric(depsurv$secchi)
summary(depsurv$secchi)

#secchi bottom
str(depsurv$seccbot)
unique(depsurv$seccbot)
temp1 = plyr::count(depsurv$seccbot)
#NA where blank
depsurv = depsurv %>% mutate(seccbot = na_if(seccbot,''))
#change 'b' to 'y'
depsurv = depsurv %>% mutate(seccbot = replace(seccbot, seccbot=='b', 'y'))
temp2 = plyr::count(depsurv$seccbot)
#scope
str(depsurv$scope)
unique(depsurv$scope)
#seems fine!
#rep
str(depsurv$rep)
unique(depsurv$rep)
#qacert
str(depsurv$qa_cert)
unique(depsurv$qa_cert)
depsurv = depsurv %>% mutate(qa_cert = na_if(qa_cert,''))
#gloeo?
str(depsurv$gloeo)
unique(depsurv$gloeo)
#comments
unique(depsurv$comments)
#check for issues and mark resolved
depsurv = depsurv %>% select(-midascheck)

#save in library
write.csv(depsurv, "C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/survey.dep.csv", row.names = F)

