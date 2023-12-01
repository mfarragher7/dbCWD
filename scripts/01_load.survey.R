#load survey data
# general, secchi, personnel, etc
#created 2023-03-08

#contains: 
#  DEP data 1970s through 2018
#  CWD data 2019-20, plus additional pre-2019 data
#  CWD 2021
#  CWD 2022 

#cleaned data stored here: library/survey.1974-2021.csv 


#libraries
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
  select(-surveyor1,-surveyor2,-surveyor3,-surveyor4,-midascheck) %>% 
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
  mutate(agency = replace(agency, grepl('e',agency),'ei'))
cwdsurv = cwdsurv %>% 
  mutate(time=NA)%>% 
  mutate(comments=NA) %>% 
  mutate(gloeo=NA) %>% 
  mutate(project=3) %>% 
  mutate(db='cwd98-20')
cwdsurv = cwdsurv %>% 
  rename(scope=scopetype) %>% 
  rename(seccbot=bottom) %>% 
  rename(secchi=depth) %>% 
  rename(qa_cert=qacert)%>% 
  rename(surveyors=surveyor) %>% 
  select(-midascheck)
  
names(depsurv)
names(cwdsurv)


#* db compare #############
#look for samples from dep that arent in cwd, and vice versa 

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




#try with secchi value as well, see if theyre different
cwd98to18 = cwd98to18 %>%  mutate(secID2 = paste(secID, secchi, sep='_'))
dep98to18 = dep98to18 %>%  mutate(secID2 = paste(secID, secchi, sep='_'))
#as vector
cwdID2 = unique(cwd98to18$secID2)
depID2 = unique(dep98to18$secID2)
#identify IDs that exist in cwd db only
cwdonly2 = setdiff(cwdID2, depID2)
cwdonly2
#identify IDs that exist in dep db only
deponly2 = setdiff(depID2, cwdID2)
deponly2
#rbind
check2 = rbind(cwd98to18, dep98to18)
#combine uniqueID vectors
uniqueID2 = c(cwdonly2, deponly2)

#subset of all profiles that are in either cwd or dep dbs but NOT BOTH
checksub2 = check2[check2$secID2 %in% uniqueID2, ]
#check
unique(checksub2$secID2) #332 total. compared to 209....

checksub = checksub %>% select(lake,date,secchi,db,sampID,secID)
checksub2 = checksub2 %>% select(lake,date,secchi,db,sampID,secID, secID2)
# use second method, with secchi in ID

#subset of cwd 98-18 profiles NOT in dep df, to be added to dep df
cwdpre2019unique = cwd98to18[cwd98to18$secID2 %in% cwdonly2, ] #82 observations compared to 38

#subset cwd 2019/20
cwd1920 = cwdsurv %>% 
  filter(date > '2019-01-01' & date < '2020-12-31')

#merge dfs together so far....
names(depsurv)
names(cwdpre2019unique)
names(cwd1920)
cwdpre2019unique = cwdpre2019unique %>% select(-secID, -secID2)
surveythru2020 = rbind(depsurv, cwdpre2019unique)
surveythru2020 = rbind(surveythru2020, cwd1920)
names(surveythru2020)




#CWD 21/22 ########

cwd21 = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.cwd/cwd.secchi.2021.csv",header=T)
cwd22 = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.cwd/survey123/CWD_2022_GENERAL.csv",header=T)

#lowercase and rename cols
cwd21 = cwd21 %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(station=basin) %>% 
  dplyr::rename(date=sampdate) %>% 
  mutate(date = as.Date(date, format='%m/%d/%Y'))
length(unique(cwd21$date)) #number of unique sample dates

cwd22 = cwd22 %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(lake=laknam) %>% 
  dplyr::rename(date=sampdate) %>% 
  mutate(db='cwd22')

  
#merge
names(cwd21)
names(cwd22)

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
  select(-surveyor1,-surveyor2,-surveyor3,-surveyor4) %>% 
  mutate(db='cwd21')

cwd22 = cwd22 %>% 
  mutate(sampID=paste(midas, station, date, agency, sep="_")) %>% 
  mutate(surveyors = paste(surveyor1,surveyor2,surveyor3,surveyor4, sep=',')) %>% 
  select(-surveyor1,-surveyor2,-surveyor3,-surveyor4)

cwd2122 = rbind(cwd21,cwd22)



#rename lakes
unique(cwd2122$lake)
#check midas first
cwd2122 = cwd2122 %>% mutate(midascheck = paste(midas,lake))
unique(cwd2122$midascheck)
#save
temp1 = plyr::count(cwd2122$lake)
temp = plyr::count(cwd2122$midascheck)
cwd2122 = cwd2122 %>% 
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
temp2 = plyr::count(cwd2122$lake)
#check for same number. yup
sum(temp1$freq)
sum(temp2$freq)

#check stuff
unique(cwd2122$station)
unique(cwd2122$agency)
names(cwd2122)
cwd2122 = cwd2122 %>% select(-midascheck)

names(cwd2122)
names(surveythru2020)

survey.full = rbind(cwd2122,surveythru2020)

survey.full = survey.full %>% 
  select(sampID, midas, lake, station, 
         date, time, agency, project, db, 
         secchi, rep, seccbot, scope, 
         surveyors, qa_cert, 
         windvel, winddir, cloudcvr, 
         lat, long, gloeo, comments)




#* QC ######
#general
names(survey.full)
str(survey.full)
unique(survey.full$midas)
survey.full$midas = as.numeric(survey.full$midas)
unique(survey.full$lake)
unique(survey.full$date)
unique(survey.full$station)
survey.full$station = as.numeric(survey.full$station)
unique(survey.full$comments)
unique(survey.full$lat)
unique(survey.full$long) #should all be negative, ill fix that one day
unique(survey.full$agency)
unique(survey.full$project)
survey.full$project = as.numeric(survey.full$project)
unique(survey.full$db)

#secchi
str(survey.full$secchi)
length(survey.full$secchi)
survey.full$secchi = as.numeric(survey.full$secchi)
summary(survey.full$secchi)
temp1 = plyr::count(survey.full$secchi)
#99 obviously not real. 72 should be 5.72
survey.full = survey.full %>% 
  mutate(secchi = replace(secchi,secchi==72.00,5.72)) %>% 
  filter(secchi < 99)
#leave NA values. but remove duplicates
survey.full$dups = paste(survey.full$midas,
                         survey.full$station,
                         survey.full$date,
                         survey.full$secchi,
                         survey.full$qa_cert,
                         survey.full$rep,
                         sep='_')
dups = plyr::count(survey.full$dups)
dups_ = dups %>% filter(freq>1) #184 dups
#drop 5254_3_2022-09-10_4.13_fi-xxxx_1, the ones with no lat/long
survey.full = survey.full %>% 
  filter(!(grepl('5254_3_2022-09-10_4.13_fi-xxxx_1', dups) & is.na(lat)))
#other one with 4 duplicates, 98_1_1998-06-29_4.75__1
#^^ all the same reading. will be removed with distinct()
#all others are duplicated once (so there's 2) so distinct() will remove dups
survey.full = survey.full %>% distinct(dups,  .keep_all = TRUE)
str(survey.full$secchi)
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
survey.full$scope = as.numeric(survey.full$scope)

#seems fine!
#rep
str(survey.full$rep)
unique(survey.full$rep)
survey.full$rep = as.numeric(survey.full$rep)

#qacert
str(survey.full$qa_cert)
unique(survey.full$qa_cert)
survey.full = survey.full %>% mutate(qa_cert = na_if(qa_cert,''))
survey.full = survey.full %>% 
  mutate(qa_cert = replace(qa_cert,qa_cert=='fi-xxxx','fi-3142'))

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

#gloeo?
str(survey.full$gloeo)
unique(survey.full$gloeo)




#save in library
write.csv(survey.full, "C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/survey.1974-2022.csv", row.names = F)

