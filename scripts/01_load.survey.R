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

#create sample id
depsurv = depsurv %>% 
  mutate(sampID=paste(midas, station, date, agency, sep="_"))

#get year and month
depsurv$year = lubridate::year(depsurv$date)
depsurv$month = lubridate::month(depsurv$date)


#CWD #######
#* CWD 19/20 #####



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

