#load profile data for temp and oxygen
#created 2023-01-18 

#libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)


#Profiles ######

#load dep data
#load cwd data for 2019-2020
#load cwd new 2021 data


#dep data 1975-2018
deppro = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.dep/CWD_DO_TEMP_PROFILES.csv",header=T)
str(deppro)

#lowercase everything and rename cols
deppro = deppro %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(lake=laknam) %>% 
  dplyr::rename(datetime=sampdate)

#format date
deppro = deppro %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=T) %>% 
  mutate(date = as.Date(date))

length(unique(deppro$date)) #number of unique sample dates

#rename lakes
unique(deppro$lake)
#check midas first
deppro = deppro %>% mutate(midascheck = paste(midas,lake))
unique(deppro$midascheck)

#save
plyr::count(deppro$lake)
plyr::count(deppro$midascheck)

#problems 
#5254 smith (weeks) real midas 0254
#5254 french p , in somerville midas 5454
#5254 pleasant
#3814 long (mcwain) actual midas is 3418
#3814 cochnewagon

temp = deppro[deppro$midas==5254,]
temp = deppro[deppro$midas==3814,]

#drop french weeks and mcwain ponds
deppro = deppro %>% 
  filter(! lake=='french p') %>% 
  filter(! lake=='smith (weeks') %>% 
  filter(! lake=='long (mcwain')

temp1 = plyr::count(deppro$lake)

#rename
deppro = deppro %>% 
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

temp2 = plyr::count(deppro$lake)
#gained 1 lakes... little cobbossee
temp = deppro %>% filter(grepl('cobb',lake))
#correct?

#check for same number of rows. yup
sum(temp1$freq)
sum(temp2$freq)

#create sample id
deppro = deppro %>% 
  mutate(sampID=paste(midas, station, date, agency, sep="_"))

#get year and month
deppro$year = lubridate::year(deppro$date)
deppro$month = lubridate::month(deppro$date)




#* cwd 1998-2020 #######

#load cwd data and subset 2019 2020
cwdpro = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.cwd/cwd.profiles.98-2020.csv',header=T)

#lowercase everything and rename cols
cwdpro = cwdpro %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(station=basin) %>% 
  dplyr::rename(date=sampdate)

#format date
cwdpro$date = as.Date(cwdpro$date, format='%m/%d/%Y')

#rename lakes
unique(cwdpro$lake)
#check midas first
cwdpro = cwdpro %>% mutate(midascheck = paste(midas,lake))
unique(cwdpro$midascheck)
#save
plyr::count(cwdpro$lake)
plyr::count(cwdpro$midascheck)

temp1 = plyr::count(cwdpro$lake)

#rename
cwdpro = cwdpro %>% 
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

temp2 = plyr::count(cwdpro$lake)

#check for same number of rows. yup
sum(temp1$freq)
sum(temp2$freq)

#create sample id
cwdpro = cwdpro %>% 
  mutate(sampID=paste(midas, station, date, agency, sep="_"))

#get year and month
cwdpro$year = lubridate::year(cwdpro$date)
cwdpro$month = lubridate::month(cwdpro$date)



#* db comparison! ####

#check to see which profiles from dep/cwd match or not or something....
names(cwdpro)
names(deppro)

#subset 98-18. will be pulling out unique profiles (CWD only) from sampIDs to add to 'complete' db
cwdpro98to18 = cwdpro %>% 
  filter(date > '1998-01-01' & date < '2018-12-31') %>% 
  select(sampID, midas,lake,station,date,agency,depth,temp,oxygen,oxymeth,year,month) %>% 
  mutate(db='cwd') %>% 
  mutate(meter=NA) %>% 
  mutate(calib=NA)

#subset same timeframe. wont be using this subsetted db in 'complete' db 
deppro98to18 = deppro %>% 
  filter(date > '1998-01-01' & date < '2018-12-31') %>% 
  select(sampID,midas,lake,station,date,agency,depth,temp,oxygen,oxymeth,meter,calib,year,month) %>% 
  mutate(db='dep')

#get sampIDs as vector
cwdproID = unique(cwdpro98to18$sampID)
depproID = unique(deppro98to18$sampID)

#identify IDs that exist in cwd db only
cwdonly = setdiff(cwdproID, depproID)
cwdonly
#identify IDs that exist in dep db only
deponly = setdiff(depproID, cwdproID)
deponly

#rbind
procheck = rbind(cwdpro98to18, deppro98to18)
#combine uniqueID vectors
uniquepros = c(cwdonly, deponly)

#subset of all profiles that are in either cwd or dep dbs but NOT BOTH
prochecksub = procheck[procheck$sampID %in% uniquepros, ]
#check
unique(prochecksub$sampID) #97 total 

#subset of cwd 98-18 profiles NOT in dep df, to be added to deppro df
cwdpre2019unique = cwdpro98to18[cwdpro98to18$sampID %in% cwdonly, ]

#subset cwd 2019/20
cwdpro1920 = cwdpro %>% 
  filter(date > '2019-01-01' & date < '2020-12-31') %>% 
  mutate(db='cwd') %>% 
  mutate(meter=NA) %>% 
  mutate(calib=NA) %>% 
  select(-midascheck)

deppro = deppro %>% 
  select(-project, -time, -midascheck) %>% 
  mutate(db='dep')


#merge dfs together so far....

names(deppro)
names(cwdpre2019unique)
names(cwdpro1920)

profiles.thru2020 = rbind(deppro, cwdpre2019unique)
profiles.thru2020 = rbind(profiles.thru2020, cwdpro1920)
names(profiles.thru2020)


#* cwd 2021 #######
cwdpro21 = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/db.cwd/cwd.profiles.2021.csv',header=T)

#lowercase everything and rename cols
cwdpro21 = cwdpro21 %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.))%>% 
  dplyr::rename(station=basin) %>% 
  dplyr::rename(date=sampdate)

#format date
cwdpro21$date = as.Date(cwdpro21$date, format='%m/%d/%Y')

#rename lakes
unique(cwdpro21$lake)
#check midas first
cwdpro21 = cwdpro21 %>% mutate(midascheck = paste(midas,lake))
unique(cwdpro21$midascheck)
#save
plyr::count(cwdpro21$lake)
plyr::count(cwdpro21$midascheck)

temp1 = plyr::count(cwdpro21$lake)

#rename
cwdpro21 = cwdpro21 %>% 
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

temp2 = plyr::count(cwdpro21$lake)

#check for same number of rows. yup
sum(temp1$freq)
sum(temp2$freq)

#create sample id
cwdpro21 = cwdpro21 %>% 
  mutate(sampID=paste(midas, station, date, agency, sep="_")) %>% 
  mutate(db='cwd') %>% 
  select(-midascheck, -project)

#get year and month
cwdpro21$year = lubridate::year(cwdpro21$date)
cwdpro21$month = lubridate::month(cwdpro21$date)

names(cwdpro21)

#COMBINE
profiles = rbind(profiles.thru2020, cwdpro21)


#* QC ####
str(profiles)
unique(profiles$midas)
profiles$midas = as.numeric(profiles$midas)
unique(profiles$station)
profiles$station = as.numeric(profiles$station)
#DEPTH
unique(profiles$depth)
profiles$depth = as.numeric(profiles$depth)
ddd = plyr::count(profiles$depth) #34 profiles at 999
profiles = profiles %>% filter(depth <100)
#TEMPERATURE
unique(profiles$temp)
profiles$temp = as.numeric(profiles$temp)
ttt = plyr::count(profiles$temp) #13 temps at 99.9
profiles = profiles %>% filter(temp <50)
#OXYGEN
unique(profiles$oxygen)
profiles$oxygen = as.numeric(profiles$oxygen)
ooo = plyr::count(profiles$oxygen) #8 do at 99.9
profiles = profiles %>% filter(oxygen <50)
#other
unique(profiles$oxymeth)
profiles = profiles %>% mutate(oxymeth = na_if(oxymeth,''))
unique(profiles$meter)
profiles =  profiles %>% select(-meter)
unique(profiles$calib)
profiles = profiles %>% mutate(calib = na_if(calib,''))
profiles$midas = as.numeric(profiles$midas)
unique(profiles$lake)
unique(profiles$sampID)
length(unique(profiles$sampID)) #6308 unique profiles
unique(profiles$db)
str(profiles)

summary(profiles$temp)
summary(profiles$oxygen)


#save
write.csv(profiles, "C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/profiles.1975-2021.csv", row.names = F)





#* summary ####
#summarize all profiles
profiles = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/profiles.1975-2021.csv',header=T)
names(profiles)
str(profiles)

#summary of each profile
spro = ddply(profiles, .(sampID, midas, lake, station, date, agency, db), summarize, 
             profile.max.depth = max(depth),
             #temp - min, max, mean, sd, se, thermocline depth, etc
             temp.min = min(temp), 
             temp.max = max(temp), 
             temp.mean = mean(temp),
             temp.sd = sd(temp), 
             temp.se = temp.sd / sqrt(sum(!is.na(temp))),
             thermo.depth = NA,
             temp.epi = NA,
             temp.hypo = NA,
             temp.top5m=NA,
             meta.top=NA,
             meta.bottom=NA,
             #oxygen
             do.min=min(oxygen), 
             do.max=max(oxygen), 
             do.mean=mean(oxygen),
             do.sd=sd(oxygen),
             do.se=do.sd / sqrt(sum(!is.na(oxygen))),
             do.max.depth=NA)

#save sample IDs
length(unique(profiles$sampID))
samp = unique(profiles$sampID)

# thermocline depth and epi/hypo temps
for (i in 1:length(samp)){ #for every unique sample,
  td = profiles[profiles$sampID == samp[i], ] #temp dataframe that subsets profile df by each sampID
  td = td[!duplicated(td$depth), ] #remove duplicate depths
  thermo = thermo.depth(td$temp, td$depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  spro[i,14] = thermo[1]  #paste thermocline depth in 12th column of row i*j == i in spro df
  #epi temp
  td1 = td[td$depth < thermo[1], ] #subset each df with depths above thermocline depth
  epi.temp = mean(td1$temp) #get average temp of epi from subsetted df
  spro[i,15] = epi.temp[1] #paste epi temp
  #hypo temp
  td2 = td[td$depth > thermo[1], ] #subset each df with depths below thermocline depth
  hypo.temp = mean(td2$temp) #get average temp of hypo from subsetted df
  spro[i,16] = hypo.temp[1] 
  #top5m
  td3 = td[td$depth <= 5,] #subset top 5m from each profile
  top5temp = mean(td3$temp) #mean temp of top 5m
  spro[i,17] = top5temp[1] }  #paste hypo temp, close loop

# get meta depths
for (i in 1:length(samp)){ #for every unique sample,
  td = profiles[profiles$sampID == samp[i], ] #temp dataframe that subsets profile df by each sampID
  td = td[!duplicated(td$depth), ] #remove duplicate depths
  meta = meta.depths(td$temp, td$depth, slope=0.1, seasonal=F, mixed.cutoff=1) 
  spro[i,18] = meta[1]   #top
  spro[i,19] = meta[2] }  #bottom

# get max DO depth for each profile
for (i in 1:length(samp)){ #for every unique sample,
  td = profiles[profiles$sampID == samp[i], ] #temp dataframe that subsets profile df by each sampID
  td = td[td$oxygen == max(td$oxygen),] #subset row of only max oxygen
  do.max.depth = td$depth #save depth at max do
  spro[i,25] = do.max.depth[1] }   #paste depth of max DO 

#check 
test1 = spro %>% filter(lake=='dexter' & date=='2013-05-22')
test1$thermo.depth
test1$temp.epi
test1$temp.hypo

test2 = profiles %>% filter(lake=='dexter' & date=='2013-05-22')
thermo.depth(test2$temp, test2$depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) 
mean(filter(test2, depth < 4.5)$temp) 
mean(filter(test2, depth > 4.5)$temp) 

#modify df
spro = spro %>% 
  mutate(thermo.depth = replace(thermo.depth,thermo.depth=='NaN',NA))  #change NaN to NA

str(spro)

#save profile summary
#year and month
spro$year = lubridate::year(spro$date)
spro$month = lubridate::month(spro$date)

temp = plyr::count(spro$sampID) #one of each....good, if true.,..,


write.csv(spro, "C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/profiles.1975-2021.summary.csv", row.names = F)

#undated and re-ran 2023-01-12 with new 2021 datas





#compare with volume weighted epi/hypo temps once i get bathymetry data

#load wilson bathy
wb = read.csv("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/bathy.wilson.csv")

#summary table for just wilson
subp = profiles %>% filter(lake=='wilson')            

pbj = ddply(subp, .(sampID, midas, lake, date), summarize, 
            temp.min = min(temp), 
            temp.max = max(temp), 
            temp.mean = mean(temp),
            temp.sd = sd(temp), 
            temp.se = temp.sd / sqrt(sum(!is.na(temp))),
            thermo.depth = NA,
            temp.epi = NA,
            temp.hypo = NA,
            temp.epi.bathy = NA,
            temp.hypo.bathy = NA)


#save sample IDs
length(unique(pbj$sampID))
samp = unique(pbj$sampID)

# thermocline depth and epi/hypo temps
for (i in 1:length(samp)){ #for every unique sample,
  td = subp[subp$sampID == samp[i], ] #temp dataframe that subsets profile df by each sampID
  td = td[!duplicated(td$depth), ] #remove duplicate depths
  thermo = thermo.depth(td$temp, td$depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  pbj[i,10] = thermo[1]  #paste thermocline depth in 12th column of row i*j == i in spro df
  #epi temp
  td1 = td[td$depth < thermo[1], ] #subset each df with depths above thermocline depth
  epi.temp = mean(td1$temp) #get average temp of epi from subsetted df
  pbj[i,11] = epi.temp[1] #paste epi temp
  #hypo temp
  td2 = td[td$depth > thermo[1], ] #subset each df with depths below thermocline depth
  hypo.temp = mean(td2$temp) #get average temp of hypo from subsetted df
  pbj[i,12] = hypo.temp[1]  }  #paste hypo temp



#add bathymetry derived temps using rLakeAnalyzer
str(wb)
wb$depth.top = as.numeric(wb$depth.top)
wb$depth.bottom = as.numeric(wb$depth.bottom)
wb$vol.m3 = as.numeric(wb$vol.m3)
str(wb)


#test function
subsub = filter(subp, date == '2008-05-07')
subsub
epi.temperature(td$temp, td$depth, wb$vol.m3, wb$depth.top)

#should be area instead of volume, damn

for (i in 1:length(samp)){ #for every unique sample,
  td = subp[subp$sampID == samp[i], ] #temp dataframe that subsets profile df by each sampID
  td = td[!duplicated(td$depth), ] #remove duplicate depths
  epi.t = epi.temperature(td$temp, td$depth, wb$vol.m3, wb$depth.top)  #get mean epi temp
  pbj[i,13] = epi.t[1] #paste in df
  hypo.t = hypo.temperature(td$temp, td$depth, wb$vol.m3, wb$depth.top) #get mean hypo temp
  pbj[i,14] = hypo.t[1] } #paste in df



#compare
pbjtest = pbj %>% 
  pivot_longer(cols = c('temp.epi', 
                        'temp.epi.bathy',
                        'temp.hypo',
                        'temp.hypo.bathy'),
               names_to = 'layer',
               values_to = 'temp')

ggplot(pbjtest, aes(x=date, y=temp, color=layer)) +
  geom_point() +
  geom_line()





