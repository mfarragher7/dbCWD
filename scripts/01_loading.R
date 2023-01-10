# cwd long term data
# using data jeremy sent me 2022-11-14
# 1971-2018. will eventually include recent years too


#QC to-do: 
# format date as yyyy-mm-dd. separate date and time
# create sampleID column with midas_station_date
# outliers. remove 999 type stuff. check min and max for each parameter


#libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)

#metadata
md = read.csv("‪C:\Users\CWD2-Matt\OneDrive\Database\/CWD/dbCWD/db.raw/lakemd.csv")


#General ######
#GENERAL SURVEY INFO and SECCHI

#load df
general = read.csv("~/CWD/dbCWD/db.raw/db.dep/CWD_GENERAL.csv")

#lowercase everything and rename cols
general = general %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(lake=laknam) %>% 
  dplyr::rename(datetime=sampdate)
  
#format date
general = general %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=T) %>% 
  mutate(date = as.Date(date))

length(unique(general$date)) #number of unique sample dates

#rename lakes
unique(general$lake)
#check midas first
general = general %>% mutate(midascheck = paste(midas,lake))
unique(general$midascheck)

#save
temp1 = plyr::count(general$lake)
temp = plyr::count(general$midascheck)
   
general = general %>% 
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

temp2 = plyr::count(general$lake)
  
#check for same number. yup
sum(temp1$freq)
sum(temp2$freq)

#create sample id
general = general %>% 
  mutate(sampleid=paste(midas, station, date, agency, sep="_"))

#get year and month
general$year = lubridate::year(general$date)
general$month = lubridate::month(general$date)


#* QC ######
names(general)

#wind velocity
str(general$windvel)
temp1 = plyr::count(general$windvel)
#change to numeric and compare
general$windvel = as.numeric(general$windvel)
temp2 = plyr::count(general$windvel)
summary(general$windvel)

#wind direction. look up codes...
str(general$winddir)
temp1 = plyr::count(general$winddir)
#change to numeric and compare
general$winddir = as.numeric(general$winddir)
temp2 = plyr::count(general$winddir)
summary(general$winddir)

#clouds
str(general$cloudcvr)
temp1 = plyr::count(general$cloudcvr)
unique(general$cloudcvr)
#NA where blank
general = general %>% mutate(cloudcvr = na_if(cloudcvr,''))

#secchi
str(general$secchi)
length(general$secchi)
temp1 = plyr::count(general$secchi)
general$secchi = as.numeric(general$secchi)
summary(general$secchi)

#secchi bottom
str(general$seccbot)
unique(general$seccbot)
temp1 = plyr::count(general$seccbot)
#NA where blank
general = general %>% mutate(seccbot = na_if(seccbot,''))
#change 'b' to 'y'
general = general %>% mutate(seccbot = replace(seccbot, seccbot=='b', 'y'))
temp2 = plyr::count(general$seccbot)
#scope
str(general$scope)
unique(general$scope)
#seems fine!
#rep
str(general$rep)
unique(general$rep)
#qacert
str(general$qa_cert)
unique(general$qa_cert)
general = general %>% mutate(qa_cert = na_if(qa_cert,''))
#gloeo?
str(general$gloeo)
unique(general$gloeo)
#comments
unique(general$comments)
#check for issues and mark resolved
general = general %>% select(-midascheck)

#save in library
write.csv(general, "~/CWD/dbCWD/library/survey.dep.csv", row.names = F)









#Profiles ######
profiles = read.csv("~/CWD/dbCWD/db.raw/db.dep/CWD_DO_TEMP_PROFILES.csv")
str(profiles)

#lowercase everything and rename cols
profiles = profiles %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(lake=laknam) %>% 
  dplyr::rename(datetime=sampdate)

#format date
profiles = profiles %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=T) %>% 
  mutate(date = as.Date(date))

length(unique(profiles$date)) #number of unique sample dates

#rename lakes
unique(profiles$lake)
#check midas first
profiles = profiles %>% mutate(midascheck = paste(midas,lake))
unique(profiles$midascheck)

#save
plyr::count(profiles$lake)
plyr::count(profiles$midascheck)

#problems 

#5254 smith (weeks) real midas 0254
#5254 french p , in somerville midas 5454
#5254 pleasant
#3814 long (mcwain) actual midas is 3418
#3814 cochnewagon

temp = profiles[profiles$midas==5254,]
temp = profiles[profiles$midas==3814,]

#drop french weeks and mcwain ponds
profiles = profiles %>% 
  filter(! lake=='french p') %>% 
  filter(! lake=='smith (weeks') %>% 
  filter(! lake=='long (mcwain')
  
temp1 = plyr::count(profiles$lake)

#rename
profiles = profiles %>% 
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

temp2 = plyr::count(profiles$lake)
#gained 1 lakes... little cobbossee
temp = profiles %>% filter(grepl('cobb',lake))
#correct?

#check for same number of rows. yup
sum(temp1$freq)
sum(temp2$freq)

#create sample id
profiles = profiles %>% 
  mutate(sampleid=paste(midas, station, date, agency, sep="_"))

#get year and month
profiles$year = lubridate::year(profiles$date)
profiles$month = lubridate::month(profiles$date)


#* QC ####
str(profiles)
unique(profiles$midas)
profiles$midas = as.numeric(profiles$midas)
unique(profiles$station)
profiles$station = as.numeric(profiles$station)
unique(profiles$depth)
profiles$depth = as.numeric(profiles$depth)
unique(profiles$temp)
profiles$temp = as.numeric(profiles$temp)
unique(profiles$oxygen)
profiles$oxygen = as.numeric(profiles$oxygen)
unique(profiles$oxymeth)
profiles = profiles %>% mutate(oxymeth = na_if(oxymeth,''))
unique(profiles$meter)
unique(profiles$calib)
profiles = profiles %>% mutate(calib = na_if(calib,''))
profiles$midas = as.numeric(profiles$midas)
profiles = profiles %>% select(-midascheck)
str(profiles)

summary(profiles$temp)
summary(profiles$oxygen)


#save
write.csv(profiles, "~/CWD/dbCWD/library/profiles.dep.csv", row.names = F)




#* summary ####
names(profiles)

#summary of each profile
spro = ddply(profiles, .(sampleid, midas, lake, station, date, agency), summarize, 
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
length(unique(profiles$sampleid))
samp = unique(profiles$sampleid)

# thermocline depth and epi/hypo temps
for (i in 1:length(samp)){ #for every unique sample,
  td = profiles[profiles$sampleid == samp[i], ] #temp dataframe that subsets profile df by each sampleid
  td = td[!duplicated(td$depth), ] #remove duplicate depths
  thermo = thermo.depth(td$temp, td$depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  spro[i,13] = thermo[1]  #paste thermocline depth in 12th column of row i*j == i in spro df
  #epi temp
  td1 = td[td$depth < thermo[1], ] #subset each df with depths above thermocline depth
  epi.temp = mean(td1$temp) #get average temp of epi from subsetted df
  spro[i,14] = epi.temp[1] #paste epi temp
  #hypo temp
  td2 = td[td$depth > thermo[1], ] #subset each df with depths below thermocline depth
  hypo.temp = mean(td2$temp) #get average temp of hypo from subsetted df
  spro[i,15] = hypo.temp[1]  }  #paste hypo temp
  

# get meta depths
for (i in 1:length(samp)){ #for every unique sample,
  td = profiles[profiles$sampleid == samp[i], ] #temp dataframe that subsets profile df by each sampleid
  td = td[!duplicated(td$depth), ] #remove duplicate depths
  meta = meta.depths(td$temp, td$depth, slope=0.1, seasonal=F, mixed.cutoff=1) 
  spro[i,16] = meta[1]   #top
  spro[i,17] = meta[2] }  #bottom
    
# get max DO depth for each profile
for (i in 1:length(samp)){ #for every unique sample,
  td = profiles[profiles$sampleid == samp[i], ] #temp dataframe that subsets profile df by each sampleid
  td = td[td$oxygen == max(td$oxygen),] #subset row of only max oxygen
  do.max.depth = td$depth #save depth at max do
  spro[i,23] = do.max.depth[1] }   #paste depth of max DO 

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

temp = plyr::count(spro$sampleid)


write.csv(spro, "~/CWD/dbCWD/library/profiles.dep.summary.csv", row.names = F)





#compare with volume weighted epi/hypo temps once i get bathymetry data

#load wilson bathy
wb = read.csv("~/CWD/dbCWD/db.raw/bathy.wilson.csv")

#summary table for just wilson
subp = profiles %>% filter(lake=='wilson')            

pbj = ddply(subp, .(sampleid, midas, lake, date), summarize, 
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
length(unique(pbj$sampleid))
samp = unique(pbj$sampleid)

# thermocline depth and epi/hypo temps
for (i in 1:length(samp)){ #for every unique sample,
  td = subp[subp$sampleid == samp[i], ] #temp dataframe that subsets profile df by each sampleid
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
  td = subp[subp$sampleid == samp[i], ] #temp dataframe that subsets profile df by each sampleid
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

















#Phos DEP ######
phos = read.csv("~/CWD/dbCWD/db.raw/db.dep/CWD_PHOS.csv")

#lowercase everything and rename cols
phos = phos %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(lake=laknam) %>% 
  dplyr::rename(datetime=sampdate)

#format date
phos = phos %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=T) %>% 
  mutate(date = as.Date(date))

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
unique(phos$agency) #weird
unique(phos$project) #not sure
unique(phos$lab) #fine
unique(phos$qual)#good
unique(phos$type) #good
#depth
unique(phos$depth)
phos$depth = as.numeric(phos$depth)
#rep
unique(phos$rep)
phos$rep = as.numeric(phos$rep)
#count
unique(phos$count)
phos$count = as.numeric(phos$count)


#Phosphorus ppm
unique(phos$tp_ppm)
str(phos$tp_ppm)
temp = plyr::count(phos$tp_ppm)
sum(temp$freq) #13589 samples wow
phos$tp_ppm = as.numeric(phos$tp_ppm)
temp2 = plyr::count(phos$tp_ppm)
sum(temp2$freq) #the same. 
summary(phos$tp_ppm)

#get year and month
phos$year = lubridate::year(phos$date)
phos$month = lubridate::month(phos$date)


#many values different from CWD dbCWD values, not good



#save
phos = phos %>% select(-midascheck)

write.csv(phos, "~/CWD/dbCWD/library/tp.dep.csv", row.names = F)










#Phos CWD ########

phos = read.csv("~/CWD/dbCWD/db.raw/db.cwd/cwd.tp.csv")


#lowercase everything and rename cols
phos = phos %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(date=sampdate)

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
phos$basin = as.numeric(phos$basin)
unique(phos$type) #good
#depth
unique(phos$depth)
phos$depth = as.numeric(phos$depth) #remvove 999s on lake by lake basis i guess for now
#rep
unique(phos$r)
phos$r = as.numeric(phos$r)

#Phosphorus ppm
unique(phos$tp_ppm)
str(phos$tp_ppm)
temp = plyr::count(phos$tp_ppm)
sum(temp$freq) #6486 compared to 13589 dep 
phos$tp_ppm = as.numeric(phos$tp_ppm)
temp2 = plyr::count(phos$tp_ppm)
sum(temp2$freq) #the same. 
summary(phos$tp_ppm)

#change 99.999s to NA. 17 total
phos = phos %>% mutate(tp_ppm = replace(tp_ppm, tp_ppm > 99, NA))

#get year and month
phos$year = lubridate::year(phos$date)
phos$month = lubridate::month(phos$date)


#many values different from CWD dbCWD values, not good


#save
phos = phos %>% select(-midascheck)
phos = phos %>% mutate(sampleid=paste(midas, basin, date, sep="_"))


write.csv(phos, "~/CWD/dbCWD/library/tp.cwd.csv", row.names = F)












#Chl ######
chl = read.csv("~/CWD/dbCWD/db.raw/db.dep/CWD_CHLORO.csv")
str(chl)


#lowercase everything and rename cols
chl = chl %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(lake=laknam) %>% 
  dplyr::rename(datetime=sampdate)

#format date
chl = chl %>% 
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

#get year and month
chl$year = lubridate::year(chl$date)
chl$month = lubridate::month(chl$date)


#save
chl = chl %>% select(-midascheck)

write.csv(chl, "~/CWD/dbCWD/library/chlorophyll.dep.csv", row.names = F)














#Chem DEP ######
chem = read.csv("~/CWD/dbCWD/db.raw/db.dep/CWD_CHEM.csv")
names(chem)
str(chem)

#lowercase everything and rename cols
chem = chem %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(lake=laknam) %>% 
  dplyr::rename(datetime=sampdate)

#format date
chem = chem %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=T) %>% 
  mutate(date = as.Date(date))

length(unique(chem$date)) #number of unique sample dates

#rename lakes
unique(chem$lake)
#check midas first
chem = chem %>% mutate(midascheck = paste(midas,lake))
unique(chem$midascheck)

#save
temp1 = plyr::count(chem$lake)
temp = plyr::count(chem$midascheck)

chem = chem %>% 
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

temp2 = plyr::count(chem$lake)

#check for same number. yup
sum(temp1$freq)
sum(temp2$freq)

#create sample id
chem = chem %>% 
  mutate(sampleid=paste(midas, station, date, agency, sep="_"))

#get year and month
chem$year = lubridate::year(chem$date)
chem$month = lubridate::month(chem$date)



#* QC ########
str(chem)
chem$midas = as.numeric(chem$midas)
chem$station = as.numeric(chem$station)
unique(chem$station)
unique(chem$project)
chem$project = as.numeric(chem$project)
unique(chem$depth)
chem$depth = as.numeric(chem$depth)
unique(chem$type)
chem = chem %>% mutate(type = na_if(type,''))
#ph
unique(chem$ph)
chem$ph = as.numeric(chem$ph)
unique(chem$phmeth)
chem = chem %>% mutate(phmeth = na_if(phmeth,''))
unique(chem$phlab)
chem = chem %>% mutate(phlab = na_if(phlab,''))
summary(chem$ph)
#color
unique(chem$color)
chem$color = as.numeric(chem$color)
unique(chem$aort)
chem = chem %>% mutate(aort = na_if(aort,''))
unique(chem$colrmeth)
chem = chem %>% mutate(colrmeth = na_if(colrmeth,''))
unique(chem$colrlab)
chem = chem %>% mutate(colrlab = na_if(colrlab,''))
summary(chem$color)
#conduct
unique(chem$conduct)
chem$conduct = as.numeric(chem$conduct)
unique(chem$condmeth)
chem = chem %>% mutate(condmeth = na_if(condmeth,''))
unique(chem$condlab)
chem = chem %>% mutate(condlab = na_if(condlab,''))
summary(chem$conduct)
#alkalinity
unique(chem$alk)
chem$alk = as.numeric(chem$alk)
unique(chem$alkmeth)
chem = chem %>% mutate(alkmeth = na_if(alkmeth,''))
unique(chem$alklab)
chem = chem %>% mutate(alklab = na_if(alklab,''))
summary(chem$alk)

chem = chem %>% select(-midascheck)

#save
write.csv(chem, "~/CWD/dbCWD/library/chem.dep.csv", row.names = F)







#Chem 2 #######

# ~ a d v a n c e d  c h e m i s t r y ~ * 

ac = read.csv("~/CWD/dbCWD/db.raw/db.dep/CWD_ADV_CHEM_1996-2012.csv")
str(ac)
names(ac)

#lowercase everything and rename cols
ac = ac %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(lake=sampleid) %>% 
  dplyr::rename(datetime=sampdate)

#format date
ac = ac %>% 
  separate(datetime, c("date", "time"), sep = " ", remove=T) %>% 
  mutate(date = as.Date(date))

length(unique(ac$date)) #number of unique sample dates

#rename lakes
unique(ac$lake)
#check midas first
ac = ac %>% mutate(midascheck = paste(midas,lake))
unique(ac$midascheck)

#save
temp1 = plyr::count(ac$lake)
temp = plyr::count(ac$midascheck)

ac = ac %>% 
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

temp2 = plyr::count(ac$lake)

#check for same number. yup
sum(temp1$freq)
sum(temp2$freq)

#create sample id
ac = ac %>% 
  mutate(sampleid=paste(midas, station, date, agency, sep="_"))

#get year and month
ac$year = lubridate::year(ac$date)
ac$month = lubridate::month(ac$date)



#* QC ########
str(ac)
names(ac)
ac$midas = as.numeric(ac$midas)
ac$station = as.numeric(ac$station)
ac = ac %>% rename(depth=depth_m)
unique(ac$depth)
ac$depth = as.numeric(ac$depth)
summary(ac$depth)
unique(ac$type)
unique(ac$rep)
ac$rep = as.numeric(ac$rep)
#variables
#ca
unique(ac$ca_mg_l)
ac$ca_mg_l = as.numeric(ac$ca_mg_l)
summary(ac$ca_mg_l)
#mg
unique(ac$mg_mg_l)
ac$mg_mg_l = as.numeric(ac$mg_mg_l)
summary(ac$mg_mg_l)
#k
unique(ac$k_mg_l)
ac$k_mg_l = as.numeric(ac$k_mg_l)
summary(ac$k_mg_l)
#na
unique(ac$na_mg_l)
ac$na_mg_l = as.numeric(ac$na_mg_l)
summary(ac$na_mg_l)
#cl_ueq_l
unique(ac$cl_ueq_l)
ac$cl_ueq_l = as.numeric(ac$cl_ueq_l)
summary(ac$cl_ueq_l)
#no3_ueq_l 
unique(ac$no3_ueq_l) #useless!
#so4_ueq_l
unique(ac$so4_ueq_l)
ac$so4_ueq_l = as.numeric(ac$so4_ueq_l)
summary(ac$so4_ueq_l)
#doc
unique(ac$doc_mg_l)
ac$doc_mg_l = as.numeric(ac$doc_mg_l)
summary(ac$doc_mg_l)
#anc
unique(ac$anc_ueq_l)
ac$anc_ueq_l = as.numeric(ac$anc_ueq_l)
summary(ac$anc_ueq_l)
#nh4
unique(ac$nh4_mg_l)
#si
unique(ac$si_mg_l)
ac$si_mg_l = as.numeric(ac$si_mg_l)
summary(ac$si_mg_l)
#al 
unique(ac$al_ug_l) #no
#fe
unique(ac$fe_ug_l)
ac$fe_ug_l = as.numeric(ac$fe_ug_l)
summary(ac$fe_ug_l)
#cond
unique(ac$con_us_cm)
ac$con_us_cm = as.numeric(ac$con_us_cm)
summary(ac$con_us_cm)
#eq
unique(ac$eq_ph)
#t color
unique(ac$tcolor_pcu)
ac$tcolor_pcu = as.numeric(ac$tcolor_pcu)
summary(ac$tcolor_pcu)
#a color
unique(ac$acolor_pcu)
ac$acolor_pcu = as.numeric(ac$acolor_pcu)
summary(ac$acolor_pcu)
#clph
unique(ac$clph)
ac$clph = as.numeric(ac$clph)
summary(ac$clph)
#anc ph
unique(ac$ancph)
ac$ancph = as.numeric(ac$ancph)
summary(ac$ancph)
#tn
unique(ac$tn_mg_)
ac$tn_mg_ = as.numeric(ac$tn_mg_)
summary(ac$tn_mg_)






#Chem CWD ########
#cwd database chem stuff. ph, alk, color, conductivity


chem2 = read.csv("~/CWD/dbCWD/db.raw/db.cwd/cwd.chem.csv")


names(chem2)
str(chem2)

#lowercase everything and rename cols
chem2 = chem2 %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) %>% 
  dplyr::rename(date=sampdate)

length(unique(chem2$date)) #number of unique sample dates

#rename lakes
unique(chem2$lake)
#check midas first
chem2 = chem2 %>% mutate(midascheck = paste(midas,lake))
unique(chem2$midascheck)

#save
temp1 = plyr::count(chem2$lake)
temp = plyr::count(chem2$midascheck)

chem2 = chem2 %>% 
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

temp2 = plyr::count(chem2$lake)

#check for same number. yup
sum(temp1$freq)
sum(temp2$freq)

#get year and month
chem2$year = lubridate::year(chem2$date)
chem2$month = lubridate::month(chem2$date)



#* QC ########
str(chem2)
chem2$midas = as.numeric(chem2$midas)
chem2$basin = as.numeric(chem2$basin)
unique(chem2$basin)
unique(chem2$depth)
chem2$depth = as.numeric(chem2$depth)
unique(chem2$type)
#ph
unique(chem2$ph)
chem2$ph = as.numeric(chem2$ph)
temp1 = plyr::count(chem2$ph)
#drop 9999s
chem2 = chem2 %>% mutate(ph = replace(ph, ph > 99, NA))
summary(chem2$ph)
#color
unique(chem2$color)
chem2$color = as.numeric(chem2$color)
plyr::count(chem2$color)
chem2 = chem2 %>% mutate(color = replace(color, color > 99, NA))
summary(chem2$color)
#conduct
unique(chem2$conduct)
chem2$conduct = as.numeric(chem2$conduct)
plyr::count(chem2$conduct)
#drop 9999999
chem2 = chem2 %>% mutate(conduct = replace(conduct, conduct > 999, NA))
summary(chem2$conduct)
#alkalinity
unique(chem2$alk)
chem2$alk = as.numeric(chem2$alk)
plyr::count(chem2$alk)
chem2 = chem2 %>% mutate(alk = replace(alk, alk > 99, NA))
summary(chem2$alk)


#save
chem2 = chem2 %>% select(-midascheck)
chem2 = chem2 %>% mutate(sampleid=paste(midas, basin, date, sep="_"))

write.csv(chem2, "~/CWD/dbCWD/library/chem.cwd.csv", row.names = F)














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
wtp = read.csv("~/CWD/dbCWD/db.raw/dbCWD/tp.wilson2021.csv")
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


