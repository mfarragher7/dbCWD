# cwd long term data
# using data jeremy sent me 2022-11-14
# 1971-2018. will eventually include recent years too

# file paths -> -> ->
# read in locally, modify, save to library
# for further manipulation, read in using github web addresses. must be in 'raw' format


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
md = read.csv("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/lakemd.csv")



#Secchi and survey, in new code :)
#temp and do profiles, in new code
#chlorophyll,  in new code













#Phos DEP ######
phos = read.csv("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.dep/CWD_PHOS.csv")

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

write.csv(phos, "C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/tp.dep.csv", row.names = F)










#Phos CWD ########

phos = read.csv("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.cwd/cwd.tp.csv")


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




#save
phos = phos %>% select(-midascheck)
phos = phos %>% mutate(sampleid=paste(midas, basin, date, sep="_"))






















#Chem DEP ######
chem = read.csv("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.dep/CWD_CHEM.csv")
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
write.csv(chem, "C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/chem.dep.csv", row.names = F)







#Chem 2 #######

# ~ a d v a n c e d  c h e m i s t r y ~ * 

ac = read.csv("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.dep/CWD_ADV_CHEM_1996-2012.csv")
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


chem2 = read.csv("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.cwd/cwd.chem.csv")


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

write.csv(chem2, "C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/chem.cwd.csv", row.names = F)












