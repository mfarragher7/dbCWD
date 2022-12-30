# Summary statistics for profiles
# updated 2020-11-25

#load survey data 
full.exo = read.csv("library/db.exo.csv",header=T)
full.c3 = read.csv("library/db.c3.csv",header=T)
seasonID = read.csv("library/seasonID.csv",header=T)

#libraries
library(plyr)
library(dplyr)
library(rLakeAnalyzer)
library(stringr)


#Workflow
#min, max, mean, sd
#of exo (temp, DO,fdom, cond) and c3 (temp, chl, phyco, cdom)
#plus depth of max for DO, chl, phyco, cond, and thermocline  

#exo summary ####
summary.exo = ddply(full.exo, .(lakedate), summarize,  #select data and group
                    temp_min_exo=min(Temp), temp_max_exo=max(Temp), temp_mean_exo=mean(Temp), temp_sd_exo=sd(Temp), #temp - min, max, mean, sd
                    temp_se_exo=temp_sd_exo/sqrt(sum(!is.na(Temp))), thermocline_depth_exo=NA,  #temp se, empty thermo.depth
                    do_min=min(DO_sat), do_max=max(DO_sat), do_mean=mean(DO_sat), do_sd=sd(DO_sat), #do_sat min, max, mean, sd
                    do_se=do_sd/sqrt(sum(!is.na(DO_sat))), do_max_depth=NA,  #do_sat se, empty DO max depth
                    fdom_min=min(fDOM_RFU_corr), fdom_max=max(fDOM_RFU_corr), fdom_mean=mean(fDOM_RFU_corr), fdom_sd=sd(fDOM_RFU_corr), #fdom min, max, mean, sd 
                    fdom_se=fdom_sd/sqrt(sum(!is.na(fDOM_RFU_corr))), fdom_max_depth=NA, #fdom se, empty fdom max depth
                    cond_min=min(SpCond), cond_max=max(SpCond), cond_mean=mean(SpCond), cond_sd=sd(SpCond),  #cond min, max, mean, sd
                    cond_se=cond_sd/sqrt(sum(!is.na(SpCond))), cond_max_depth=NA) #cond se, empty cond max depth
              
#4 for loops dor depth max
#save list of exo lakedates. can't use unique() because not in the same order. re-do for each dataframe
lake.dates = summary.exo$lakedate
#thermocline depth
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = full.exo[full.exo$lakedate == lake.dates[i], ] #temp dataframe that subsets summary.exo by each lakedate
  thermo = thermo.depth(temp$Temp,temp$Depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  summary.exo[i,7] = thermo[1] }    #calculate thermocline depth in 7th column of row i*j == i = for each unique date
#get max DO depth for each profile
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = full.exo[full.exo$lakedate == lake.dates[i], ] #temp dataframe that subsets full.exo by each lakedate
  temp = temp[temp$DO_sat == max(temp$DO_sat),] #subset row of only max DO_sat
  do_max_depth = temp$Depth #save Depth at max DO_sat
  summary.exo[i,13] = do_max_depth[1] }   #paste depth of max DO in 13th column of row i*j == i = for each unique date
#get max fdom depth for each profile
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = full.exo[full.exo$lakedate == lake.dates[i], ] #temp dataframe that subsets full.exo by each lakedate
  temp = temp[temp$fDOM_RFU_corr == max(temp$fDOM_RFU_corr),] #subset row of only max fdom
  do_max_fdom = temp$Depth #save Depth at max fdom
  summary.exo[i,19] = do_max_fdom[1] }   #paste depth of max fdom in 19th column of row i*j == i = for each unique date
#get max spcond depth for each profile
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = full.exo[full.exo$lakedate == lake.dates[i], ] #temp dataframe that subsets full.exo by each lakedate
  temp = temp[temp$SpCond == max(temp$SpCond),] #subset row of only max spCond
  do_max_spcond = temp$Depth #save Depth at max fdom
  summary.exo[i,25] = do_max_spcond[1] }   #paste depth of max spcond in 25th column of row i*j == i = for each unique date
#check structure
str(summary.exo)

#clean up exo summary issues
summary.exo = summary.exo %>% 
  #mutate_if(is.numeric, list(~na_if(lakedate, 'JP_2020-08-19'))) %>% #remove JP 2020-08-19 date (No EXO profile)
  mutate(thermocline_depth_exo = replace(thermocline_depth_exo,thermocline_depth_exo=='NaN',NA)) %>%  #change NaN to NA
  mutate(thermocline_depth_exo = replace(thermocline_depth_exo, grepl("JP_2020-02-21",lakedate),NA)) %>% #remove winter thermocline values
  mutate(thermocline_depth_exo = replace(thermocline_depth_exo, grepl("SC_2020-02-21",lakedate),NA)) %>% 
  mutate(thermocline_depth_exo = replace(thermocline_depth_exo, grepl("WH_2020-02-25",lakedate),NA))
#remove JP 2020-08-19 data (No EXO profile)
summary.exo[summary.exo$lakedate=='JP_2020-08-19',2:25] = NA

#fix seasonID, etc
summary.exo = summary.exo %>% separate(lakedate, c("LakeID", "Date"), sep = "_", remove=F)
summary.exo = join(summary.exo,seasonID,by='lakedate')
#move seasonID up
summary.exo = summary.exo %>% relocate(seasonID, .after = Date)
#coerce date
summary.exo$Date = as.Date(summary.exo$Date)
str(summary.exo)

#write csv
write.csv(summary.exo,'library/profiles_exo.csv',row.names=F)  


#C3 loess ####

#new df to work through loess smoothed profiles
c3.loess = full.c3 

#add smoothed profile for each profile
#c3.loess$chla_loess = predict(loess(Chlorophyll_a~Depth, data=full.c3, span=0.75))
#doesn't work like that

#compare to smoothed summary
#test - modeling profiles and save in new dataframe
#test=c3.loess %>% filter(lakedate=='SC_2020-08-03') #temp dataframe
#plot(Chlorophyll_a~Depth,test)
#plot(chla_loess~Depth,test)
#test another
#test=c3.loess %>% filter(lakedate=='JP_2020-10-12') #temp dataframe
#plot(Chlorophyll_a~Depth,test)
#plot(chla_loess~Depth,test)

# used these functions
#predict(loess(Chlorophyll_a~Depth, data=c3.loess, span=0.75))
#predict(loess(Phycocyanin~Depth, data=c3.loess, span=0.75))
#predict(loess(CDOM~Depth, data=c3.loess, span=0.75))

#summary for smoothed profiles. mostly empty, 1 for loop for each parameter
summary.c3.loess = ddply(c3.loess, .(lakedate), summarize,
                         temp_min_c3=min(Temp), temp_max_c3=max(Temp), temp_mean_c3=mean(Temp), temp_sd_c3=sd(Temp), #temp min, max, mean, sd
                         temp_se_c3=temp_sd_c3/sqrt(sum(!is.na(Temp))), thermocline_depth_c3 = NA, #empty thermo.depth
                         chl_min=NA, chl_max=NA, chl_mean=NA, chl_sd=NA, chl_se=NA, chl_max_depth = NA, #chl
                         phyco_min=NA, phyco_max=NA, phyco_mean=NA, phyco_sd=NA, phyco_se=NA, phyco_max_depth = NA, #phyco
                         cdom_min=NA, cdom_max=NA, cdom_mean=NA, cdom_sd=NA, cdom_se=NA, cdom_max_depth = NA) #cdom

#4 x for loop
#save lakedates for c3
lake.dates = summary.c3.loess$lakedate
#thermocline depth. same as above
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3.loess[c3.loess$lakedate == lake.dates[i], ] #temp dataframe that subsets c3.loess by each lakedate
  thermo = thermo.depth(temp$Temp,temp$Depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  summary.c3.loess[i,7] = thermo[1] }  #calculate thermocline depth in col 7
#chlorophyll 
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3.loess[c3.loess$lakedate == lake.dates[i], ] #temp dataframe that subsets c3.loess by each lakedate
  temp$chla_loess = predict(loess(Chlorophyll_a~Depth, data=temp, span=0.8)) #new column of loessed profile
  summary.c3.loess[i,8] = min(temp$chla_loess) #min 
  summary.c3.loess[i,9] = max(temp$chla_loess) #max 
  summary.c3.loess[i,10] = mean(temp$chla_loess) #mean
  summary.c3.loess[i,11] = sd(temp$chla_loess) #sd
  summary.c3.loess[i,12] = sd(temp$chla_loess)/sqrt(sum(!is.na(temp$chla_loess))) #se 
  temp = temp[temp$chla_loess == max(temp$chla_loess),] #subset row of only max Chlorophyll_a
  chl_max_depth = temp$Depth #save Depth at max Chlorophyll_a
  summary.c3.loess[i,13] = chl_max_depth[1] } #paste depth of max chl in 13th col
#phycocyanin
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3.loess[c3.loess$lakedate == lake.dates[i], ] #temp dataframe that subsets c3.loess by each lakedate
  temp$phy_loess = predict(loess(Phycocyanin~Depth, data=temp, span=0.8)) #new col for loessed profile
  summary.c3.loess[i,14] = min(temp$phy_loess) #min 
  summary.c3.loess[i,15] = max(temp$phy_loess) #max 
  summary.c3.loess[i,16] = mean(temp$phy_loess) #mean
  summary.c3.loess[i,17] = sd(temp$phy_loess) #sd
  summary.c3.loess[i,18] = sd(temp$phy_loess)/sqrt(sum(!is.na(temp$phy_loess))) #se 
  temp = temp[temp$phy_loess == max(temp$phy_loess),] #subset row of only max Phycocyanin
  phyco_max_depth = temp$Depth #save Depth at max Phycocyanin
  summary.c3.loess[i,19] = phyco_max_depth[1] }  #paste depth of max phyco in 19th col
#CDOM
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3.loess[c3.loess$lakedate == lake.dates[i], ] #temp dataframe that subsets c3.loess by each lakedate
  temp$cdom_loess = predict(loess(CDOM~Depth, data=temp, span=0.8)) #new col for loessed profile
  summary.c3.loess[i,20] = min(temp$cdom_loess) #min 
  summary.c3.loess[i,21] = max(temp$cdom_loess) #max 
  summary.c3.loess[i,22] = mean(temp$cdom_loess) #mean
  summary.c3.loess[i,23] = sd(temp$cdom_loess) #sd
  summary.c3.loess[i,24] = sd(temp$cdom_loess)/sqrt(sum(!is.na(temp$cdom_loess))) #se 
  temp = temp[temp$cdom_loess == max(temp$cdom_loess),] #subset row of only max cdom
  cdom_max_depth = temp$Depth #save Depth at max cdom
  summary.c3.loess[i,25] = cdom_max_depth[1] }  #paste depth of max cdom in 25th col

#fix summary table
summary.c3.loess = summary.c3.loess %>% 
  mutate(thermocline_depth_c3 = replace(thermocline_depth_c3,thermocline_depth_c3=='NaN',NA)) 
#change 0's in cdom to NA
summary.c3.loess[summary.c3.loess==0] = NA
summary.c3.loess$cdom_max_depth[summary.c3.loess$cdom_max_depth<2] = NA


#fix seasonID, etc
summary.c3.loess = summary.c3.loess %>% separate(lakedate, c("LakeID", "Date"), sep = "_", remove=F)
summary.c3.loess = join(summary.c3.loess,seasonID,by='lakedate')
#move seasonID up
summary.c3.loess = summary.c3.loess %>% relocate(seasonID, .after = Date)
#coerce date
summary.c3.loess$Date = as.Date(summary.c3.loess$Date)
str(summary.c3.loess)

#write csv
write.csv(summary.c3.loess,'library/profiles_c3loess.csv',row.names=F)  


