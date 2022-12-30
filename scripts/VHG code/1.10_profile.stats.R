#concise profiles summary
#2021-04-18


#EXO
#C3 - loess smoothed
#kd, 1%PAR
#nutrients (averaged and each layer)
#DCM



#libraries
library(plyr)
library(dplyr)
library(rLakeAnalyzer)
library(tidyr)
library(tidyverse)


#EXO #########

#load EXO
exo = read.csv("library/db.exo.csv",header=T)


summary.exo = ddply(exo, .(lakedate,LakeID,Date,seasonID), summarize,  #select data and group
                #temp - min, max, mean, sd, se, thermocline depth
                    temp.min.exo=min(Temp), 
                    temp.max.exo=max(Temp), 
                    temp.mean.exo=mean(Temp), 
                    temp.sd.exo=sd(Temp), 
                    temp.se.exo=temp.sd.exo/sqrt(sum(!is.na(Temp))), 
                    thermocline.depth.exo=NA,
                #do.sat
                    do.min=min(DO_sat), 
                    do.max=max(DO_sat), 
                    do.mean=mean(DO_sat), 
                    do.sd=sd(DO_sat), 
                    do.se=do.sd/sqrt(sum(!is.na(DO_sat))), 
                    do.max.depth=NA,  
                 #fDOM
                    fdom.min=min(fDOM_RFU_corr), 
                    fdom.max=max(fDOM_RFU_corr), 
                    fdom.mean=mean(fDOM_RFU_corr), 
                    fdom.sd=sd(fDOM_RFU_corr),
                    fdom.se=fdom.sd/sqrt(sum(!is.na(fDOM_RFU_corr))), 
                    fdom.max.depth=NA, 
                 #conductivity
                    cond.min=min(SpCond), 
                    cond.max=max(SpCond), 
                    cond.mean=mean(SpCond), 
                    cond.sd=sd(SpCond), 
                    cond.se=cond.sd/sqrt(sum(!is.na(SpCond))), 
                    cond.max.depth=NA) 


#4 for loops to calculate max depth of each parameter

#save list of exo lakedates. 
lake.dates = summary.exo$lakedate

#thermocline depth
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = exo[exo$lakedate == lake.dates[i], ] #temp dataframe that subsets summary.exo by each lakedate
  thermo = thermo.depth(temp$Temp,temp$Depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  summary.exo[i,10] = thermo[1] }    #calculate thermocline depth in 10th column of row i*j == i = for each unique date

#get max DO depth for each profile
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = exo[exo$lakedate == lake.dates[i], ] #temp dataframe that subsets full.exo by each lakedate
  temp = temp[temp$DO_sat == max(temp$DO_sat),] #subset row of only max DO_sat
  do.max.depth = temp$Depth #save Depth at max DO_sat
  summary.exo[i,16] = do.max.depth[1] }   #paste depth of max DO 

#get max fdom depth for each profile
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = exo[exo$lakedate == lake.dates[i], ] #temp dataframe that subsets full.exo by each lakedate
  temp = temp[temp$fDOM_RFU_corr == max(temp$fDOM_RFU_corr),] #subset row of only max fdom
  do_max_fdom = temp$Depth #save Depth at max fdom
  summary.exo[i,22] = do_max_fdom[1] }   #paste depth of max fdom

#get max spcond depth for each profile
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = exo[exo$lakedate == lake.dates[i], ] #temp dataframe that subsets full.exo by each lakedate
  temp = temp[temp$SpCond == max(temp$SpCond),] #subset row of only max spCond
  do_max_spcond = temp$Depth #save Depth at max fdom
  summary.exo[i,28] = do_max_spcond[1] }   #paste depth of max spcond 

#check structure
str(summary.exo)

#clean up exo summary issues
summary.exo = summary.exo %>% 
  #mutate_if(is.numeric, list(~na_if(lakedate, 'JP_2020-08-19'))) %>% #remove JP 2020-08-19 date (No EXO profile)
  mutate(thermocline.depth.exo = replace(thermocline.depth.exo,thermocline.depth.exo=='NaN',NA)) %>%  #change NaN to NA
  mutate(thermocline.depth.exo = replace(thermocline.depth.exo, grepl("JP_2020-02-21",lakedate),NA)) %>% #remove winter thermocline values
  mutate(thermocline.depth.exo = replace(thermocline.depth.exo, grepl("SC_2020-02-21",lakedate),NA)) %>% 
  mutate(thermocline.depth.exo = replace(thermocline.depth.exo, grepl("WH_2020-02-25",lakedate),NA))

#remove JP 2020-08-19 data (No EXO profile)
summary.exo[summary.exo$lakedate=='JP_2020-08-19',5:28] = NA

#coerce date
summary.exo$Date = as.Date(summary.exo$Date)
str(summary.exo)

#write csv
write.csv(summary.exo,'library/profiles.exo.csv',row.names=F)  







# C3 #########

#load C3
c3 = read.csv("library/db.c3.csv",header=T)

#remove top 0.5m
c3 = c3 %>% filter(Depth > 0.5)



#loess functions
#predict(loess(Chlorophyll_a~Depth, data=c3.loess, span=0.8))
#predict(loess(Phycocyanin~Depth, data=c3.loess, span=0.8))
#predict(loess(CDOM~Depth, data=c3.loess, span=0.8))

#compare to smoothed summary
# (see script 1.11 for better loess smoothing span comparison)
# span=0.8 using loess function, span=0.75 using ggplot loess option. they match
temp = c3 %>% filter(lakedate=='SC_2020-08-03')
plot(Chlorophyll_a ~ Depth, temp)
plot(predict(loess(Chlorophyll_a~Depth, data=temp, span=0.8)) ~ Depth, temp)


#buoyancy frequency function
#buoyancy.freq(wtr, depths)
#wtr: a numeric vector of water temperature in degrees C
#depths: a numeric vector corresponding to the depths (in m) of the wtr measurements

#compare c3 and exo. report max buoyancy freq?
temp2 = exo %>% filter(lakedate=='SC_2020-08-03') 

buoyancy.freq(temp$Temp,temp$Depth); max(buoyancy.freq(temp$Temp,temp$Depth))
buoyancy.freq(temp2$Temp,temp2$Depth); max(buoyancy.freq(temp2$Temp,temp2$Depth))


#schmidt stability index
#load bathymetry data
bath.jp = read.csv("library/bath.jp.csv",header=T)
bath.sc = read.csv("library/bath.sc.csv",header=T)
bath.bb = read.csv("library/bath.bb.csv",header=T)
bath.wh = read.csv("library/bath.wh.csv",header=T)

#how function looks
#schmidt.stability(wtr, depths, bthA, bthD, sal = 0)
#wtr: numeric vector of water temperature in degrees C
#depths: a numeric vector corresponding to the depths (in m) of the wtr measurements
#bthA: a numeric vector of cross sectional areas (m^2) corresponding to bthD depths
#bthD: a numeric vector of depths (m) which correspond to areal measures in bthA
#sal: a numeric vector of salinity in Practical Salinity Scale units

#test, check salinity
schmidt.stability(temp$Temp,temp$Depth,bath.sc$area_m2,bath.sc$upper_m,sal=0)
schmidt.stability(temp$Temp,temp$Depth,bath.sc$area_m2,bath.sc$upper_m,sal=0.02)


temp = c3 %>% filter(lakedate=='JP_2020-08-04')
schmidt.stability(temp$Temp,temp$Depth,bath.jp$area_m2,bath.jp$upper_m,sal=0)








#save summary dataframe for smoothed profiles

summary.c3 = ddply(c3, .(lakedate,LakeID,Date,seasonID), summarize,
                #temp - min, max, mean, sd, se, thermocline depth, buoyancy freq, schmidt stability 
                   temp.min.c3=min(Temp), 
                   temp.max.c3=max(Temp), 
                   temp.mean.c3=mean(Temp), 
                   temp.sd.c3=sd(Temp),
                   temp.se.c3=temp.sd.c3/sqrt(sum(!is.na(Temp))), 
                   thermocline.depth.c3=NA, 
                   buoyancy.freq=NA,
                   schmidt.index=NA,
                #fchl
                   fchl.min=NA, 
                   fchl.max=NA, 
                   fchl.mean=NA, 
                   fchl.sd=NA, 
                   fchl.se=NA, 
                   fchl.max.depth=NA,
                #phyco
                   phyco.min=NA, 
                   phyco.max=NA, 
                   phyco.mean=NA, 
                   phyco.sd=NA,
                   phyco.se=NA, 
                   phyco.max.depth=NA,
                #cdom
                   cdom.min=NA, 
                   cdom.max=NA, 
                   cdom.mean=NA, 
                   cdom.sd=NA, 
                   cdom.se=NA,
                   cdom.max.depth=NA) 


#for loops

#save lakedates for c3
lake.dates = summary.c3$lakedate

#thermocline depth. same as above
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3[c3$lakedate == lake.dates[i], ] #temp dataframe that subsets c3 by each lakedate
  thermo = thermo.depth(temp$Temp,temp$Depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  summary.c3[i,10] = thermo[1] }  #calculate thermocline depth in col 10

#buoyancy freq
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3[c3$lakedate == lake.dates[i], ] 
  bf = max(buoyancy.freq(temp$Temp,temp$Depth)) #run bf function, save max value
  summary.c3[i,11] = bf[1] }  #calculate bf

#schmidt stability index
#save schmidt stability values
for (i in 1:length(lake.dates)){ #for every lakedate,
  
  temp = c3[c3$lakedate == lake.dates[i], ] #temp dataframe that subsets c3 df by each lakedate
  temp = temp[!duplicated(temp$Depth), ] #remove duplicate depths
  temp = temp[rowSums(is.na(temp)) != ncol(temp), ] 
  temp = temp[!duplicated(temp$Depth), ] 
  
  if(grepl('JP',temp$LakeID)){  #if lakedate [i] is for JP
    #calculate schmidt stability using jp bathy
    summary.c3[i,12] = schmidt.stability(temp$Temp,temp$Depth,bath.jp$area_m2,bath.jp$upper_m,sal=0) 
  }
  else{
    if(grepl('SC',temp$LakeID)){
      summary.c3[i,12] = schmidt.stability(temp$Temp,temp$Depth,bath.sc$area_m2,bath.sc$upper_m,sal=0)
    }
    else{
      if(grepl('BB',temp$LakeID)){
        summary.c3[i,12] = schmidt.stability(temp$Temp,temp$Depth,bath.bb$area_m2,bath.bb$upper_m,sal=0)
      }
      else{
        if(grepl('WH',temp$LakeID)){
          summary.c3[i,12] = schmidt.stability(temp$Temp,temp$Depth,bath.wh$area_m2,bath.wh$upper_m,sal=0) 
        }
      }
    }
  }
}



#chlorophyll 
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3[c3$lakedate == lake.dates[i], ] 
  temp$chla_loess = predict(loess(Chlorophyll_a~Depth, data=temp, span=0.8)) #new column of loessed profile
  summary.c3[i,13] = min(temp$chla_loess) #min 
  summary.c3[i,14] = max(temp$chla_loess) #max 
  summary.c3[i,15] = mean(temp$chla_loess) #mean
  summary.c3[i,16] = sd(temp$chla_loess) #sd
  summary.c3[i,17] = sd(temp$chla_loess)/sqrt(sum(!is.na(temp$chla_loess))) #se 
  temp = temp[temp$chla_loess == max(temp$chla_loess),] #subset row of only max Chlorophyll_a
  chl_max_depth = temp$Depth #save Depth at max Chlorophyll_a
  summary.c3[i,18] = chl_max_depth[1] } #paste depth of max chl in 13th col

#phycocyanin
for (i in 1:length(lake.dates)){ #for every lakedate...
  temp = c3[c3$lakedate == lake.dates[i], ] 
  temp$phy_loess = predict(loess(Phycocyanin~Depth, data=temp, span=0.8)) #new col for loess profile
  summary.c3[i,19] = min(temp$phy_loess) #min 
  summary.c3[i,20] = max(temp$phy_loess) #max 
  summary.c3[i,21] = mean(temp$phy_loess) #mean
  summary.c3[i,22] = sd(temp$phy_loess) #sd
  summary.c3[i,23] = sd(temp$phy_loess)/sqrt(sum(!is.na(temp$phy_loess))) #se 
  temp = temp[temp$phy_loess == max(temp$phy_loess),] #subset row of only max Phycocyanin
  phyco_max_depth = temp$Depth 
  summary.c3[i,24] = phyco_max_depth[1] }  

#CDOM
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3[c3$lakedate == lake.dates[i], ] 
  temp$cdom_loess = predict(loess(CDOM~Depth, data=temp, span=0.8))
  summary.c3[i,25] = min(temp$cdom_loess) #min 
  summary.c3[i,26] = max(temp$cdom_loess) #max 
  summary.c3[i,27] = mean(temp$cdom_loess) #mean
  summary.c3[i,28] = sd(temp$cdom_loess) #sd
  summary.c3[i,29] = sd(temp$cdom_loess)/sqrt(sum(!is.na(temp$cdom_loess))) #se 
  temp = temp[temp$cdom_loess == max(temp$cdom_loess),] #subset row of only max cdom
  cdom_max_depth = temp$Depth 
  summary.c3[i,30] = cdom_max_depth[1] } 

#change NaN to NA (includes winter dates)
summary.c3 = summary.c3 %>% 
  mutate(thermocline.depth.c3=replace(thermocline.depth.c3,thermocline.depth.c3=='NaN',NA)) 


#coerce date
summary.c3$Date = as.Date(summary.c3$Date)
str(summary.c3)

#write csv
write.csv(summary.c3,'library/profiles.c3loess.csv',row.names=F)  





# kd / PAR #############

#model kd and z 1% PAR from DOC and chlorophyll concentrations for each sample
#using jasmine's limnology class notes

#formulas:
  #kd = 0.22[DOC] + 0.07[chla] - 0.05
  #z1%par = 4.6/kd

#load data
nooch = read.csv('library/db.nooch.csv',header=T)


#just epi DOC and chl from each date
kd1 = ddply(filter(nooch,site=='Epi'), 
            .(lakedate,LakeID,Date,seasonID),
            summarize,
            kd = (doc.mgpl*0.22)+(chla.ugpl*0.07)-0.05,
            z1 = 4.6/kd)

#average epi/meta DOC and chl for each date
kd2 = ddply(filter(nooch,site=='Epi' | site=='Meta'),
            .(lakedate,LakeID,Date,seasonID), 
            summarize,
            mean_doc = mean(doc.mgpl), 
            mean_chla = mean(chla.ugpl),
            kd = (mean_doc*0.22)+(mean_chla*0.07)-0.05,
            z1 = 4.6/kd)

#average 3 depth samples of DOC and chl for each date
kd3 = ddply(nooch,
            .(lakedate,LakeID,Date,seasonID), 
            summarize,
            mean_doc = mean(doc.mgpl), 
            mean_chla = mean(chla.ugpl),
            kd = (mean_doc*0.22)+(mean_chla*0.07)-0.05,
            z1 = 4.6/kd)


#save df
write.csv(kd1,'library/profiles.kd.csv',row.names=F)






#Nutrients ########


#think through code first. need epi_DOC, meta_DOC, hypo_DOC??? 
#for every param? or just average everything together. 

#load data
nooch = read.csv('library/db.nooch.csv',header=T)
seasonID = read.csv('library/seasonID.csv',header=T)
names(nooch)

#widen
nooch.wide = nooch %>% 
  pivot_wider(id_cols = lakedate,
              names_from = site,
              values_from = c(chla.ugpl,doc.mgpl,tp.ugpl,nh4.ugpl,no3.ugpl,tn.ugpl,din.ugpl,din.tp),
              names_sep = ".") %>% 
  set_names(~ str_to_lower(.) )  

names(nooch)

summary.nooch = ddply(nooch, 
                      .(lakedate,LakeID,Date,seasonID),
                      summarize,
                      chla.ugpl.avg = mean(chla.ugpl),
                      chla.ugpl.sd = sd(chla.ugpl),
                      doc.mgpl.avg = mean(doc.mgpl),
                      doc.mgpl.sd = sd(doc.mgpl),
                      tp.ugpl.avg = mean(tp.ugpl),
                      tp.ugpl.sd = sd(tp.ugpl),
                      tn.ugpl.avg = mean(tn.ugpl),
                      tn.ugpl.sd = sd(tn.ugpl),
                      nh4.ugpl.avg = mean(nh4.ugpl),
                      nh4.ugpl.sd = sd(nh4.ugpl),
                      no3.ugpl.avg = mean(no3.ugpl),
                      no3.ugpl.sd = sd(no3.ugpl),
                      din.ugpl.avg = mean(din.ugpl),
                      din.ugpl.sd = sd(din.ugpl),
                      din.tp.avg = mean(din.tp),
                      din.tp.sd = sd(din.tp))

#merge
summary.nooch = join(summary.nooch,nooch.wide,by='lakedate')
str(summary.nooch)




# DCM ###############

#using Lofton 2020 script as reference

#source dependent scripts
source('scripts/LoftonDCMscrpits/Script_S2_GND_fit_functions.R')

#load data
profiles = read.csv('library/db.c3.csv',header=T)
profiles = profiles %>% filter(Depth > 0.5)

str(profiles)



output.df = ddply(profiles, "lakedate", function (profiles){
  DCM.fit    = fit.GND(profiles$Depth, profiles$Chlorophyll_a)
  DCM.r2.fit = r2.fit(DCM.fit$par, profiles$Depth, profiles$Chlorophyll_a) 
  DCM.depth  = DCM.fit$par[3]
  DCM.std    = DCM.fit$par[4]
  Profile.depth = max(profiles$Depth)
  add.par    = DCM.fit$par[1]
  multi.par  = DCM.fit$par[2]
  DCM.shape  = DCM.fit$par[5]
  chl.max.value = max(profiles$Chlorophyll_a)
  DCM.depth.manual = profiles[which.max(profiles$Chlorophyll_a),"Depth"]
  # calculate DCM top depth, will set to 0 if this is above surface of lake
  DCM.breadth.top = c()
  if (DCM.depth - DCM.std < 0) {
    DCM.breadth.top = 0
  } else if (DCM.depth - DCM.std >= 0) {
    DCM.breadth.top = DCM.depth - DCM.std
  }
  # calculate the DCM bottom depth, will set to max. depth of profile if below
  DCM.breadth.bottom = c() # 
  if (DCM.depth + DCM.std > max(profiles$Depth)) {
    DCM.breadth.bottom = max(profiles$Depth)
  } else {
    DCM.breadth.bottom = DCM.depth + DCM.std
  }
  # calculate peak width
  peak.width = DCM.breadth.bottom - DCM.breadth.top
  # calculate standardized peak width
  stand.peak.width = peak.width/Profile.depth
  # calculate avg. chl-a
  chl.avg = mean(profiles$Chlorophyll_a, na.rm = TRUE)
  data.frame(DCM.r2.fit = round(DCM.r2.fit, digits = 3), 
             DCM.depth.curvefit  = round(DCM.depth, digits = 3), 
             DCM.std    = round(DCM.std, digits = 3),
             DCM.shape  = round(DCM.shape, digits =3),
             add.par    = add.par, 
             multi.par  = multi.par,
             DCM.breadth.top = round(DCM.breadth.top, digits = 3),
             DCM.breadth.bottom = round(DCM.breadth.bottom, digits = 3),
             peak.width = round(peak.width, digits = 3),
             stand.peak.width = round(stand.peak.width, digits = 3),
             Profile.depth = Profile.depth,
             chl.max.value = chl.max.value,
             chl.avg = round(chl.avg, digits = 2),
             DCM.depth.manual = DCM.depth.manual
  )
})


#make lowercase
output.df = output.df %>% set_names(~ str_to_lower(.))
#add seasonID, etc
output.df = output.df %>% separate(lakedate, c("LakeID", "Date"), sep = "_", remove=F)
output.df = join(output.df,seasonID,by='lakedate')
#move seasonID up
output.df = output.df %>% relocate(seasonID, .after = Date)
#coerce date
output.df$Date = as.Date(output.df$Date)
str(output.df)

#save df
write.csv(output.df, "library/profiles.chl.dcm.csv",row.names=F)






#Combine ####

#remove redundant ID cols
sum_c3 = summary.c3 %>% dplyr::select(-LakeID,-Date,-seasonID)
sum_kd = kd1 %>% dplyr::select(-LakeID,-Date,-seasonID)
sum_nooch = summary.nooch %>% dplyr::select(-LakeID,-Date,-seasonID)
sum_dcm = output.df %>% dplyr::select(-LakeID,-Date,-seasonID)

#joins
p1 = join(summary.exo,sum_c3,by='lakedate')
p2 = join(p1,sum_kd,by='lakedate')
p3 = join(p2,sum_dcm,by='lakedate')
p4 = join(p3,sum_nooch,by='lakedate')
str(p4)

#add max depth column
p4 = p4 %>% 
  mutate(max.depth=ifelse(LakeID=='JP',50,NA)) %>% 
  mutate(max.depth=replace(max.depth,LakeID=='SC',13.5)) %>% 
  mutate(max.depth=replace(max.depth,LakeID=='BB',12)) %>% 
  mutate(max.depth=replace(max.depth,LakeID=='WH',9)) 
  
#other metrics to calculate after combining:

#if no thermocilne present, mixing depth = profile max depth
p4$thermocline.depth.c3 = ifelse(is.na(p4$thermocline.depth.c3), p4$profile.depth, p4$thermocline.depth.c3)

#ratio of z1% to max depth
p4$z1.maxdepth = p4$z1 / p4$max.depth

#ratio of DCM:MaxDepth
p4$dcm.maxdepth = p4$dcm.depth.curvefit / p4$max.depth

#mixing depth: max depth
p4$mixing.maxdepth = p4$thermocline.depth.c3 / p4$max.depth


#save 
write.csv(p4, "library/profiles.all.csv",row.names=F)












#end