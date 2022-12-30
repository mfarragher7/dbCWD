# Compare summary stats from smoothed and non-smoothed C3 profiles. 
# updated 2020-11-25

#libraries
library(plyr)
library(dplyr)
library(rLakeAnalyzer)
library(ggplot2)



#copy-and-pasted from 1.10_profiles.sum.stats.R



#C3 smooth ####

#how to smooth profiles. IMPORTANT !!!!!
#ggplot::stat_smooth loess default span = 0.75
#chla_smooth = predict(loess(Chlorophyll_a~Depth, data=full.c3, span=0.75))
#phy_smooth = predict(loess(Phycocyanin~Depth, data=full.c3, span=0.75))
#cdom_smooth = predict(loess(CDOM~Depth, data=full.c3, span=0.75))

#compare to smoothed summary
#test - modeling profiles and save in new dataframe
test=c3 %>% filter(lakedate=='SC_2020-08-03') #temp dataframe
plot(Chlorophyll_a~Depth,test) 
#test loess and check
test$chla_loess = predict(loess(Chlorophyll_a~Depth, data=test, span=0.8)) 
plot(chla_loess~Depth,test) #compare



#******  start here again  *******
#new df to work through loess smoothed profiles
c3.smooth = c3 

#summary for smoothed profiles. mostly empty, 1 for loop for each parameter
summary.c3.smooth = ddply(c3.smooth, .(lakedate), summarize,
                          temp_min_c3=min(Temp), temp_max_c3=max(Temp), temp_mean_c3=mean(Temp), thermocline_depth_c3 = NA, #min, max, mean temp, empty thermo.depth
                          chl_min=NA, chl_max=NA, chl_mean=NA, chl_max_depth = NA, #chl
                          phyco_min=NA, phyco_max=NA, phyco_mean=NA, phyco_max_depth = NA, #phyco
                          cdom_min=NA, cdom_max=NA, cdom_mean=NA, cdom_max_depth = NA) #cdom

#4 x for loop
#save lakedates for c3
lake.dates = summary.c3.smooth$lakedate
#thermocline depth. same as above
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3.smooth[c3.smooth$lakedate == lake.dates[i], ] #temp dataframe that subsets c3.smooth by each lakedate
  temp = temp[!duplicated(c3.smooth$Depth), ] #remove duplicate depths
  temp = temp[rowSums(is.na(temp)) != ncol(temp), ] #remove rows where all cols = NA
  temp = temp[!duplicated(temp$Depth), ] #remove duplicate depths.... again
  thermo = thermo.depth(temp$Temp,temp$Depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  summary.c3.smooth[i,5] = thermo[1] }  #calculate thermocline depth in 5th col
#chlorophyll 
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3.smooth[c3.smooth$lakedate == lake.dates[i], ] #temp dataframe that subsets c3.smooth by each lakedate
  temp$chla_smooth = predict(loess(Chlorophyll_a~Depth, data=temp, span=0.8)) #new column of smoothed profile
  summary.c3.smooth[i,6] = min(temp$chla_smooth) #min 
  summary.c3.smooth[i,7] = max(temp$chla_smooth) #max 
  summary.c3.smooth[i,8] = mean(temp$chla_smooth) #mean
  temp = temp[temp$chla_smooth == max(temp$chla_smooth),] #subset row of only max Chlorophyll_a
  chl_max_depth = temp$Depth #save Depth at max Chlorophyll_a
  summary.c3.smooth[i,9] = chl_max_depth[1] }  #paste depth of max chl in 9th col
#phycocyanin
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3.smooth[c3.smooth$lakedate == lake.dates[i], ] #temp dataframe that subsets c3.smooth by each lakedate
  temp$phy_smooth = predict(loess(Phycocyanin~Depth, data=temp, span=0.8)) #new col for smoothed profile
  summary.c3.smooth[i,10] = min(temp$phy_smooth) #min 
  summary.c3.smooth[i,11] = max(temp$phy_smooth) #max 
  summary.c3.smooth[i,12] = mean(temp$phy_smooth) #mean
  temp = temp[temp$phy_smooth == max(temp$phy_smooth),] #subset row of only max Phycocyanin
  phyco_max_depth = temp$Depth #save Depth at max Phycocyanin
  summary.c3.smooth[i,13] = phyco_max_depth[1] }  #paste depth of max phyco in 13th col
#CDOM
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3.smooth[c3.smooth$lakedate == lake.dates[i], ] #temp dataframe that subsets c3.smooth by each lakedate
  temp$cdom_smooth = predict(loess(CDOM~Depth, data=temp, span=0.8)) #new col for smoothed profile
  summary.c3.smooth[i,14] = min(temp$cdom_smooth) #min 
  summary.c3.smooth[i,15] = max(temp$cdom_smooth) #max 
  summary.c3.smooth[i,16] = mean(temp$cdom_smooth) #mean
  temp = temp[temp$cdom_smooth == max(temp$cdom_smooth),] #subset row of only max cdom
  cdom_max_depth = temp$Depth #save Depth at max cdom
  summary.c3.smooth[i,17] = cdom_max_depth[1] }  #paste depth of max cdom in 17th col

#change 'NaN'
summary.c3.smooth = summary.c3.smooth %>% mutate(thermocline_depth_c3 = replace(thermocline_depth_c3,thermocline_depth_c3=='NaN',NA))  


#c3 raw ####
#**** skip over unless comparing to smoothed ****

#summary for unsmoothed profiles
summary.c3.raw = ddply(full.c3, .(lakedate), summarize,
                       temp_min_c3=min(Temp), temp_max_c3=max(Temp), temp_mean_c3=mean(Temp),  thermocline_depth_c3 = NA, #min, max, mean temp, empty thermo.depth
                       chl_min=min(Chlorophyll_a), chl_max=max(Chlorophyll_a), chl_mean=mean(Chlorophyll_a), chl_max_depth = NA, #chl
                       phyco_min=min(Phycocyanin), phyco_max=max(Phycocyanin), phyco_mean=mean(Phycocyanin), phyco_max_depth = NA, #phyco
                       cdom_min=min(CDOM), cdom_max=max(CDOM), cdom_mean=mean(CDOM), cdom_max_depth = NA) #cdom

#4 x for loop
#save lakedates for c3
lake.dates = summary.c3.raw$lakedate
#thermocline depth
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = full.c3[full.c3$lakedate == lake.dates[i], ] #temp dataframe that subsets full.c3 by each lakedate
  temp = temp[!duplicated(full.c3$Depth), ] #remove duplicate depths
  temp = temp[rowSums(is.na(temp)) != ncol(temp), ] #remove rows where all cols = NA
  temp = temp[!duplicated(temp$Depth), ] #remove duplicate depths.... again
  thermo = thermo.depth(temp$Temp,temp$Depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  summary.c3.raw[i,5] = thermo[1] }  #calculate thermocline depth in 5th col
#depth of max chlorophyll for each profile
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = full.c3[full.c3$lakedate == lake.dates[i], ] #temp dataframe that subsets full.c3 by each lakedate
  temp = temp[temp$Chlorophyll_a == max(temp$Chlorophyll_a),] #subset row of only max Chlorophyll_a
  chl_max_depth = temp$Depth #save Depth at max Chlorophyll_a
  summary.c3.raw[i,9] = chl_max_depth[1] }  #paste depth of max chl in 9th col
#depth of max phyco
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = full.c3[full.c3$lakedate == lake.dates[i], ] #temp dataframe that subsets full.c3 by each lakedate
  temp = temp[temp$Phycocyanin == max(temp$Phycocyanin),] #subset row of only max Phycocyanin
  phyco_max_depth = temp$Depth #save Depth at max Phycocyanin
  summary.c3.raw[i,13] = phyco_max_depth[1] }  #paste depth of max chl in 13th col
#depth of max CDOM
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = full.c3[full.c3$lakedate == lake.dates[i], ] #temp dataframe that subsets full.c3 by each lakedate
  temp = temp[temp$CDOM == max(temp$CDOM),] #subset row of only max cdom
  cdom_max_depth = temp$Depth #save Depth at max cdom
  summary.c3.raw[i,17] = cdom_max_depth[1] }  #paste depth of max chl in 17th col

#change 'NaN'
summary.c3.raw = summary.c3.raw %>% mutate(thermocline_depth_c3 = replace(thermocline_depth_c3,thermocline_depth_c3=='NaN',NA))
  
  



  
# TEST ####

#test plots - use predict(loess()) instead


#save loess profile
test=c3 %>% filter(lakedate=='JP_2020-10-12') #temp dataframe
test$chla_smooth = predict(loess(Chlorophyll_a~Depth, data=test, span=0.8)) 

#check raw profile
raw=ggplot(test,aes(x=Depth,y=Chlorophyll_a)) +
  stat_smooth(method = "loess",  size=3, se=F, show.legend=F, span=0.75) +
  geom_point(size=1, shape=1) +
  scale_y_continuous(limits=c(0,200)) +
  scale_x_reverse(n.breaks=6) +
  coord_flip(); raw

#check smooth
smooth=ggplot(test,aes(x=Depth,y=chla_smooth)) +
  geom_point(size=2) +
  scale_x_reverse(n.breaks=6) +
  scale_y_continuous(limits=c(0,200)) +
  coord_flip(); smooth



#test another sample
test=full.c3 %>% filter(lakedate=='SC_2020-06-14') #temp dataframe
test$chla_smooth = predict(loess(Chlorophyll_a~Depth, data=test, span=0.8)) 


#check raw profile
raw=ggplot(test,aes(x=Depth,y=Chlorophyll_a)) +
  stat_smooth(method = "loess",  size=3, se=F, show.legend=F, span=0.75) +
  geom_point(size=1, shape=1) +
  scale_y_continuous(limits=c(60,210)) +
  scale_x_reverse(n.breaks=6) +
  coord_flip(); raw

#check smooth
smooth=ggplot(test,aes(x=Depth,y=chla_smooth)) +
  geom_point(size=2) +
  scale_x_reverse(n.breaks=6) +
  scale_y_continuous(limits=c(60,210))+
  coord_flip() 
smooth









