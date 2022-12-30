#DCM thickness
#model chl profiles using loess smoothing, get depth max chl, and thickness
#2020-11-27 happy thnxgiving

#DCM thickness is the range in depth from one sd +/- from chl max. 

#load survey data 
full.exo = read.csv("output/full.exo.csv",header=T)
full.c3 = read.csv("output/full.c3.csv",header=T)

#libraries
library(plyr)

#keep it simple stupid!
c3.loess = full.c3 

#summary df
summary.c3.loess = ddply(c3.loess, .(lakedate), summarize,
                         temp_min_c3=min(Temp), temp_max_c3=max(Temp), temp_mean_c3=mean(Temp), temp_sd_c3=sd(Temp), #temp min, max, mean, sd
                         temp_se_c3=temp_sd_c3/sqrt(sum(!is.na(Temp))), thermocline_depth_c3 = NA, #empty thermo.depth
                         chl_min=NA, chl_max=NA, chl_mean=NA, chl_sd=NA, chl_se=NA, chl_max_depth = NA) #chl

#4 x for loop
#save lakedates for c3
lake.dates = summary.c3.loess$lakedate
#thermocline depth. same as above
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3.loess[c3.loess$lakedate == lake.dates[i], ] #temp dataframe that subsets c3.loess by each lakedate
  temp = temp[!duplicated(c3.loess$Depth), ] #remove duplicate depths
  temp = temp[rowSums(is.na(temp)) != ncol(temp), ] #remove rows where all cols = NA
  temp = temp[!duplicated(temp$Depth), ] #remove duplicate depths.... again
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


#fix thermoclines
summary.c3.loess = summary.c3.loess %>% 
  mutate(thermocline_depth_c3 = replace(thermocline_depth_c3,thermocline_depth_c3=='NaN',NA)) 
#add lakedate
summary.c3.loess = summary.c3.loess %>% 
  separate(lakedate, c("LakeID", "Date"), sep = "_", remove=F)
#add seasonID
summary.c3.loess = join(summary.c3.loess,seasonIDs,by="lakedate")

#coerce date
summary.c3.loess$Date = as.Date(summary.c3.loess$Date)
str(summary.c3.loess)





#DCM summary

#make empty dataframe 
#summary for smoothed profiles. mostly empty, 1 for loop for each parameter
summary.dcm = ddply(c3.loess, .(lakedate), summarize,
                    chl_min=NA, chl_max=NA, chl_mean=NA, 
                    chl_sd=NA, chl_se=NA, chl_max_depth = NA,
                    dcm_neg=NA, dcm_pos=NA) #depth of 1 positive sd and 1 negative sd from max chl depth
                       
#save lakedates for c3
lake.dates = summary.dcm$lakedate
#get DCM and DCM thickness for each profile
for (i in 1:length(lake.dates)){ #for every lakedate,
  temp = c3.loess[c3.loess$lakedate == lake.dates[i], ] #temp dataframe that subsets c3.loess by each lakedate
  temp$chla_loess = predict(loess(Chlorophyll_a~Depth, data=temp, span=0.8)) #new column of loess profile, span matches ggplot loess method (0.75)
  summary.dcm[i,2] = min(temp$chla_loess) #paste value of min chl in col 2
  summary.dcm[i,3] = max(temp$chla_loess) #max 
  summary.dcm[i,4] = mean(temp$chla_loess) #mean
  summary.dcm[i,5] = sd(temp$chla_loess) #sd
  summary.dcm[i,6] = sd(temp$chla_loess)/sqrt(sum(!is.na(temp$chla_loess))) #se 
  temp1 = temp[temp$chla_loess == max(temp$chla_loess),] #subset row of only max Chlorophyll_a
  chl_max_depth = temp1$Depth #save Depth at max Chlorophyll_a
  summary.dcm[i,7] = chl_max_depth[1] #paste depth of max chl in col 7
  #get the depth at one sd (of chla value) below max chl
  neg_chl = max(temp$chla_loess) - sd(temp$chla_loess) #save chla value at -1 sd
  dcm_neg = with(temp, approx(chla_loess,Depth,xout=neg_chl)) #approximate Depth given chla value
  summary.dcm[i,8] = dcm_neg[2] #paste depth. 2nd value in list. lower boundary of dcm thickness
  #get the depth at one sd (of chla value) above the depth of max chl
  pos_chl = max(temp$chla_loess) + sd(temp$chla_loess) #save chla value at +1 sd
  dcm_pos = ifelse(pos_chl>)

}



#this code works but I give up. I'm misdirected!!








#this prints 2 values, an estimate of x and y. 
#with(temp, approx(chla_loess,Depth,xout=neg_chl)) 


#example from stack overflow
#Q: have df like this
#lkp <- data.frame(
#  x=c(0,0.2,0.65,0.658,1.3,1.76,2.7), 
#  y=c(1,1,1,0.942,0.942, 0.92, 0.89))

#want to do this
#for X=0.2 Y=1 (exact lookup) 
#for X=2 Y=0.91 (linear interpolation between the last 2 rows of the data frame)


#A:
#with(lkp, approx(x, y, xout=c(0.2, 2)))
#$x
#[1] 0.2 2.0

#$y
#[1] 1.0000000 0.9123404