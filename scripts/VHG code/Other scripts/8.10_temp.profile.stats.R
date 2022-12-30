#Temperature profile calculations
#Thermocline depth
#Buoyancy frequency ???
#Schmidt stability index (SSI)
#created 2021-02-02

#load bathymetry data
bath.jp = read.csv("library/bath.jp.csv",header=T)
bath.sc = read.csv("library/bath.sc.csv",header=T)
bath.bb = read.csv("library/bath.bb.csv",header=T)
bath.wh = read.csv("library/bath.wh.csv",header=T)
#load profiles
full.c3 = read.csv("library/full.c3.csv",header=T)

#load libraries
library(rLakeAnalyzer)
library(dplyr)
library(plyr)

#how function looks
#schmidt.stability(wtr, depths, bthA, bthD, sal = 0)
  #wtr: numeric vector of water temperature in degrees C
  #depths: a numeric vector corresponding to the depths (in m) of the wtr measurements
  #bthA: a numeric vector of cross sectional areas (m^2) corresponding to bthD depths
  #bthD: a numeric vector of depths (m) which correspond to areal measures in bthA
  #sal: a numeric vector of salinity in Practical Salinity Scale units

#need output table for each lakedate

#need to can use save vector for each lake in the for loop xx.bath$upper_m and xx.bath$area_m2


#subset profiles
profiles = full.c3 %>% select(LakeID,Date,lakedate,seasonID,Depth,Temp)

#make output table 
output = ddply(profiles, .(LakeID,Date,lakedate,seasonID,lakedate), summarize, 
               thermocline=NA, buoyancy_freq=NA, ssi=NA)


#save lakedates
lake.dates = output$lakedate

#thermocline depth
for (i in 1:length(lake.dates)){ #for every lakedate
  
  temp = profiles[profiles$lakedate == lake.dates[i], ] #temp dataframe that subsets output by each lakedate
  temp = temp[!duplicated(temp$Depth), ] #remove duplicate depths
  temp = temp[rowSums(is.na(temp)) != ncol(temp), ] #remove rows where all cols = NA
  temp = temp[!duplicated(temp$Depth), ] #remove duplicate depths.... again
  
  thermo = thermo.depth(temp$Temp,temp$Depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  output[i,5] = thermo[1] 
  
 # buoy = buoyancy.freq(temp$Temp,temp$Depth)
  #output[i,6] = buoy[1] 
}   


#save schmidt stability values
for (i in 1:length(lake.dates)){ #for every lakedate,
  
  temp = profiles[profiles$lakedate == lake.dates[i], ] #temp dataframe that subsets profiles by each lakedate
  temp = temp[!duplicated(temp$Depth), ] #remove duplicate depths
  temp = temp[rowSums(is.na(temp)) != ncol(temp), ] 
  temp = temp[!duplicated(temp$Depth), ] 
  
  if(grepl('JP',temp$LakeID)){  #if lakedate [i] is for JP
    output[i,7] = schmidt.stability(temp$Temp,temp$Depth,bath.jp$area_m2,bath.jp$upper_m,sal=0) #calculate schmidt stability using jp bathy
  }
  else{
    if(grepl('SC',temp$LakeID)){
      output[i,7] = schmidt.stability(temp$Temp,temp$Depth,bath.sc$area_m2,bath.sc$upper_m,sal=0)
    }
    else{
      if(grepl('BB',temp$LakeID)){
        output[i,7] = schmidt.stability(temp$Temp,temp$Depth,bath.bb$area_m2,bath.bb$upper_m,sal=0)
      }
      else{
        if(grepl('WH',temp$LakeID)){
          output[i,7] = schmidt.stability(temp$Temp,temp$Depth,bath.wh$area_m2,bath.wh$upper_m,sal=0) 
        }
      }
    }
  }
}



#save csv
write.csv(output,"library/profiles_temperature.stats.csv",row.names=F)

#meta depths?? meta.depths()
