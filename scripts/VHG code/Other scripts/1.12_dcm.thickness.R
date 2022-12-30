#version 2 of DCM analysis
#using Lofton 2020 script as reference



#libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyr)


#source dependent scripts
source('scripts/LoftonDCMscrpits/Script_S2_GND_fit_functions.R')

#load data
seasonID= read.csv('library/seasonID.csv',header=T)
profiles = read.csv('library/db.c3.csv',header=T)
str(profiles)

################################################
# this ddply can be used if you have a file with multiple lakes (or dates if you change the "lake" 
# in line 16 to be the name of the column that identifies the each unique profile in your df.

output.df = ddply(profiles, "lakedate", function (profiles){
  DCM.fit    = fit.GND(profiles$Depth, profiles$Chlorophyll_a)
  DCM.r2.fit = r2.fit(DCM.fit$par, profiles$Depth, profiles$Chlorophyll_a) 
  DCM.depth  = DCM.fit$par[3]
  DCM.std    = DCM.fit$par[4]
  Max.depth = max(profiles$Depth)
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
  stand.peak.width = peak.width/Max.depth
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
             Max.depth = Max.depth,
             chl.max.value = chl.max.value,
             chl.avg = round(chl.avg, digits = 2),
             DCM.depth.manual = DCM.depth.manual
  )
})



#add seasonID, etc
output.df = output.df %>% separate(lakedate, c("LakeID", "Date"), sep = "_", remove=F)
output.df = join(output.df,seasonID,by='lakedate')
#move seasonID up
output.df = output.df %>% relocate(seasonID, .after = Date)
#coerce date
output.df$Date = as.Date(output.df$Date)
str(output.df)


write.csv(output.df, "library/profiles_chl_peak_width_results.csv",row.names = FALSE)






















