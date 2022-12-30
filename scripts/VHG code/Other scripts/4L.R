
# Quatro Lagos
# aka 4L

rm(list=ls()) 

# set wd, load libraries
setwd("~/Maine/Acadia/QuatroLagos/NPS Datasets")
library(dplyr)
library(RSQLite)

#
###
##### append old and new 2020 dataframes
##
#

# call in data and subset
data = read.csv("4L_WQProfile_2006-2019.csv")
df = subset(data, select = c(4:12,19))
df$LakeID = NA  
df = df[,c(1,11,2:10)]                   # reorder columns 
df[grepl("Bubble Pond", df$Site),2] = 'BB' # made lakeIDs
df[grepl("Jordan Pond", df$Site),2] = 'JP'
df[grepl("Witch Hole Pond", df$Site),2] = 'WH'
df[grepl("Seal Cove Pond", df$Site),2] = 'SC'

df$Date = as.Date(df$Date)
df$Time = format(strptime(df$Time, "%I:%M:%S %p"),format="%H:%M")  #format time to H:M

names(df)[5] <- "Depth"      #rename columns
names(df)[6] <- "Temp"
names(df)[7] <- "DO_mg"
names(df)[8] <- "DO_sat"
names(df)[10] <- "SpCond"

data2 = read.csv("~/Maine/Acadia/QuatroLagos/2020 sampling/4L_exo.csv")
df2 = subset(data2, select = c(1:7,10,12,13))   #subset
df2$Year = NA   # make year column
df2$Date = as.Date(df2$Date)
df2$Year = as.numeric(format(df2$Date, "%Y"))   #pull out year from dates
df2 = df2[,c(1:6,10,9,8,7,11)]    #reorder
names = colnames(df)             #copy column names
colnames(df2) = names

df_full = rbind(df,df2)   #combine dataframes :)
df_full <- subset(df_full, !is.na(Depth))
df_full <- subset(df_full, !is.na(Temp))


# ok now to ask some questions
library(rLakeAnalyzer)
library(LakeMetabolizer)

#subset WH
lake_dps = c(df_full$LakeID)
lake_datapointlist = as.data.frame(table(lake_dps))
WH = as.data.frame(matrix(NA, ncol=11, nrow=957))
colnames(WH) = names                     
WH = subset(df_full, LakeID %in% "WH")

#single day WH
WH_13July06 = as.data.frame(matrix(NA, ncol=11, nrow=10))
colnames(WH_13July06) = names
WH$Date = as.character(WH$Date)   # needed to make Dates into characters for some reason??? why
WH_13July06 = subset(WH, Date %in% "2006-07-13")



#calculate thermocline depth using LakeMetabolizer !

thermo.depth(WH_13July06$Temp,WH_13July06$Depth,    #temp and depth columns #look up min density gradient for thermocline... "Smin = ???"
             seasonal=TRUE,                         #deepest density gradient in profile - 
             index=FALSE,                           #boolean value indicated if index of the thermocline depth, instead of the depth value, should be returned
             mixed.cutoff=1)                        #thermo.depth and meta.depths are not calculated     

# it worked

#make output for WH thermocline depths
WH$Date = as.character(WH$Date)
WH_dates = unique(WH$Date)
WH_out = as.data.frame(matrix(NA, ncol=3, nrow=94))
colnames(WH_out) = c("Year","Date", "Thermocline")
output_names = colnames(WH_out)

# fill in dates. holy fuck that took forever
make.unique(WH_dates, sep = ".")
WH_out$Date = WH_dates
WH_out$Date = as.Date(WH_out$Date)
WH_out$Year = as.numeric(format(WH_out$Date, "%Y"))   #pull out year from dates again

# run for loop

for (i in 1:length(WH_dates)){
  
  temp = WH[WH$Date == WH_dates[i], ] 
  temp = temp[!duplicated(temp$Depth), ]
  
  thermo = thermo.depth(temp$Temp,temp$Depth,seasonal=TRUE, index=FALSE, mixed.cutoff=1)
  
  WH_out[i,3] = thermo[1] #calculate thermocline depth in 3rd column of row i*j == i = for each unique date
  
  print(thermo)
  
}

# it worked! 



#now jordan 
JP = as.data.frame(matrix(NA, ncol=11, nrow=2773))
colnames(JP) = names                     
JP = subset(df_full, LakeID %in% "JP")

JP_dates = unique(JP$Date)    #96 unique dates
JP_out = as.data.frame(matrix(NA, ncol=3, nrow=length(JP_dates)))
colnames(JP_out) = output_names

make.unique(JP_dates, sep = ".") # pull in dates and year
JP_out$Date = JP_dates
JP_out$Date = as.Date(JP_out$Date)
WH_out$Year = as.numeric(format(WH_out$Date, "%Y"))   

for (i in 1:length(JP_dates)){
  temp = JP[JP$Date == JP_dates[i], ] 
  temp = temp[!duplicated(temp$Depth), ]
  thermo = thermo.depth(temp$Temp,temp$Depth, seasonal=TRUE, index=FALSE, mixed.cutoff=1)
  JP_out[i,3] = thermo[1] 
  print(thermo)
}




# Bubble
BB = as.data.frame(matrix(NA, ncol=11, nrow=1195))
colnames(BB) = names                     
BB = subset(df_full, LakeID %in% "BB")

BB_dates = unique(BB$Date)    #96 unique dates
BB_out = as.data.frame(matrix(NA, ncol=3, nrow=length(BB_dates)))
colnames(BB_out) = output_names

make.unique(BB_dates, sep = ".") # pull in dates and year
BB_out$Date = BB_dates
BB_out$Date = as.Date(BB_out$Date)
WH_out$Year = as.numeric(format(WH_out$Date, "%Y"))   

for (i in 1:length(BB_dates)){
  temp = BB[BB$Date == BB_dates[i], ] 
  temp = temp[!duplicated(temp$Depth), ]
  thermo = thermo.depth(temp$Temp,temp$Depth, seasonal=TRUE, index=FALSE, mixed.cutoff=1)
  BB_out[i,3] = thermo[1] 
  print(thermo)
}

# all sorts of fucked up. why??? so many NaNs



# Seal Cove
SC = as.data.frame(matrix(NA, ncol=11, nrow=1267))
colnames(SC) = names                     
SC = subset(df_full, LakeID %in% "SC")

SC_dates = unique(SC$Date)    #96 unique dates
SC_out = as.data.frame(matrix(NA, ncol=3, nrow=length(SC_dates)))
colnames(SC_out) = output_names

make.unique(SC_dates, sep = ".") # pull in dates and year
SC_out$Date = SC_dates
SC_out$Date = as.Date(SC_out$Date)
WH_out$Year = as.numeric(format(WH_out$Date, "%Y"))   

for (i in 1:length(SC_dates)){
  temp = SC[SC$Date == SC_dates[i], ] 
  temp = temp[!duplicated(temp$Depth), ]
  thermo = thermo.depth(temp$Temp,temp$Depth, seasonal=TRUE, index=FALSE, mixed.cutoff=1)
  SC_out[i,3] = thermo[1] 
  print(thermo)
}







#test a weird one
# removed bottom 10.02 depth, so lowest dep now 10.01, changed thermo depth from 10 to 3.3m

# make dataframe
WH_20090818 = as.data.frame(matrix(NA, ncol=11, nrow=12))
colnames(WH_20090818) = names
WH_20090818$Date = as.character(WH_20090818$Date)  
WH_20090818 = subset(WH, Date %in% "2009-08-18")
WH_20090818_ = WH_20090818[-c(12),]     

thermo.depth(WH_20090818_$Temp,WH_20090818_$Depth, seasonal=TRUE, index=FALSE,mixed.cutoff=1)  
















# see if aggregating mean temps for repeated depths can work for single date,
# also need to compare how aggragating vs removing duplicates
# using 2010-05-25 as example

# make dataframe
temp_test = as.data.frame(matrix(NA, ncol=11, nrow=12))
colnames(temp_test) = names
temp_test$Date = as.character(temp_test$Date)  
temp_test = subset(WH, Date %in% "2010-05-25")

#aggregate
temp_test_output = as.data.frame(matrix(NA, ncol=11, nrow=length(unique(temp_test$Depth))))

temp_Temp = aggregate(temp_test$Temp, by = list(temp_test$Depth), FUN = mean)
colnames(temp_Temp) = c("Depth","Temp")
temp_DO_mg = aggregate(temp_test$DO_mg, by = list(temp_test$Depth), FUN = mean)
colnames(temp_DO_mg) = c("Depth","DO_mg")
temp_DO_sat = aggregate(temp_test$DO_sat, by = list(temp_test$Depth), FUN = mean)
colnames(temp_DO_sat) = c("Depth","DO_sat")
temp_pH = aggregate(temp_test$pH, by = list(temp_test$Depth), FUN = mean)
colnames(temp_pH) = c("Depth","pH")
temp_SpCond = aggregate(temp_test$SpCond, by = list(temp_test$Depth), FUN = mean)
colnames(temp_SpCond) = c("Depth","SpCond")

temp_test_output = merge(temp_Temp, temp_DO_mg, by = "Depth")
temp_test_output = merge(temp_test_output, temp_DO_sat, by = "Depth")
temp_test_output = merge(temp_test_output, temp_pH, by = "Depth")
temp_test_output = merge(temp_test_output, temp_SpCond, by = "Depth")


thermo.depth(temp_test_output$Temp,temp_test_output$Depth, seasonal=TRUE, index=FALSE, mixed.cutoff=1)      

# 2.628304

# thats the value with averaged temp


# now here's "duplicates removed" instead
temp_test_2 = temp_test[!duplicated(temp_test$Depth), ]
temp = temp[!duplicated(temp$Depth), ]
thermo.depth(temp_test_2$Temp,temp_test_2$Depth, seasonal=TRUE, index=FALSE, mixed.cutoff=1)      

#          2.623894
#             vs.
#          2.628304 

#            hmm,
#                 hmmmmmm

