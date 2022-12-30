#phosphorus stuff
#created 2022-12-20


#libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)
library(ggpubr)



# workflow:
# sampleID for date, station, core/grab, depth
# average together values with reps using new ID
# profiles within 2 meters of bottom grab, interpolate linear-ly
# profiles >2m from bottom grab, interpolate using quadratic/graded or something non-linear
# convert to volume weighted kg TP



#load data
tp = read.csv('~/CWD/dbCWD/library/tp.cwd.csv', header=T)


#pleasant pond ########

pptp = filter(tp, lake=='pleasant')
plyr::count(pptp$basin)
#only use station 1 for now
pptp = pptp %>% filter(basin==1)
#check depths
plyr::count(pptp$depth)
pptp = pptp %>% filter(depth < 10)
str(pptp)
pptp$date = as.Date(pptp$date)

#how many duplicate reps in dataset?
unique(pptp$r)        
temp = plyr::count(pptp$r) 
sum(temp$freq)              
# 631 total p samples, 72 reps (2)
# how many core and grab samples, and how many reps in each?
pptp$pID = paste(pptp$date, pptp$type, pptp$depth, sep='_')
pptp$pID
length(unique(pptp$pID))        
temp = plyr::count(pptp$pID)

sum(temp$freq)              




#average multiple reps together

              
pptpr = pptp %>% 
  group_by(pID, date, depth) %>% 
  summarize(tp.ppm.mean = mean(tp_ppm))

temp2 = plyr::count(pptpr$)
























