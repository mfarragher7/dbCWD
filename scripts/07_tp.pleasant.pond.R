# Pleasant Pond phosphorus 
#created 2023-01-24

#load libraries
library(plyr)
library(tidyverse)
library(lubridate)
library(ggplot2)


#load tp df


tp = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/tp.cwd.1998-2021.csv',header=T)
pptp = tp %>% filter(lake=='pleasant')

#2000 through 2021... where's 1998, 1999? plus need all the old time data too....


str(pptp)
pptp$date = as.Date(pptp$date)


#workflow

#average together reps

#for dates with profiles - 
  #add even depths, and interpolate

#for dates with cores and a grab - 
  #use core value for all depths within core, interpolate to grab depth
  # if grab depth includes core depth, exclude core value for that depth

#for dates with only one grab... oh well!

#for dates with only shallow cores - mark as epi only I guess


#average together reps from same date-depth-type
pptp$depthID = paste(pptp$sampleid, pptp$depth, pptp$type, sep='_')
plyr::count(pptp$r)

pptpr = ddply(pptp, .(depthID, sampleid, station, date, agency, depth, type),
              summarise, 
              tp = mean(tp.ppm))

  
#new ID col, all profiles should share a typeID
# cores + grabs should share a sampleID
pptpr$typeID = paste(pptpr$sampleid, pptpr$type, sep='_')

length(unique(pptpr$sampleid))
length(unique(pptpr$depthID))
length(unique(pptpr$typeID))

str(pptpr)
temp = plyr::count(pptpr$depth)
#conditional filter, if profile has '0.5' or other decimal, pull out all sampleids that match

temp2 = pptpr %>% 
  group_by(sampleid) %>% 
  filter(0.5 %in% depth | 
           1.5 %in% depth |
           2.5 %in% depth |
           3.5 %in% depth |
           5.5 %in% depth |
           7.5 %in% depth |
           8.2 %in% depth |
           8.3 %in% depth |
           8.4 %in% depth |
           8.5 %in% depth)

#checked it out, can round all decimals up to nearest whole number
pptpr$dep.new = ceiling(pptpr$depth)
temp = plyr::count(pptpr$dep.new)

#just station 1 for now....
pptps1 = pptpr %>% filter(station==1)

#roundabout way of getting IDs from profiles with 'core AND grab'
core.grab = ddply(pptps1, .(sampleid), summarize, core.grab=NA)

#save ID
sampID = unique(pptps1$sampleid)

for (i in 1:length(sampID)){ #for every unique sample,
  td = pptps1[pptps1$sampleid == sampID[i], ] #temp dataframe that subsets cgcheck df by each sampleid
  ifelse((('c' %in% td$type) & ('g' %in% td$type)), #if c and g present, 
         (core.grab[i,2] = 'b'),  #paste b for both 
         ifelse((('c' %in% td$type) & !('g' %in% td$type)), #if c present but g isnt
                (core.grab[i,2] = 'c'),  #paste c
                ifelse((!('c' %in% td$type) & ('g' %in% td$type)), # if c not present but g is,
                       (core.grab[i,2] = 'g'), #paste g
                       NA)
         )
         
  )
}
 

#merge new col back into tp df
pptpcg = merge(pptps1, core.grab, by='sampleid')










#Core/Grab ########
# a few cases where there's 5 grabs and a core. best to drop cores? 
tpcg = pptpcg %>% filter(core.grab=='b')
tpcg = tpcg %>% rename(sampID = sampleid)
tpcg = tpcg %>% filter(station==1)

#more depth cols
tpcg = tpcg %>% 
  mutate(core.dep = ifelse(type=='c', dep.new, NA)) %>% 
  mutate(grab.dep = ifelse(type=='g', dep.new, NA))

#need to know which sampID have enough grabs and their depths to add to other df
cgsum = ddply(tpcg, .(sampID, date), summarize, 
              tp.mean = mean(tp),
              n.core = length(which(type=='c')),
              core.dep = NA,
              n.grab = length(which(type=='g')),
              min.grab.dep = NA,
              max.grab.dep = NA)

#get core depth, and min and max grab depths
sampID = unique(cgsum$sampID)
for (i in 1:length(sampID)){ #for every unique sample
  td = tpcg[tpcg$sampID == sampID[i], ] #temp df that subsets df by each sample
  core = td[td$type == 'c', ] #just core
  core.depth = core$dep.new
  cgsum[i,5] = core.depth[1]   #paste core depth
  grabs = td[td$type == 'g', ] #just grabs
  min.grab.depth = min(grabs$dep.new) #get min depth of grabs
  max.grab.depth = max(grabs$dep.new) #get max depth of grabs
  cgsum[i,7] = min.grab.depth[1]   #paste min
  cgsum[i,8] = max.grab.depth[1]   #paste max
}

#all of the profiles with 5 grabs don't need to be in this df. 
#remove and add to other df. bc cores don't matter!
temp = cgsum %>% filter(n.grab == 5)
more.profiles = temp$sampID
#if sampID matches, change to grab only. fixed code later to subset the cores out from 'grabs only' df
pptpcg = pptpcg %>% 
  mutate(core.grab = replace(core.grab, sampleid %in% more.profiles, 'g')) 

#another new ID col 
tpcg$mergeID = paste(tpcg$sampID, tpcg$dep.new, sep='_')

#new df with space for interpolated values 0 to 9m
emptydf = data.frame(sampID = sampID)
tempdf = expand.grid(int.depth = seq(1,9), sampID = sampID)
tempdf$mergeID = paste(tempdf$sampID, tempdf$int.depth, sep='_')
tempdf = tempdf %>% select(-sampID)
#merge and other changes
cgwerk = join(tempdf, tpcg, by='mergeID')
names(cgwerk)
cgwerk = cgwerk %>% 
  select(-depth) %>% 
  rename(depth = dep.new)
cgwerk$sampID = str_sub(cgwerk$mergeID, end=-3)
cgwerk$date = str_sub(cgwerk$sampID, start=8, end=-4)
cgwerk$date = as.Date(cgwerk$date)
cgwerk = cgwerk %>% select(sampID, date, depth, tp, type, core.dep, grab.dep, int.depth)

#think through interpolation...
#tp for core depths the length of core
#if core extends to grab depth or deeper, use grab tp values instead
#interpolate NAs






#interpolate!!!
library(zoo)
tppro$tp.int = zoo::na.approx(tppro$tp)

















#GRABS ONLYZ #####

#subset
pptppro = pptpcg %>% filter(core.grab=='g' & type=='g') #also drops extra core samples from core/grab cases
plyr::count(pptppro$sampleid)


#workflow: 
#interpolate values but hopefully not if there's only one top or bottom
#summarize profile df first 

prosum = ddply(pptppro, .(sampleid, date), summarize,
               #depth
               n.depth = length(unique(dep.new)),
               min.dep = min(dep.new),
               max.dep = max(dep.new), 
               #tp 
               tp.mean = mean(tp),
               tp.min = min(tp),
               tp.max = max(tp),
               tp.sd = sd(tp))


# one single bottom grab 2011-09-14 with no core. checks out. what other data is missing....

#save sampID
sampID = unique(pptppro$sampleid)

#another new ID col plus some other changes
pptppro = pptppro %>% rename(sampID = sampleid)
pptppro$mergeID = paste(pptppro$sampID, pptppro$dep.new, sep='_')

#new df with space for interpolated values 0 to 9m
emptydf = data.frame(sampID = sampID)
tempdf = expand.grid(int.depth = seq(1,9), sampID = sampID)
tempdf$mergeID = paste(tempdf$sampID, tempdf$int.depth, sep='_')
tempdf = tempdf %>% select(-sampID)

#merge
tppro = join(tempdf, pptppro, by='mergeID')
names(tppro)
tppro = tppro %>% select(mergeID,int.depth,tp)
tppro$sampID = str_sub(tppro$mergeID, end=-3)
tppro$date = str_sub(tppro$sampID, start=8, end=-4)
tppro$date = as.Date(tppro$date)
tppro = tppro %>% 
  rename(depth=int.depth) %>% 
  select(sampID, date, depth, tp)

#interpolate!!!
library(zoo)
tppro$tp.int = zoo::na.approx(tppro$tp)

#some key issues, 
# need to identify where there are incomplete profiles and not extrapolate to 9m. 
#2006-10-26 and 2009-06-25 ends at 7m, ~15 profiles end at 8m

#if 9m  AND 8m are NA in column 'tp' but filled in in 'tp.int', remove row
#pull in MAX DEPTH from sumamry DF and use that to define conditions for removal
maxdep = prosum %>% select(sampleid, min.dep, max.dep, n.depth)
maxdep = maxdep %>% rename(sampID = sampleid)
#join
tppro = join(tppro, maxdep, by='sampID')
temp = plyr::count(tppro$tp.int)
str(tppro)
tppro$depth = as.numeric(tppro$depth)

#change tp.int to NA if maxdep/tp dep are = 7/8, 7/9, or 8/9. 
tpfix = tppro %>% 
  mutate(tp.int = replace(tp.int, (grepl(7, max.dep) & grepl(8, depth)), NA)) %>% 
  mutate(tp.int = replace(tp.int, (grepl(7, max.dep) & grepl(9, depth)), NA)) %>% 
  mutate(tp.int = replace(tp.int, (grepl(8, max.dep) & grepl(9, depth)), NA))

#change one problem date 2011-09-14 (one depth, just one grab) to all NA
tpfix = tpfix %>% 
  mutate(tp.int = replace(tp.int, date=='2011-09-14', NA))
  
#add bathyometric values
#repeat last value bc that's everything '>7m' 
bathy = data.frame(depth = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                  vol = c(2.021572, 1.400789, 0.993113,
                          0.771754, 0.514930, 0.219404,
                          0.067407, 0.013743, 0.013743))         
str(bathy)                         
#join
tpvw = join(tpfix, bathy, by='depth')
#get kg (vol weighted)
tpvw$tpkg = (tpvw$tp.int * tpvw$vol)
#now is a good time to drop NA tp.ints
tpvw = tpvw %>% drop_na(tp.int)
#make actually kg
tpvw$tpkg = tpvw$tpkg * 1000

#get summary df for total P vol weighted kg
vwprosum = ddply(tpvw, .(sampID, date), summarize,
                 tp.kg = sum(tpkg),
                 tp.kg.epi = NA,
                 tp.kg.hypo = NA,
                 tp.min = min(tpkg),
                 tp.max = max(tpkg), 
                 tp.mean = mean(tpkg),
                 tp.sd = sd(tpkg),
                 max.dep = mean(max.dep),
                 n.dep = mean(n.depth))

#save ID
sampID = unique(vwprosum$sampID)
#get epi and hypo tpkg
for (i in 1:length(sampID)){ #for every unique sample
  td = tpvw[tpvw$sampID == sampID[i], ] #temp df that subsets df by each sample
  td.epi = td[td$depth <= 5,] #subset top 5m from each profile
  epi.kg = sum(td.epi$tpkg) #mean tpkg of top 5m
  vwprosum[i,4] = epi.kg[1]   #paste value
  td.hypo = td[td$depth > 5,] #subset below 5m from each profile
  hypo.kg = sum(td.hypo$tpkg) #mean tpkg of 6-9m
  vwprosum[i,5] = hypo.kg[1]   #paste value
}

#divide by total vol
vwprosum$tp.vw = vwprosum$tp.kg / 6.002712 #PP total vol
vwprosum$epi.tpvw = vwprosum$tp.kg.epi / (2.021572 + 1.400789 +0.993113 + 0.771754 + 0.514930)
vwprosum$hypo.tpvw = vwprosum$tp.kg.hypo / (0.219404 + 0.067407 + 0.013743 + 0.013743)

#check 
head(vwprosum)
vwprosum$test = vwprosum$tp.kg.epi + vwprosum$tp.kg.hypo  
vwprosum$test = round(vwprosum$test, digits = 5)
vwprosum$tp.kg = round(vwprosum$tp.kg, digits = 5)
common = intersect(vwprosum$tp.kg, vwprosum$test)
length(common) #100 out of 100!

#vol weighted values for epi/hypo not supposed to match overall vol weighted 
vwprosum$test2 = vwprosum$epi.tpvw + vwprosum$hypo.tpvw  
common = intersect(vwprosum$tpvw, vwprosum$test2)
length(common) #yay

#compare to wendy's work
#2009-09-15
check = tpvw %>% filter(date=='2009-09-15')
sum(check$tpkg)
#wendy didn't include 8+ m grab but I did. 
check2 = tpvw %>% filter(date=='2009-08-18')
sum(check2$tpkg)

check = vwprosum %>% filter(date=='2009-09-15')
check$tp.vw
#28.92 compared to Wendy's 27.82, again i included bottom grab

#quick plot for fun
ggplot(vwprosum, aes(x=date, y=tp.vw)) + 
  geom_point() +
  geom_line()

#why is 2018-09-17 tp = 0.71311....
check2 = tpvw %>% filter(date=='2018-09-17') #seems off by one order of magnitude





