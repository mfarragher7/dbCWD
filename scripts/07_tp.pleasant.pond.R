# Pleasant Pond phosphorus 
#created 2023-01-24

#load libraries
library(plyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(ggplot2)

#load tp 
tp = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/tp.cwd.1998-2021.csv',header=T)
pptp = tp %>% filter(lake=='pleasant')

#2000 through 2021... where's 1998, 1999? plus need all the older data too....
str(pptp)
pptp$date = as.Date(pptp$date)

#workflow
#average together reps

#for dates with cores + grabs - 
  #use core value for all depths within core, interpolate to grab depth
  # if grab depth includes core depth, exclude core value for that depth
#for dates with profiles OR grabs - 
  #add even depths, and interpolate
#for dates with only one grab... oh well!
#for dates with only shallow cores - mark as epi only I guess

#average together reps from same date-depth-type
pptp$depthID = paste(pptp$sampleid, pptp$depth, pptp$type, sep='_')
plyr::count(pptp$r)

pptpr = ddply(pptp, .(depthID, sampleid, station, date, agency, depth, type),
              summarise, 
              tp = mean(tp.ppm))

length(unique(pptpr$sampleid))
length(unique(pptpr$depthID))

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
                       NA))) 
}

#merge new col back into tp df
pptpcg = merge(pptps1, core.grab, by='sampleid')





#C+G ########
#work through profiles with cores and grabs

cg = pptpcg %>% filter(core.grab=='b')
cg = cg %>% rename(sampID = sampleid)

#more depth cols
cg = cg %>% 
  mutate(core.dep = ifelse(type=='c', dep.new, NA)) %>% 
  mutate(grab.dep = ifelse(type=='g', dep.new, NA))

#need to know which sampID have enough grabs/ depths to drop here and add to other df
cgsum = ddply(cg, .(sampID, date), summarize, 
              tp.mean = mean(tp),
              n.core = length(which(type=='c')),
              core.dep = NA,
              n.grab = length(which(type=='g')),
              min.grab.dep = NA,
              max.grab.dep = NA)

#get core depth, and min and max grab depths
sampID = unique(cgsum$sampID)
for (i in 1:length(sampID)){ #for every unique sample
  td = cg[cg$sampID == sampID[i], ] #temp df that subsets df by each sample
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
#now remove extra cores from core/grab df
#make depthID NA, then remove. 
#removs 72 values, makes sense: 12 profiles with 5g + 1c each. cores subsetted out later too
cg = cg %>% 
  mutate(depthID = replace(depthID, (sampID %in% more.profiles), NA)) %>% 
         drop_na(depthID)
#28 profiles
length(unique(cg$sampID)) 

#are there cores deeper than or equal to max grab?
cgsum = cgsum %>% 
  mutate(core.deeper = ifelse((core.dep > max.grab.dep), 'y', NA)) %>% 
  mutate(cg.dep.equal = ifelse((core.dep == max.grab.dep), 'y', NA))
  
#save sampIDs
#core < grab 
grab.deeper = cgsum %>% filter(is.na(core.deeper) & is.na(cg.dep.equal))
grab.deeperID = grab.deeper$sampID
#c reo > grab
core.deeper = cgsum %>% filter(core.deeper == 'y')
core.deeperID = core.deeper$sampID
#equal
cg.dep.equal = cgsum %>% filter(cg.dep.equal == 'y')
cg.dep.equalID = cg.dep.equal$sampID
#get max grab dep 
cg.max.grab.dep = cgsum %>% select(sampID, max.grab.dep)


#** interpolation ########
#begin building table for interpolated depths
#another new ID col for merging
cg$mergeID = paste(cg$sampID, cg$dep.new, sep='_')
length(unique(cg$mergeID))
#new df with space for interpolated values 0 to 9m
sampID = unique(cg$sampID)
emptydf = data.frame(sampID = sampID)
tempdf = expand.grid(int.depth = seq(1,9), sampID = sampID)
tempdf$mergeID = paste(tempdf$sampID, tempdf$int.depth, sep='_')
tempdf = tempdf %>% select(-sampID)
#check
plyr::count(tempdf$int.depth) # ok. 
#merge and other changes
cgwerk = join(tempdf, cg, by='mergeID')
names(cgwerk)
str(cgwerk)
cgwerk = cgwerk %>% 
  select(-depth) %>% 
  rename(depth = dep.new)
cgwerk$sampID = str_sub(cgwerk$mergeID, end=-3)
cgwerk$date = str_sub(cgwerk$sampID, start=8, end=-4)
length(unique(cgwerk$sampID))
cgwerk$date = as.Date(cgwerk$date)
cgwerk = cgwerk %>% select(sampID, date, depth, tp, type, core.dep, grab.dep, int.depth)
plyr::count(cgwerk$int.depth) # too many depths because sometimes g and c depths match.

#tp for core depths the length of core
#if core extends to grab depth or deeper, use grab tp values instead
#interpolate NAs
#drop 'extra' depths from cores if c/g depths match

#add max grab depths
cgwerk = join (cgwerk, cg.max.grab.dep, by='sampID')

#seperate dfs, cant figure out how to ifelse my way out of this
cgwerk = cgwerk %>% 
  mutate(who.deeper = ifelse((sampID %in% grab.deeperID), 'g',
                             ifelse((sampID %in% core.deeperID), 'c',
                                     ifelse((sampID %in% cg.dep.equalID), 'equal', NA))))

#df weird
str(cgwerk)
cgwerk$int.depth = as.numeric(cgwerk$int.depth)
cgwerk = as.data.frame(cgwerk)
str(cgwerk)


#** grabs deeper ###########

#group by type, drag up tp values
cg.normal = cgwerk %>% 
  filter(who.deeper == 'g') %>% 
  fill(type, .direction = "up") %>% 
  group_by(sampID) %>% 
  mutate(tp.temp = tp) %>% 
  group_by(sampID, type) %>% 
  fill(tp.temp, .direction = "up") %>% 
  ungroup()

#delete dragged down tp values from cores 
cg.normal = cg.normal %>% 
  group_by(sampID) %>% 
  mutate(tp.temp = replace(tp.temp, (type=='g' & is.na(tp)), NA))
  
#remove extra rows where int.depth > max grab depth
cg.normal = cg.normal %>% 
  mutate(drop.extra = ifelse(int.depth > max.grab.dep, 1, NA)) %>% 
  filter(is.na(drop.extra))

#if core and grab depths match, take grab depth
# 5254_1_2021-10-08_cw double 7m 
# 5254_1_2020-09-18_cw double 6m
plyr::count(cg.normal$int.depth) 

#omg a for loop
cg.normal$cg.match = NA
cg.normal$depID = paste(cg.normal$sampID, cg.normal$int.depth, sep='_')
depID = cg.normal$depID

#for every unique depthID....
for (i in 1:length(depID)){ 
  td = cg.normal[cg.normal$depID == depID[i], ]
  ifelse(nrow(td) > 1, #if there's depths that match
         (cg.normal[i,'cg.match'] = paste('y')), #paste yes
         NA) } # or not
#subset
cg.normal = cg.normal %>% 
  mutate(drop.cg.match = ifelse(cg.match=='y' & type=='c', 'y', NA)) %>% 
  filter(is.na(drop.cg.match))
# maybe finally ready to interpolate!!!
cg.normal$tp.int = zoo::na.approx(cg.normal$tp.temp)


#** C=G ##################
#core depth = grab depth 
cg.eq = cgwerk %>%
  filter(who.deeper == 'equal') %>% 
  mutate(tp.int = tp) %>% 
  fill(tp.int, .direction = "up") %>% 
  mutate(tp.int = ifelse(!is.na(core.dep), NA, tp.int)) %>% 
  filter(!is.na(tp.int))

#** core deeper ###########
#group by type, drag up tp values
cg.deepcore = cgwerk %>%
  filter(who.deeper == 'c') %>% 
  mutate(tp.temp = tp) %>% 
  mutate(tp.temp = replace(tp.temp, type=='g', NA)) %>% 
  fill(tp.temp, .direction = "up") %>% 
  mutate(tp.temp = replace(tp.temp, type=='g', NA)) %>% 
  mutate(tp.temp2 = ifelse(is.na(tp.temp), tp, tp.temp))
#drop tp value of max core depth
cg.deepcore = cg.deepcore %>% 
  mutate(tp.temp2 = ifelse(!is.na(core.dep), NA, tp.temp2)) %>% 
  filter(!is.na(tp.temp2))
#drop cores greater than max grab depths


#** rejoin #######
#reunite normal (grabs deeper), core deeper, and cg equal dfs with interpolated values
names(cg.normal)
cg.normal.nice = cg.normal %>% 
  select(sampID, date, type, who.deeper, int.depth, tp.int)
names(cg.eq)  
cg.eq.nice = cg.eq %>% 
  select(sampID, date, type, who.deeper, int.depth, tp.int)
names(cg.deepcore)  
cg.deepcore.nice = cg.deepcore %>% 
  select(sampID, date, type, who.deeper, int.depth, tp.temp2) %>% 
  rename(tp.int = tp.temp2)

#MERGE
cg.int = rbind(cg.normal.nice,
               cg.eq.nice,
               cg.deepcore.nice)

cg.int = cg.int %>% rename(depth = int.depth)
  
#end 1/3rd of work, when there's cores & grabs in one profile







#GRABS #####
#profiles of just grabs
#subset
gpro = pptpcg %>% filter(core.grab=='g' & type=='g') %>%  #also drops extra core samples from core/grab cases
  rename(sampID = sampleid)
plyr::count(gpro$sampID)

#workflow: 
#interpolate values but hopefully not if there's only one top or bottom
#summarize profile df first 

prosum = ddply(gpro, .(sampID, date), summarize,
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
sampID = unique(gpro$sampID)

#another new ID col 
gpro$mergeID = paste(gpro$sampID, gpro$dep.new, sep='_')

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
  
#** vol weight ############
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
tpvw$tpkg = (tpvw$tp.int *  tpvw$vol)
#now is a good time to drop NA tp.ints
tpvw = tpvw %>% drop_na(tp.int)
#make actually kg
tpvw$tpkg = tpvw$tpkg *  1000

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




#CORES ########
#cores only 

tpcore = pptpcg %>% filter(core.grab=='c')
tpcore = tpcore %>% rename(sampID = sampleid)
tpcore = tpcore %>% filter(station==1)
sampID = unique(tpcore$sampID)

#new merge ID
tpcore$mergeID = paste(tpcore$sampID, tpcore$dep.new, sep='_')

#new df with space for interpolated values 0 to 9m
emptydf = data.frame(sampID = sampID)
tempdf = expand.grid(int.depth = seq(1,9), sampID = sampID)
tempdf$mergeID = paste(tempdf$sampID, tempdf$int.depth, sep='_')
tempdf = tempdf %>% select(-sampID)

#merge
cr = join(tempdf, tpcore, by='mergeID')
names(cr)
cr = cr %>% select(mergeID,int.depth,tp)
cr$sampID = str_sub(cr$mergeID, end=-3)
cr$date = str_sub(cr$sampID, start=8, end=-4)
cr$date = as.Date(cr$date)
cr = cr %>% 
  rename(depth=int.depth) %>% 
  select(sampID, date, depth, tp)





#remove bottom depths where no data before interpolation



#interpolate!!!
library(zoo)
tppro$tp.int = zoo::na.approx(tppro$tp)







#MERGE ########



#rejoin tp interpolated values and such from C/G, cores only, grabs only. 



#VOL WEIGHT ############################
#just do this once!!!!


#add bathymetric values
#repeat last value bc that's everything '>7m' 
bathy = data.frame(depth = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                   vol = c(2.021572, 1.400789, 0.993113,
                           0.771754, 0.514930, 0.219404,
                           0.067407, 0.013743, 0.013743))         
str(bathy)                         
#join
cg.int = join(cg.int, bathy, by='depth')








