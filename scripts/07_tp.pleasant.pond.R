# Pleasant Pond phosphorus 
#created 2023-01-24

#load libraries
library(plyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(ggplot2)

#load tp #####
tp = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/tp.cwd.1998-2021.csv',header=T)
pptp = tp %>% filter(lake=='pleasant')

#2000 through 2021... where's 1998, 1999? plus need all the older data too....
str(pptp)
pptp$date = as.Date(pptp$date)
pptp = pptp %>% rename(sampID = sampleid)

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
pptp$depthID = paste(pptp$sampID, pptp$depth, pptp$type, sep='_')
plyr::count(pptp$r)

pptpr = ddply(pptp, .(depthID, sampID, station, date, agency, depth, type),
              summarise, 
              tp = mean(tp.ppm))

length(unique(pptpr$sampID))
length(unique(pptpr$depthID))

str(pptpr)
temp = plyr::count(pptpr$depth)
#conditional filter, if profile has '0.5' or other decimal, pull out all sampleids that match
temp2 = pptpr %>% 
  group_by(sampID) %>% 
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
core.grab = ddply(pptps1, .(sampID), summarize, core.grab=NA)
#save ID
sampID = unique(pptps1$sampID)

for (i in 1:length(sampID)){ #for every unique sample,
  td = pptps1[pptps1$sampID == sampID[i], ] #temp dataframe that subsets cgcheck df by each sampID
  ifelse((('c' %in% td$type) & ('g' %in% td$type)), #if c and g present, 
         (core.grab[i,2] = 'b'),  #paste b for both 
         ifelse((('c' %in% td$type) & !('g' %in% td$type)), #if c present but g isnt
                (core.grab[i,2] = 'c'),  #paste c
                ifelse((!('c' %in% td$type) & ('g' %in% td$type)), # if c not present but g is,
                       (core.grab[i,2] = 'g'), #paste g
                       NA))) 
}

#merge new col back into tp df
pptpcg = merge(pptps1, core.grab, by='sampID')



#outliers ########
high.tp = pptpcg %>% filter(tp > 0.1) 
#all at or below 7m depth except 1
#2018-09-17 3.5m grab = 0.27. off by 1 order of mag?
check = pptpcg %>% filter(date=='2018-09-17') 
#fix manually
pptpcg = pptpcg %>% 
  mutate(tp = ifelse(sampID=='5254_1_2018-09-17_cw' & depth==3.5, 0.027, tp))
str(pptpcg)




#C+G ########
#work through profiles with cores and grabs

cg = pptpcg %>% filter(core.grab=='b')

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
  mutate(core.grab = replace(core.grab, sampID %in% more.profiles, 'g')) 
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
gpro = pptpcg %>% filter(core.grab=='g' & type=='g')  #also drops extra core samples from core/grab cases
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
gprow = join(tempdf, gpro, by='mergeID')
names(gprow)
gprow = gprow %>% select(mergeID,int.depth,tp)
gprow$sampID = str_sub(gprow$mergeID, end=-3)
gprow$date = str_sub(gprow$sampID, start=8, end=-4)
gprow$date = as.Date(gprow$date)
gprow = gprow %>% 
  rename(depth=int.depth) %>% 
  select(sampID, date, depth, tp)

#interpolate!!!
gprow$tp.int = zoo::na.approx(gprow$tp)

#some key issues, 
# need to identify where there are incomplete profiles and not extrapolate to 9m. 
#2006-10-26 and 2009-06-25 ends at 7m, ~15 profiles end at 8m

#if 9m  AND 8m are NA in column 'tp' but filled in in 'tp.int', remove row
#pull in MAX DEPTH from sumamry DF and use that to define conditions for removal
maxdep = prosum %>% select(sampID, min.dep, max.dep, n.depth)
#join
gprow = join(gprow, maxdep, by='sampID')
temp = plyr::count(gprow$tp.int)
str(gprow)
gprow$depth = as.numeric(gprow$depth)

#change tp.int to NA if maxdep/tp dep are = 7/8, 7/9, or 8/9. 
gprow = gprow %>% 
  mutate(tp.int = replace(tp.int, (grepl(7, max.dep) & grepl(8, depth)), NA)) %>% 
  mutate(tp.int = replace(tp.int, (grepl(7, max.dep) & grepl(9, depth)), NA)) %>% 
  mutate(tp.int = replace(tp.int, (grepl(8, max.dep) & grepl(9, depth)), NA))

#change one problem date 2011-09-14 (one depth, just one grab) to all NA
gprow = gprow %>% 
  mutate(tp.int = replace(tp.int, date=='2011-09-14', NA))

grabs.int = gprow %>% 
  filter(!is.na(tp.int))

#end 2/3rd of types of tp profiles







#CORES ########
#cores only 
core = pptpcg %>% filter(core.grab=='c')
sampID = unique(core$sampID)
#save sampID and core depth for later
core.depths = core %>% 
  select(sampID, depth) %>% 
  rename(core.depth = depth)
#new merge ID
core$mergeID = paste(core$sampID, core$dep.new, sep='_')
#new df with space for interpolated values 0 to 9m
emptydf = data.frame(sampID = sampID)
tempdf = expand.grid(int.depth = seq(1,9), sampID = sampID)
tempdf$mergeID = paste(tempdf$sampID, tempdf$int.depth, sep='_')
tempdf = tempdf %>% select(-sampID)
#merge
core2 = join(tempdf, core, by='mergeID')
names(core2)
#subset, clean up
core2 = core2 %>% select(mergeID,int.depth,tp)
core2$sampID = str_sub(core2$mergeID, end=-3)
core2$date = str_sub(core2$sampID, start=8, end=-4)
core2$date = as.Date(core2$date)
core2 = core2 %>% 
  rename(depth=int.depth) %>% 
  select(sampID, date, depth, tp)
#join core.depths back into df
core2 = join(core2, core.depths, by='sampID')
#remove bottom depths where no data before interpolation
core2 = core2 %>% 
  group_by(sampID) %>% 
  mutate(drop.extra = ifelse((depth > core.depth), 'bye', NA)) %>% 
  filter(is.na(drop.extra)) %>% 
  select(-drop.extra)
#fill in
core2 = core2 %>% 
  mutate(tp.int = tp) %>% 
  fill(tp.int, .direction='up')
#clean some more
core.int = core2 %>%  
  select(sampID, date, depth, tp.int) %>% 
  rename(tp = tp.int) 

  



#MERGE ########
#rejoin tp interpolated values and such from C/G, cores only, grabs only. 
#cores only
names(core.int)
core.int.nice = core.int %>% 
  mutate(type = 'c') %>% 
  mutate(set = 'c') 
names(core.int.nice)
# cores/grabs
names(cg.int)
cg.int.nice = cg.int %>% 
  select(sampID, date, depth, tp.int, type) %>% 
  rename(tp = tp.int) %>% 
  mutate(set = 'cg')
names(cg.int.nice)
#grabs only
names(grabs.int)
grabs.int.nice = grabs.int %>% 
  select(sampID, date, depth, tp.int) %>% 
  rename(tp = tp.int) %>% 
  mutate(type = 'g') %>% 
  mutate(set = 'g')
names(grabs.int.nice)

#merge
tp.clean = rbind(grabs.int.nice,
                 core.int.nice,
                 cg.int.nice)

str(tp.clean)
summary(tp.clean$tp)
# nice






#VOL WEIGHT ############################
#transform into volumetrically weighted tp


#add bathymetric values
#repeat last value bc that's everything '>7m' 
bathy = data.frame(depth = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                   vol = c(2.021572, 1.400789, 0.993113,
                           0.771754, 0.514930, 0.219404,
                           0.067407, 0.013743, 0.013743))         
str(bathy)                         
#join
tp.vw = join(tp.clean, bathy, by='depth')
#get kg 
tp.vw$tp.kg = (tp.vw$tp * tp.vw$vol * 1000)

#* drop 9m rows ######################
#tp.vw
#maybe later



#get summary df for total P vol weighted kg
tp.sum = ddply(tp.vw, .(sampID, date, set), summarize,
               tp.kg.total = sum(tp.kg),
               tp.kg.epi = NA,
               tp.kg.hypo = NA,
               tp.kg.min = min(tp.kg),
               tp.kg.max = max(tp.kg), 
               tp.kg.mean = mean(tp.kg),
               tp.kg.sd = sd(tp.kg),
               pro.depth = max(depth))
str(tp.sum)
#get epi and hypo tpkg
#save ID
sampID = unique(tp.sum$sampID)
for (i in 1:length(sampID)){ 
  td = tp.vw[tp.vw$sampID == sampID[i], ] #subset by sample
  td.epi = td[td$depth <= 5,] #subset top 5m from each profile
  epi.kg = sum(td.epi$tp.kg) #mean tpkg of top 5m
  tp.sum[i,5] = epi.kg[1]   #paste value
  td.hypo = td[td$depth > 5,] #subset below 5m from each profile
  hypo.kg = sum(td.hypo$tp.kg) #mean tpkg of 6-9m
  tp.sum[i,6] = hypo.kg[1]   #paste value
}

#divide by total vol for actual volume weighted
tp.sum$tp.vw = tp.sum$tp.kg.total / 6.002712 #PP total vol
tp.sum$tp.vw.epi = tp.sum$tp.kg.epi / (2.021572 + 1.400789 +0.993113 + 0.771754 + 0.514930)
tp.sum$tp.vw.hypo = tp.sum$tp.kg.hypo / (0.219404 + 0.067407 + 0.013743 + 0.013743)

#check 
test = tp.sum
test$test = test$tp.kg.epi + test$tp.kg.hypo  
test$test = round(test$test, digits = 2)
test$tp.kg.total = round(test$tp.kg.total, digits = 2)
length(intersect(test$tp.kg.total, test$test)) #134/135
#vol weighted values for epi/hypo not supposed to match overall vol weighted 
test$test2 = test$tp.vw.epi + test$tp.vw.hypo  
length(intersect(test$tp.vw, test$test2))

#compare to wendy's work
#2009-09-15
check = tp.kg %>% filter(date=='2009-09-15')
sum(check$tp.kg)
#wendy didn't include 8+ m grab but I did. 
check2 = tp.kg %>% filter(date=='2009-08-18')
sum(check2$tp.kg)

check = tp.sum %>% filter(date=='2009-09-15')
check$tp.vw
#28.92 compared to Wendy's 27.82, again i included bottom grab


#YEARLY avg ############
tp.sum$year = lubridate::year(tp.sum$date)
names(tp.sum)

tp.yr = ddply(tp.sum, .(year), summarize,
              n = length(unique(date)),
              tp.vw.min = min(tp.vw),
              tp.vw.max = max(tp.vw), 
              tp.vw.mean = mean(tp.vw),
              tp.vw.sd = sd(tp.vw),
              tp.vw.epi.min = min(tp.vw.epi),
              tp.vw.epi.max = max(tp.vw.epi), 
              tp.vw.epi.mean = mean(tp.vw.epi),
              tp.vw.epi.sd = sd(tp.vw.epi),
              tp.vw.hypo.min = min(tp.vw.hypo),
              tp.vw.hypo.max = max(tp.vw.hypo), 
              tp.vw.hypo.mean = mean(tp.vw.hypo),
              tp.vw.hypo.sd = sd(tp.vw.hypo))
   
#add 90s values for total mean TPppb
tp90s = data.frame(year = c(1991, 1992, 1992, 1994, 1995,
                            1996, 1997, 1998, 1999), 
                   tp.vw.mean = c(28, 23, 22, 19, 24, 23, 22, 25, 20))
             
tp.yr = dplyr::bind_rows(tp90s, tp.yr)              
         


 
#PLOTS ########
#quick plot for fun
ggplot(tp.sum, aes(x=date, y=tp.vw, color=set)) + 
  geom_point()

ggplot(tp.sum, aes(x=date, y=tp.vw)) + 
  geom_point() + 
  geom_line(linetype=3)
  
ggplot(tp.sum, aes(x=date, y=tp.vw)) + 
  geom_point() + 
  stat_smooth(method="loess", 
              linewidth=0.75,
              se=T, 
              show.legend=F)



#yearly
ggplot(tp.yr, aes(x=year, y=tp.vw.mean)) + 
  geom_point() + 
  stat_smooth(method="loess", 
              linewidth=0.5,
              se=F, 
              show.legend=F,
              mapping=aes(alpha=0.1)) + 
  scale_y_continuous(limits=c(15,30)) +
  labs(title='Internal phosphorus loading 1991-2021',
       y='TP ppb (vol. weighted)',
       x='Date') +
  theme_bw() +
    theme(title=element_text(size=10))
  
 
#seasonal variability, error bars
ggplot(tp.yr, aes(x=year, y=tp.vw.mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=tp.vw.mean - tp.vw.sd, 
                    ymax=tp.vw.mean + tp.vw.sd), 
                width=.2,
                position=position_dodge(.9)) +
  stat_smooth(method="loess", 
              linewidth=0.5,
              se=F, 
              show.legend=F,
              mapping=aes(alpha=0.1)) + 
  scale_y_continuous(limits=c(10,35)) +
  scale_x_continuous(limits=c(1999.5,2022)) +
  labs(title='Standard deviation of TP within season 2000-2021',
       y='TP ppb (vol. weighted)',
       x='Date') +
  theme_bw() +
  theme(title=element_text(size=10))


#seasonal variability, min and max
temp = tp.yr %>% 
  tidyr::pivot_longer(
    cols = c(tp.vw.min, tp.vw.mean, tp.vw.max), 
    names_to = 'Value',
    values_to = 'tp.val') %>% 
  mutate(Value = replace(Value, grepl('tp.vw.min',Value),'Min')) %>% 
  mutate(Value = replace(Value, grepl('tp.vw.mean',Value),'Mean')) %>% 
  mutate(Value = replace(Value, grepl('tp.vw.max',Value),'Max'))

ggplot(temp,
       aes(x=year, y=tp.val, color=Value)) +
  geom_point() + 
  stat_smooth(method="loess", 
              linewidth=0.5,
              se=F, 
              show.legend=F,
              mapping=aes(alpha=0.1)) + 
  scale_y_continuous(limits=c(10,40)) +
  scale_x_continuous(limits=c(2000,2021)) +
  labs(title='TP seasonal variability 2000-2021',
       y='TP ppb (vol. weighted)',
       x='Date') +
  theme_bw() +
  theme(title=element_text(size=10))


#epi/hypo contribution
ggplot((tp.yr %>% 
         tidyr::pivot_longer(
           cols = c(tp.vw.epi.mean, tp.vw.hypo.mean), 
                    names_to = 'Layer',
                    values_to = 'tp.val') %>% 
          mutate(Layer = replace(Layer, grepl('tp.vw.epi.mean',Layer),'Epi')) %>% 
          mutate(Layer = replace(Layer, grepl('tp.vw.hypo.mean',Layer),'Hypo'))),
       aes(x=year, y=tp.val, color=Layer)) +
  geom_point() + 
  stat_smooth(method="loess", 
              linewidth=0.5,
              se=F, 
              show.legend=F,
              mapping=aes(alpha=0.1)) + 
  scale_y_continuous(limits=c(15,60)) +
  scale_x_continuous(limits=c(2000,2021)) +
  labs(title='Epi and Hypo TP contribution 2000-2021',
       y='TP ppb (vol. weighted)',
       x='Date') +
  theme_bw() +
  theme(title=element_text(size=10))

           












