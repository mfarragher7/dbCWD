#Load all survey data: EXO, C3, water chem, secchi, phytoplankton, bathymetry
#make dataframe transformations here instead in any of the figure or summary scripts
#see SampleLog excel doc for neater list of sample date groups as seasonIDs
#updated 2021-06-03


#libraries
library(dplyr)
library(tidyr)
library(plyr)


#EXO ####

#EXO NOTES
#one profile missing - JP August 19th. code to filter it out:
#full.exo = full.exo %>% filter(!lakedate=='JP_2020-08-19')

full.exo = read.csv("dbSurvey/dbEXO.csv",header=TRUE)
#coerce date
full.exo$Date = as.Date(full.exo$Date)
#model fdom columns
full.exo$fDOM_RFU_corr = full.exo$fDOM_RFU/(1+(-0.01*(full.exo$Temp-25)))
full.exo$fDOM_raw_corr = full.exo$fDOM_raw/(1+(-0.01*(full.exo$Temp-25)))
#rename first column
colnames(full.exo)[1] = "LakeID"
#edit and add columns 
full.exo = full.exo %>% 
  mutate(lakename=ifelse(grepl('JP',LakeID),'Jordan Pond',NA)) %>% #add lake name column 
  mutate(lakename=replace(lakename,grepl('SC',LakeID), 'Seal Cove Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('WH',LakeID), 'Witch Hole Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('BB',LakeID), 'Bubble Pond')) %>% 
  mutate(lakedate=paste(LakeID,Date,sep="_")) %>% #add lakedate ID column
  #add season seasonID column
  mutate(seasonID=ifelse(grepl('2020-02-21',lakedate) | grepl('2020-02-25',lakedate),'winter1',NA)) %>% #winter1
  mutate(seasonID=replace(seasonID,grepl('2020-03-25',lakedate),'spring1')) %>% #spring1
  mutate(seasonID=replace(seasonID,grepl('2020-05-13',lakedate),'spring2')) %>% #spring2
  mutate(seasonID=replace(seasonID,grepl('2020-05-26',lakedate) | grepl('2020-05-28',lakedate),'spring3')) %>% #spring3
  mutate(seasonID=replace(seasonID,grepl('2020-06-13',lakedate) | grepl('2020-06-14',lakedate),'spring4')) %>% #spring4
  mutate(seasonID=replace(seasonID,grepl('2020-06-28',lakedate) | grepl('2020-07-02',lakedate),'summer1')) %>% #summer1
  mutate(seasonID=replace(seasonID,grepl('2020-07-20',lakedate) | grepl('2020-07-21',lakedate),'summer2')) %>% #summer2
  mutate(seasonID=replace(seasonID,grepl('2020-08-03',lakedate) | grepl('2020-08-04',lakedate),'summer3')) %>% #summer3
  mutate(seasonID=replace(seasonID,grepl('2020-08-19',lakedate),'summer4')) %>% #summer4
  mutate(seasonID=replace(seasonID,grepl('2020-09-05',lakedate) | grepl('2020-09-06',lakedate),'fall1')) %>% #fall1
  mutate(seasonID=replace(seasonID,grepl('2020-09-24',lakedate) | grepl('2020-09-27',lakedate),'fall2')) %>% #fall2
  mutate(seasonID=replace(seasonID,grepl('2020-10-11',lakedate) | grepl('2020-10-12',lakedate) | grepl('2020-10-15',lakedate),'fall3')) %>%  #fall3
  mutate(seasonID=replace(seasonID,grepl('2020-10-31',lakedate) | grepl('2020-11-06',lakedate),'fall4')) #fall4
#check structure
str(full.exo)
#write csv
write.csv(full.exo, "library/db.exo.csv", row.names=FALSE)

#seasonIDs ####
seasonID = full.exo %>% dplyr::select(lakedate,seasonID) %>% distinct(lakedate, .keep_all = TRUE)
#write csv
write.csv(seasonID, "library/seasonID.csv", row.names=FALSE)



#C3 ####
full.c3 = read.csv("dbSurvey/dbC3.csv",header=TRUE)
#coerce date, fix messed up date
full.c3$Date = as.Date(full.c3$Date, format="%m/%d/%y") #the one date with incorrect date gets turned to NA
full.c3$Date = full.c3$Date %>% replace_na("2020-07-02") #incorrect date (NA) changed to correct date
#model cdom
full.c3$CDOM = full.c3$CDOM_raw/(1+(-0.01*(full.c3$Temp-25)))
#rename first column
colnames(full.c3)[1] = "LakeID"
#edit and add columns 
full.c3 = full.c3 %>% 
  mutate(lakename=ifelse(grepl('JP',LakeID),'Jordan Pond',NA)) %>% #add lakename
  mutate(lakename=replace(lakename,grepl('SC',LakeID), 'Seal Cove Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('WH',LakeID), 'Witch Hole Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('BB',LakeID), 'Bubble Pond')) %>% 
  mutate(lakedate=paste(LakeID,Date,sep="_")) #add lakedate ID column
#add season seasonID column
full.c3 = join(full.c3,seasonID,by='lakedate')
#check structure
str(full.c3)
#remove negative depths
full.c3 = full.c3 %>% filter(Depth > 0)
#remove duplicate depths from each profiles
full.c3 = full.c3 %>% group_by(lakedate) %>% filter(duplicated(Depth) == FALSE)
#write csv
write.csv(full.c3, "library/db.c3.csv", row.names=FALSE)


library(purrr)
library(stringr)

#Nutrients ####
full.nooch = read.csv("dbSurvey/dbNutrients.csv",header=TRUE)
#check structure
str(full.nooch)
#coerce date
full.nooch$Date = as.Date(full.nooch$Date)
#Replace 'below detection limits' with actual detection limit
full.nooch = full.nooch %>% 
  select(-SRP_ugpl) %>% 
  mutate(NH4_mgpl=replace(NH4_mgpl,NH4_mgpl=='MRL<0.005',0.005)) %>% 
  mutate(NO3_mgpl=replace(NO3_mgpl,NO3_mgpl=='MRL<0.0025',0.0025))
#check structure
str(full.nooch)
#coerce to numeric 
full.nooch$NH4_mgpl = as.numeric(full.nooch$NH4_mgpl)
full.nooch$NO3_mgpl = as.numeric(full.nooch$NO3_mgpl)
#convert to ugpl
full.nooch$NH4_ugpl = full.nooch$NH4_mgpl*1000
full.nooch$NO3_ugpl = full.nooch$NO3_mgpl*1000
full.nooch$TN_ugpl = full.nooch$TN_mgpl*1000
#add DIN and DIN:TP
full.nooch$DIN_ugpl = full.nooch$NH4_ugpl + full.nooch$NO3_ugpl
full.nooch$DIN_TP = full.nooch$DIN / full.nooch$TP_ugpl
#check structure
str(full.nooch)
#rename first col
colnames(full.nooch)[1] = "LakeID"
#rename column, add lakedate, sampleID, lakedepth, and seasonID
full.nooch = full.nooch %>% 
  mutate(lakedate=paste(LakeID,Date,sep="_")) %>%  #add lakedate ID column
  mutate(lakedepth=paste(LakeID,Date,Site,Depth,sep="_")) #lakedepth ID with date, site, depth
#add season seasonID column
full.nooch = join(full.nooch,seasonID,by='lakedate')
#check structure
str(full.nooch)
#to lowercase
nooch = full.nooch %>% 
  set_names(~ str_to_lower(.) ) %>% 
  rename_with(stringr::str_replace, pattern = "_", replacement = ".") %>% 
  dplyr::select(-tn.mgpl,-nh4.mgpl,-no3.mgpl,-chl.vol_ml) %>% 
  dplyr::rename(LakeID = lakeid) %>% 
  dplyr::rename(Date = date) %>% 
  dplyr::rename(seasonID = seasonid)
#write csv
write.csv(nooch, "library/db.nooch.csv", row.names=FALSE)

#full.nutrients[,4:12] = sapply(full.nutrients[,4:12],as.numeric)
  



#Secchi ####
full.secchi = read.csv("dbSurvey/dbSecchi.csv",header=TRUE)
#check structure
str(full.secchi)
#coerce date
full.secchi$Date = as.Date(full.secchi$Date)
#fix date
colnames(full.secchi)[1] = "LakeID"
#add lakename, lakedate, 
full.secchi = full.secchi %>% 
  mutate(lakename=ifelse(grepl('JP',LakeID),'Jordan Pond',NA)) %>% #add lake name column 
  mutate(lakename=replace(lakename,grepl('SC',LakeID), 'Seal Cove Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('WH',LakeID), 'Witch Hole Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('BB',LakeID), 'Bubble Pond')) %>% 
  mutate(lakedate=paste(LakeID,Date,sep="_"))  #add lakedate ID column
#add season seasonID column
full.secchi = join(full.secchi,seasonID,by='lakedate')
#write csv
write.csv(full.secchi, "library/db.secchi.csv", row.names=FALSE)




# Bathymetry #######
#load bathy data
#area and volume of each 1m section for each lake

#JP
bath.jp = read.csv("dbBathymetry/bathy_csvs/JP_vol.csv",header=TRUE)
colnames(bath.jp)[1] = "LakeID" 
bath.jp[,2:5] = sapply(bath.jp[,2:5],as.numeric)
str(bath.jp)
#write csv
write.csv(bath.jp, "library/bath.jp.csv", row.names=FALSE)

#SC
bath.sc = read.csv("dbBathymetry/bathy_csvs/SC_vol.csv",header=TRUE)
colnames(bath.sc)[1] = "LakeID" 
bath.sc[,2:5] = sapply(bath.sc[,2:5],as.numeric)
str(bath.sc)
#write csv
write.csv(bath.sc, "library/bath.sc.csv", row.names=FALSE)

#BB
bath.bb = read.csv("dbBathymetry/bathy_csvs/BB_vol.csv",header=TRUE)
colnames(bath.bb)[1] = "LakeID" 
bath.bb[,2:5] = sapply(bath.bb[,2:5],as.numeric)
str(bath.bb)
#write csv
write.csv(bath.bb, "library/bath.bb.csv", row.names=FALSE)

#WH
bath.wh = read.csv("dbBathymetry/bathy_csvs/WH_vol.csv",header=TRUE)
colnames(bath.wh)[1] = "LakeID" 
bath.wh[,2:5] = sapply(bath.wh[,2:5],as.numeric)
str(bath.wh)
#write csv
write.csv(bath.wh, "library/bath.wh.csv", row.names=FALSE)

#load other lake info metadata
bath.all = read.csv("dbBathymetry/bathy_csvs/Lake_descriptions.csv",header=T)
colnames(bath.all)[1] = "LakeID" 
str(bath.all)
bath.all$Date = as.Date(bath.all$Date)
bath.all[,c(3,6)] = sapply(bath.all[,c(3,6)],as.numeric)
#write csv
write.csv(bath.all, "library/bath.metadata.csv", row.names=FALSE)









#Phyto ####
#load raw phyto
phyto = read.csv("dbPhyto/dbPhyto.csv",header=T)
str(phyto)
colnames(phyto)[1] = "LakeID"
sort(unique(phyto$group))
sort(unique(phyto$taxa))
sort(unique(phyto$sampleID))
#fix taxa:

#UGOs
#ugo1 - 'medium centric' -> cyst
#ugo2 - 'popcorn' -> synura, x 6 (on average)
#ugo3 - 'brown blob' -> athecate dino
#ugo4 - 'medium radial' -> golenkinia
#ugo5 - 'double long spine' -> Chryso / bitrichia
#ugo6 - 'double short spine' -> chlorophyta / korschikoviella 
#ugo7 - 'medium coccoid v1' -> chryso / chromulina
#ugo8 - 'medium coccoid v2' -> chloro / chlamydomonas
#ugo9 - 'large coccoid' -> athecate dino
#ugo10 - 'coccoid chain' -> cyanophyta / unid. cyano
#ugo11 - 'ovate coccoid' -> cyanophyta / dolichospermum
#ugo12 - 'blue blob' -> junk
#ugo13 - 'brown blob' -> junk
#ugo14 - 'green blob' -> junk

#ugo1
phy.coded = phyto %>% 
  mutate(group.clean = ifelse(taxa=='ugo1 medium centric','unid. cysts', group)) %>%  #if ugo1, group=unid cyst, otherwise, keep
  mutate(taxa.clean = ifelse(taxa=='ugo1 medium centric','unid. cyst', taxa)) 

#ugo2
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo2 medium popcorn', 'synurophyta')) %>% 
  mutate(count.new = ifelse(taxa=='ugo2 medium popcorn', count*6, count)) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo2 medium popcorn', 'synura'))

#ugo3
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo3 medium brown', 'dinophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo3 medium brown', 'athecate dino.'))

#ugo4
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo4 medium radial', 'chlorophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo4 medium radial', 'golenkinia'))

#ugo5
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo5 double long spine', 'chrysophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo5 double long spine', 'bitrichia'))

#ugo6
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo6 double short spine', 'chlorophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo6 double short spine', 'korschikoviella'))

#ugo7
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo7 medium coccoid v1', 'chrysophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo7 medium coccoid v1', 'chromulina'))

#ugo8 
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo8 medium coccoid v2', 'chlorophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo8 medium coccoid v2', 'chlamydomonas'))

#ugo9 
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo9 large coccoid', 'dinophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo9 large coccoid', 'athecate dino.'))

#ugo10 
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo10 coccoid chain', 'unid. non-flagellated')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo10 coccoid chain', 'unid. cyanobacteria'))

#ugo11 
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo11 ovate coccoid', 'cyanophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo11 ovate coccoid', 'dolichospermum'))

#ugo12
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo12 blue blob', 'unid. non-flagellated')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo12 blue blob', 'blue blob'))

#ugo13
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo13 brown blob', 'unid. non-flagellated')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo13 brown blob', 'brown blob'))

#ugo14
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ugo14 green blob', 'unid. non-flagellated')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ugo14 green blob', 'green blob'))

#check
sort(unique(phy.coded$group.clean))
sort(unique(phy.coded$taxa.clean))



#UFOs
#ufo1 - 'unflagellated' -> unid small coccoid
#ufo2,6,7 - 'equal short flagella v1' -> 2,6,7 'katablepharid 1'
#ufo3 - 'equal short flagella v2' -> katablepharid 2
#ufo4 - 'unequal long flagella v1' -> katablepharid 3
#ufo5 - 'unequal long flagella v2' -> unid cryptophyta
#ufo8 -> phacus
#ufo9  - 'ufo9 cf chytrid' 

#ufo1
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ufo1 unflagellated', 'unid. non-flagellated')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ufo1 unflagellated', 'unid. small coccoid'))

#ufo 2,6,7
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,
                               taxa=='ufo2 equal short flagella v1'|
                               taxa=='ufo6 equal long flagella v1' |
                               taxa=='ufo7 unequal short flagella v1','cryptophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,
                                taxa=='ufo2 equal short flagella v1'|
                                taxa=='ufo6 equal long flagella v1' |
                                taxa=='ufo7 unequal short flagella v1','katablepharid 1'))

#ufo3 
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ufo3 equal short flagella v2', 'cryptophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ufo3 equal short flagella v2', 'katablepharid 2'))

#ufo4
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ufo4 unequal long flagella v1', 'cryptophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ufo4 unequal long flagella v1', 'katablepharid 3'))

#ufo5
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ufo5 unequal long flagella v2', 'cryptophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ufo5 unequal long flagella v2', 'unid. cryptophyte'))

#ufo8
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ufo8', 'euglena')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ufo8', 'phacus'))

#ufo9
phy.coded = phy.coded %>% 
  mutate(group.clean = replace(group.clean,taxa=='ufo9 cf chytrid', 'chytrids')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='ufo9 cf chytrid', 'chytrid'))

#check
sort(unique(phy.coded$group.clean))
sort(unique(phy.coded$taxa.clean))


#fix other stuff

#greens. fix all cosmarium
phy.coded = phy.coded %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='cosmarium','cosmarium 1')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='desmid type1','cosmarium 2')) %>% 
  mutate(group.clean = replace(group.clean,taxa=='haptophyta type2','chlorophyta')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='haptophyta type2','cosmarium 3'))

#diatoms
#combine girdle views
phy.coded = phy.coded %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='centric','centric diatom')) %>% 
  mutate(taxa.clean = replace(taxa.clean,grepl('girdle',taxa),'girdle view diatom'))

#cyanophyta
#name unknowns
phy.coded = phy.coded %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='cyanophyta type1','merismopedia')) %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='cyanophyta type2','snowella')) 
  
#dinophyta
#change to athecate
phy.coded = phy.coded %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='dinophyta type2','athecate dino.')) 

#haptophtya
phy.coded = phy.coded %>% 
  mutate(taxa.clean = replace(taxa.clean,taxa=='haptophyta type1','cf chrysochromulina'))

#check
sort(unique(phy.coded$group.clean))
sort(unique(phy.coded$taxa.clean))



#merge counts from each sample by transect
phy.merged = ddply(phy.coded, 
                   .(sampleID,
                     LakeID,
                     Date,
                     Site,
                     Depth,
                     vol.settled.ml,
                     group.clean,
                     taxa.clean),
                   summarize,
                   n = sum(count.new))

#add lakedate, etc
phy.merged = phy.merged %>% 
  mutate(lakedate=paste(LakeID,Date,sep="_"))  #add lakedate ID column
#add season seasonID column
seasonID = read.csv('library/seasonID.csv',header=T)
phy.merged = join(phy.merged,seasonID,by='lakedate')
#move seasonID up
phy.merged = phy.merged %>% 
  relocate(seasonID, .after = Date) %>% 
  relocate(lakedate, .after = Date)
#fix date
phy.merged$Date = as.Date(phy.merged$Date)

#get list of all taxa
plist = phy.merged %>% mutate(phyID=paste(group.clean,taxa.clean,sep="_")) 
plist = as.data.frame(sort(unique(plist$phyID)))
#rename first col
colnames(plist)[1] = 'phyID'
plist = plist %>% separate(phyID, c("group", "taxa"), sep = "_", remove=F) %>% select(-1)
#save
write.csv(plist, "library/db.phyto.taxa.csv", row.names=F)
#coded trophic mode in excel
trophic = read.csv("library/trophic.modes.csv",header=T)
trophic = trophic %>% select(-1)
#rename taxa columns
phy.merged = phy.merged %>% rename(group=group.clean) %>% rename(taxa=taxa.clean)
#combine
phy.merged = join(phy.merged,trophic,by='taxa')



#save
write.csv(phy.merged, "library/db.phyto.csv", row.names=F)























