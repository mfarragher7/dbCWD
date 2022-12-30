# Clean data mining databases using mostly tidyverse  
# Created June 17 2020 by MJF
# Edited Aug. 22 2020 by KH

#Packages and libraries ####
#*Checks for packages ####
if(!require(plyr)){install.packages("plyr")}
if(!require(readr)){install.packages("readr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(tm)){install.packages("tm")}
if(!require(stringr)){install.packages("stringr")}
if(!require(ggplot2)){install.packages("ggplot2")}

#*Load libraries ####
library(plyr)
library(readr)
library(dplyr)
library(tidyverse)
library(tm)
library(stringr)
library(ggplot2)

#load data
#SINGLE PONDS ####
#*Read in metadata ####
singlemd10 = read_csv("RawData/OtherData/Metadata_DataMining_SinglePonds.csv")
#transpose
singlemd9 = singlemd10 %>% tibble::rownames_to_column() %>% pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>% column_to_rownames(var="name")
singlemd9
#first row as column names
names(singlemd9) = singlemd9 %>% slice(1) %>% unlist()
singlemd8 = singlemd9 %>% slice(-1)
#delete text from 'numeric' columns (Definitions/whatAuthorsCallIt)
singlemd8
singlemd8[2,5:6] = NA 
singlemd8
#convert everything to lowercase
singlemd7 = mutate_all(singlemd8,tolower) 
#fix column names
singlemd6 = singlemd7 %>% 
  set_names(~ str_to_lower(.) %>% 
              str_replace_all(" ","") %>%
              str_replace_all("/","_") %>% 
              str_replace_all("-","_") %>% 
              str_replace_all("and_or","_") %>% 
              str_replace_all("cdom_","cdom") %>% 
              str_replace_all("tss_mg","tss_mgpl") %>% 
              str_replace_all("min","min_") %>% 
              str_replace_all("mean","mean_") %>% 
              str_replace_all("max","max_") %>% 
              str_replace_all("othernotesanduniquepoints","notes")) %>% 
  add_row(ponduse='multiple') %>%  #add 'multiple' to ponduse
  add_row(landuse='multiple') %>% #add 'multiple' to landuse
  add_row(surfacewaterconnectivity='other') %>% #add 'other' to surfacewaterconnectivity
  add_row(trophic_status='hypertrophic') %>% #add trophic status
  add_row(trophic_status='dystrophic') %>% #add trophic status
  add_row(trophic_status='oligo-mesotrophic') %>% #add trophic status
  add_row(trophic_status='meso-eutrophic') %>%  #add trophic status
  mutate(hydrology=replace(hydrology,hydrology=='intermittment','intermittent')) # we spelled 'intermittent' wrong in the metadata, oops
singlemd6
#fix '( ) ?' symbols
names(singlemd6)[names(singlemd6)=="longitude(decimaldegree)"] = "longitude"
names(singlemd6)[names(singlemd6)=="latitude(decimaldegree)"] = "latitude"
names(singlemd6)[names(singlemd6)=="whatdotheauthorscallit?"] = "whatauthorscallit"
singlemd6

#*Read in data ####
singleponds10 = read_csv("RawData/CombinedDataframes/Combined_SinglePonds.csv")
singleponds10 = read_csv("RawData/CombinedDataframes/combined_singleponds_coord_fix.csv")
singleponds10

#delete extra column
singleponds9 = select(singleponds10, -c(X1)) 
#rename columns
singleponds8 = singleponds9 %>% 
  set_names(~ str_to_lower(.) %>% 
              str_replace_all(" ","") %>%
              str_replace_all("/","_") %>% 
              str_replace_all("-","_") %>% 
              str_replace_all("and_or","_") %>% 
              str_replace_all("cdom_","cdom") %>% 
              str_replace_all("min","min_") %>% 
              str_replace_all("mean","mean_") %>% 
              str_replace_all("trophicstatus","trophic_status") %>% 
              str_replace_all("max","max_") %>% 
              str_replace_all("othernotesanduniquepoints","notes"))
singleponds8
names(singleponds8)[names(singleponds8) == "longitude(decimaldegree)"] = "longitude"
names(singleponds8)[names(singleponds8) == "latitude(decimaldegree)"] = "latitude"
names(singleponds8)[names(singleponds8) == "whatdotheauthorscallit?"] = "whatauthorscallit"
#check that colnames match
names(singleponds8)
names(singlemd6)
#singlenames = c(names(singleponds),names(singlemd6))
#unique(singlenames)
#columns out of order between metadata and pond database. change singleponds column order to match metadata
#if the function relocate() doesn't work you need to update your dplyr package!!!
singleponds7 = singleponds8 %>% relocate(surfacewaterconnectivity, .after = managed)
#column names and positions all match now 
#set all values to lowercase
singleponds6 = singleponds7 %>% mutate_if(is.character, str_to_lower)


#*QAQC ####
#check structure
str(singleponds6)

#**Numeric columns ####
rapply(singleponds6,function(.)length(unique(.)))
#check for non-numeric values (i.e <, commas, text, etc)
#generate boxplots

#***Physical characteristics ####
#min depth
singleponds6 %>% count(min_depth_m, sort = TRUE) 
#delete column, not enough data, and min values of zero
singleponds5 = singleponds6 %>% select(-min_depth_m) 
#1736 NA, all 2m or less

#max depth
temp = singleponds5 %>% count(max_depth_m, sort = TRUE) 
# < 10 over 100m
#histogram 
ggplot(singleponds5,aes(max_depth_m)) +
  ggtitle('max depth - single') +
  geom_histogram(color='gray') 
#summary
summary(singleponds5$max_depth_m) #maximum 403m deep, mean 7.255m
#box plot
ggplot(singleponds5,aes(x="",y=max_depth_m)) + #need blank 'x' to add geom_jitter
  geom_boxplot(color='black',outlier.shape = NA) + #hide outliers duplicated by geom_jitter
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_depth_m,na.rm=TRUE)),color='red') + #3 std.dev 
  #geom_hline(aes(yintercept=-3*sd(max_depth_m,na.rm=TRUE)),color='red') + # negative std.dev
  ggtitle('max depth - single')

#mean depth
temp = singleponds5 %>% count(mean_depth_m, sort = TRUE)
#histogram
ggplot(singleponds5,aes(mean_depth_m))+
  ggtitle('mean depth - single')+
  geom_histogram(color='gray')
#summary
summary(singleponds5$mean_depth_m)
#box plot
ggplot(singleponds5,aes(x="",y=mean_depth_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_depth_m,na.rm=TRUE)),color='red') + 
  ggtitle('mean depth - single')

#min surface area
singleponds5 %>% count(min_surfacearea_m2, sort = TRUE) 
#delete min s.a.
singleponds4 = singleponds5 %>% select(-min_surfacearea_m2) 

#max s.a.
temp = singleponds4 %>% count(max_surfacearea_m2, sort = TRUE) 
#histogram
ggplot(singleponds4,aes(log(max_surfacearea_m2)))+
  ggtitle('max surface area - single')+
  geom_histogram(color='light gray') 
#summary
summary(singleponds4$max_surfacearea_m2)
#box plot
ggplot(singleponds4,aes(x="",y=log(max_surfacearea_m2))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(max_surfacearea_m2,na.rm=TRUE))),color='red') + 
  ggtitle('max surface area - single')

#mean s.a.
#fix mean surface area, coerce to numeric
temp = singleponds4 %>% count(mean_surfacearea_m2, sort = TRUE) 
#coerce
singleponds4$mean_surfacearea_m2 = as.numeric(singleponds4$mean_surfacearea_m2)
temp_check = singleponds4 %>% count(mean_surfacearea_m2, sort = TRUE)
#histogram
ggplot(singleponds4,aes(log(mean_surfacearea_m2)))+
  ggtitle('mean surface area - single')+
  geom_histogram(color='gray')
#summary
summary(singleponds4$mean_surfacearea_m2)
#box plot
ggplot(singleponds4,aes(x="",y=log(mean_surfacearea_m2))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(mean_surfacearea_m2,na.rm=TRUE))),color='red') + 
  ggtitle('mean surface area - single')

#volume
singleponds4 %>% count(volume_m3, sort = TRUE) 
#delete
singleponds3 = singleponds4 %>% select(-volume_m3) 

#***Temperature ####
#delete
singleponds2 = singleponds3 %>% select(-min_temp_c,-max_temp_c,-mean_temp_c) #delete columns

#***Macrophytes percent coverage ####
temp  = singleponds2 %>% count(macrophytes_percentcover, sort = TRUE) 
#histogram
ggplot(singleponds2,aes(macrophytes_percentcover))+
  ggtitle('macrophytes percent coverage - single')+
  stat_bin(binwidth=1,color='gray')
#highest counts at 0% and 100%
#summary
summary(singleponds2$macrophytes_percentcover)
#box plot
ggplot(singleponds2,aes(x="",y=macrophytes_percentcover)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  ggtitle('macrophytes_percentcover - single')

#***pH ####
temp = singleponds2 %>% count(ph, sort = TRUE) 
#remove non-numeric values
singleponds1 = singleponds2 %>% 
  mutate(ph=replace(ph,ph=='*'|grepl('mean',ph)|grepl('-',ph),NA))
#coerce
singleponds1$ph = as.numeric(singleponds1$ph) #plus 6 NA
temp_check = singleponds1 %>% count(ph, sort = TRUE) 
#hist
ggplot(singleponds1,aes(ph))+
  ggtitle('ph - single')+
  stat_bin(binwidth=0.1,color='gray')
#summary
summary(singleponds1$ph)
#box plot
ggplot(singleponds1,aes(x="",y=ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(ph,na.rm=TRUE)),color='red') + 
  ggtitle('ph - single')

#***Turbidity ####
temp = singleponds1 %>% count(turbidity_secchi_m, sort = TRUE) 
#remove turbidity reported as NTU or FNU
singleponds11 = singleponds1 %>% 
  mutate(turbidity_secchi_m=replace(turbidity_secchi_m,grepl('turbidity in ftu',notes) | grepl('turbidity in fnu',notes),NA))  # flag 'troph' in notes
temp_check = singleponds11 %>% count(turbidity_secchi_m, sort = TRUE) 
#hist
ggplot(singleponds11,aes(turbidity_secchi_m))+
  ggtitle('turbidity_secchi - single')+
  geom_histogram(color='gray')
#summary
summary(singleponds11$turbidity_secchi_m)
#boxplot
ggplot(singleponds11,aes(x="",y=turbidity_secchi_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(turbidity_secchi_m,na.rm=TRUE)),color='red') + 
  ggtitle('turbidity_secchi - single')

#***TSS ####
temp = singleponds11 %>% count(tss_mgpl, sort = TRUE)
#histogram
ggplot(singleponds11,aes(log(tss_mgpl)))+
  ggtitle('tss_mgpl- single')+
  geom_histogram(color='gray')
#summary
summary(singleponds11$tss_mgpl)
#boxplot
ggplot(singleponds11,aes(x="",y=log(tss_mgpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(tss_mgpl,na.rm=TRUE))),color='red') + 
  ggtitle('tss_mgpl - single')

#***Color ####
singleponds11 %>% count(color, sort = TRUE) 
singleponds11 %>% count(color_abs_440nm, sort = TRUE) 
singleponds12 = singleponds11 %>% select(-color,-color_abs_440nm) #delete

#***DOC/CDOM ####
singleponds13 = singleponds12 %>% select(-cdom) #delete
temp = singleponds13 %>% count(doc_mgpl, sort = TRUE) 
#replace non-numeric values
singleponds14 = singleponds13 %>% 
  mutate(doc_mgpl=replace(doc_mgpl,doc_mgpl=='ma',NA)) #replace letters with NA
#coerce
singleponds14$doc_mgpl = as.numeric(singleponds14$doc_mgpl) #plus ~35 NA
temp_check = singleponds14 %>% count(doc_mgpl, sort = TRUE) 
#hist
ggplot(singleponds14,aes(doc_mgpl))+
  ggtitle('doc - single')+
  stat_bin(binwidth=5,color='gray')
#summary
summary(singleponds14$doc_mgpl)
#boxplot
ggplot(singleponds14,aes(x="",y=doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('doc_mgpl - single')

#***Chlorophyll ####
temp = singleponds14 %>% count(chla_ugpl, sort = TRUE) 
#remove non-numeric values
singleponds15 = singleponds14 %>% 
  mutate(chla_ugpl=replace(chla_ugpl,chla_ugpl=='*'|chla_ugpl=='<5'|grepl('-',chla_ugpl),NA)) #replace with NA
#coerce
singleponds15$chla_ugpl = as.numeric(singleponds15$chla_ugpl) #plus 4 NA
temp_check = singleponds15 %>% count(chla_ugpl, sort = TRUE) 
#hist
ggplot(singleponds15,aes(log(chla_ugpl)))+
  ggtitle('chlorophyll - single')+
  geom_histogram(color='gray')
#summary
summary(singleponds15$chla_ugpl)
#boxplot
ggplot(singleponds15,aes(x="",y=log(chla_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(chla_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('chla_ugpl - single')

#***Nutrients ####
#TP
temp = singleponds15 %>% count(tp_ugpl, sort = TRUE) 
#remove non-numeric values
singleponds16 = singleponds15 %>% 
  mutate(tp_ugpl=replace(tp_ugpl,tp_ugpl=='*'|grepl('-',tp_ugpl),NA)) #replace with NA
#coerce
singleponds16$tp_ugpl = as.numeric(singleponds16$tp_ugpl) #plus 6 NA
temp_check = singleponds16 %>% count(tp_ugpl, sort = TRUE) 
#hist
ggplot(singleponds16,aes(log(tp_ugpl)))+
  ggtitle('tp - single')+
  geom_histogram(color='gray')
#summary
summary(singleponds16$tp_ugpl)
#boxplot
ggplot(singleponds16,aes(x="",y=log(tp_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(tp_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('tp_ugpl - single')

#TN
temp = singleponds16 %>% count(tn_ugpl, sort = TRUE) 
#hist
ggplot(singleponds16,aes(log(tn_ugpl)))+
  ggtitle('tn - single')+
  geom_histogram(color='gray')
#one ~100,000 value
#summary
summary(singleponds16$tn_ugpl)
#boxplot
ggplot(singleponds16,aes(x="",y=log(tn_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(tn_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('tn_ugpl - single')

#conductivity
temp = singleponds16 %>% count(cond_uspcm, sort = TRUE) 
#remove non-numeric values
singleponds17 = singleponds16 %>% 
  mutate(cond_uspcm=replace(cond_uspcm,cond_uspcm=='*'|grepl('-',cond_uspcm)|grepl('/',cond_uspcm)|grepl('y',cond_uspcm),NA)) #replace with NA
#coerce
singleponds17$cond_uspcm = as.numeric(singleponds17$cond_uspcm) #plus 17 NA
temp_check = singleponds17 %>% count(cond_uspcm, sort = TRUE)
#hist
ggplot(singleponds17,aes(log(as.numeric(cond_uspcm))))+
  ggtitle('conductivity - single')+
  geom_histogram(color='gray')
#5 ponds > 10,000
#summary
summary(singleponds17$cond_uspcm)
#boxplot
ggplot(singleponds17,aes(x="",y=log(cond_uspcm))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(cond_uspcm,na.rm=TRUE))),color='red') + 
  ggtitle('cond_uspcm - single')

#***Canopy cover percentage ####
singleponds17 %>% count(canopycover_percentofpond, sort = TRUE) 
singleponds18 = singleponds17 %>% select(-canopycover_percentofpond) #delete

#end numeric column QAQC
str(singleponds18)


#**Categorical columns ####
#number of unique values in each column
rapply(singleponds18,function(x)length(unique(x)))

#workflow: systematically check every categorical column for non-metadata values and changing to 'other' or 'multiple' or 'NA'
#generate column 'check_notes_for' and append with key words pertaining to each column
#save unique 'other' values outside of metadata, generate dataframe later
#replace non-metadata values with either 'other' or 'multiple'

#***Lake Name / What do the authors call the water body? ####
#save  pondnames and author term 
s_author_term = singleponds18 %>% count(whatauthorscallit, sort = TRUE) 
s_pond_names = singleponds18 %>% count(pondname, sort = TRUE) 

#***Human built / manipulation ####
#check unique values. both, manipulated, human built, etc
temp = singleponds18 %>% count(humanbuilt_manipulated, sort = TRUE) 
#change 'human built' and 'both' to "y"
#delete rows with 'biomanipulation' or 'dredged'
singleponds19 = singleponds18 %>%
  mutate(humanbuilt_manipulated=replace(humanbuilt_manipulated,humanbuilt_manipulated=="human built","y")) %>% #change to 'y'
  mutate(humanbuilt_manipulated=replace(humanbuilt_manipulated,humanbuilt_manipulated=="both","y")) %>% #change to 'y'
  mutate(humanbuilt_manipulated=replace(humanbuilt_manipulated,humanbuilt_manipulated=="manipulated","y")) %>% #change to 'y'
  filter(!grepl('dredged',humanbuilt_manipulated)) #delete rows with dredged (deletes other biomanipulation too)
#check
singleponds19 %>% count(humanbuilt_manipulated, sort = TRUE) 
#save summary
s_humanbuilt = singleponds19 %>% 
  count(humanbuilt_manipulated, sort = TRUE) %>% 
  mutate(humanbuilt_manipulated=replace_na(humanbuilt_manipulated,'NA')) %>% 
  rename(humanbuilt_manipulated_n = n)

#***Pond use ####
s_ponduse_start = singleponds19 %>% count(ponduse, sort = TRUE)
#save ponduse from metadata
s_ponduse_md = unique(singlemd6$ponduse)
#workflow:
#making minor fixes to 'synonyms'
#generate column 'check_notes_for' and append with key word 'pertaining'use' to each column
#change non-metadata values (i.e: ecotourism) to 'other' in 'ponduse' column
#change multiple values (i.e: forested/natural) to 'multiple' in ponduse column
#these 'other' and 'multiple' values wont be flagged in check_notes column, already knew what they were. 
#ignore warning 'number of items to replace is not a multiple of replacement length', that makes sense. 
singleponds20 = singleponds19 %>% 
  mutate(ponduse=replace(ponduse,ponduse=="agricultural*","farm")) %>% #change 'agricultural' to farm in original pond use column
  mutate(ponduse=replace(ponduse,ponduse=="reservoir","water storage")) %>%   #change reservoir to water storage in original pond use column
  mutate(check_notes_for=ifelse(grepl('use',notes),'pond or land use',NA)) %>% # new column, flag notes that contain 'use' to check  notes for ponduse AND OR landuse
  mutate(check_notes_for=replace(check_notes_for,grepl('other',ponduse) | grepl('multiple',ponduse),paste(check_notes_for,'ponduse other or multiple',sep=', ',collapse=NULL))) %>%   # check for 'other' or 'multiple', if so, paste in check_notes column
  mutate(ponduse=replace(ponduse,grepl('/',ponduse) | grepl(',',ponduse),'multiple')) %>%  # change values >1 to 'multiple'
  mutate(ponduse=replace(ponduse,!ponduse %in% s_ponduse_md,'other'))  # if ponduse value != metadata category, change to 'other'
#check
singleponds20 %>% count(ponduse, sort = TRUE)
#save dataframe of 'other' ponduse counts
s_ponduse_other = s_ponduse_start %>% 
  mutate(ponduse=replace(ponduse,ponduse=="agricultural*","farm")) %>% #change 'agricultural' to farm 
  mutate(ponduse=replace(ponduse,grepl('irrigation',ponduse),"irrigation")) %>% #save irrigation
  mutate(ponduse=replace(ponduse,ponduse=="reservoir","water storage")) %>% #change reservoir to water storage 
  mutate(ponduse=replace(ponduse,ponduse=="other: nutrient retention","nutrient retention")) %>% #save nutrient retention
  mutate(ponduse=replace(ponduse,grepl('ecotourism',ponduse),"ecotourism")) %>% #save ecotourism
  mutate(ponduse=replace(ponduse,grepl('/',ponduse),NA)) %>% # change values >1 to 'multiple'
  mutate(ponduse=replace(ponduse,ponduse %in% s_ponduse_md,NA)) %>%  # if ponduse value = metadata category, change to NA
  drop_na(ponduse) %>% #delete NAs
  rename(ponduse_other = ponduse) %>% #rename columns
  rename(ponduse_other_n = n)
#get final 'other' and 'multiple' counts after notes check
#save summary
s_ponduse = singleponds20 %>% 
  count(ponduse, sort = TRUE) %>% 
  mutate(ponduse=replace_na(ponduse,'NA')) %>% 
  rename(ponduse_n = n)

#***Land use ####
#89 unique values
s_landuse_start = singleponds20 %>% count(landuse, sort = TRUE)
#landuse from metadata 
s_landuse_md = unique(singlemd6$landuse)
#workflow:
#fix typos and synonyms first (agriculture = agricultural, etc)
#append check_notes column for other/multiple values
#change multiples (i.e:forested/agricultural) to multiple
#change non-metadata single values to 'other'
singleponds21 = singleponds20 %>% 
  mutate(landuse=replace(landuse,landuse=='agriculture','agricultural')) %>%  # fix typo
  mutate(landuse=replace(landuse,landuse=='mixed','multiple')) %>%  # fix typo
  mutate(check_notes_for=replace(check_notes_for,grepl('other',landuse) | grepl('multiple',landuse),paste(check_notes_for,'landuse other or multiple',sep=', ',collapse=NULL))) %>%   # check for 'other' or 'multiple', if so, paste in check_notes column
  mutate(landuse=replace(landuse,grepl('/',landuse),'multiple')) %>%   # change multiples to 'multiple'
  mutate(landuse=replace(landuse,!landuse %in% s_landuse_md,'other'))  # if landnduse value != metadata category, change to 'other'
#check  
singleponds21 %>% count(landuse, sort = TRUE)
#save 'others'
s_landuse_other = s_landuse_start %>% 
  mutate(landuse=replace(landuse,landuse=='agriculture','agricultural')) %>%  # fix typo
  mutate(landuse=replace(landuse,landuse=='mixed','multiple')) %>%  # fix typo
  mutate(landuse=replace(landuse,grepl('/',landuse),NA)) %>%   # change multiples to NA
  mutate(landuse=replace(landuse,landuse %in% s_landuse_md,NA)) %>%  # if landuse value = metadata category, change to NA
  drop_na(landuse) %>%  #delete NAs
  rename(landuse_other = landuse) %>% #rename columns
  rename(landuse_other_n = n)
#get final 'other' and 'multiple' counts after notes check
#save summary
s_landuse = singleponds21 %>% 
  count(landuse, sort = TRUE) %>% 
  mutate(landuse=replace_na(landuse,'NA')) %>% 
  rename(landuse_n = n)

#***Managed ####
#check count of unique answers. only y, n, NA. easy!
singleponds21 %>% count(managed, sort = TRUE)
#flag use of word 'manage' in notes. 
singleponds22 = singleponds21 %>% 
  mutate(check_notes_for=replace(check_notes_for,grepl('manage',notes),paste(check_notes_for,'manage',sep=', ',collapse=NULL))) # flag notes that contain 'manage' 
#save summary
s_managed = singleponds22 %>% 
  count(managed, sort = TRUE) %>% 
  mutate(managed=replace_na(managed,'NA')) %>% 
  rename(managed_n = n)

#***Surface water connectivity ####
#check unique values
singleponds22 %>% count(surfacewaterconnectivity, sort = TRUE)
#metadata categories
s_surfacewaterconnectivity_md = unique(singlemd6$surfacewaterconnectivity)
#workflow:
#fix typos/synonyms
#append check_notes column for other/multiple values
#change non-metadata values to 'NA' 
singleponds23 = singleponds22 %>% 
  mutate(surfacewaterconnectivity=replace(surfacewaterconnectivity,grepl('urban',surfacewaterconnectivity),'inline')) %>%  # changes 'inline (urban)' to just 'inline'
  mutate(check_notes_for=replace(check_notes_for,grepl('connectivity',notes),paste(check_notes_for,'connectivity',sep=', ',collapse=NULL))) %>%    # flag 'connectivity' in notes
  mutate(check_notes_for=replace(check_notes_for,grepl('multiple',surfacewaterconnectivity),paste(check_notes_for,'connectivity - multiple',sep=', ',collapse=NULL))) %>%   # check for 'multiple', check notes
  mutate(surfacewaterconnectivity=replace(surfacewaterconnectivity,!surfacewaterconnectivity %in% s_surfacewaterconnectivity_md,NA))  # if surfacewaterconnectivity value != metadata category, change to 'other'
#fits metadata now
singleponds23 %>% count(surfacewaterconnectivity, sort = TRUE)
#save summary
s_surfacewaterconnectivity = singleponds23 %>% 
  count(surfacewaterconnectivity, sort = TRUE) %>% 
  mutate(surfacewaterconnectivity=replace_na(surfacewaterconnectivity,'NA')) %>% 
  rename(surfacewaterconnectivity_n = n)

#***Hydrology ####
#count unique values
singleponds23 %>% count(hydrology, sort = TRUE)
#metadata categories
unique(singlemd6$hydrology)
#workflow:
#fix typos
#append check_notes for 'hydro'
#change 'seasonal/intermittent' to 'multiple'
singleponds24 = singleponds23 %>% 
  mutate(hydrology=replace(hydrology,grepl('intermittment',hydrology),'intermittent')) %>%   #fix spelling
  mutate(hydrology=replace(hydrology,grepl('mulitple',hydrology),'multiple')) %>%   #fix spelling
  mutate(check_notes_for=replace(check_notes_for,grepl('hydro',notes),paste(check_notes_for,'hydro',sep=', ',collapse=NULL))) %>%    # flag 'hydro' in notes
  mutate(check_notes_for=replace(check_notes_for,grepl('multiple',hydrology),paste(check_notes_for,'hydrology - multiple',sep=', ',collapse=NULL))) %>%   # check for 'multiple', check notes
  mutate(hydrology=replace(hydrology,grepl('seasonal/intermittent',hydrology),'multiple'))   #replace one 'multiple'
#cleaned up, fits metadata.   
singleponds24 %>% count(hydrology, sort = TRUE)
#check_notes check
singleponds24 %>% count(check_notes_for, sort = TRUE)
#save summary
s_hydrology = singleponds24 %>% 
  count(hydrology, sort = TRUE) %>% 
  mutate(hydrology=replace_na(hydrology,'NA')) %>% 
  rename(hydrology_n = n)

#***Trophic Status ####
# check count of unique values
singleponds24 %>% count(trophic_status, sort = TRUE)   # hypertrophic, in between ie: meso-oligo
#check metadata categories
unique(singlemd6$trophic_status)
#workflow:
#fix typos and asterisks
#append check_notes
singleponds25 = singleponds24 %>% 
  mutate(trophic_status=replace(trophic_status,trophic_status=='eutrophoc','eutrophic')) %>%  #fix spelling
  mutate(trophic_status=replace(trophic_status,trophic_status=='eutrophic*','eutrophic')) %>%  #replace asterisks with no asterisks
  mutate(check_notes_for=replace(check_notes_for,grepl('troph',notes),paste(check_notes_for,'troph',sep=', ',collapse=NULL)))  # flag 'troph' in notes
#check
singleponds25 %>% count(trophic_status, sort = TRUE)
#save summary
s_trophic_status = singleponds25 %>% 
  count(trophic_status, sort = TRUE) %>% 
  mutate(trophic_status=replace_na(trophic_status,'NA')) %>% 
  rename(trophic_status_n = n)

#***Fish presence ####
singleponds25 %>% count(fishpresence, sort = TRUE) #nothing to change
#append check_notes for fish
singleponds26 = singleponds25 %>% 
  mutate(check_notes_for=replace(check_notes_for,grepl('fish',notes),paste(check_notes_for,'fish',sep=', ',collapse=NULL)))  # flag 'fish' in notes
#save summary
s_fishpresence = singleponds25 %>% 
  count(fishpresence, sort = TRUE) %>% 
  mutate(fishpresence=replace_na(fishpresence,'NA')) %>% 
  rename(fishpresence_n = n)

#***Macrophyte presence ####
singleponds25 %>% count(macrophytespresence, sort = TRUE)
#change yes to y
#append notes for macrophytes
singleponds26 = singleponds25 %>% 
  mutate(macrophytespresence=replace(macrophytespresence,macrophytespresence=='yes','y')) %>% #change to y
  mutate(check_notes_for=replace(check_notes_for,grepl('macrophyte',notes) | grepl('plant',notes),paste(check_notes_for,'macrophytes or plants',sep=', ',collapse=NULL)))  # flag 'plant or macrophytes' in notes
#save summary
s_macrophytespresence = singleponds26 %>% 
  count(macrophytespresence, sort = TRUE) %>% 
  mutate(macrophytespresence=replace_na(macrophytespresence,'NA')) %>% 
  rename(macrophytespresence_n = n)


#*Finish single ponds ####
#append check_notes for remaining numeric values
#check check notes
singleponds26 %>% count(check_notes_for, sort = TRUE)
unique(singleponds26$check_notes_for)
singleponds27 = singleponds26 %>% 
  mutate(check_notes_for=replace(check_notes_for,grepl('depth',notes),paste(check_notes_for,'depth',sep=', ',collapse=NULL))) %>%   # check notes for 'depth'
  mutate(check_notes_for=replace(check_notes_for,grepl('surface area',notes),paste(check_notes_for,'surface area',sep=', ',collapse=NULL))) %>%   # check notes for 'surface area'
  mutate(check_notes_for=replace(check_notes_for,grepl('volume',notes),paste(check_notes_for,'volume',sep=', ',collapse=NULL))) %>%   # check notes for 'volume'
  mutate(check_notes_for=replace(check_notes_for,grepl('ph',notes),paste(check_notes_for,'ph',sep=', ',collapse=NULL))) %>%   # check notes for 'ph'
  mutate(check_notes_for=replace(check_notes_for,grepl('turb',notes),paste(check_notes_for,'turb',sep=', ',collapse=NULL))) %>%   # check notes for 'turb'
  mutate(check_notes_for=replace(check_notes_for,grepl('tss',notes),paste(check_notes_for,'tss',sep=', ',collapse=NULL))) %>%   # check notes for 'tss'
  mutate(check_notes_for=replace(check_notes_for,grepl('secchi',notes),paste(check_notes_for,'secchi',sep=', ',collapse=NULL))) %>%   # check notes for 'secchi'
  mutate(check_notes_for=replace(check_notes_for,grepl('doc',notes),paste(check_notes_for,'doc',sep=', ',collapse=NULL))) %>%   # check notes for 'doc'
  mutate(check_notes_for=replace(check_notes_for,grepl('chloro',notes),paste(check_notes_for,'chloro',sep=', ',collapse=NULL))) %>%   # check notes for 'chloro'
  mutate(check_notes_for=replace(check_notes_for,grepl('tp',notes) | grepl('phosphorus',notes),paste(check_notes_for,'tp/phosphorus',sep=', ',collapse=NULL))) %>%   # check notes for 'phosphorus'
  mutate(check_notes_for=replace(check_notes_for,grepl('tn',notes) | grepl('nitrogen',notes),paste(check_notes_for,'tn/nitrogen',sep=', ',collapse=NULL))) %>%   # check notes for 'nitrogen'
  mutate(check_notes_for=replace(check_notes_for,grepl('conduct',notes),paste(check_notes_for,'conductivity',sep=', ',collapse=NULL))) %>%   # check notes for 'conductivity'
  mutate(check_notes_for=str_remove(check_notes_for,'NA, ')) #get rid of weird "NA"
#check again
unique(singleponds27$check_notes_for)

#**Subset data ####
str(singleponds27)
#Save - 175 ponds with no size data
# no_size_single = singleponds27 %>% filter(is.na(min_surfacearea_m2) & is.na(mean_surfacearea_m2) & is.na(max_surfacearea_m2) & 
#                                           is.na(min_depth_m) & is.na(max_depth_m) & is.na(mean_depth_m))
#save lakes >10m depth, and >1km2 surface area
# singleponds_large = singleponds27 %>% 
#   filter(mean_depth_m > 10 | max_depth_m > 10 | mean_surfacearea_m2 > 1000000 | max_surfacearea_m2 > 1000000)
#Subset out large water bodies


#remove rows where mean or max depth < 10, AND mean or max surface area < 1 km^2 AND is called a lake (or lakes) by authors

#singleponds_test = singleponds27 %>% filter((mean_depth_m < 10 | max_depth_m < 10 | (is.na(max_depth_m) & is.na(mean_depth_m))))


#singleponds_test2 = singleponds_test %>% filter((mean_surfacearea_m2 < 1000000 | max_surfacearea_m2 < 1000000 | (is.na(max_surfacearea_m2) & is.na(mean_surfacearea_m2))))


# singleponds_test3 = singleponds27 %>% 
#   filter((mean_depth_m < 10 | max_depth_m < 10 | (is.na(max_depth_m) & is.na(mean_depth_m))) &
#            (mean_surfacearea_m2 < 1000000 | max_surfacearea_m2 < 1000000 | (is.na(max_surfacearea_m2) & is.na(mean_surfacearea_m2))))


# singleponds_test4 = singleponds27 %>% 
#   mutate(depth=ifelse((is.na(max_depth_m) & is.na(mean_depth_m)),NA,ifelse(mean_depth_m < 10 | max_depth_m < 10,'shallow','deep'))) %>% 
#   mutate(size=ifelse((is.na(max_surfacearea_m2) & is.na(mean_surfacearea_m2)),NA,ifelse(mean_surfacearea_m2 < 1000000 | max_surfacearea_m2 < 1000000,'small','large')))

singleponds_final <- singleponds27 %>% filter((mean_depth_m < 10 | max_depth_m < 10 | is.na(max_depth_m) == TRUE | is.na(mean_depth_m) == TRUE)
& (mean_surfacearea_m2 < 10000000 | max_surfacearea_m2 <10000000 |is.na(mean_surfacearea_m2==TRUE) | is.na(max_surfacearea_m2==TRUE)))



#**Numeric summary table ####
#save numeric columns
num = c("max_depth_m","mean_depth_m","max_surfacearea_m2","mean_surfacearea_m2","macrophytes_percentcover",
        "ph","turbidity_secchi_m","tss_mgpl","doc_mgpl","chla_ugpl","tp_ugpl","tn_ugpl","cond_uspcm")
#counts
temp1 = singleponds_final %>% 
  select(num) %>% 
  summarise_all(funs(sum(!is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="n") 
#min
temp2 = singleponds_final %>% 
  select(num) %>% 
  summarize_all(min,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="min") 
#max
temp3 = singleponds_final %>% 
  select(num) %>% 
  summarize_all(max,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="max") 
#mean
temp4 = singleponds_final %>% 
  select(num) %>% 
  summarize_all(mean,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="mean") 
#sd
temp5 = singleponds_final %>% 
  select(num) %>% 
  summarize_all(sd,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="sd") 
#merge
singleponds_summary_numeric = merge(temp1,temp2,by="variable")
singleponds_summary_numeric = merge(singleponds_summary_numeric,temp3,by="variable")
singleponds_summary_numeric = merge(singleponds_summary_numeric,temp4,by="variable")
singleponds_summary_numeric = merge(singleponds_summary_numeric,temp5,by="variable")
#fix scientific notation
singleponds_summary_numeric = format(singleponds27_summary_numeric, scientific=F)

#**Categorical summary table ####
#click on column heads to see total counts of each value, including NA's which are actually typed out as 'NA'
singleponds_summary_categorical = rbind.fill(s_author_term,s_pond_names,s_managed,s_humanbuilt,s_ponduse,s_landuse,
                                             s_surfacewaterconnectivity,s_trophic_status,s_hydrology,s_fishpresence,s_macrophytespresence)

#**Save 'others' list ####
s_ponduse_other
s_landuse_other
#kind of worked
singleponds_other=rbind.fill(s_ponduse_other,s_landuse_other)

#**Save CSVs ####
#write.csv(singleponds_final, "Data_cleaned/Singleponds_clean.csv")
#write.csv(singleponds_summary_categorical, "Data_cleaned/Singleponds_summary_categorical.csv")
#write.csv(singleponds_summary_numeric, "Data_cleaned/Singleponds_summary_numeric.csv")
#write.csv(no_size_single, "Data_cleaned/Singleponds_no_size_data.csv")
#write.csv(singleponds_large, "Data_cleaned/SinglePonds_large.csv")

#END singleponds


#MULTIPLE PONDS #############################################################################################################
#*Read in metadata ####
multimd = read_csv("RawData/OtherData/Metadata_DataMining_MultiplePonds.csv")
#transpose
multimd1 = multimd %>% tibble::rownames_to_column() %>% pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>% column_to_rownames(var="name")
#first row as column names
names(multimd2) = multimd1 %>% slice(1) %>% unlist()
#two identical columns "Standard deviation_m" renamed base r 
colnames(multimd2)[23] = "Standard deviation depth_m" 
colnames(multimd2)[28] = "Standard deviation surface area_m"
# delete first row
multimd3 = multimd2 %>% slice(-1)
#delete text from 'numeric' columns (Definitions/whatAuthorsCallIt)
multimd3[2,5:6] = NA 
#convert everything to lowercase
multimd4 = mutate_all(multimd3,tolower) 
#fix colnames
multimd5 = multimd4 %>% set_names(~ str_to_lower(.) %>% 
                                  str_replace_all(" ","") %>%
                                  str_replace_all("/","_") %>% 
                                  str_replace_all("and_or","_") %>% 
                                  str_replace_all("standarddeviation","stdev_") %>%
                                  str_replace_all("standarddev","stdev_") %>%
                                  str_replace_all("sdtss","stdev_tss") %>%
                                  str_replace_all("tss_mgl","tss_mgpl") %>%
                                  str_replace_all("sdturb","stdev_turb") %>% 
                                  str_replace_all("turb_ntu","turbidity_ntu") %>% 
                                  str_replace_all("surfacearea_m","surfacearea_m2") %>% 
                                  str_replace_all("min","min_") %>%
                                  str_replace_all("min__","min_") %>%    
                                  str_replace_all("max","max_") %>%
                                  str_replace_all("median","median_") %>%
                                  str_replace_all("mean","mean_")%>% 
                                  str_replace_all("othernotesanduniquepoints","notes")) %>% 
  add_row(humanbuilt_manipulated='both') %>%#add 'both' to humanbuilt/manipulated
  add_row(fishpresence='both') %>%#add 'both' to fishpresence
  add_row(ponduse='multiple') %>%  #add 'multiple' to ponduse
  add_row(landuse='multiple') %>% #add 'multiple' to landuse
  add_row(surfacewaterconnectivity='other') %>%  #add 'other' to surfacewaterconnectivity
  mutate(hydrology=replace(hydrology,hydrology=='intermittment','intermittent')) # we spelled 'intermittent' wrong in the metadata, oops
#fix other colnames 
names(multimd5)[names(multimd5) == "longitude(decimaldegree)"] = "longitude"
names(multimd5)[names(multimd5) == "latitude(decimaldegree)"] = "latitude"
names(multimd5)[names(multimd5) == "whatdotheauthorscallit?"] = "whatauthorscallit"

#*Read in data ####
multiponds1 = read_csv("RawData/CombinedDataframes/Combined_MultiplePonds.csv")
#delete extra column
multiponds2 = select(multiponds1, -c(X1)) 
#fix colnames
multiponds3 = multiponds2 %>% set_names(~ str_to_lower(.) %>% 
                                        str_replace_all(" ","") %>%
                                        str_replace_all("/","_") %>% 
                                        str_replace_all("and_or","_") %>% 
                                        str_replace_all("standarddeviation","stdev_") %>%
                                        str_replace_all("standarddev","stdev_") %>%
                                        str_replace_all("sdtss","stdev_tss") %>%
                                        str_replace_all("tss_mgl","tss_mgpl") %>%
                                        str_replace_all("sdturb","stdev_turb") %>% 
                                        str_replace_all("turb_ntu","turbidity_ntu") %>% 
                                        str_replace_all("min","min_") %>%
                                        str_replace_all("min__","min_") %>%    
                                        str_replace_all("max","max_") %>%
                                        str_replace_all("median","median_") %>%
                                        str_replace_all("mean","mean_")%>% 
                                        str_replace_all("othernotesanduniquepoints","notes"))
#fix other colnames base r, couldn't figure out how to deal with ' ( ) ?  symbols
names(multiponds3)[names(multiponds3) == "longitude(decimaldegree)"] = "longitude"
names(multiponds3)[names(multiponds3) == "latitude(decimaldegree)"] = "latitude"
names(multiponds3)[names(multiponds3) == "whatdotheauthorscallit?"] = "whatauthorscallit"
#check that colnames match
names(multiponds3)
names(multimd5)
#multinames = c(names(multiponds3),names(multimd5))
#unique(multinames)
#sort(unique(multinames))
#columns out of order between metadata and data. 
# move fish presence from column 64 to 78. 
multiponds4 = multiponds3 %>% relocate(fishpresence, .after = stdev_tss_mgpl)
#they match now.
# set all values to lowercase
multiponds5 = multiponds4 %>% mutate_if(is.character, str_to_lower)


#*QAQC ####
#**Numeric columns ####
rapply(multiponds5,function(.)length(unique(.)))
#check for non-numeric values (i.e <, commas, text, etc)
#generate boxplots

#***Number of water bodies ####
#two values in 'n' non numeric... 21.2 million, and 'variable'
multi_n_check = multiponds5 %>% select(n,notes)
unique(multiponds5$n)
# fix non-numeric values, add 'check_notes_for' column to add notes to check for
multiponds6 = multiponds5 %>% 
  mutate(n=replace(n,n=="21.2 million",21200000)) %>% #change to numeric 
  mutate(n=replace(n,n=="8.4 million polygons",8400000)) %>% #change to numeric 
  mutate(n=replace(n,n=="48 (divided 3 grous)",48)) %>% #change to numeric
  mutate(n=replace(n,n=="49 (divided 3 grous)",49)) %>% #change to numeric
  mutate(n=replace(n,n=="50 (divided 3 grous)",50)) %>% #change to numeric 
  mutate(n=replace(n,n=="variable",103)) %>% #change 'variable' to 103, the maximum number for variable n for different metrics 
  mutate(check_notes_for=ifelse(n=="200*",'contact author for size info',NA)) %>% # new column, flag "200*" for contact author (from notes) 
  mutate(n=replace(n,n=="200*",200)) %>%  #change to numeric 
  mutate(n=replace(n,n=="83*",83))  #change to numeric, don't need to flag
#coerce
multiponds6$n = as.numeric(multiponds6$n)
#check
temp_check = plyr::count(multiponds6$n)
str(multiponds6$n)

#***Depth ####
#min depth
temp = plyr::count(multiponds6$min_depth_m) #need to use plyr::count instead of dplyr, couldn't work out errors
#summary
summary(multiponds6$min_depth_m)
#box plot
ggplot(multiponds6,aes(x="",y=min_depth_m)) + #need blank 'x' to add geom_jitter
  geom_boxplot(color='black',outlier.shape = NA) + #hide outliers duplicated by geom_jitter
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_depth_m,na.rm=TRUE)),color='red') + #3 std.dev 
  ggtitle('min_depth_m - multi')

#max depth
temp = plyr::count(multiponds6$max_depth_m) 
#histogram 
ggplot(multiponds6,aes(max_depth_m)) +
  ggtitle('max depth - multi') +
  geom_histogram(color='gray') 
#summary
summary(multiponds6$max_depth_m)
#box plot
ggplot(multiponds6,aes(x="",y=max_depth_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_depth_m,na.rm=TRUE)),color='red') + 
  ggtitle('max depth - multi')

#mean depth
temp = plyr::count(multiponds6$mean_depth_m) 
#coerce 
multiponds6$mean_depth_m = as.numeric(multiponds6$mean_depth_m)
#histogram
ggplot(multiponds6,aes(mean_depth_m))+
  ggtitle('mean depth - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds6$mean_depth_m)
#box plot
ggplot(multiponds6,aes(x="",y=mean_depth_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_depth_m,na.rm=TRUE)),color='red') + 
  ggtitle('mean depth - multi')

#median depth
temp = plyr::count(multiponds6$median_depth_m) 
#histogram
ggplot(multiponds6,aes(median_depth_m))+
  ggtitle('median depth - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds6$median_depth_m)
#box plot
#regenerate plot from above
ggplot(multiponds6,aes(x="",y=median_depth_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(median_depth_m,na.rm=TRUE)),color='red') + 
  ggtitle('median depth - multi')

#sd depth
temp = plyr::count(multiponds6$stdev_depth_m) 
#coerce
multiponds6$stdev_depth_m = as.numeric(multiponds6$stdev_depth_m)
#histogram
ggplot(multiponds6,aes(stdev_depth_m))+
  ggtitle('median depth - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds6$stdev_depth_m)
#box plot
ggplot(multiponds6,aes(x="",y=stdev_depth_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(stdev_depth_m,na.rm=TRUE)),color='red') + 
  ggtitle('sd depth - multi')

#***Surface Area ####
#min surface area
temp = plyr::count(multiponds6$min_surfacearea_m2) 
#coerce
multiponds6$min_surfacearea_m2 = as.numeric(multiponds6$min_surfacearea_m2)
#histogram
ggplot(multiponds6,aes(log(min_surfacearea_m2)))+
  ggtitle('min surface area - multi')+
  geom_histogram(color='light gray') 
#summary
summary(multiponds6$min_surfacearea_m2)
#box plot
ggplot(multiponds6,aes(x="",y=log(min_surfacearea_m2))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(min_surfacearea_m2,na.rm=TRUE))),color='red') + 
  ggtitle('min surface area - multi')

#max s.a.
temp = plyr::count(multiponds6$max_surfacearea_m2) 
#coerce
multiponds6$max_surfacearea_m2 = as.numeric(multiponds6$max_surfacearea_m2)
#histogram
ggplot(multiponds6,aes(log(max_surfacearea_m2)))+
  ggtitle('max surface area - multi')+
  geom_histogram(color='light gray') 
#summary
summary(multiponds6$max_surfacearea_m2)
#box plot
ggplot(multiponds6,aes(x="",y=log(max_surfacearea_m2))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(max_surfacearea_m2,na.rm=TRUE))),color='red') + 
  ggtitle('max surface area - multi')

#mean s.a.
temp = plyr::count(multiponds6$mean_surfacearea_m2) 
#histogram
ggplot(multiponds6,aes(log(mean_surfacearea_m2)))+
  ggtitle('mean surface area - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds6$mean_surfacearea_m2)
#box plot
ggplot(multiponds6,aes(x="",y=log(mean_surfacearea_m2))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(mean_surfacearea_m2,na.rm=TRUE))),color='red') + 
  ggtitle('mean surface area - multi')

#median s.a.
temp = plyr::count(multiponds6$median_surfacearea_m2) 
#histogram
ggplot(multiponds6,aes(log(median_surfacearea_m2)))+
  ggtitle('mean surface area - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds6$median_surfacearea_m2)
#box plot
ggplot(multiponds6,aes(x="",y=log(median_surfacearea_m2))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(median_surfacearea_m2,na.rm=TRUE))),color='red') + 
  ggtitle('median surface area - multi')

#sd s.a.
temp = plyr::count(multiponds6$stdev_surfacearea_m2) 
#coerce
multiponds6$stdev_surfacearea_m2 = as.numeric(multiponds6$stdev_surfacearea_m2)
#histogram
ggplot(multiponds6,aes(log(stdev_surfacearea_m2)))+
  ggtitle('mean surface area - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds6$stdev_surfacearea_m2)
#box plot
ggplot(multiponds6,aes(x="",y=log(stdev_surfacearea_m2))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(stdev_surfacearea_m2,na.rm=TRUE))),color='red') + 
  ggtitle('sd surface area - multi')

#***Temperature ####
#delete
multiponds7 = multiponds6 %>% select(-min_temp_c,-max_temp_c,-mean_temp_c,-median_temp_c,-stdev_temp_c) #delete columns

#***DOC ####
#not a lot of data... delete?
#min doc
temp = plyr::count(multiponds7$min_doc_mgpl) 
#summary
summary(multiponds7$min_doc_mgpl)
#boxplot
ggplot(multiponds7,aes(x="",y=min_doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('min doc - multi')

#max doc
temp = plyr::count(multiponds7$max_doc_mgpl) 
#summary
summary(multiponds7$max_doc_mgpl)
#boxplot
ggplot(multiponds7,aes(x="",y=max_doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('max doc - multi')

#mean doc
temp = plyr::count(multiponds7$mean_doc_mgpl) 
#summary
summary(multiponds7$mean_doc_mgpl)
#boxplot
ggplot(multiponds7,aes(x="",y=mean_doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('mean doc - multi')

#median doc
temp = plyr::count(multiponds7$median_doc_mgpl) 
#coerce
multiponds7$median_doc_mgpl = as.numeric(multiponds7$median_doc_mgpl)
#summary
summary(multiponds7$median_doc_mgpl)
#boxplot
ggplot(multiponds7,aes(x="",y=median_doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(median_doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('median doc - multi')

#sd doc
temp = plyr::count(multiponds7$stdev_doc_mgpl) 
#summary
summary(multiponds7$stdev_doc_mgpl)
#boxplot
ggplot(multiponds7,aes(x="",y=stdev_doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(stdev_doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('sd doc - multi')

#***Chlorophyll ####
#min
temp = plyr::count(multiponds7$min_chla_ugpl) 
#summary
summary(multiponds7$min_chla_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(min_chla_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(min_chla_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('min_chla - multi')

#max
temp = plyr::count(multiponds7$max_chla_ugpl) 
#summary
summary(multiponds7$max_chla_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(max_chla_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(max_chla_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('max chla - multi')

#mean
temp = plyr::count(multiponds7$mean_chla_ugpl) 
#summary
summary(multiponds7$mean_chla_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(mean_chla_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(mean_chla_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('mean chla - multi')

#median
temp = plyr::count(multiponds7$median_chla_ugpl) 
#summary
summary(multiponds7$median_chla_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(median_chla_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(median_chla_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('median chla - multi')

#sd
temp = plyr::count(multiponds7$stdev_chla_ugpl) 
#summary
summary(multiponds7$stdev_chla_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(stdev_chla_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(stdev_chla_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('sd chla - multi')

#***Nutrients ####
#TP min
temp = plyr::count(multiponds7$min_tp_ugpl) 
#coerce
multiponds7$min_tp_ugpl = as.numeric(multiponds7$min_tp_ugpl)
#summary
summary(multiponds7$min_tp_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(min_tp_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(min_tp_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('min_tp_ugpl - multi')

#max
temp = plyr::count(multiponds7$max_tp_ugpl) 
#coerce
multiponds7$max_tp_ugpl = as.numeric(multiponds7$max_tp_ugpl)
#summary
summary(multiponds7$min_tp_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(min_tp_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(min_tp_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('min_tp_ugpl - multi')

#mean
temp = plyr::count(multiponds7$mean_tp_ugpl) 
#summary
summary(multiponds7$mean_tp_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(mean_tp_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(mean_tp_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('mean_tp_ugpl - multi')

#median
temp = plyr::count(multiponds7$median_tp_ugpl) 
#coerce
multiponds7$median_tp_ugpl = as.numeric(multiponds7$median_tp_ugpl)
#summary
summary(multiponds7$median_tp_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(median_tp_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(median_tp_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('median_tp_ugpl - multi')

#sd
temp = plyr::count(multiponds7$stdev_tp_ugpl) 
#summary
summary(multiponds7$stdev_tp_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(stdev_tp_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(stdev_tp_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('stdev_tp_ugpl - multi')

#TN
#min
temp = plyr::count(multiponds7$min_tn_ugpl) 
#coerce
multiponds7$min_tn_ugpl = as.numeric(multiponds7$min_tn_ugpl)
#summary
summary(multiponds7$min_tn_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(min_tn_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(min_tn_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('min_tn_ugpl - multi')

#max
temp = plyr::count(multiponds7$max_tn_ugpl) 
#summary
summary(multiponds7$min_tn_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(min_tn_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(min_tn_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('min_tn_ugpl - multi')

#mean
temp = plyr::count(multiponds7$mean_tn_ugpl) 
#summary
summary(multiponds7$mean_tn_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(mean_tn_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(mean_tn_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('mean_tn_ugpl - multi')

#median
temp = plyr::count(multiponds7$median_tn_ugpl) 
#coerce
multiponds7$median_tn_ugpl = as.numeric(multiponds7$median_tn_ugpl)
#summary
summary(multiponds7$median_tn_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(median_tn_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(median_tn_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('median_tn_ugpl - multi')

#sd
temp = plyr::count(multiponds7$stdev_tn_ugpl) 
#summary
summary(multiponds7$stdev_tn_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=log(stdev_tn_ugpl))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(stdev_tn_ugpl,na.rm=TRUE))),color='red') + 
  ggtitle('stdev_tn_ugpl - multi')

#***Conductivity ####
#min
temp = plyr::count(multiponds7$min_cond_uspcm) 
#summary
summary(multiponds7$min_cond_uspcm)
#boxplot
ggplot(multiponds7,aes(x="",y=log(min_cond_uspcm))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(min_cond_uspcm,na.rm=TRUE))),color='red') + 
  ggtitle('min cond - multi')

#max
temp = plyr::count(multiponds7$max_cond_uspcm) 
#summary
summary(multiponds7$max_cond_uspcm)
#boxplot
ggplot(multiponds7,aes(x="",y=log(max_cond_uspcm))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(max_cond_uspcm,na.rm=TRUE))),color='red') + 
  ggtitle('max cond - multi')

#mean
temp = plyr::count(multiponds7$mean_cond_uspcm) 
#summary
summary(multiponds7$mean_cond_uspcm)
#boxplot
ggplot(multiponds7,aes(x="",y=log(mean_cond_uspcm))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(mean_cond_uspcm,na.rm=TRUE))),color='red') + 
  ggtitle('mean cond - multi')

#median
temp = plyr::count(multiponds7$median_cond_uspcm) 
#coerce
multiponds7$median_cond_uspcm = as.numeric(multiponds7$median_cond_uspcm)
#summary
summary(multiponds7$median_cond_uspcm)
#boxplot
ggplot(multiponds7,aes(x="",y=log(median_cond_uspcm))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(median_cond_uspcm,na.rm=TRUE))),color='red') + 
  ggtitle('median cond - multi')

#sd
temp = plyr::count(multiponds7$stdev_cond_uspcm) 
#coerce
multiponds7$stdev_chla_ugpl = as.numeric(multiponds7$stdev_cond_uspcm)
#summary
summary(multiponds7$stdev_cond_uspcm)
#boxplot
ggplot(multiponds7,aes(x="",y=log(stdev_cond_uspcm))) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(stdev_cond_uspcm,na.rm=TRUE))),color='red') + 
  ggtitle('sd cond - multi')

#***pH ####
#min
temp = plyr::count(multiponds7$min_ph) 
#summary
summary(multiponds7$min_chla_ugpl)
#boxplot
ggplot(multiponds7,aes(x="",y=min_ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_ph,na.rm=TRUE)),color='red') + 
  ggtitle('min ph - multi')

#max
temp = plyr::count(multiponds7$max_ph) 
#summary
summary(multiponds7$max_ph)
#boxplot
ggplot(multiponds7,aes(x="",y=max_ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  #geom_hline(aes(yintercept=3*sd(max_ph,na.rm=TRUE)),color='red') + 
  ggtitle('max ph - multi')

#mean
temp = plyr::count(multiponds7$mean_ph) 
#summary
summary(multiponds7$mean_ph)
#boxplot
ggplot(multiponds7,aes(x="",y=mean_ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  #geom_hline(aes(yintercept=3*sd(mean_ph,na.rm=TRUE)),color='red') + 
  ggtitle('mean ph - multi')

#median
temp = plyr::count(multiponds7$median_ph) 
#coerce
multiponds7$median_ph = as.numeric(multiponds7$median_ph)
#summary
summary(multiponds7$median_ph)
#boxplot
ggplot(multiponds7,aes(x="",y=median_ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  #geom_hline(aes(yintercept=3*sd(median_ph,na.rm=TRUE)),color='red') + 
  ggtitle('median ph - multi')

#sd
temp = plyr::count(multiponds7$stdev_ph) 
#summary
summary(multiponds7$stdev_ph)
#boxplot
ggplot(multiponds7,aes(x="",y=stdev_ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  #geom_hline(aes(yintercept=log(3*sd(stdev_ph,na.rm=TRUE))),color='red') + 
  ggtitle('sd ph - multi')

#***Turbidity ####
#Secchi
#min
temp = plyr::count(multiponds7$min_turbidity_secchi_m) 
#summary
summary(multiponds7$min_turbidity_secchi_m)
#boxplot
ggplot(multiponds7,aes(x="",y=min_turbidity_secchi_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_turbidity_secchi_m,na.rm=TRUE)),color='red') + 
  ggtitle('min_turbidity_secchi_m - multi')

#max
temp = plyr::count(multiponds7$max_turbidity_secchi_m) 
#summary
summary(multiponds7$max_turbidity_secchi_m)
#boxplot
ggplot(multiponds7,aes(x="",y=max_turbidity_secchi_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_turbidity_secchi_m,na.rm=TRUE)),color='red') + 
  ggtitle('max_turbidity_secchi_m - multi')

#mean
temp = plyr::count(multiponds7$mean_turbidity_secchi_m) 
#coerce
multiponds7$mean_turbidity_secchi_m = as.numeric(multiponds7$mean_turbidity_secchi_m)
#summary
summary(multiponds7$mean_turbidity_secchi_m)
#boxplot
ggplot(multiponds7,aes(x="",y=mean_turbidity_secchi_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_turbidity_secchi_m,na.rm=TRUE)),color='red') + 
  ggtitle('mean_turbidity_secchi_m - multi')

#sd
temp = plyr::count(multiponds7$stdev_turbidity_secchi_m) 
#summary
summary(multiponds7$stdev_turbidity_secchi_m)
#boxplot
ggplot(multiponds7,aes(x="",y=stdev_turbidity_secchi_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(stdev_turbidity_secchi_m,na.rm=TRUE))),color='red') + 
  ggtitle('stdev_turbidity_secchi_m - multi')

#NTUs
#min
temp = plyr::count(multiponds7$min_turbidity_ntu) 
#summary
summary(multiponds7$min_turbidity_ntu)
#boxplot
ggplot(multiponds7,aes(x="",y=min_turbidity_ntu)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_turbidity_ntu,na.rm=TRUE)),color='red') + 
  ggtitle('min_turbidity_ntu - multi')

#max
temp = plyr::count(multiponds7$max_turbidity_ntu) 
#summary
summary(multiponds7$max_turbidity_ntu)
#boxplot
ggplot(multiponds7,aes(x="",y=max_turbidity_ntu)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_turbidity_ntu,na.rm=TRUE)),color='red') + 
  ggtitle('max_turbidity_ntu - multi')

#mean
temp = plyr::count(multiponds7$mean_turbidity_ntu) 
#summary
summary(multiponds7$mean_turbidity_ntu)
#boxplot
ggplot(multiponds7,aes(x="",y=mean_turbidity_ntu)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_turbidity_ntu,na.rm=TRUE)),color='red') + 
  ggtitle('mean_turbidity_ntu - multi')

#median
temp = plyr::count(multiponds7$median_turbidity_ntu) 
#coerce
multiponds7$median_turbidity_ntu = as.numeric(multiponds7$median_turbidity_ntu)
#summary
summary(multiponds7$median_turbidity_ntu)
#boxplot
ggplot(multiponds7,aes(x="",y=median_turbidity_ntu)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  ggtitle('median_turbidity_ntu - multi')

#sd
temp = plyr::count(multiponds7$stdev_turbidity_ntu) 
#summary
summary(multiponds7$stdev_turbidity_ntu)
#boxplot
ggplot(multiponds7,aes(x="",y=stdev_turbidity_ntu)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(stdev_turbidity_ntu,na.rm=TRUE))),color='red') + 
  ggtitle('stdev_turbidity_ntu - multi')

#***TSS ####
#min
temp = plyr::count(multiponds7$min_tss_mgpl) 
#summary
summary(multiponds7$min_tss_mgpl)
#boxplot
ggplot(multiponds7,aes(x="",y=min_tss_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_tss_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('min_tss_mgpl - multi')

#max
temp = plyr::count(multiponds7$max_tss_mgpl) 
#summary
summary(multiponds7$max_tss_mgpl)
#boxplot
ggplot(multiponds7,aes(x="",y=max_tss_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_tss_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('max_tss_mgpl - multi')

#mean
temp = plyr::count(multiponds7$mean_tss_mgpl) 
#summary
summary(multiponds7$mean_tss_mgpl)
#boxplot
ggplot(multiponds7,aes(x="",y=mean_tss_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_tss_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('mean_tss_mgpl - multi')

#median
temp = plyr::count(multiponds7$median_tss_mgpl) 
#coerce
multiponds7$median_tss_mgpl = as.numeric(multiponds7$median_tss_mgpl)
#summary
summary(multiponds7$median_tss_mgpl)
#boxplot
ggplot(multiponds7,aes(x="",y=median_tss_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  ggtitle('median_tss_mgpl - multi')

#sd
temp = plyr::count(multiponds7$stdev_tss_mgpl) 
#summary
summary(multiponds7$stdev_tss_mgpl)
#boxplot
ggplot(multiponds7,aes(x="",y=stdev_tss_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(stdev_tss_mgpl,na.rm=TRUE))),color='red') + 
  ggtitle('stdev_tss_mgpl - multi')


#**Categorical columns ####
#number of unique values in each column
rapply(singleponds,function(x)length(unique(x)))

#QAQC workflow: systematically check every categorical column for non-metadata values and changing to 'other' or 'multiple'
#generate column 'check_notes_for' and append with key words pertaining to each column
#save unique 'other' values outside of metadata, generate dataframe later
#replace non-metadata values with either 'other' or 'multiple'

#***Lake Name / What do the authors call the water body? ####
m_author_term = plyr::count(multiponds7$whatauthorscallit) 
colnames(m_author_term) = c("whatauthorscallit","whatauthorscallit_n")
m_pond_names = plyr::count(multiponds7$pondname) #rename columns for later
colnames(m_author_term) = c("pondname","pondname_n")

#***Human built / manipulation ####
plyr::count(multiponds7$humanbuilt_manipulated) 
#change 'human built' and 'both' to "y"
multiponds8 = multiponds7 %>%
  mutate(humanbuilt_manipulated=replace(humanbuilt_manipulated,grepl(",",humanbuilt_manipulated) | grepl("natural",humanbuilt_manipulated),"both")) %>%  #change to 'both'
  mutate(check_notes_for=replace(check_notes_for,grepl('manip',notes) | grepl('built',notes),paste(check_notes_for,'manipulation or human built',sep=', ',collapse=NULL)))  #flag notes that contain 'use' to check  notes for ponduse AND OR landuse
#save summary
m_humanbuilt = plyr::count(multiponds8$humanbuilt_manipulated) %>% 
  rename(humanbuilt_manipulated = x) %>% 
  rename(humanbuilt_manipulated_n = freq) %>% 
  mutate(humanbuilt_manipulated=replace_na(humanbuilt_manipulated,'NA')) 

#***Ponduse ####
#save original list
m_ponduse_start = plyr::count(multiponds8$ponduse) 
#save ponduse from metadata
m_ponduse_md = unique(multimd$ponduse)
#no 'others' to save
#workflow:
#generate column 'check_notes_for' and append with key word 'pertaining to use' to each column
#change multiple values (i.e: water retention or recreation) to 'multiple' in ponduse column
#ignore warning 'number of items to replace is not a multiple of replacement length', that makes sense. 
multiponds8 = multiponds8 %>% 
  mutate(ponduse=replace(ponduse,ponduse=="none",NA)) %>% #change 'none' to NA
  mutate(ponduse=replace(ponduse,ponduse=="irrigation",'farm')) %>% #change 'irrigation' to 'farm'
  mutate(ponduse=replace(ponduse,grepl('recreation',ponduse) | grepl('-',ponduse),'multiple')) %>%  # change values >1 to 'multiple'
  mutate(check_notes_for=replace(check_notes_for,grepl('other',ponduse) | grepl('multiple',ponduse),paste(check_notes_for,'ponduse other or multiple',sep=', ',collapse=NULL)))  %>% # check for 'other' or 'multiple', if so, paste in check_notes column
  mutate(check_notes_for=replace(check_notes_for,grepl('use',notes),paste(check_notes_for,'pond or land use',sep=', ',collapse=NULL)))  #flag notes that contain 'use' to check  notes for ponduse AND OR landuse
#check   
plyr::count(multiponds8$ponduse) 
m_ponduse_start
#save summary
m_ponduse = plyr::count(multiponds8$ponduse) %>% 
  rename(ponduse = x) %>% 
  rename(ponduse_n = freq) %>% 
  mutate(ponduse=replace_na(ponduse,'NA')) 

#***Land use ####
#save original list
m_landuse_start = plyr::count(multiponds8$landuse) 
#save ponduse from metadata
m_landuse_md = unique(multimd$landuse)
#workflow:
#append check_notes column for other/multiple values
#change multiples (i.e:forested/agricultural) to multiple
#change non-metadata single values to 'other'
multiponds9 = multiponds8 %>% 
  mutate(check_notes_for=replace(check_notes_for,grepl('other',landuse) | grepl('multiple',landuse),paste(check_notes_for,'landuse other or multiple',sep=', ',collapse=NULL))) %>%   # check for 'other' or 'multiple', if so, paste in check_notes column
  mutate(landuse=replace(landuse,grepl('/',landuse) | grepl('-',landuse),'multiple')) %>%   # change multiples to 'multiple'
  mutate(landuse=replace(landuse,!landuse %in% m_landuse_md,'other'))  # if landnduse value != metadata category, change to 'other'
#check  
plyr::count(multiponds9$landuse) 
m_landuse_start
#save summary
m_landuse = plyr::count(multiponds9$landuse) %>% 
  rename(landuse = x) %>% 
  rename(landuse_n = freq) %>% 
  mutate(landuse=replace_na(landuse,'NA')) 

#***Managed ####
#check count of unique answers. only y, n, NA.
plyr::count(multiponds9$managed) 
#flag use of word 'manage' in notes. 
multiponds10 = multiponds9 %>% 
  mutate(check_notes_for=replace(check_notes_for,grepl('manage',notes),paste(check_notes_for,'manage',sep=', ',collapse=NULL))) # flag notes that contain 'manage' 
#save summary
m_managed =  plyr::count(multiponds10$managed) %>% 
  rename(managed = x) %>% 
  rename(managed_n = freq) %>% 
  mutate(managed=replace_na(managed,'NA')) 

#***Surface water connectivity ####
#check unique values
plyr::count(multiponds10$surfacewaterconnectivity) 
#metadata categories
m_surfacewaterconnectivity_md = unique(multimd$surfacewaterconnectivity)
#workflow:
#fix a few
#append check_notes column for other/multiple values
multiponds11 = multiponds10 %>% 
  mutate(surfacewaterconnectivity=replace(surfacewaterconnectivity,grepl('/',surfacewaterconnectivity),'isolated')) %>%  # changes 'isolated (no inflow/ourflow)' to just 'isolated'
  mutate(surfacewaterconnectivity=replace(surfacewaterconnectivity,grepl('and',surfacewaterconnectivity),'multiple')) %>%  # changes multi to 'multiple'
  mutate(surfacewaterconnectivity=replace(surfacewaterconnectivity,surfacewaterconnectivity=='n',NA)) %>%  # changes n to 'NA'
  mutate(check_notes_for=replace(check_notes_for,grepl('connectivity',notes),paste(check_notes_for,'connectivity',sep=', ',collapse=NULL))) %>%    # flag 'connectivity' in notes
  mutate(check_notes_for=replace(check_notes_for,grepl('multiple',surfacewaterconnectivity),paste(check_notes_for,'connectivity - multiple',sep=', ',collapse=NULL)))   # check for 'multiple', check notes
#check
plyr::count(multiponds11$surfacewaterconnectivity) 
#save summary
m_surfacewaterconnectivity = plyr::count(multiponds11$surfacewaterconnectivity) %>% 
  rename(surfacewaterconnectivity = x) %>% 
  rename(surfacewaterconnectivity_n = freq) %>% 
  mutate(surfacewaterconnectivity=replace_na(surfacewaterconnectivity,'NA')) 

#***Hydrology ####
#count unique values
plyr::count(multiponds11$hydrology) 
#metadata categories
unique(multimd$hydrology)
#workflow:
#fix typos
#append check_notes for 'hydro'
#change 'seasonal/intermittent' to 'multiple'
multiponds12 = multiponds11 %>% 
  mutate(hydrology=replace(hydrology,hydrology=='seasonal','intermittent')) %>%   #change seasonal to 'intermittent'
  mutate(hydrology=replace(hydrology,hydrology=='intermittment','intermittent')) %>%   #fix typo
  mutate(hydrology=replace(hydrology,grepl('/',hydrology) | grepl(',',hydrology) | grepl('both',hydrology),'multiple')) %>%   #change multiples to 'multiple'
  mutate(check_notes_for=replace(check_notes_for,grepl('hydro',notes),paste(check_notes_for,'hydro',sep=', ',collapse=NULL))) %>%    # flag 'hydro' in notes
  mutate(check_notes_for=replace(check_notes_for,grepl('multiple',hydrology),paste(check_notes_for,'hydrology - multiple',sep=', ',collapse=NULL)))   # check for 'multiple', check notes
#check
plyr::count(multiponds12$hydrology) 
#check_notes check
plyr::count(multiponds12$check_notes_for) 
#save summary
m_hydrology = plyr::count(multiponds12$hydrology) %>% 
  rename(hydrology = x) %>% 
  rename(hydrology_n = freq) %>% 
  mutate(hydrology=replace_na(hydrology,'NA')) 

#***Trophic Status ####
# check count of unique values
plyr::count(multiponds12$trophic_status) 
#check metadata categories
unique(multimd$trophic_status)
#check n against range of trophic statuses
multi_trophstatus_check = multiponds12 %>% select(author,year,trophic_status,n,whatauthorscallit,pondname,notes) %>% filter(!is.na(trophic_status))
#workflow:
#change slashes, dashes to "to" to denote range of statuses
#change abbreviated to full term
#append check_notes
multiponds13 = multiponds12 %>% 
  mutate(trophic_status=replace(trophic_status,trophic_status=='meso - hypereutrophic','mesotrophic to hypertrophic')) %>%    #change these to 'to'
  mutate(trophic_status=replace(trophic_status,trophic_status=='meso to hypertrophic','mesotrophic to hypertrophic')) %>%    #change these to 'to'
  mutate(trophic_status=replace(trophic_status,trophic_status=='oligo to eutrophic','oligotrophic to eutrophic')) %>%    #change these to 'to'
  mutate(trophic_status=replace(trophic_status,trophic_status=='oligo to extremely eutrophic','oligotrophic to hypertrophic')) %>%    #change these to 'to'
  mutate(trophic_status=replace(trophic_status,trophic_status=='oligotrophic-mesotrophic-eutrophic','oligotrophic to eutrophic')) %>%    #change these to 'to'
  mutate(trophic_status=replace(trophic_status,trophic_status=='oligotrophic/mesotrophic','oligotrophic to mesotrophic')) %>%    #change these to 'to'
  mutate(trophic_status=replace(trophic_status,trophic_status=='ultraoligo to hypereutrophic','ultraoligo to hypertrophic')) %>%    #change these to 'to'
  mutate(check_notes_for=replace(check_notes_for,grepl('multiple',trophic_status),paste(check_notes_for,'trophic status - multiple',sep=', ',collapse=NULL))) %>%   # check for 'multiple'
  mutate(check_notes_for=replace(check_notes_for,grepl('troph',notes),paste(check_notes_for,'troph',sep=', ',collapse=NULL)))  # flag 'troph' in notes
#check
plyr::count(multiponds13$trophic_status) 
#save summary
m_trophic_status = plyr::count(multiponds13$trophic_status) %>% 
  rename(trophic_status = x) %>% 
  rename(trophic_status_n = freq) %>% 
  mutate(trophic_status=replace_na(trophic_status,'NA')) 

#***Fish presence ####
plyr::count(multiponds13$fishpresence) 
#change non-yes or no to 'both'
multiponds14 = multiponds13 %>% 
  mutate(fishpresence=replace(fishpresence,!fishpresence=='y' & !fishpresence=='n','both')) %>% #change answers to 'both'
  mutate(check_notes_for=replace(check_notes_for,grepl('fish',notes),paste(check_notes_for,'fish',sep=', ',collapse=NULL)))  # flag 'fish' in notes
#check
plyr::count(multiponds14$fishpresence) 
#save summary
m_fishpresence = plyr::count(multiponds14$fishpresence) %>% 
  rename(fishpresence = x) %>% 
  rename(fishpresence_n = freq) %>% 
  mutate(fishpresence=replace_na(fishpresence,'NA')) 

#***Macrophytes presence ####
plyr::count(multiponds14$macrophytespresence) 
#change yes to y
#append notes for macrophytes
multiponds15 = multiponds14 %>% 
  mutate(macrophytespresence=replace(macrophytespresence,!macrophytespresence=='y' & !macrophytespresence=='n','both')) %>% #change answers to 'both'
  mutate(check_notes_for=replace(check_notes_for,grepl('macrophyte',notes) | grepl('plant',notes),paste(check_notes_for,'macrophytes or plants',sep=', ',collapse=NULL)))  # flag 'plant or macrophytes' in notes
#save summary
m_macrophytespresence = plyr::count(multiponds15$macrophytespresence) %>% 
  rename(macrophytespresence = x) %>% 
  rename(macrophytespresence_n = freq) %>% 
  mutate(macrophytespresence=replace_na(macrophytespresence,'NA')) 

#*Finish multi ponds ####
#append check_notes for remaining numeric values
#check check notes
plyr::count(multiponds15$check_notes_for) 
unique(multiponds15$check_notes_for)
multiponds16 = multiponds15 %>% 
  mutate(check_notes_for=replace(check_notes_for,grepl('depth',notes),paste(check_notes_for,'depth',sep=', ',collapse=NULL))) %>%   # check notes for 'depth'
  mutate(check_notes_for=replace(check_notes_for,grepl('surface area',notes),paste(check_notes_for,'surface area',sep=', ',collapse=NULL))) %>%   # check notes for 'surface area'
  mutate(check_notes_for=replace(check_notes_for,grepl('volume',notes),paste(check_notes_for,'volume',sep=', ',collapse=NULL))) %>%   # check notes for 'volume'
  mutate(check_notes_for=replace(check_notes_for,grepl('ph',notes),paste(check_notes_for,'ph',sep=', ',collapse=NULL))) %>%   # check notes for 'ph'
  mutate(check_notes_for=replace(check_notes_for,grepl('turb',notes),paste(check_notes_for,'turb',sep=', ',collapse=NULL))) %>%   # check notes for 'turb'
  mutate(check_notes_for=replace(check_notes_for,grepl('tss',notes),paste(check_notes_for,'tss',sep=', ',collapse=NULL))) %>%   # check notes for 'tss'
  mutate(check_notes_for=replace(check_notes_for,grepl('secchi',notes),paste(check_notes_for,'secchi',sep=', ',collapse=NULL))) %>%   # check notes for 'secchi'
  mutate(check_notes_for=replace(check_notes_for,grepl('doc',notes),paste(check_notes_for,'doc',sep=', ',collapse=NULL))) %>%   # check notes for 'doc'
  mutate(check_notes_for=replace(check_notes_for,grepl('chloro',notes),paste(check_notes_for,'chloro',sep=', ',collapse=NULL))) %>%   # check notes for 'chloro'
  mutate(check_notes_for=replace(check_notes_for,grepl('tp',notes) | grepl('phosphorus',notes),paste(check_notes_for,'tp/phosphorus',sep=', ',collapse=NULL))) %>%   # check notes for 'phosphorus'
  mutate(check_notes_for=replace(check_notes_for,grepl('tn',notes) | grepl('nitrogen',notes),paste(check_notes_for,'tn/nitrogen',sep=', ',collapse=NULL))) %>%   # check notes for 'nitrogen'
  mutate(check_notes_for=replace(check_notes_for,grepl('conduct',notes),paste(check_notes_for,'conductivity',sep=', ',collapse=NULL))) %>%   # check notes for 'conductivity'
  mutate(check_notes_for=str_remove(check_notes_for,'NA, ')) #get rid of weird "NA"
#check again
unique(multiponds16$check_notes_for)

#**Subset data ####
#check multiponds structure
str(multiponds16)
#101 rows with no size data
no_size_multi = multiponds16 %>% filter(is.na(min_surfacearea_m2) & is.na(mean_surfacearea_m2) & is.na(max_surfacearea_m2) & 
                                        is.na(min_depth_m) & is.na(max_depth_m) & is.na(mean_depth_m))
#save lakes >10m depth, and >1km2 surface area
multiponds_large = multiponds16 %>% 
  filter(mean_depth_m > 10 | max_depth_m > 10 | mean_surfacearea_m2 > 1000000 | max_surfacearea_m2 > 1000000)
#remove rows where mean or max depth < 10, AND mean or max surface area < 1 km^2 AND is called a lake (or lakes) by authors
# multiponds17 = multiponds16 %>% 
#   filter(mean_depth_m < 10 | max_depth_m < 10 & mean_surfacearea_m2 < 1000000 | max_surfacearea_m2 < 1000000 & !whatauthorscallit %in% c('lake','lakes'))

multiponds_final <- multiponds16 %>% filter((mean_depth_m < 10 | max_depth_m < 10 | is.na(max_depth_m) == TRUE | is.na(mean_depth_m) == TRUE)
                                              & (mean_surfacearea_m2 < 10000000 | max_surfacearea_m2 <10000000 |is.na(mean_surfacearea_m2==TRUE) | is.na(max_surfacearea_m2==TRUE)))

#**Numeric summary table ####
#save numeric columns
num = c("min_depth_m","max_depth_m","mean_depth_m","median_depth_m","stdev_depth_m","min_surfacearea_m2","max_surfacearea_m2",       
        "mean_surfacearea_m2","median_surfacearea_m2","stdev_surfacearea_m2","min_doc_mgpl","max_doc_mgpl","mean_doc_mgpl",
        "median_doc_mgpl","stdev_doc_mgpl","min_chla_ugpl","max_chla_ugpl","mean_chla_ugpl","median_chla_ugpl","stdev_chla_ugpl",
        "min_tp_ugpl","max_tp_ugpl","mean_tp_ugpl","median_tp_ugpl","stdev_tp_ugpl","min_tn_ugpl","max_tn_ugpl","mean_tn_ugpl",
        "median_tn_ugpl","stdev_tn_ugpl","min_cond_uspcm","max_cond_uspcm","mean_cond_uspcm","median_cond_uspcm",
        "stdev_cond_uspcm","min_ph","max_ph","mean_ph","median_ph","stdev_ph","min_turbidity_secchi_m","max_turbidity_secchi_m",
        "mean_turbidity_secchi_m","stdev_turbidity_secchi_m","min_turbidity_ntu","max_turbidity_ntu","mean_turbidity_ntu",       
        "median_turbidity_ntu","stdev_turbidity_ntu","min_tss_mgpl","max_tss_mgpl","median_tss_mgpl","mean_tss_mgpl","stdev_tss_mgpl")
#counts
temp1 = multiponds17 %>% 
  select(num) %>% 
  summarise_all(funs(sum(!is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="n") 
#min
temp2 = multiponds17 %>% 
  select(num) %>% 
  summarize_all(min,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="min") 
#max
temp3 = multiponds17 %>% 
  select(num) %>% 
  summarize_all(max,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="max") 
#mean
temp4 = multiponds17 %>% 
  select(num) %>% 
  summarize_all(mean,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="mean") 
#sd
temp5 = multiponds17 %>% 
  select(num) %>% 
  summarize_all(sd,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="sd") 
#merge
multiponds_summary_numeric = merge(temp1,temp2,by="variable")
multiponds_summary_numeric = merge(multiponds_summary_numeric,temp3,by="variable")
multiponds_summary_numeric = merge(multiponds_summary_numeric,temp4,by="variable")
multiponds_summary_numeric = merge(multiponds_summary_numeric,temp5,by="variable")
#fix scientific notation
multiponds_summary_numeric = format(multiponds_summary_numeric, scientific=F)

#**Categorical summary table ####
#click on column heads to see total counts of each value, including NA's which are actually typed out as 'NA'
multiponds_summary_categorical = rbind.fill(m_author_term,m_pond_names,m_managed,m_humanbuilt,m_ponduse,m_landuse,
                                            m_surfacewaterconnectivity,m_trophic_status,m_hydrology,m_fishpresence,m_macrophytespresence)

#**Save CSVs ####
#write.csv(multiponds_final, "Data_cleaned/MultiplePonds_clean.csv")
#write.csv(multiponds_summary_categorical, "Data_cleaned/MultiplePonds_summary_categorical.csv")
#write.csv(multiponds_summary_numeric, "Data_cleaned/MultiplePonds_summary_numeric.csv")
#write.csv(no_size_Multiple, "Data_cleaned/MultiplePonds_no_size_data.csv")
#write.csv(multiponds_large, "Data_cleaned/MultiplePonds_large.csv")


#END