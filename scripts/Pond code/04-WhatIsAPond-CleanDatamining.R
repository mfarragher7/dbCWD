# Clean data mining databases using mostly tidyverse  
# Created June 17 2020 by MJF

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
singlemd = read_csv("RawData/OtherData/Metadata_DataMining_SinglePonds.csv")
#transpose
singlemd = singlemd %>% tibble::rownames_to_column() %>% pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>% column_to_rownames(var="name")
#first row as column names
names(singlemd) = singlemd %>% slice(1) %>% unlist()
singlemd = singlemd %>% slice(-1)
#delete text from 'numeric' columns (Definitions/whatAuthorsCallIt)
singlemd[2,5:6] = NA 
#convert everything to lowercase
singlemd = mutate_all(singlemd,tolower) 
#fix column names
singlemd = singlemd %>% 
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
#fix '( ) ?' symbols
names(singlemd)[names(singlemd)=="longitude(decimaldegree)"] = "longitude"
names(singlemd)[names(singlemd)=="latitude(decimaldegree)"] = "latitude"
names(singlemd)[names(singlemd)=="whatdotheauthorscallit?"] = "whatauthorscallit"


#*Read in data ####
singleponds = read_csv("RawData/CombinedDataframes/combined_singleponds_coord_fix.csv")
#delete extra column
singleponds = select(singleponds, -c(X1)) 
#rename columns
singleponds = singleponds %>% 
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
names(singleponds)[names(singleponds) == "longitude(decimaldegree)"] = "longitude"
names(singleponds)[names(singleponds) == "latitude(decimaldegree)"] = "latitude"
names(singleponds)[names(singleponds) == "whatdotheauthorscallit?"] = "whatauthorscallit"
#check that colnames match
names(singleponds)
names(singlemd)
#singlenames = c(names(singleponds),names(singlemd))
#unique(singlenames)
#columns out of order between metadata and pond database. change singleponds column order to match metadata
 #if the function relocate() doesn't work you need to update your dplyr package!!!
singleponds = singleponds %>% relocate(surfacewaterconnectivity, .after = managed)
#column names and positions all match now 
 #set all values to lowercase
singleponds = singleponds %>% mutate_if(is.character, str_to_lower)


#*QAQC ####
#check structure
str(singleponds)

#**Numeric columns ####
rapply(singleponds,function(.)length(unique(.)))
#check for non-numeric values (i.e <, commas, text, etc)
#generate boxplots

#***Physical characteristics ####
#min depth
plyr::count(singleponds$min_depth_m) 
#delete column, not enough data, and min values of zero

#max depth
temp = plyr::count(singleponds$max_depth_m) 
# < 10 over 100m
#histogram 
ggplot(singleponds,aes(max_depth_m)) +
  ggtitle('max depth - single') +
  geom_histogram(color='gray') 
#summary
summary(singleponds$max_depth_m)
#box plot
ggplot(singleponds,aes(x="",y=max_depth_m)) + #need blank 'x' to add geom_jitter
  geom_boxplot(color='black',outlier.shape = NA) + #hide outliers duplicated by geom_jitter
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_depth_m,na.rm=TRUE)),color='red') + #3 std.dev 
  ggtitle('max depth - single')

#mean depth
temp =  plyr::count(singleponds$mean_depth_m)
#histogram
ggplot(singleponds,aes(mean_depth_m))+
  ggtitle('mean depth - single') +
  scale_x_continuous(trans='log', breaks=c(1, 10, 100)) +
  geom_histogram(color='gray')
#summary
summary(singleponds$mean_depth_m)
#box plot
ggplot(singleponds,aes(x="",y=mean_depth_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_depth_m,na.rm=TRUE)),color='red') + 
  ggtitle('mean depth - single')

#min surface area
plyr::count(singleponds$min_surfacearea_m2) 

#max s.a.
temp = singleponds %>% dplyr::count(max_surfacearea_m2, sort = TRUE) 
#histogram
ggplot(singleponds,aes(max_surfacearea_m2))+
  scale_x_continuous(trans='log', breaks=c(1, 10, 100, 1000, 100000, 100000)) +
  ggtitle('max surface area - single')+
  geom_histogram(color='light gray') 
#summary
summary(singleponds$max_surfacearea_m2)
#box plot
ggplot(singleponds,aes(x="",y=max_surfacearea_m2)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(1, 10, 100, 1000, 10000, 100000, 1000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_surfacearea_m2,na.rm=TRUE)),color='red') + 
  ggtitle('max surface area - single')

#mean s.a.
#fix mean surface area, coerce to numeric
temp = plyr::count(singleponds$mean_surfacearea_m2) 
#coerce
singleponds$mean_surfacearea_m2 = as.numeric(singleponds$mean_surfacearea_m2)
temp_check = plyr::count(singleponds$mean_surfacearea_m2) 
#histogram
ggplot(singleponds,aes(mean_surfacearea_m2))+
  ggtitle('mean surface area - single')+
  scale_x_continuous(trans='log', breaks=c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)) +
  geom_histogram(color='gray')
#summary
summary(singleponds$mean_surfacearea_m2)
#box plot
ggplot(singleponds,aes(x="",y=mean_surfacearea_m2)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_surfacearea_m2,na.rm=TRUE)),color='red') + 
  ggtitle('mean surface area - single')

#volume
plyr::count(singleponds$volume_m3) 
#delete
singleponds = singleponds %>% select(-volume_m3) 

#***Temperature ####
#delete
singleponds = singleponds %>% select(-min_temp_c,-max_temp_c,-mean_temp_c) #delete columns

#***Macrophytes percent coverage ####
temp  = plyr::count(singleponds$macrophytes_percentcover) 
#histogram
ggplot(singleponds,aes(macrophytes_percentcover))+
  ggtitle('macrophytes percent coverage - single')+
  stat_bin(binwidth=1,color='gray')
#highest counts at 0% and 100%
#summary
summary(singleponds$macrophytes_percentcover)
#box plot
ggplot(singleponds,aes(x="",y=macrophytes_percentcover)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  ggtitle('macrophytes_percentcover - single')

#***pH ####
temp = plyr::count(singleponds$ph) 
#remove non-numeric values
singleponds = singleponds %>% 
  mutate(ph=replace(ph,ph=='*'|grepl('mean',ph)|grepl('-',ph),NA))
#coerce
singleponds$ph = as.numeric(singleponds$ph) #plus 6 NA
temp_check = plyr::count(singleponds$ph) 
#hist
ggplot(singleponds,aes(ph))+
  ggtitle('ph - single')+
  stat_bin(binwidth=0.1,color='gray')
#summary
summary(singleponds$ph)
#box plot
ggplot(singleponds,aes(x="",y=ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(ph,na.rm=TRUE)),color='red') + 
  ggtitle('ph - single')

#***Turbidity ####
temp = plyr::count(singleponds$turbidity_secchi_m) 
#remove turbidity reported as NTU or FNU
singleponds = singleponds %>% 
  mutate(turbidity_secchi_m=replace(turbidity_secchi_m,grepl('turbidity in ftu',notes) | grepl('turbidity in fnu',notes),NA))  # flag 'troph' in notes
temp_check = plyr::count(singleponds$turbidity_secchi_m) 
#hist
ggplot(singleponds,aes(turbidity_secchi_m))+
  scale_x_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  ggtitle('turbidity_secchi - single')+
  geom_histogram(color='gray')
#summary
summary(singleponds$turbidity_secchi_m)
#boxplot
ggplot(singleponds,aes(x="",y=turbidity_secchi_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(turbidity_secchi_m,na.rm=TRUE)),color='red') + 
  ggtitle('turbidity_secchi - single')

#***TSS ####
temp = plyr::count(singleponds$tss_mgpl)
#histogram
ggplot(singleponds,aes(tss_mgpl))+
  scale_x_continuous(trans='log', breaks=c(0.01, 0.1, 1, 10, 100, 1000)) +
  ggtitle('tss_mgpl- single')+
  geom_histogram(color='gray')
#summary
summary(singleponds$tss_mgpl)
#boxplot
ggplot(singleponds,aes(x="",y=tss_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.01, 0.1, 1, 10, 100, 1000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(tss_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('tss_mgpl - single')

#***Color ####
plyr::count(singleponds$color) 
plyr::count(singleponds$color_abs_440nm) 
singleponds = singleponds %>% select(-color,-color_abs_440nm) #delete

#***DOC/CDOM ####
singleponds = singleponds %>% select(-cdom) #delete
temp = plyr::count(singleponds$doc_mgpl) 
#replace non-numeric values
singleponds = singleponds %>% 
  mutate(doc_mgpl=replace(doc_mgpl,doc_mgpl=='ma',NA)) #replace letters with NA
#coerce
singleponds$doc_mgpl = as.numeric(singleponds$doc_mgpl) #plus ~35 NA
temp_check = plyr::count(singleponds$doc_mgpl) 
#hist
ggplot(singleponds,aes(doc_mgpl))+
  scale_x_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  ggtitle('doc - single') +
  geom_histogram(color='gray')
#summary
summary(singleponds$doc_mgpl)
#boxplot
ggplot(singleponds,aes(x="",y=doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('doc_mgpl - single')

#***Chlorophyll ####
temp =  plyr::count(singleponds$chla_ugpl) 
#remove non-numeric values
singleponds = singleponds %>% 
  mutate(chla_ugpl=replace(chla_ugpl,chla_ugpl=='*'|chla_ugpl=='<5'|grepl('-',chla_ugpl),NA)) #replace with NA
#coerce
singleponds$chla_ugpl = as.numeric(singleponds$chla_ugpl) #plus 4 NA
temp_check = plyr::count(singleponds$chla_ugpl) 
#hist
ggplot(singleponds,aes(chla_ugpl))+
  ggtitle('chlorophyll - single')+
  scale_x_continuous(trans='log', breaks=c(0.01, 0.1, 1, 10, 100)) +
  geom_histogram(color='gray')
#summary
summary(singleponds$chla_ugpl)
#boxplot
ggplot(singleponds,aes(x="",y=chla_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.01, 0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(chla_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('chla_ugpl - single')

#***Nutrients ####
#TP
temp =  plyr::count(singleponds$tp_ugpl) 
#remove non-numeric values
singleponds = singleponds %>% 
  mutate(tp_ugpl=replace(tp_ugpl,tp_ugpl=='*'|grepl('-',tp_ugpl),NA)) #replace with NA
#coerce
singleponds$tp_ugpl = as.numeric(singleponds$tp_ugpl) #plus 6 NA
temp_check = plyr::count(singleponds$tp_ugpl) 
#hist
ggplot(singleponds,aes(tp_ugpl))+
  ggtitle('tp - single')+
  scale_x_continuous(trans='log', breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)) +
  geom_histogram(color='gray')
#summary
summary(singleponds$tp_ugpl)
#boxplot
ggplot(singleponds,aes(x="",y=tp_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(tp_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('tp_ugpl - single')

#TN
temp = plyr::count(singleponds$tn_ugpl) 
#hist
ggplot(singleponds,aes(tn_ugpl))+
  scale_x_continuous(trans='log', breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)) +
  ggtitle('tn - single')+
  geom_histogram(color='gray')
#one ~100,000 value
#summary
summary(singleponds$tn_ugpl)
#boxplot
ggplot(singleponds,aes(x="",y=tn_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(tn_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('tn_ugpl - single')

#conductivity
temp = plyr::count(singleponds$cond_uspcm) 
#remove non-numeric values
singleponds = singleponds %>% 
  mutate(cond_uspcm=replace(cond_uspcm,cond_uspcm=='*'|grepl('-',cond_uspcm)|grepl('/',cond_uspcm)|grepl('y',cond_uspcm),NA)) #replace with NA
#coerce
singleponds$cond_uspcm = as.numeric(singleponds$cond_uspcm) #plus 17 NA
temp_check = plyr::count(singleponds$cond_uspcm) 
#hist
ggplot(singleponds,aes(cond_uspcm))+
  scale_x_continuous(trans='log', breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)) +
  ggtitle('conductivity - single')+
  geom_histogram(color='gray')
#5 ponds > 10,000
#summary
summary(singleponds$cond_uspcm)
#boxplot
ggplot(singleponds,aes(x="",y=cond_uspcm)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(cond_uspcm,na.rm=TRUE)),color='red') + 
  ggtitle('cond_uspcm - single')

#***Canopy cover percentage ####
plyr::count(singleponds$canopycover_percentofpond) 
singleponds = singleponds %>% select(-canopycover_percentofpond) #delete

#end numeric column QAQC
str(singleponds)


#**Categorical columns ####
#number of unique values in each column
rapply(singleponds,function(x)length(unique(x)))

#workflow: systematically check every categorical column for non-metadata values and changing to 'other' or 'multiple' or 'NA'
 #generate column 'check_notes_for' and append with key words pertaining to each column
 #save unique 'other' values outside of metadata, generate dataframe later
 #replace non-metadata values with either 'other' or 'multiple'


#***Human built / manipulation ####
#check unique values. both, manipulated, human built, etc
temp = plyr::count(singleponds$humanbuilt_manipulated) 
#change 'human built' and 'both' to "y"
#delete rows with 'biomanipulation' or 'dredged'
singleponds = singleponds %>%
  mutate(humanbuilt_manipulated=replace(humanbuilt_manipulated,humanbuilt_manipulated=="human built","y")) %>% #change to 'y'
  mutate(humanbuilt_manipulated=replace(humanbuilt_manipulated,humanbuilt_manipulated=="both","y")) %>% #change to 'y'
  mutate(humanbuilt_manipulated=replace(humanbuilt_manipulated,humanbuilt_manipulated=="manipulated","y")) %>% #change to 'y'
  filter(!grepl('dredged',humanbuilt_manipulated)) #delete rows with dredged (deletes other biomanipulation too)
#check
plyr::count(singleponds$humanbuilt_manipulated) 


#***Pond use ####
s_ponduse_start = plyr::count(singleponds$ponduse)
#save ponduse from metadata
s_ponduse_md = unique(singlemd$ponduse)
#workflow:
 #making minor fixes to 'synonyms'
 #generate column 'check_notes_for' and append with key word 'pertaining'use' to each column
 #change non-metadata values (i.e: ecotourism) to 'other' in 'ponduse' column
 #change multiple values (i.e: forested/natural) to 'multiple' in ponduse column
   #these 'other' and 'multiple' values wont be flagged in check_notes column, already knew what they were. 
 #ignore warning 'number of items to replace is not a multiple of replacement length', that makes sense. 
singleponds = singleponds %>% 
  mutate(ponduse=replace(ponduse,ponduse=="agricultural*","farm")) %>% #change 'agricultural' to farm in original pond use column
  mutate(ponduse=replace(ponduse,ponduse=="reservoir","water storage")) %>%   #change reservoir to water storage in original pond use column
  mutate(check_notes_for=ifelse(grepl('use',notes),'pond or land use',NA)) %>% # new column, flag notes that contain 'use' to check  notes for ponduse AND OR landuse
  mutate(check_notes_for=replace(check_notes_for,grepl('other',ponduse) | grepl('multiple',ponduse),paste(check_notes_for,'ponduse other or multiple',sep=', ',collapse=NULL))) %>%   # check for 'other' or 'multiple', if so, paste in check_notes column
  mutate(ponduse=replace(ponduse,grepl('/',ponduse) | grepl(',',ponduse),'multiple')) %>%  # change values >1 to 'multiple'
  mutate(ponduse=replace(ponduse,!ponduse %in% s_ponduse_md,'other'))  # if ponduse value != metadata category, change to 'other'
#check
plyr::count(singleponds$ponduse)
#save dataframe of 'other' ponduse counts
s_ponduse_start = s_ponduse_start %>% dplyr::rename(ponduse=x) %>% dplyr::rename(ponduse_n = freq)
s_ponduse_other = s_ponduse_start %>% 
  mutate(ponduse=replace(ponduse,ponduse=="agricultural*","farm")) %>% #change 'agricultural' to farm 
  mutate(ponduse=replace(ponduse,grepl('irrigation',ponduse),"irrigation")) %>% #save irrigation
  mutate(ponduse=replace(ponduse,ponduse=="reservoir","water storage")) %>% #change reservoir to water storage 
  mutate(ponduse=replace(ponduse,ponduse=="other: nutrient retention","nutrient retention")) %>% #save nutrient retention
  mutate(ponduse=replace(ponduse,grepl('ecotourism',ponduse),"ecotourism")) %>% #save ecotourism
  mutate(ponduse=replace(ponduse,grepl('/',ponduse),NA)) %>% # change values >1 to 'multiple'
  mutate(ponduse=replace(ponduse,ponduse %in% s_ponduse_md,NA)) %>%  # if ponduse value = metadata category, change to NA
  drop_na(ponduse) %>% #delete NAs
  dplyr::rename(ponduse_other = ponduse) %>% #rename columns
  dplyr::rename(ponduse_other_n = ponduse_n)%>%
  add_row(ponduse_other="mine subsistence",ponduse_other_n=4)


#***Land use ####
#89 unique values
s_landuse_start = plyr::count(singleponds$landuse)
#landuse from metadata 
s_landuse_md = unique(singlemd$landuse)
#workflow:
 #fix typos and synonyms first (agriculture = agricultural, etc)
 #append check_notes column for other/multiple values
 #change multiples (i.e:forested/agricultural) to multiple
 #change non-metadata single values to 'other'
singleponds = singleponds %>% 
  mutate(landuse=replace(landuse,landuse=='agriculture','agricultural')) %>%  # fix typo
  mutate(landuse=replace(landuse,landuse=='mixed','multiple')) %>%  # fix typo
  mutate(check_notes_for=replace(check_notes_for,grepl('other',landuse) | grepl('multiple',landuse),paste(check_notes_for,'landuse other or multiple',sep=', ',collapse=NULL))) %>%   # check for 'other' or 'multiple', if so, paste in check_notes column
  mutate(landuse=replace(landuse,grepl('/',landuse),'multiple')) %>%   # change multiples to 'multiple'
  mutate(landuse=replace(landuse,!landuse %in% s_landuse_md,'other'))  # if landnduse value != metadata category, change to 'other'
#save 'others'
s_landuse_other = s_landuse_start %>% dplyr::rename(landuse_other=x) %>% dplyr::rename(landuse_other_n = freq) %>% 
  mutate(landuse_other=replace(landuse_other,landuse_other=='agriculture','agricultural')) %>%  # fix typo
  mutate(landuse_other=replace(landuse_other,landuse_other=='mixed','multiple')) %>%  # fix typo
  mutate(landuse_other=replace(landuse_other,grepl('/',landuse_other),NA)) %>%   # change multiples to NA
  mutate(landuse_other=replace(landuse_other,landuse_other %in% s_landuse_md,NA)) %>%  # if landuse value = metadata category, change to NA
  drop_na(landuse_other) %>%  #delete NAs
  add_row(landuse_other="heathland",landuse_other_n=16)



#***Managed ####
#check count of unique answers. only y, n, NA. easy!
plyr::count(singleponds$managed)
#flag use of word 'manage' in notes. 
singleponds = singleponds %>% 
  mutate(check_notes_for=replace(check_notes_for,grepl('manage',notes),paste(check_notes_for,'manage',sep=', ',collapse=NULL))) # flag notes that contain 'manage' 


#***Surface water connectivity ####
#check unique values
plyr::count(singleponds$surfacewaterconnectivity)
#metadata categories
s_surfacewaterconnectivity_md = unique(singlemd$surfacewaterconnectivity)
#workflow:
 #fix typos/synonyms
 #append check_notes column for other/multiple values
 #change non-metadata values to 'NA' 
singleponds = singleponds %>% 
  mutate(surfacewaterconnectivity=replace(surfacewaterconnectivity,grepl('urban',surfacewaterconnectivity),'inline')) %>%  # changes 'inline (urban)' to just 'inline'
  mutate(check_notes_for=replace(check_notes_for,grepl('connectivity',notes),paste(check_notes_for,'connectivity',sep=', ',collapse=NULL))) %>%    # flag 'connectivity' in notes
  mutate(check_notes_for=replace(check_notes_for,grepl('multiple',surfacewaterconnectivity),paste(check_notes_for,'connectivity - multiple',sep=', ',collapse=NULL))) %>%   # check for 'multiple', check notes
  mutate(surfacewaterconnectivity=replace(surfacewaterconnectivity,!surfacewaterconnectivity %in% s_surfacewaterconnectivity_md,NA))  # if surfacewaterconnectivity value != metadata category, change to 'other'
#fits metadata now
plyr::count(singleponds$surfacewaterconnectivity)


#***Hydrology ####
#count unique values
plyr::count(singleponds$hydrology)
#metadata categories
unique(singlemd$hydrology)
#workflow:
 #fix typos
 #append check_notes for 'hydro'
 #change 'seasonal/intermittent' to 'multiple'
singleponds = singleponds %>% 
  mutate(hydrology=replace(hydrology,grepl('intermittment',hydrology),'intermittent')) %>%   #fix spelling
  mutate(hydrology=replace(hydrology,grepl('mulitple',hydrology),'multiple')) %>%   #fix spelling
  mutate(check_notes_for=replace(check_notes_for,grepl('hydro',notes),paste(check_notes_for,'hydro',sep=', ',collapse=NULL))) %>%    # flag 'hydro' in notes
  mutate(check_notes_for=replace(check_notes_for,grepl('multiple',hydrology),paste(check_notes_for,'hydrology - multiple',sep=', ',collapse=NULL))) %>%   # check for 'multiple', check notes
  mutate(hydrology=replace(hydrology,grepl('seasonal/intermittent',hydrology),'multiple'))   #replace one 'multiple'


#***Trophic Status ####
# check count of unique values
plyr::count(singleponds$trophic_status)   # hypertrophic, in between ie: meso-oligo
#check metadata categories
unique(singlemd$trophic_status)
#workflow:
 #fix typos and asterisks
 #append check_notes
singleponds = singleponds %>% 
  mutate(trophic_status=replace(trophic_status,trophic_status=='eutrophoc','eutrophic')) %>%  #fix spelling
  mutate(trophic_status=replace(trophic_status,trophic_status=='eutrophic*','eutrophic')) %>%  #replace asterisks with no asterisks
  mutate(check_notes_for=replace(check_notes_for,grepl('troph',notes),paste(check_notes_for,'troph',sep=', ',collapse=NULL)))  # flag 'troph' in notes
#check
singleponds %>% count(trophic_status, sort = TRUE)


#***Fish presence ####
plyr::count(singleponds$fishpresence) #nothing to change
#append check_notes for fish
singleponds = singleponds %>% 
  mutate(check_notes_for=replace(check_notes_for,grepl('fish',notes),paste(check_notes_for,'fish',sep=', ',collapse=NULL)))  # flag 'fish' in notes


#***Macrophyte presence ####
plyr::count(singleponds$macrophytespresence)
#change yes to y
#append notes for macrophytes
singleponds = singleponds %>% 
  mutate(macrophytespresence=replace(macrophytespresence,macrophytespresence=='yes','y')) %>% #change to y
  mutate(check_notes_for=replace(check_notes_for,grepl('macrophyte',notes) | grepl('plant',notes),paste(check_notes_for,'macrophytes or plants',sep=', ',collapse=NULL)))  # flag 'plant or macrophytes' in notes


#***Append check_notes #### 
 #for remaining numeric values
#check check notes
plyr::count(singleponds$check_notes_for)
unique(singleponds$check_notes_for)
singleponds = singleponds %>% 
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
unique(singleponds$check_notes_for)


#* QC PART 2 ####
#check check_notes column in google sheet, make corrections here
  #replace temporary marsh with other for what authorscallit
  singleponds<-singleponds%>%mutate(whatauthorscallit = replace(whatauthorscallit, author=="pont, crivelli & guillot"&year==1991&journal=="freshwater biology", "other"))
  #delete definition
  singleponds<-singleponds%>%mutate(definitionofpond = replace(definitionofpond, author=="moller and rordam"&year==1985&journal=="oikos", NA))
  #rescale DOC units
  singleponds<-singleponds%>%mutate(doc_mgpl = ifelse(author=="morris et al"&year==2013&journal=="plosone", doc_mgpl*12.011/1000,doc_mgpl))
  #add in seasonally average DOC
  singleponds<-singleponds%>%mutate(doc_mgpl = ifelse(author=="attermeyer, katrin; grossart, hans-peter; flury, sabine; premke, katrin"&year==2017&journal=="aquatic sciences"&mean_surfacearea_m2==3630, 22.8,doc_mgpl))
  singleponds<-singleponds%>%mutate(doc_mgpl = ifelse(author=="attermeyer, katrin; grossart, hans-peter; flury, sabine; premke, katrin"&year==2017&journal=="aquatic sciences"&mean_surfacearea_m2==2650, 24.6,doc_mgpl))
  singleponds<-singleponds%>%mutate(doc_mgpl = ifelse(author=="attermeyer, katrin; grossart, hans-peter; flury, sabine; premke, katrin"&year==2017&journal=="aquatic sciences"&mean_surfacearea_m2==850, 24.5,doc_mgpl))
  singleponds<-singleponds%>%mutate(doc_mgpl = ifelse(author=="attermeyer, katrin; grossart, hans-peter; flury, sabine; premke, katrin"&year==2017&journal=="aquatic sciences"&mean_surfacearea_m2==4290, 19.6,doc_mgpl))
  singleponds<-singleponds%>%mutate(doc_mgpl = ifelse(author=="attermeyer, katrin; grossart, hans-peter; flury, sabine; premke, katrin"&year==2017&journal=="aquatic sciences"&mean_surfacearea_m2==1050, 23.4,doc_mgpl))
  singleponds<-singleponds%>%mutate(doc_mgpl = ifelse(author=="attermeyer, katrin; grossart, hans-peter; flury, sabine; premke, katrin"&year==2017&journal=="aquatic sciences"&mean_surfacearea_m2==1290, 13.3,doc_mgpl))
  singleponds<-singleponds%>%mutate(doc_mgpl = ifelse(author=="attermeyer, katrin; grossart, hans-peter; flury, sabine; premke, katrin"&year==2017&journal=="aquatic sciences"&mean_surfacearea_m2==570, 14.1,doc_mgpl))
  singleponds<-singleponds%>%mutate(doc_mgpl = ifelse(author=="attermeyer, katrin; grossart, hans-peter; flury, sabine; premke, katrin"&year==2017&journal=="aquatic sciences"&mean_surfacearea_m2==4000, 25.9,doc_mgpl))
  singleponds<-singleponds%>%mutate(doc_mgpl = ifelse(author=="attermeyer, katrin; grossart, hans-peter; flury, sabine; premke, katrin"&year==2017&journal=="aquatic sciences"&mean_surfacearea_m2==1790, 31.7,doc_mgpl))
  singleponds<-singleponds%>%mutate(doc_mgpl = ifelse(author=="attermeyer, katrin; grossart, hans-peter; flury, sabine; premke, katrin"&year==2017&journal=="aquatic sciences"&mean_surfacearea_m2==1470, 14.4,doc_mgpl))
  #Replace weird pH value with NA
  singleponds<-singleponds%>%mutate(ph = ifelse(author=="eskinazi-sant'anna, eneida maria; pace, michael l."&year==2018&journal=="journal of plankton research"&pondname=="carioca lake", NA,ph))
  #remove paper where data exists in multiponds instead
  singleponds=singleponds %>% filter(!author=='fairchild')
  #add European Pond Conversation Network as author for missing author
  singleponds=singleponds%>%mutate(author = replace(author, year==2007&journal=="ann. limnol. - int. j. lim", "European Pond Conservation Network"))
  #fix this paper with messed up year and vol
  #legnouo, samways & simaika, aquatic conservation : marine and freshwater ecosystems
  singleponds=singleponds%>%mutate(year = replace(year, author=="legnouo, samways & simaika"&journal=="aquatic conservation : marine and freshwater ecosystems", 2014))
  singleponds=singleponds%>%mutate(volume_issue = replace(volume_issue, author=="legnouo, samways & simaika"&journal=="aquatic conservation : marine and freshwater ecosystems", 24))
  #fix this other paper, year is wrong
  #teissier, samuel; peretyatko, anatoly; de backer, sylvia; triest, ludwig, hydrobiologia	
  singleponds=singleponds%>%mutate(year = replace(year, author=="teissier, samuel; peretyatko, anatoly; de backer, sylvia; triest, ludwig"&journal=="hydrobiologia", 2012))
  #remove special character in author name
  singleponds=singleponds%>%mutate(author = replace(author, author=="marková et al.","markova et al."))
  #fix other author swap
  singleponds=singleponds%>%mutate(author = replace(author, author=="pietruszynski","mimouni, ea; pinel-alloul, b; beisner, be; legendre, p"))
  singleponds=singleponds%>%mutate(journal = replace(journal, author=="mimouni, ea; pinel-alloul, b; beisner, be; legendre, p","ecosphere"))
  #fix another paper year
  singleponds=singleponds%>%mutate(year = replace(year, author=="de meester",2005))
  #fix paper year and vol
  singleponds=singleponds%>%mutate(year=replace(year,author=="sahuquillo, poquet,  rueda & miracle",2007)) %>% 
    mutate(volume_issue=replace(volume_issue,author=="sahuquillo, poquet,  rueda & miracle",43)) 
  #fix paper year and vol
  singleponds=singleponds%>%mutate(year=replace(year,author=="strat, daniela",2016)) %>% 
    mutate(volume_issue=replace(volume_issue,author=="strat, daniela",32)) 
    
    
  
#fix incorrect tp values
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="lacillo", 23, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="aguas cernidas", 48, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="la yega", 20, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="los peces", 24, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="pies juntos", 50, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="camposagrado i", 39, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="camposagrado ii", 25, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="castromil", 101, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="roya grande", 32, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="roya pequena", 57, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="el cuadro", 9, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="mancas", 13, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="el payon", 56, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="clara grande", 10, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="clara pequena", 25, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="pedrina", 20, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&journal=="aquatic conservation-marine and freshwater ecosystems"&pondname=="sotillo", 23, tp_ugpl))
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="de vincente et al"&year==2012&journal=="limnetica"&pondname=="medina", NA, tp_ugpl))
  singleponds=singleponds%>%mutate(tn_ugpl=ifelse(author=="de vincente et al"&year==2012&journal=="limnetica"&pondname=="medina", NA, tn_ugpl))
  #change to NAs
  singleponds=singleponds%>%mutate(tp_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&pondname=="pies juntos", NA, tp_ugpl))
  singleponds=singleponds%>%mutate(tn_ugpl=ifelse(author=="martinez-sanz, carlos; cenzano, cynthia s. s.; fernandez-alaez, margarita; garcia-criado, francisco"&year==2012&pondname=="pies juntos", NA, tn_ugpl))
  
#fix surface area
  #why are there ponds with sa < 1m2 ???
  #what = singleponds %>% filter(mean_surfacearea_m2 < 1)
  #one paper entered incorrectly
  singleponds=singleponds%>%mutate(mean_surfacearea_m2=ifelse(author=="gilbert, juan diego; de vicente, inmaculada; jimenez-melero, raquel; guerrero, francisco"&year==2017&journal=="freshwater science"&pondname=="castillo", 5000, mean_surfacearea_m2))
  singleponds=singleponds%>%mutate(mean_surfacearea_m2=ifelse(author=="gilbert, juan diego; de vicente, inmaculada; jimenez-melero, raquel; guerrero, francisco"&year==2017&journal=="freshwater science"&pondname=="orcera", 4000, mean_surfacearea_m2))
  singleponds=singleponds%>%mutate(mean_surfacearea_m2=ifelse(author=="gilbert, juan diego; de vicente, inmaculada; jimenez-melero, raquel; guerrero, francisco"&year==2017&journal=="freshwater science"&pondname=="ardal", 300, mean_surfacearea_m2))
  singleponds=singleponds%>%mutate(mean_surfacearea_m2=ifelse(author=="gilbert, juan diego; de vicente, inmaculada; jimenez-melero, raquel; guerrero, francisco"&year==2017&journal=="freshwater science"&pondname=="santisteban", 13000, mean_surfacearea_m2))
  singleponds=singleponds%>%mutate(mean_surfacearea_m2=ifelse(author=="gilbert, juan diego; de vicente, inmaculada; jimenez-melero, raquel; guerrero, francisco"&year==2017&journal=="freshwater science"&pondname=="hituelo", 42000, mean_surfacearea_m2))
  singleponds=singleponds%>%mutate(mean_surfacearea_m2=ifelse(author=="gilbert, juan diego; de vicente, inmaculada; jimenez-melero, raquel; guerrero, francisco"&year==2017&journal=="freshwater science"&pondname=="quinta", 57000, mean_surfacearea_m2))
  singleponds=singleponds%>%mutate(mean_surfacearea_m2=ifelse(author=="gilbert, juan diego; de vicente, inmaculada; jimenez-melero, raquel; guerrero, francisco"&year==2017&journal=="freshwater science"&pondname=="casillas", 9000, mean_surfacearea_m2))
  

#**Subset data ####
str(singleponds)
#Save - 175 ponds with no size data
no_size_single = singleponds %>% filter(is.na(min_surfacearea_m2) & is.na(mean_surfacearea_m2) & is.na(max_surfacearea_m2) & 
                                          is.na(min_depth_m) & is.na(max_depth_m) & is.na(mean_depth_m))
#save lakes >10m depth, and >1km2 surface area
#n=303
singleponds_large = singleponds %>% 
  filter(mean_depth_m > 10 | max_depth_m > 10 | mean_surfacearea_m2 > 1000000 | max_surfacearea_m2 > 1000000)


#remove rows where mean or max depth < 10, AND mean or max surface area < 1 km^2 AND is called a lake (or lakes) by authors

nrow(singleponds %>% filter((mean_depth_m < 10 | max_depth_m < 10 | is.na(max_depth_m) == TRUE & is.na(mean_depth_m) == TRUE)  & (mean_surfacearea_m2 < 10000000 | max_surfacearea_m2 < 10000000 |is.na(mean_surfacearea_m2==TRUE) & is.na(max_surfacearea_m2==TRUE))))
#Check how many large lakes were removed
#n=218
nrow(singleponds)-nrow(singleponds %>% filter((mean_depth_m < 10 | max_depth_m < 10 | is.na(max_depth_m) == TRUE & is.na(mean_depth_m) == TRUE)  & (mean_surfacearea_m2 < 10000000 | max_surfacearea_m2 < 10000000 |is.na(mean_surfacearea_m2==TRUE) & is.na(max_surfacearea_m2==TRUE))))
#Check how many have NA for both SA and depth
#n=179 - keep these, many seem good but not sure about the worth - maybe eliminate if they are 
singleponds %>% filter((is.na(max_depth_m) == TRUE & is.na(mean_depth_m) == TRUE)  & (is.na(mean_surfacearea_m2==TRUE) & is.na(max_surfacearea_m2==TRUE)))%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,mean_depth_m,mean_surfacearea_m2,max_surfacearea_m2)%>%print(n=Inf)
#Check for only the deepest and largest (with numbers in both)
#n=61
nrow(singleponds %>% filter((mean_depth_m > 10 | max_depth_m > 10)  & (mean_surfacearea_m2 > 10000000 | max_surfacearea_m2 > 10000000 ))%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,mean_depth_m,mean_surfacearea_m2,max_surfacearea_m2)%>%print(n=Inf))
#Check for only the deepest 
#n=143
nrow(singleponds %>% filter((mean_depth_m > 10 | max_depth_m > 10))%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,mean_depth_m,mean_surfacearea_m2,max_surfacearea_m2)%>%print(n=Inf))
#Check for only the largest 
#n=144
nrow(singleponds %>% filter((mean_surfacearea_m2 > 10000000 | max_surfacearea_m2 > 10000000 ))%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,mean_depth_m,mean_surfacearea_m2,max_surfacearea_m2)%>%print(n=Inf))

#Create a depth dummy variable that is -999 is there is no depth and the max of max and mean depth if there is at least 1.
single_ponds_noLarge<-singleponds%>%
  mutate(depthDummy=ifelse((is.na(max_depth_m) == TRUE & is.na(mean_depth_m) == TRUE),-999,pmax(mean_depth_m,max_depth_m,na.rm=T)))%>% #depthDummy is -999 if NAs in both, otherwise the max
  mutate(areaDummy=ifelse((is.na(mean_surfacearea_m2) == TRUE & is.na(max_surfacearea_m2) == TRUE),-999,pmax(mean_surfacearea_m2, max_surfacearea_m2,na.rm=T)))%>% #areaDummy is -999 if NAs in both, otherwise the max
  filter(!(depthDummy>10&areaDummy>10000000))%>% #remove big and deep
  filter(!(depthDummy==-999&areaDummy>10000000))%>% #remove large but NA (-999) for depth
  filter(!(depthDummy>10&areaDummy==-999)) #remove deep but NA (-999) for area
  

#Find out the deep and big that remain
single_ponds_noLarge%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,mean_depth_m,mean_surfacearea_m2,max_surfacearea_m2,depthDummy,areaDummy)%>%filter(depthDummy>10)%>%print(n=Inf)
single_ponds_noLarge%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,mean_depth_m,mean_surfacearea_m2,max_surfacearea_m2,depthDummy,areaDummy)%>%filter(areaDummy>10000000)%>%print(n=Inf)


#**Numeric summary table ####
#DCR note for MJF: should this be on single_ponds_noLarge??####
#save numeric columns
num = c("max_depth_m","mean_depth_m","max_surfacearea_m2","mean_surfacearea_m2","macrophytes_percentcover",
        "ph","turbidity_secchi_m","tss_mgpl","doc_mgpl","chla_ugpl","tp_ugpl","tn_ugpl","cond_uspcm")
#counts
temp1 = single_ponds_noLarge %>% 
  select(num) %>% 
  summarise_all(funs(sum(!is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="n") 
#min
temp2 = single_ponds_noLarge %>% 
  select(num) %>% 
  summarize_all(min,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="min") 
#max
temp3 = single_ponds_noLarge %>% 
  select(num) %>% 
  summarize_all(max,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="max") 
#mean
temp4 = single_ponds_noLarge %>% 
  select(num) %>% 
  summarize_all(mean,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="mean") 
#sd
temp5 = single_ponds_noLarge %>% 
  select(num) %>% 
  summarize_all(sd,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="sd") 
#merge
singleponds_summary_numeric = merge(temp1,temp2,by="variable")
singleponds_summary_numeric = merge(singleponds_summary_numeric,temp3, by="variable")
singleponds_summary_numeric = merge(singleponds_summary_numeric,temp4, by="variable")
singleponds_summary_numeric = merge(singleponds_summary_numeric,temp5, by="variable")
#as numeric
singleponds_summary_numeric[,2:6] = sapply(singleponds_summary_numeric[,2:6],as.numeric)
#fix scientific notation and decimals
singleponds_summary_numeric[,2:6] = format(scientific=F, round(singleponds_summary_numeric[,2:6], 3), nsmall=3)


#**Categorical summary table ####
#save author term 
s_author_term = plyr::count(single_ponds_noLarge$whatauthorscallit) %>% dplyr::rename(whatauthorscallit = x, whatauthorscallit_n = freq) %>%  mutate(whatauthorscallit=replace_na(whatauthorscallit,'NA')) 
#save manipulated summary
s_humanbuilt = plyr::count(single_ponds_noLarge$humanbuilt_manipulated)  %>% dplyr::rename(humanbuilt_manipulated = x, humanbuilt_manipulated_n = freq) %>%  mutate(humanbuilt_manipulated=replace_na(humanbuilt_manipulated,'NA')) 
#save ponduse summary
s_ponduse = plyr::count(single_ponds_noLarge$ponduse)  %>% dplyr::rename(ponduse = x, ponduse_n = freq) %>%  mutate(ponduse=replace_na(ponduse,'NA'))
#save landuse summary
s_landuse = plyr::count(single_ponds_noLarge$landuse) %>% dplyr::rename(landuse = x, landuse_n = freq) %>% mutate(landuse=replace_na(landuse,'NA'))
#save managed summary
s_managed = plyr::count(single_ponds_noLarge$managed) %>% dplyr::rename(managed = x, managed_n = freq) %>% mutate(managed=replace_na(managed,'NA'))
#save surfacewaterconnectivity summary
s_surfacewaterconnectivity = plyr::count(single_ponds_noLarge$surfacewaterconnectivity) %>% dplyr::rename(surfacewaterconnectivity = x, surfacewaterconnectivity_n = freq) %>% mutate(surfacewaterconnectivity=replace_na(surfacewaterconnectivity,'NA'))
#save hydro summary
s_hydrology = plyr::count(single_ponds_noLarge$hydrology) %>% dplyr::rename(hydrology = x, hydrology_n = freq) %>% mutate(hydrology=replace_na(hydrology,'NA'))
#save trophic status summary
s_trophic_status = plyr::count(single_ponds_noLarge$trophic_status) %>% dplyr::rename(trophic_status = x, trophic_status_n = freq) %>% mutate(trophic_status=replace_na(trophic_status,'NA'))
#save fishpresence summary
s_fishpresence = plyr::count(single_ponds_noLarge$fishpresence) %>% dplyr::rename(fishpresence = x, fishpresence_n = freq) %>% mutate(fishpresence=replace_na(fishpresence,'NA')) 
#save macrophytes summary
s_macrophytespresence = plyr::count(single_ponds_noLarge$macrophytespresence) %>% dplyr::rename(macrophytespresence = x, macrophytespresence_n = freq) %>% mutate(macrophytespresence=replace_na(macrophytespresence,'NA')) 


#merge categorical summary
singleponds_summary_categorical = rbind.fill(s_author_term,s_managed,s_humanbuilt,s_ponduse,s_landuse,
                          s_surfacewaterconnectivity,s_trophic_status,s_hydrology,s_fishpresence,s_macrophytespresence)

#**Save 'others' list ####
s_ponduse_other
s_landuse_other
#kind of worked
singleponds_other=rbind.fill(s_ponduse_other,s_landuse_other)



#**Save CSVs ####
write.csv(single_ponds_noLarge, "Data_cleaned/Singleponds_Cleaned.csv",row.names=FALSE)
write.csv(singleponds_summary_numeric, "Data_cleaned/OtherCSVs/Singleponds_summary_numeric.csv",row.names=FALSE)
write.csv(no_size_single, "Data_cleaned/OtherCSVs/Singleponds_no_size_data.csv",row.names=FALSE)
write.csv(singleponds_large, "Data_cleaned/OtherCSVs/Singleponds_large.csv",row.names=FALSE)
write.csv(s_author_term, "Data_cleaned/OtherCSVs/Singleponds_author_term.csv",row.names=FALSE)

#saved locally and fixed manually
#write.csv(singleponds_summary_categorical, "C:/Users/mfarr/Desktop/Singleponds_summary_categorical_unfixed.csv",row.names=FALSE)
#write.csv(singleponds_other, "C:/Users/mfarr/Desktop/Singleponds_other_unfixed.csv",row.names=FALSE)




#END singleponds







#MULTIPLE PONDS #############################################################################################################
#*Read in metadata ####
multimd = read_csv("RawData/OtherData/Metadata_DataMining_MultiplePonds.csv")
#transpose
multimd = multimd %>% tibble::rownames_to_column() %>% pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>% column_to_rownames(var="name")
#first row as column names
names(multimd) = multimd %>% slice(1) %>% unlist()
#two identical columns "Standard deviation_m" renamed base r 
colnames(multimd)[23] = "Standard deviation depth_m" 
colnames(multimd)[28] = "Standard deviation surface area_m"
# delete first row
multimd = multimd %>% slice(-1)
#delete text from 'numeric' columns (Definitions/whatAuthorsCallIt)
multimd[2,5:6] = NA 
#convert everything to lowercase
multimd = mutate_all(multimd,tolower) 
#fix colnames
multimd = multimd %>% set_names(~ str_to_lower(.) %>% 
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
names(multimd)[names(multimd) == "longitude(decimaldegree)"] = "longitude"
names(multimd)[names(multimd) == "latitude(decimaldegree)"] = "latitude"
names(multimd)[names(multimd) == "whatdotheauthorscallit?"] = "whatauthorscallit"

#*Read in data ####
multiponds = read_csv("RawData/CombinedDataframes/Combined_MultiplePonds.csv")
#delete extra column
multiponds = select(multiponds, -c(X1)) 
#fix colnames
multiponds = multiponds %>% set_names(~ str_to_lower(.) %>% 
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
names(multiponds)[names(multiponds) == "longitude(decimaldegree)"] = "longitude"
names(multiponds)[names(multiponds) == "latitude(decimaldegree)"] = "latitude"
names(multiponds)[names(multiponds) == "whatdotheauthorscallit?"] = "whatauthorscallit"
#check that colnames match
names(multiponds)
names(multimd)
#multinames = c(names(multiponds),names(multimd))
#unique(multinames)
#sort(unique(multinames))
#columns out of order between metadata and data. 
# move fish presence from column 64 to 78. 
multiponds = multiponds %>% relocate(fishpresence, .after = stdev_tss_mgpl)
#they match now.
# set all values to lowercase
multiponds = multiponds %>% mutate_if(is.character, str_to_lower)


#*QAQC ####
#**Numeric columns ####
rapply(multiponds,function(.)length(unique(.)))
#check for non-numeric values (i.e <, commas, text, etc)
#generate boxplots

#***Number of water bodies ####
#two values in 'n' non numeric... 21.2 million, and 'variable'
multi_n_check = multiponds %>% select(n,notes)
unique(multiponds$n)
# fix non-numeric values, add 'check_notes_for' column to add notes to check for
multiponds = multiponds %>% 
  mutate(n=replace(n,n=="48 (divided 3 grous)",48)) %>% #change to numeric
  mutate(n=replace(n,n=="49 (divided 3 grous)",49)) %>% #change to numeric
  mutate(n=replace(n,n=="50 (divided 3 grous)",50)) %>% #change to numeric 
  mutate(n=replace(n,n=="variable",103)) %>% #change 'variable' to 103, the maximum number for variable n for different metrics 
  mutate(check_notes_for=ifelse(n=="200*",'contact author for size info',NA)) %>% # new column, flag "200*" for contact author (from notes) 
  mutate(n=replace(n,n=="200*",200)) %>%  #change to numeric 
  mutate(n=replace(n,n=="83*",83))  %>% #change to numeric, don't need to flag
  filter(!n=="21.2 million" & !n=="8.4 million polygons") #remove these big ones
#coerce to numeric
multiponds$n = as.numeric(multiponds$n) #none removed, good
  str(multiponds)
  
#coerce
multiponds$n = as.numeric(multiponds$n)
#check
temp_check = plyr::count(multiponds$n)
str(multiponds$n)

#***Depth ####
#min depth
temp = plyr::count(multiponds$min_depth_m) #need to use plyr::count instead of dplyr, couldn't work out errors
#summary
summary(multiponds$min_depth_m)
#box plot
ggplot(multiponds,aes(x="",y=min_depth_m)) + #need blank 'x' to add geom_jitter
  geom_boxplot(color='black',outlier.shape = NA) + #hide outliers duplicated by geom_jitter
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_depth_m,na.rm=TRUE)),color='red') + #3 std.dev 
  ggtitle('min_depth_m - multi')

#max depth
temp = plyr::count(multiponds$max_depth_m) 
#histogram 
ggplot(multiponds,aes(max_depth_m)) +
  ggtitle('max depth - multi') +
  geom_histogram(color='gray') 
#summary
summary(multiponds$max_depth_m)
#box plot
ggplot(multiponds,aes(x="",y=max_depth_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_depth_m,na.rm=TRUE)),color='red') + 
  ggtitle('max depth - multi')

#mean depth
temp = plyr::count(multiponds$mean_depth_m) 
#coerce 
multiponds$mean_depth_m = as.numeric(multiponds$mean_depth_m)
#histogram
ggplot(multiponds,aes(mean_depth_m))+
  scale_x_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  ggtitle('mean depth - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds$mean_depth_m)
#box plot
ggplot(multiponds,aes(x="",y=mean_depth_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_depth_m,na.rm=TRUE)),color='red') + 
  ggtitle('mean depth - multi')

#median depth
temp = plyr::count(multiponds$median_depth_m) 
#histogram
ggplot(multiponds,aes(median_depth_m))+
  scale_x_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  ggtitle('median depth - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds$median_depth_m)
#box plot
#regenerate plot from above
ggplot(multiponds,aes(x="",y=median_depth_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(median_depth_m,na.rm=TRUE)),color='red') + 
  ggtitle('median depth - multi')

#sd depth
temp = plyr::count(multiponds$stdev_depth_m) 
#coerce
multiponds$stdev_depth_m = as.numeric(multiponds$stdev_depth_m)
#histogram
ggplot(multiponds,aes(stdev_depth_m))+
  ggtitle('median depth - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds$stdev_depth_m)
#box plot
ggplot(multiponds,aes(x="",y=stdev_depth_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(stdev_depth_m,na.rm=TRUE)),color='red') + 
  ggtitle('sd depth - multi')

#***Surface Area ####
#min surface area
temp = plyr::count(multiponds$min_surfacearea_m2) 
#coerce
multiponds$min_surfacearea_m2 = as.numeric(multiponds$min_surfacearea_m2)
#histogram
ggplot(multiponds,aes(min_surfacearea_m2))+
  scale_x_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)) +
  ggtitle('min surface area - multi')+
  geom_histogram(color='light gray') 
#summary
summary(multiponds$min_surfacearea_m2)
#box plot
ggplot(multiponds,aes(x="",y=min_surfacearea_m2)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_surfacearea_m2,na.rm=TRUE)),color='red') + 
  ggtitle('min surface area - multi')

#max s.a.
temp = plyr::count(multiponds$max_surfacearea_m2) 
#coerce
multiponds$max_surfacearea_m2 = as.numeric(multiponds$max_surfacearea_m2)
#histogram
ggplot(multiponds,aes(max_surfacearea_m2))+
  scale_x_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  ggtitle('max surface area - multi')+
  geom_histogram(color='light gray') 
#summary
summary(multiponds$max_surfacearea_m2)
#box plot
ggplot(multiponds,aes(x="",y=max_surfacearea_m2)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_surfacearea_m2,na.rm=TRUE)),color='red') + 
  ggtitle('max surface area - multi')

#mean s.a.
temp = plyr::count(multiponds$mean_surfacearea_m2) 
#histogram
ggplot(multiponds,aes(mean_surfacearea_m2))+
  scale_x_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  ggtitle('mean surface area - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds$mean_surfacearea_m2)
#box plot
ggplot(multiponds,aes(x="",y=mean_surfacearea_m2)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_surfacearea_m2,na.rm=TRUE)),color='red') + 
  ggtitle('mean surface area - multi')

#median s.a.
temp = plyr::count(multiponds$median_surfacearea_m2) 
#histogram
ggplot(multiponds,aes(median_surfacearea_m2))+
  scale_x_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  ggtitle('mean surface area - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds$median_surfacearea_m2)
#box plot
ggplot(multiponds,aes(x="",y=median_surfacearea_m2)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(median_surfacearea_m2,na.rm=TRUE)),color='red') + 
  ggtitle('median surface area - multi')

#sd s.a.
temp = plyr::count(multiponds$stdev_surfacearea_m2) 
#coerce
multiponds$stdev_surfacearea_m2 = as.numeric(multiponds$stdev_surfacearea_m2)
#histogram
ggplot(multiponds,aes(stdev_surfacearea_m2))+
  scale_x_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  ggtitle('mean surface area - multi')+
  geom_histogram(color='gray')
#summary
summary(multiponds$stdev_surfacearea_m2)
#box plot
ggplot(multiponds,aes(x="",y=stdev_surfacearea_m2)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(stdev_surfacearea_m2,na.rm=TRUE)),color='red') + 
  ggtitle('sd surface area - multi')

#***Temperature ####
#delete
multiponds = multiponds %>% select(-min_temp_c,-max_temp_c,-mean_temp_c,-median_temp_c,-stdev_temp_c) #delete columns

#***DOC ####
#not a lot of data... delete?
#min doc
temp = plyr::count(multiponds$min_doc_mgpl) 
#summary
summary(multiponds$min_doc_mgpl)
#boxplot
ggplot(multiponds,aes(x="",y=min_doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('min doc - multi')

#max doc
temp = plyr::count(multiponds$max_doc_mgpl) 
#summary
summary(multiponds$max_doc_mgpl)
#boxplot
ggplot(multiponds,aes(x="",y=max_doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('max doc - multi')

#mean doc
temp = plyr::count(multiponds$mean_doc_mgpl) 
#summary
summary(multiponds$mean_doc_mgpl)
#boxplot
ggplot(multiponds,aes(x="",y=mean_doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('mean doc - multi')

#median doc
temp = plyr::count(multiponds$median_doc_mgpl) 
#coerce
multiponds$median_doc_mgpl = as.numeric(multiponds$median_doc_mgpl)
#summary
summary(multiponds$median_doc_mgpl)
#boxplot
ggplot(multiponds,aes(x="",y=median_doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(median_doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('median doc - multi')

#sd doc
temp = plyr::count(multiponds$stdev_doc_mgpl) 
#summary
summary(multiponds$stdev_doc_mgpl)
#boxplot
ggplot(multiponds,aes(x="",y=stdev_doc_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(stdev_doc_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('sd doc - multi')

#***Chlorophyll ####
#min
temp = plyr::count(multiponds$min_chla_ugpl) 
#summary
summary(multiponds$min_chla_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=min_chla_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_chla_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('min_chla - multi')

#max
temp = plyr::count(multiponds$max_chla_ugpl) 
#summary
summary(multiponds$max_chla_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=max_chla_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_chla_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('max chla - multi')

#mean
temp = plyr::count(multiponds$mean_chla_ugpl) 
#summary
summary(multiponds$mean_chla_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=mean_chla_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_chla_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('mean chla - multi')

#median
temp = plyr::count(multiponds$median_chla_ugpl) 
#summary
summary(multiponds$median_chla_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=median_chla_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(median_chla_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('median chla - multi')

#sd
temp = plyr::count(multiponds$stdev_chla_ugpl) 
#summary
summary(multiponds$stdev_chla_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=stdev_chla_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(stdev_chla_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('sd chla - multi')

#***Nutrients ####
#TP min
temp = plyr::count(multiponds$min_tp_ugpl) 
#coerce
multiponds$min_tp_ugpl = as.numeric(multiponds$min_tp_ugpl)
#summary
summary(multiponds$min_tp_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=min_tp_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_tp_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('min_tp_ugpl - multi')

#max
temp = plyr::count(multiponds$max_tp_ugpl) 
#coerce
multiponds$max_tp_ugpl = as.numeric(multiponds$max_tp_ugpl)
#summary
summary(multiponds$min_tp_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=min_tp_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_tp_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('min_tp_ugpl - multi')

#mean
temp = plyr::count(multiponds$mean_tp_ugpl) 
#summary
summary(multiponds$mean_tp_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=mean_tp_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_tp_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('mean_tp_ugpl - multi')

#median
temp = plyr::count(multiponds$median_tp_ugpl) 
#coerce
multiponds$median_tp_ugpl = as.numeric(multiponds$median_tp_ugpl)
#summary
summary(multiponds$median_tp_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=median_tp_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(median_tp_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('median_tp_ugpl - multi')

#sd
temp = plyr::count(multiponds$stdev_tp_ugpl) 
#summary
summary(multiponds$stdev_tp_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=stdev_tp_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(stdev_tp_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('stdev_tp_ugpl - multi')

#TN
#min
temp = plyr::count(multiponds$min_tn_ugpl) 
#coerce
multiponds$min_tn_ugpl = as.numeric(multiponds$min_tn_ugpl)
#summary
summary(multiponds$min_tn_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=min_tn_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_tn_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('min_tn_ugpl - multi')

#max
temp = plyr::count(multiponds$max_tn_ugpl) 
#summary
summary(multiponds$min_tn_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=min_tn_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_tn_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('min_tn_ugpl - multi')

#mean
temp = plyr::count(multiponds$mean_tn_ugpl) 
#summary
summary(multiponds$mean_tn_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=mean_tn_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_tn_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('mean_tn_ugpl - multi')

#median
temp = plyr::count(multiponds$median_tn_ugpl) 
#coerce
multiponds$median_tn_ugpl = as.numeric(multiponds$median_tn_ugpl)
#summary
summary(multiponds$median_tn_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=median_tn_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(median_tn_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('median_tn_ugpl - multi')

#sd
temp = plyr::count(multiponds$stdev_tn_ugpl) 
#summary
summary(multiponds$stdev_tn_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=stdev_tn_ugpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(stdev_tn_ugpl,na.rm=TRUE)),color='red') + 
  ggtitle('stdev_tn_ugpl - multi')

#***Conductivity ####
#min
temp = plyr::count(multiponds$min_cond_uspcm) 
#summary
summary(multiponds$min_cond_uspcm)
#boxplot
ggplot(multiponds,aes(x="",y=min_cond_uspcm)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_cond_uspcm,na.rm=TRUE)),color='red') + 
  ggtitle('min cond - multi')

#max
temp = plyr::count(multiponds$max_cond_uspcm) 
#summary
summary(multiponds$max_cond_uspcm)
#boxplot
ggplot(multiponds,aes(x="",y=max_cond_uspcm)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_cond_uspcm,na.rm=TRUE)),color='red') + 
  ggtitle('max cond - multi')

#mean
temp = plyr::count(multiponds$mean_cond_uspcm) 
#summary
summary(multiponds$mean_cond_uspcm)
#boxplot
ggplot(multiponds,aes(x="",y=mean_cond_uspcm)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_cond_uspcm,na.rm=TRUE)),color='red') + 
  ggtitle('mean cond - multi')

#median
temp = plyr::count(multiponds$median_cond_uspcm) 
#coerce
multiponds$median_cond_uspcm = as.numeric(multiponds$median_cond_uspcm)
#summary
summary(multiponds$median_cond_uspcm)
#boxplot
ggplot(multiponds,aes(x="",y=median_cond_uspcm)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(median_cond_uspcm,na.rm=TRUE)),color='red') + 
  ggtitle('median cond - multi')

#sd
temp = plyr::count(multiponds$stdev_cond_uspcm) 
#coerce
multiponds$stdev_cond_uspcm = as.numeric(multiponds$stdev_cond_uspcm)
#summary
summary(multiponds$stdev_cond_uspcm)
#boxplot
ggplot(multiponds,aes(x="",y=stdev_cond_uspcm)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(stdev_cond_uspcm,na.rm=TRUE)),color='red') + 
  ggtitle('sd cond - multi')

#***pH ####
#min
temp = plyr::count(multiponds$min_ph) 
#summary
summary(multiponds$min_chla_ugpl)
#boxplot
ggplot(multiponds,aes(x="",y=min_ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  #scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_ph,na.rm=TRUE)),color='red') + 
  ggtitle('min ph - multi')

#max
temp = plyr::count(multiponds$max_ph) 
#summary
summary(multiponds$max_ph)
#boxplot
ggplot(multiponds,aes(x="",y=max_ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  ggtitle('max ph - multi')

#mean
temp = plyr::count(multiponds$mean_ph) 
#summary
summary(multiponds$mean_ph)
#boxplot
ggplot(multiponds,aes(x="",y=mean_ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  ggtitle('mean ph - multi')

#median
temp = plyr::count(multiponds$median_ph) 
#coerce
multiponds$median_ph = as.numeric(multiponds$median_ph)
#summary
summary(multiponds$median_ph)
#boxplot
ggplot(multiponds,aes(x="",y=median_ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  ggtitle('median ph - multi')

#sd
temp = plyr::count(multiponds$stdev_ph) 
#summary
summary(multiponds$stdev_ph)
#boxplot
ggplot(multiponds,aes(x="",y=stdev_ph)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  ggtitle('sd ph - multi')

#***Turbidity ####
#Secchi
#min
temp = plyr::count(multiponds$min_turbidity_secchi_m) 
#summary
summary(multiponds$min_turbidity_secchi_m)
#boxplot
ggplot(multiponds,aes(x="",y=min_turbidity_secchi_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  #scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_turbidity_secchi_m,na.rm=TRUE)),color='red') + 
  ggtitle('min_turbidity_secchi_m - multi')

#max
temp = plyr::count(multiponds$max_turbidity_secchi_m) 
#summary
summary(multiponds$max_turbidity_secchi_m)
#boxplot
ggplot(multiponds,aes(x="",y=max_turbidity_secchi_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  #scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_turbidity_secchi_m,na.rm=TRUE)),color='red') + 
  ggtitle('max_turbidity_secchi_m - multi')

#mean
temp = plyr::count(multiponds$mean_turbidity_secchi_m) 
#coerce
multiponds$mean_turbidity_secchi_m = as.numeric(multiponds$mean_turbidity_secchi_m)
#summary
summary(multiponds$mean_turbidity_secchi_m)
#boxplot
ggplot(multiponds,aes(x="",y=mean_turbidity_secchi_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  #scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_turbidity_secchi_m,na.rm=TRUE)),color='red') + 
  ggtitle('mean_turbidity_secchi_m - multi')

#sd
temp = plyr::count(multiponds$stdev_turbidity_secchi_m) 
#summary
summary(multiponds$stdev_turbidity_secchi_m)
#boxplot
ggplot(multiponds,aes(x="",y=stdev_turbidity_secchi_m)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(stdev_turbidity_secchi_m,na.rm=TRUE))),color='red') + 
  ggtitle('stdev_turbidity_secchi_m - multi')

#NTUs
#min
temp = plyr::count(multiponds$min_turbidity_ntu) 
#summary
summary(multiponds$min_turbidity_ntu)
#boxplot
ggplot(multiponds,aes(x="",y=min_turbidity_ntu)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_turbidity_ntu,na.rm=TRUE)),color='red') + 
  ggtitle('min_turbidity_ntu - multi')

#max
temp = plyr::count(multiponds$max_turbidity_ntu) 
#summary
summary(multiponds$max_turbidity_ntu)
#boxplot
ggplot(multiponds,aes(x="",y=max_turbidity_ntu)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_turbidity_ntu,na.rm=TRUE)),color='red') + 
  ggtitle('max_turbidity_ntu - multi')

#mean
temp = plyr::count(multiponds$mean_turbidity_ntu) 
#summary
summary(multiponds$mean_turbidity_ntu)
#boxplot
ggplot(multiponds,aes(x="",y=mean_turbidity_ntu)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_turbidity_ntu,na.rm=TRUE)),color='red') + 
  ggtitle('mean_turbidity_ntu - multi')

#median
temp = plyr::count(multiponds$median_turbidity_ntu) 
#coerce
multiponds$median_turbidity_ntu = as.numeric(multiponds$median_turbidity_ntu)
#summary
summary(multiponds$median_turbidity_ntu)
#boxplot
ggplot(multiponds,aes(x="",y=median_turbidity_ntu)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  ggtitle('median_turbidity_ntu - multi')

#sd
temp = plyr::count(multiponds$stdev_turbidity_ntu) 
#summary
summary(multiponds$stdev_turbidity_ntu)
#boxplot
ggplot(multiponds,aes(x="",y=stdev_turbidity_ntu)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=log(3*sd(stdev_turbidity_ntu,na.rm=TRUE))),color='red') + 
  ggtitle('stdev_turbidity_ntu - multi')

#***TSS ####
#min
temp = plyr::count(multiponds$min_tss_mgpl) 
#summary
summary(multiponds$min_tss_mgpl)
#boxplot
ggplot(multiponds,aes(x="",y=min_tss_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(min_tss_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('min_tss_mgpl - multi')

#max
temp = plyr::count(multiponds$max_tss_mgpl) 
#summary
summary(multiponds$max_tss_mgpl)
#boxplot
ggplot(multiponds,aes(x="",y=max_tss_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(max_tss_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('max_tss_mgpl - multi')

#mean
temp = plyr::count(multiponds$mean_tss_mgpl) 
#summary
summary(multiponds$mean_tss_mgpl)
#boxplot
ggplot(multiponds,aes(x="",y=mean_tss_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  geom_hline(aes(yintercept=3*sd(mean_tss_mgpl,na.rm=TRUE)),color='red') + 
  ggtitle('mean_tss_mgpl - multi')

#median
temp = plyr::count(multiponds$median_tss_mgpl) 
#coerce
multiponds$median_tss_mgpl = as.numeric(multiponds$median_tss_mgpl)
#summary
summary(multiponds$median_tss_mgpl)
#boxplot
ggplot(multiponds,aes(x="",y=median_tss_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  geom_jitter(position=position_jitter(width=0.2, height=0)) +
  ggtitle('median_tss_mgpl - multi')

#sd
temp = plyr::count(multiponds$stdev_tss_mgpl) 
#summary
summary(multiponds$stdev_tss_mgpl)
#boxplot
ggplot(multiponds,aes(x="",y=stdev_tss_mgpl)) +
  geom_boxplot(color='black',outlier.shape = NA) + 
  scale_y_continuous(trans='log', breaks=c(0.1, 1, 10, 100, 1000, 10000, 100000)) +
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


#***Human built / manipulation ####
plyr::count(multiponds$humanbuilt_manipulated) 
#change 'human built' and 'both' to "y"
multiponds = multiponds %>%
  mutate(humanbuilt_manipulated=replace(humanbuilt_manipulated,grepl(",",humanbuilt_manipulated) | grepl("natural",humanbuilt_manipulated),"both")) %>%  #change to 'both'
  mutate(check_notes_for=replace(check_notes_for,grepl('manip',notes) | grepl('built',notes),paste(check_notes_for,'manipulation or human built',sep=', ',collapse=NULL)))  #flag notes that contain 'use' to check  notes for ponduse AND OR landuse


#***Ponduse ####
#save original list
m_ponduse_start = plyr::count(multiponds$ponduse) 
#save ponduse from metadata
m_ponduse_md = unique(multimd$ponduse)
#no 'others' to save
#workflow:
 #generate column 'check_notes_for' and append with key word 'pertaining to use' to each column
 #change multiple values (i.e: water retention or recreation) to 'multiple' in ponduse column
 #ignore warning 'number of items to replace is not a multiple of replacement length', that makes sense. 
multiponds = multiponds %>% 
  mutate(ponduse=replace(ponduse,ponduse=="none",NA)) %>% #change 'none' to NA
  mutate(ponduse=replace(ponduse,ponduse=="irrigation",'farm')) %>% #change 'irrigation' to 'farm'
  mutate(ponduse=replace(ponduse,grepl('recreation',ponduse) | grepl('-',ponduse),'multiple')) %>%  # change values >1 to 'multiple'
  mutate(check_notes_for=replace(check_notes_for,grepl('other',ponduse) | grepl('multiple',ponduse),paste(check_notes_for,'ponduse other or multiple',sep=', ',collapse=NULL)))  %>% # check for 'other' or 'multiple', if so, paste in check_notes column
  mutate(check_notes_for=replace(check_notes_for,grepl('use',notes),paste(check_notes_for,'pond or land use',sep=', ',collapse=NULL)))  #flag notes that contain 'use' to check  notes for ponduse AND OR landuse
#check   
plyr::count(multiponds$ponduse) 
m_ponduse_start


#***Land use ####
#save original list
m_landuse_start = plyr::count(multiponds$landuse) 
#save ponduse from metadata
m_landuse_md = unique(multimd$landuse)
#workflow:
 #append check_notes column for other/multiple values
 #change multiples (i.e:forested/agricultural) to multiple
 #change non-metadata single values to 'other'
multiponds = multiponds %>% 
  mutate(check_notes_for=replace(check_notes_for,grepl('other',landuse) | grepl('multiple',landuse),paste(check_notes_for,'landuse other or multiple',sep=', ',collapse=NULL))) %>%   # check for 'other' or 'multiple', if so, paste in check_notes column
  mutate(landuse=replace(landuse,grepl('/',landuse) | grepl('-',landuse),'multiple')) %>%   # change multiples to 'multiple'
  mutate(landuse=replace(landuse,!landuse %in% m_landuse_md,'other'))  # if landnduse value != metadata category, change to 'other'
#check  
plyr::count(multiponds$landuse) 
m_landuse_start


#***Managed ####
#check count of unique answers. only y, n, NA.
plyr::count(multiponds$managed) 
#flag use of word 'manage' in notes. 
multiponds = multiponds %>% 
  mutate(check_notes_for=replace(check_notes_for,grepl('manage',notes),paste(check_notes_for,'manage',sep=', ',collapse=NULL))) # flag notes that contain 'manage' 


#***Surface water connectivity ####
#check unique values
plyr::count(multiponds$surfacewaterconnectivity) 
#metadata categories
m_surfacewaterconnectivity_md = unique(multimd$surfacewaterconnectivity)
#workflow:
 #fix a few
 #append check_notes column for other/multiple values
multiponds = multiponds %>% 
  mutate(surfacewaterconnectivity=replace(surfacewaterconnectivity,grepl('/',surfacewaterconnectivity),'isolated')) %>%  # changes 'isolated (no inflow/ourflow)' to just 'isolated'
  mutate(surfacewaterconnectivity=replace(surfacewaterconnectivity,grepl('and',surfacewaterconnectivity),'multiple')) %>%  # changes multi to 'multiple'
  mutate(surfacewaterconnectivity=replace(surfacewaterconnectivity,surfacewaterconnectivity=='n',NA)) %>%  # changes n to 'NA'
  mutate(check_notes_for=replace(check_notes_for,grepl('connectivity',notes),paste(check_notes_for,'connectivity',sep=', ',collapse=NULL))) %>%    # flag 'connectivity' in notes
  mutate(check_notes_for=replace(check_notes_for,grepl('multiple',surfacewaterconnectivity),paste(check_notes_for,'connectivity - multiple',sep=', ',collapse=NULL)))   # check for 'multiple', check notes
#check
plyr::count(multiponds$surfacewaterconnectivity) 


#***Hydrology ####
#count unique values
plyr::count(multiponds$hydrology) 
#metadata categories
unique(multimd$hydrology)
#workflow:
 #fix typos
 #append check_notes for 'hydro'
 #change 'seasonal/intermittent' to 'multiple'
multiponds = multiponds %>% 
  mutate(hydrology=replace(hydrology,hydrology=='seasonal','intermittent')) %>%   #change seasonal to 'intermittent'
  mutate(hydrology=replace(hydrology,hydrology=='intermittment','intermittent')) %>%   #fix typo
  mutate(hydrology=replace(hydrology,grepl('/',hydrology) | grepl(',',hydrology) | grepl('both',hydrology),'multiple')) %>%   #change multiples to 'multiple'
  mutate(check_notes_for=replace(check_notes_for,grepl('hydro',notes),paste(check_notes_for,'hydro',sep=', ',collapse=NULL))) %>%    # flag 'hydro' in notes
  mutate(check_notes_for=replace(check_notes_for,grepl('multiple',hydrology),paste(check_notes_for,'hydrology - multiple',sep=', ',collapse=NULL)))   # check for 'multiple', check notes
#check
plyr::count(multiponds$hydrology) 
#check_notes check
plyr::count(multiponds$check_notes_for) 


#***Trophic Status ####
# check count of unique values
plyr::count(multiponds$trophic_status) 
#check metadata categories
unique(multimd$trophic_status)
#check n against range of trophic statuses
multi_trophstatus_check = multiponds %>% select(author,year,trophic_status,n,whatauthorscallit,pondname,notes) %>% filter(!is.na(trophic_status))
#workflow:
 #change slashes, dashes to "to" to denote range of statuses
 #change abbreviated to full term
 #append check_notes
multiponds = multiponds %>% 
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
plyr::count(multiponds$trophic_status) 


#***Fish presence ####
plyr::count(multiponds$fishpresence) 
#change non-yes or no to 'both'
multiponds = multiponds %>% 
  mutate(fishpresence=replace(fishpresence,!fishpresence=='y' & !fishpresence=='n','both')) %>% #change answers to 'both'
  mutate(check_notes_for=replace(check_notes_for,grepl('fish',notes),paste(check_notes_for,'fish',sep=', ',collapse=NULL)))  # flag 'fish' in notes
#check
plyr::count(multiponds$fishpresence) 


#***Macrophytes presence ####
plyr::count(multiponds$macrophytespresence) 
#change yes to y
#append notes for macrophytes
multiponds = multiponds %>% 
  mutate(macrophytespresence=replace(macrophytespresence,!macrophytespresence=='y' & !macrophytespresence=='n','both')) %>% #change answers to 'both'
  mutate(check_notes_for=replace(check_notes_for,grepl('macrophyte',notes) | grepl('plant',notes),paste(check_notes_for,'macrophytes or plants',sep=', ',collapse=NULL)))  # flag 'plant or macrophytes' in notes


#*Append check_notes ####
#for remaining numeric values
#check check notes
plyr::count(multiponds$check_notes_for) 
unique(multiponds$check_notes_for)
multiponds = multiponds %>% 
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
unique(multiponds$check_notes_for)


#* QC PART 2 ####
#check check_notes column in google sheet, make corrections here

#write working csv. Don't need to do this again!
#write.csv(multiponds, "Data_cleaned/Multipleponds_checknotes.csv")

##cleaning data after checking notes in google sheet. 
multiponds = multiponds %>% 
  mutate(n = replace(n,author=='wezel, a.; arthaud, f.; dufloux, c.; renoud, f.; vallod, d.; robin, j.; sarrazin, b.',59)) %>% #change n ponds
  filter(!location=='cremona province, lombardy, northern italy') %>%   #remove paper where data ALSO exists in singleponds database
  filter(!author=='martinsen et al.') %>%   #remove paper where data ALSO exists in singleponds database
  mutate(biomanipulation = ifelse(author=='cothran et al.','mesocosm',NA)) %>%   #new column to flag biomanipulation - this study was just mesocosms though, not entire pond
  mutate(biomanipulation = replace(biomanipulation, notes=='biomanipulated lakes (tp reduced; then biomanipulated)','biomanipulation')) %>%   #flag biomanipulation in jeppensen 2002 paper
  mutate(min_chla_ugpl = ifelse(author=="akasaka, munemitsu; takamura, noriko"&year==2011&journal=="oikos",2,min_chla_ugpl)) %>% #rescale chl units
  mutate(max_chla_ugpl = ifelse(author=="akasaka, munemitsu; takamura, noriko"&year==2011&journal=="oikos",106.5,max_chla_ugpl)) %>% #rescale chl units
  mutate(mean_chla_ugpl = ifelse(author=="akasaka, munemitsu; takamura, noriko"&year==2011&journal=="oikos",13.6,mean_chla_ugpl)) %>% #rescale chl units
  mutate(stdev_chla_ugpl = ifelse(author=="akasaka, munemitsu; takamura, noriko"&year==2011&journal=="oikos",16,stdev_chla_ugpl)) %>% #rescale chl units
  filter(!author=='fairchild') #remove paper with data also originally entered in both single and multi. adding back in as csv. below.
  #one paper (Fairchild et al 2006) was entered with size data in singleponds and water chem in multiponds. Min, max, mean, sd for the group of ponds was calculated externally, and is read back in as csv below. 
  fairchild2006 = read_csv('RawData/OtherData/FixedData_multiponds_fairchild2006.csv')
  fairchild2006 = fairchild2006 %>% select(-c(78:95)) %>%  mutate(biomanipulation = NA) #delete empty columns, empty biomanipulation col
  #combine multiponds with fixed entry
  multiponds = rbind(multiponds, fairchild2006)
  #fix menterey author spelling
  multiponds=multiponds %>% mutate(author = replace(author,author=="menetry","menetrey"))
  #remove special character in author name
  multiponds=multiponds %>% mutate(author = replace(author,year==2017&journal=="journal of limnology","markova et al."))
  
  
  
  
  
  
  
#**Subset data ####
#check multiponds structure
str(multiponds)
#52 rows with no size data
no_size_multi = multiponds %>% filter(is.na(min_surfacearea_m2) & is.na(mean_surfacearea_m2) & is.na(max_surfacearea_m2) & is.na(mean_surfacearea_m2) &
                                          is.na(min_depth_m) & is.na(max_depth_m) & is.na(mean_depth_m) & is.na(median_depth_m))
#save lakes >10m depth, and >1km2 surface area
#54 rows of large lakes
multiponds_large = multiponds %>% 
  filter(mean_depth_m > 10 | max_depth_m > 10 | mean_surfacearea_m2 > 10000000 | max_surfacearea_m2 > 10000000)


#remove rows where mean or median or max depth < 10, AND mean or median or max surface area < 1 km^2 AND is called a lake (or lakes) by authors
nrow(multiponds %>% filter((mean_depth_m < 10 | median_depth_m < 10 | max_depth_m < 10 | is.na(max_depth_m) == TRUE & is.na(median_depth_m) == TRUE & is.na(mean_depth_m) == TRUE)  & (mean_surfacearea_m2 < 10000000 | median_surfacearea_m2 < 10000000 | max_surfacearea_m2 < 10000000 | is.na(mean_surfacearea_m2==TRUE) & is.na(median_surfacearea_m2==TRUE) & is.na(max_surfacearea_m2==TRUE))))
#Check how many large lakes were removed
#n=17
nrow(multiponds)-nrow(multiponds %>% filter((mean_depth_m < 10 | median_depth_m < 10 | max_depth_m < 10 | is.na(max_depth_m) == TRUE & is.na(median_depth_m) == TRUE & is.na(mean_depth_m) == TRUE)  & (mean_surfacearea_m2 < 10000000 | median_surfacearea_m2 < 10000000 | max_surfacearea_m2 < 10000000 | is.na(mean_surfacearea_m2==TRUE) & is.na(median_surfacearea_m2==TRUE) & is.na(max_surfacearea_m2==TRUE))))
#Check how many have NA for both SA and depth
#n=52. don't delete 
multiponds %>% filter((is.na(max_depth_m) == TRUE & is.na(median_depth_m) == TRUE & is.na(mean_depth_m) == TRUE)  & (is.na(mean_surfacearea_m2==TRUE) & is.na(median_surfacearea_m2==TRUE) & is.na(max_surfacearea_m2==TRUE)))%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,mean_depth_m,median_depth_m,mean_surfacearea_m2,median_surfacearea_m2,max_surfacearea_m2)%>%print(n=Inf)
#Check for only the deepest and largest (with numbers in both)
#n=9
nrow(multiponds %>% filter((mean_depth_m > 10 | max_depth_m > 10 | median_depth_m > 10)  & (mean_surfacearea_m2 > 10000000 | median_surfacearea_m2 > 10000000 | max_surfacearea_m2 > 10000000 ))%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,mean_depth_m,median_depth_m,mean_surfacearea_m2,median_surfacearea_m2,max_surfacearea_m2)%>%print(n=Inf))
#Check for only the deepest 
#n=25
nrow(multiponds %>% filter((mean_depth_m > 10 | median_depth_m > 10 | max_depth_m > 10))%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,median_depth_m,mean_depth_m,mean_surfacearea_m2,median_surfacearea_m2,max_surfacearea_m2)%>%print(n=Inf))
#Check for only the largest 
#n=25
nrow(multiponds %>% filter((mean_surfacearea_m2 > 10000000 | median_surfacearea_m2 > 10000000 | max_surfacearea_m2 > 10000000 ))%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,median_depth_m,mean_depth_m,mean_surfacearea_m2,median_surfacearea_m2,max_surfacearea_m2)%>%print(n=Inf))


#Create a depth dummy variable that is -999 if there is no depth and the max of max and mean depth if there is at least 1.
#n=195 rows
multi_ponds_noLarge<-multiponds%>%
  mutate(depthDummy=ifelse((is.na(max_depth_m) == TRUE & is.na(median_depth_m) == TRUE & is.na(mean_depth_m) == TRUE),-999,pmax(mean_depth_m,median_depth_m,max_depth_m,na.rm=T)))%>% #depthDummy is -999 if NAs in both, otherwise the max
  mutate(areaDummy=ifelse((is.na(mean_surfacearea_m2) == TRUE & is.na(median_surfacearea_m2) == TRUE & is.na(max_surfacearea_m2) == TRUE),-999,pmax(mean_surfacearea_m2,median_surfacearea_m2,max_surfacearea_m2,na.rm=T))) %>% #areaDummy is -999 if NAs in both, otherwise the max
  filter(!(depthDummy>10&areaDummy>10000000))%>% #remove big and deep
  filter(!(depthDummy==-999&areaDummy>10000000))%>% #remove large but NA (-999) for depth
  filter(!(depthDummy>10&areaDummy==-999)) #remove deep but NA (-999) for area


#Find out the deep and big that remain
multi_ponds_noLarge%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,mean_depth_m,median_depth_m,mean_surfacearea_m2,median_surfacearea_m2,max_surfacearea_m2,depthDummy,areaDummy)%>%filter(depthDummy>10)%>%print(n=Inf)
multi_ponds_noLarge%>%dplyr::select(author,whatauthorscallit,pondname,max_depth_m,mean_depth_m,median_depth_m,mean_surfacearea_m2,median_surfacearea_m2,max_surfacearea_m2,depthDummy,areaDummy)%>%filter(areaDummy>10000000)%>%print(n=Inf)

                                  
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
temp1 = multiponds %>% 
  select(num) %>% 
  summarise_all(funs(sum(!is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="n") 
#min
temp2 = multiponds %>% 
  select(num) %>% 
  summarize_all(min,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="min") 
#max
temp3 = multiponds %>% 
  select(num) %>% 
  summarize_all(max,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="max") 
#mean
temp4 = multiponds %>% 
  select(num) %>% 
  summarize_all(mean,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="mean") 
#sd
temp5 = multiponds %>% 
  select(num) %>% 
  summarize_all(sd,na.rm=TRUE) %>% 
  pivot_longer(everything(), names_to = "variable", values_to="sd") 
#merge
multiponds_summary_numeric = merge(temp1,temp2,by="variable")
multiponds_summary_numeric = merge(multiponds_summary_numeric,temp3, by="variable")
multiponds_summary_numeric = merge(multiponds_summary_numeric,temp4, by="variable")
multiponds_summary_numeric = merge(multiponds_summary_numeric,temp5, by="variable")
#fix structure
str(multiponds_summary_numeric)
#as numeric
multiponds_summary_numeric[,2:6] = sapply(multiponds_summary_numeric[,2:6],as.numeric)
#fix scientific notation and decimals
multiponds_summary_numeric[,2:6] = format(scientific=F, round(multiponds_summary_numeric[,2:6], 3), nsmall=3)


#**Categorical summary table ####
#save author term summary
m_author_term = plyr::count(multiponds$whatauthorscallit) %>% dplyr::rename(author_term = x, author_term_n = freq) %>%  mutate(author_term=replace_na(author_term,'NA'))
#save humanbuilt summary
m_humanbuilt = plyr::count(multiponds$humanbuilt_manipulated) %>% dplyr::rename(humanbuilt_manipulated = x, humanbuilt_manipulated_n = freq) %>% mutate(humanbuilt_manipulated=replace_na(humanbuilt_manipulated,'NA')) 
#save ponduse summary
m_ponduse = plyr::count(multiponds$ponduse) %>% rename(ponduse = x, ponduse_n = freq) %>% mutate(ponduse=replace_na(ponduse,'NA')) 
#save landuse summary
m_landuse = plyr::count(multiponds$landuse) %>% rename(landuse = x, landuse_n = freq) %>% mutate(landuse=replace_na(landuse,'NA')) 
#save managed summary
m_managed =  plyr::count(multiponds$managed) %>% rename(managed = x, managed_n = freq) %>% mutate(managed=replace_na(managed,'NA')) 
#save connectivity summary
m_surfacewaterconnectivity = plyr::count(multiponds$surfacewaterconnectivity) %>% rename(surfacewaterconnectivity = x, surfacewaterconnectivity_n = freq) %>% mutate(surfacewaterconnectivity=replace_na(surfacewaterconnectivity,'NA')) 
#save hydrology summary
m_hydrology = plyr::count(multiponds$hydrology) %>% rename(hydrology = x, hydrology_n = freq) %>% mutate(hydrology=replace_na(hydrology,'NA')) 
#save trophic status summary
m_trophic_status = plyr::count(multiponds$trophic_status) %>% rename(trophic_status = x, trophic_status_n = freq) %>% mutate(trophic_status=replace_na(trophic_status,'NA')) 
#save fish presence summary
m_fishpresence = plyr::count(multiponds$fishpresence) %>% rename(fishpresence = x, fishpresence_n = freq) %>% mutate(fishpresence=replace_na(fishpresence,'NA'))
#save macrophyte summary
m_macrophytespresence = plyr::count(multiponds$macrophytespresence) %>% rename(macrophytespresence = x, macrophytespresence_n = freq) %>% mutate(macrophytespresence=replace_na(macrophytespresence,'NA')) 

#click on column heads to see total counts of each value, including NA's which are actually typed out as 'NA'
multiponds_summary_categorical = rbind.fill(m_author_term,m_managed,m_humanbuilt,m_ponduse,m_landuse,
                                             m_surfacewaterconnectivity,m_trophic_status,m_hydrology,m_fishpresence,m_macrophytespresence)



#**Save CSVs ####
#write.csv(multi_ponds_noLarge, "Data_cleaned/MultiplePonds_Cleaned.csv",row.names=FALSE)
#write.csv(multiponds_summary_categorical, "C:/Users/mfarr/Desktop/Multipleponds_summary_categorical.csv",row.names=FALSE)
#write.csv(multiponds_summary_numeric, "Data_cleaned/OtherCSVs/Multipleponds_summary_numeric.csv",row.names=FALSE)
#write.csv(no_size_multi, "Data_cleaned/OtherCSVs/Multipleponds_no_size_data.csv",row.names=FALSE)
#write.csv(multiponds_large, "Data_cleaned/OtherCSVs/Multipleponds_large.csv",row.names=FALSE)


#END
