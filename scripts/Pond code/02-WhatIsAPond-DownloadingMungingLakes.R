#Code to read in public datasets  
#created by Katelyn King (KK) on Apr 24, 2020

#Packages and libraries####
#*Checks if you have the package and if not, install it####
if(!require(dplyr)){install.packages("dplyr")}
if(!require(LAGOSNE)){install.packages("LAGOSNE")}

#*Load libraries####
library(dplyr)
library(LAGOSNE)
#lagosne_get(dest_folder = lagos_path()) #lagosne_get downloads the latest version of data. only has to be downloaded one time per version per machine

# Download public datasets ####
#*add hydrolakes data (world) #### 
#Lake polygons (including all attributes) data was downloaded from https://www.hydrosheds.org/page/hydrolakes on Apr 24, 2020
#data file is too large to store in Git
#dat<-sf::st_read('/Users/katelynking/Desktop/HydroLAKES_v10/HydroLAKES_v10.gdb') %>% 
        # sf::st_drop_geometry() %>% #removes spatial attributes
        # select(Lake_name, Lake_area, Depth_avg, Pour_lat, Pour_long) %>% #select specific columns 
        # rename(PondName = Lake_name, "Mean surface area_m2" = Lake_area, 'Mean depth_m' = Depth_avg, 'Latitude (decimal degree)'= Pour_lat, 'Longitude (decimal degree)' = Pour_long) #rename columns

#convert area km2 to m2 
#dat$`Mean surface area_m2`<-dat$`Mean surface area_m2`* 1000000
#write.csv(dat,'RawData/SinglePonds/Hydrolakes.csv', row.names = FALSE) #write cleaned data into RawData folder of Git

dat<-read.csv('RawData/SinglePonds/Public_data/Hydrolakes.csv')

# create an empty data frame to save all of the output and name the columns the same as the other spreadsheets
output_df <-data.frame(matrix(NA, nrow = length(dat$PondName), ncol = 44))
colnames(output_df) <- c("Author","Year", "Journal", "Volume/Issue", "DefinitionOfPond", "What do the authors call it?", "Location", "Latitude (decimal degree)", "Longitude (decimal degree)", 
                         "PondName", "Human Built and/or Manipulated", "Surface water connectivity", 
                         "Pond use", "Land Use", "Managed", 'Hydrology', "TrophicStatus", "Min depth_m", "Max depth_m", "Mean depth_m", 
                         "Min Surface area_m2", "Max surface area_m2", "Mean surface area_m2", "Volume_m3", "Min Temp_C", "Max Temp_C", "Mean Temp_C", 
                         "Fish presence", "Macrophytes Presence", "Macrophytes_percentcover", "pH", "Turbidity_Secchi_m", "TSS_mgpL", "Color", "Color_abs_440nm", "DOC_mgpL", "CDOM", "Chla_ugpL", 
                         "TP_ugpL", "TN_ugpL", "Cond_uSpcm", "Canopy cover_percentofpond", "OtherNotesAndUniquePoints", "Initials-WhoReviewedPaper")

#add data from hydrolakes to the correct columns
output_df[,1] = "Messager, M.L., Lehner, B., Grill, G., Nedeva, I., Schmitt, O. "
output_df[,2] = 2016
output_df[,3] = "Nature Communications"
output_df[,4] = 13603
output_df[,8] = dat$Latitude..decimal.degrees.
output_df[,9] = dat$Longitude..decimal.degrees.
output_df[,10] = dat$PondName
output_df[,20] = dat$Mean.depth_m
output_df[,23] = dat$Mean.surface.area_m2


#*add NLA (US EPA) #### 
#data was downloaded from https://www.epa.gov/national-aquatic-resource-surveys/ on May 5, 2020
NLA_info<-read.csv("RawData/SinglePonds/Public_data/nla2012_wide_siteinfo_08232016.csv") %>% 
  select(UID, EVAL_NAME, LAT_DD83, LON_DD83, AREA_HA, LAKE_ORIGIN, STATUS, SITETYPE, INDEX_NLA) %>% 
  rename(PondName = EVAL_NAME, 'Latitude (decimal degree)'= LAT_DD83, 'Longitude (decimal degree)' = LON_DD83, "Mean surface area_m2" = AREA_HA, 'Human Built and/or Manipulated' =LAKE_ORIGIN) %>%
  filter(STATUS == "Target_Sampled") %>% #lakes that were actually sampled 
  filter(SITETYPE == "HAND" | INDEX_NLA == "Y") #remove duplicate lakes, HAND are the reference lakes and PROB (probability) lakes with index Y were used in the NLA analysis
#convert area ha to m2 
NLA_info$`Mean surface area_m2`<-NLA_info$`Mean surface area_m2`* 10000
#change names to match database
NLA_info<-NLA_info %>%
  mutate(`Human Built and/or Manipulated` = case_when(`Human Built and/or Manipulated` == 'MAN_MADE' ~ 'Y',   #if ~ then 
                                                      `Human Built and/or Manipulated` == 'NATURAL' ~ 'N' ))

NLA_info <- NLA_info[,-7:-9] #remove unwanted columns

NLA_chem<-read.csv("RawData/SinglePonds/Public_data/nla2012_waterchem_wide.csv")  %>% 
  select(UID, COND_RESULT, DOC_RESULT, PH_RESULT, PTL_RESULT, NTL_RESULT, TSS_RESULT) %>% #select columns Cond (uS/cm @ 25C), DOC (mg/L), pH, TP (ug/L), TN (mg/L), TSS (mg/L) 
  rename(Cond_uSpcm= COND_RESULT, DOC_mgpL= DOC_RESULT, pH=PH_RESULT, TP_ugpL= PTL_RESULT, TN_ugpL= NTL_RESULT, TSS_mg = TSS_RESULT)
NLA_chem$TN_ugpL<-NLA_chem$TN_ugpL*1000   #convert TN from mg to ug 

NLA_chla<-read.csv("RawData/SinglePonds/Public_data/nla2012_keyvariables_data.csv")  %>% #chla (ug/L), depth (m)
  select(UID, INDEX_SITE_DEPTH, CHLX_RESULT, TROPHIC_STATE, AMITOTAL) %>%
  rename('Max depth_m'=INDEX_SITE_DEPTH, Chla_ugpL = CHLX_RESULT, Trophic_status = TROPHIC_STATE, Macrophytes_percentcover=AMITOTAL)
NLA_chla$Macrophytes_percentcover<-NLA_chla$Macrophytes_percentcover*100   #convert from proportion to percent 

#change names to match database
NLA_chla<-NLA_chla %>%
  mutate(Trophic_status = case_when(Trophic_status == 'EUTROPHIC' ~ 'Eutrophic',   #if ~ then 
                                    Trophic_status == 'MESOTROPHIC' ~ 'Mesotrophic',
                                    Trophic_status == 'OLIGOTROPHIC' ~ 'Oligotrophic',
                                    Trophic_status == 'HYPEREUTROPHIC' ~ 'Hypereutrophic'))

#join tables 
NLA_dat<-left_join(NLA_info, NLA_chem) %>%
  left_join(NLA_chla)

NLA_dat <- NLA_dat[,-1] #remove UID

output_df <-data.frame(matrix(NA, nrow = length(NLA_dat$PondName), ncol = 44))
colnames(output_df) <- c("Author","Year", "Journal", "Volume/Issue", "DefinitionOfPond", "What do the authors call it?", "Location", "Latitude (decimal degree)", "Longitude (decimal degree)", 
                         "PondName", "Human Built and/or Manipulated", "Surface water connectivity", 
                         "Pond use", "Land Use", "Managed", 'Hydrology', "TrophicStatus", "Min depth_m", "Max depth_m", "Mean depth_m", 
                         "Min Surface area_m2", "Max surface area_m2", "Mean surface area_m2", "Volume_m3", "Min Temp_C", "Max Temp_C", "Mean Temp_C", 
                         "Fish presence", "Macrophytes Presence", "Macrophytes_percentcover", "pH", "Turbidity_Secchi_m", "TSS_mgpL", "Color", "Color_abs_440nm", "DOC_mgpL", "CDOM", "Chla_ugpL", 
                         "TP_ugpL", "TN_ugpL", "Cond_uSpcm", "Canopy cover_percentofpond", "OtherNotesAndUniquePoints", "Initials-WhoReviewedPaper")

#add data from NLA to the correct columns
output_df[,1] = "USEPA "
output_df[,2] = 2016
output_df[,8] = NLA_dat$`Latitude (decimal degree)`
output_df[,9] = NLA_dat$`Longitude (decimal degree)`
output_df[,10] = NLA_dat$PondName
output_df[,11] =NLA_dat$`Human Built and/or Manipulated`
output_df[,17] =NLA_dat$Trophic_status
output_df[,19] = NLA_dat$`Max depth_m`
output_df[,23] = NLA_dat$`Mean surface area_m2`
output_df[,30] =NLA_dat$Macrophytes_percentcover
output_df[,31] =NLA_dat$pH
output_df[,33] =NLA_dat$TSS_mg
output_df[,36] =NLA_dat$DOC_mgpL
output_df[,38] =NLA_dat$Chla_ugpL
output_df[,39] =NLA_dat$TP_ugpL
output_df[,40] =NLA_dat$TN_ugpL
output_df[,41] =NLA_dat$Cond_uSpcm

#write.csv(output_df,'RawData/SinglePonds/NLA.csv', row.names = FALSE)

#*add LAGOSNE version 1.087.3 ####
lg <- lagosne_load('1.087.3') #load data

#lake lat/lon and area 
lake_info<-lg$locus %>% 
  select(lagoslakeid, gnis_name, nhd_lat, nhd_long, lake_area_ha) %>%
  rename(PondName = gnis_name, 'Latitude (decimal degree)'= nhd_lat, 'Longitude (decimal degree)'= nhd_long)
#convert area ha to m2 
lake_info$`Mean surface area_m2`<-lake_info$lake_area_ha* 10000
lake_info <- lake_info[,-5] #remove area ha column

#connectivity data
lake_conn <- lg$lakes.geo %>%
  select(lagoslakeid,lakeconnection) %>%
  rename('Surface water connectivity' = lakeconnection)
#change connectivity names to match database
lake_conn<-lake_conn %>%
  mutate(`Surface water connectivity` = case_when(`Surface water connectivity` == 'DR_LakeStream' ~ 'Inline',   #if ~ then 
                                                  `Surface water connectivity` == 'DR_Stream' ~ 'Inline',
                                                  `Surface water connectivity` == 'Headwater' ~ 'Headwater',
                                                  `Surface water connectivity` == 'Isolated' ~ 'Isolated'))

#lake depth 
lake_depth<-lg$lakes_limno %>% 
  select(lagoslakeid, meandepth, maxdepth) %>%
  rename('Mean depth_m' = meandepth, 'Max depth_m'= maxdepth)

#nutrient and Secchi data # units are all correct 
epi_nutr <- lg$epi_nutr %>% 
  select(lagoslakeid, sampledate, tn, tp, chla, secchi, doc) %>%
  rename(TN_ugpL = tn, TP_ugpL= tp, Chla_ugpL= chla, Turbidity_Secchi_m = secchi, DOC_mgpL = doc)

# take the mean for samples collected between Jun 15 and Sep 15 (stratification period) from 1990-2011
first_year <- 1990
last_year  <- 2011
first_day  <- '0615' #i.e., '0615' for Jun 15
last_day   <- '0915'

epi_nutr$sampledate <- as.Date(epi_nutr$sampledate, format="%m/%d/%Y")
epi_nutr$monthday   <- format(epi_nutr$sampledate, format="%m%d")
epi_nutr$sampleyear <- format(epi_nutr$sampledate, format="%Y")
epi_nutr$sampleyear <- as.integer(epi_nutr$sampleyear)

# subset by sample date and year cutoffs specified above
epi_nutr_subset <- epi_nutr[epi_nutr$monthday >= first_day & epi_nutr$monthday <= last_day,]
epi_nutr_subset <- subset(epi_nutr_subset, sampleyear >= first_year & sampleyear <= last_year)

# calculate means for each water quality variable
nutr_means <- epi_nutr_subset %>%
  group_by(lagoslakeid) %>% 
  summarize(TP_ugpL=mean(TP_ugpL, na.rm=T),
            TN_ugpL=mean(TN_ugpL, na.rm=T),
            Chla_ugpL=mean(Chla_ugpL, na.rm=T),
            Turbidity_Secchi_m=mean(Turbidity_Secchi_m, na.rm=T), 
            DOC_mgpL=mean(DOC_mgpL, na.rm=T))

#join tables 
lagos_dat<-left_join(lake_info, lake_conn) %>%
  left_join(lake_depth)%>%
  left_join(nutr_means)

lagos_dat <- lagos_dat[,-1] #remove lagosid

# create empty data frame with correct column names and add LAGOS data 
output_df <-data.frame(matrix(NA, nrow = length(lagos_dat$PondName), ncol = 44))
colnames(output_df) <- c("Author","Year", "Journal", "Volume/Issue", "DefinitionOfPond", "What do the authors call it?", "Location", "Latitude (decimal degree)", "Longitude (decimal degree)", 
                         "PondName", "Human Built and/or Manipulated", "Surface water connectivity", 
                         "Pond use", "Land Use", "Managed", 'Hydrology', "TrophicStatus", "Min depth_m", "Max depth_m", "Mean depth_m", 
                         "Min Surface area_m2", "Max surface area_m2", "Mean surface area_m2", "Volume_m3", "Min Temp_C", "Max Temp_C", "Mean Temp_C", 
                         "Fish presence", "Macrophytes Presence", "Macrophytes_percentcover", "pH", "Turbidity_Secchi_m", "TSS_mgpL", "Color", "Color_abs_440nm", "DOC_mgpL", "CDOM", "Chla_ugpL", 
                         "TP_ugpL", "TN_ugpL", "Cond_uSpcm", "Canopy cover_percentofpond", "OtherNotesAndUniquePoints", "Initials-WhoReviewedPaper")

#add data from NLA to the correct columns
output_df[,1] = "Soranno et al"
output_df[,2] = 2017
output_df[,3] = "Gigascience" 
output_df[,4] = "6(12)"
output_df[,8] = lagos_dat$`Latitude (decimal degree)`
output_df[,9] = lagos_dat$`Longitude (decimal degree)`
output_df[,10] = lagos_dat$PondName
output_df[,12] = lagos_dat$`Surface water connectivity`
output_df[,19] = lagos_dat$`Max depth_m`
output_df[,20] = lagos_dat$`Mean depth_m`
output_df[,23] = lagos_dat$`Mean surface area_m2`
output_df[,32] =lagos_dat$Turbidity_Secchi_m
output_df[,36] =lagos_dat$DOC_mgpL
output_df[,38] =lagos_dat$Chla_ugpL
output_df[,39] =lagos_dat$TP_ugpL
output_df[,40] =lagos_dat$TN_ugpL

#write.csv(output_df,'RawData/SinglePonds/LAGOS.csv', row.names = FALSE)

#*add Waterbase (European Environmental Agency) ####
#data downloaded from https://www.eea.europa.eu/data-and-maps/data/waterbase-water-quality-2 on May 7, 2020

#MonitoringSite_DerivedData -- has lat, lon for each monitoring site
waterbase_info<-read.csv('RawData/Lakes/Public_data/Waterbase_v2018_1_WISE4_MonitoringSite_DerivedData.csv')

#AggregatedData -- Annual mean values and other statistics of determinants on water quality, by monitoring site
#data file is too large to store in Git
waterbase_agg<-read.csv('/Users/katelynking/Desktop/Waterbase_v2018_1_WISE4/Waterbase_v2018_1_T_WISE4_DisaggregatedData.csv') %>%
  #for aggregated use : select(monitoringSiteIdentifier, parameterWaterBodyCategory, observedPropertyDeterminandCode, resultUom, resultMeanValue, phenomenonTimeReferenceYear ) %>%
  select(monitoringSiteIdentifier, parameterWaterBodyCategory, observedPropertyDeterminandCode, phenomenonTimeSamplingDate, resultUom, resultObservedValue ) %>% 
  filter(parameterWaterBodyCategory == "LW") %>% #LW is lake water body
  filter(observedPropertyDeterminandCode == "CAS_7723-14-0" | observedPropertyDeterminandCode =='EEA_31615-01-7' |
           observedPropertyDeterminandCode == "EEA_31-02-7" | observedPropertyDeterminandCode == "EEA_3164-01-0" |
           observedPropertyDeterminandCode == "EEA_3111-01-1" | observedPropertyDeterminandCode == "EEA_3121-01-5" |
           observedPropertyDeterminandCode == "EEA_3133-05-9" | observedPropertyDeterminandCode == "EEA_3152-01-0" 
  ) 
waterbase_agg$observedPropertyDeterminandCode <- as.factor(as.character(waterbase_agg$observedPropertyDeterminandCode))

# take the mean for samples collected between Jun 15 and Sep 15 (stratification period) from 1990 to 2018
first_year <- 1990
last_year  <- 2018
first_day  <- '0615' #i.e., '0615' for Jun 15
last_day   <- '0915'

waterbase_agg$sampledate <- as.Date(waterbase_agg$phenomenonTimeSamplingDate, format="%Y-%m-%d")
waterbase_agg$monthday   <- format(waterbase_agg$sampledate, format="%m%d")
waterbase_agg$sampleyear <- format(waterbase_agg$sampledate, format="%Y")
waterbase_agg$sampleyear <- as.integer(waterbase_agg$sampleyear)

# subset by sample date and year cutoffs specified above
waterbase_subset <- waterbase_agg[waterbase_agg$monthday >= first_day & waterbase_agg$monthday <= last_day,]
waterbase_subset <- subset(waterbase_subset, sampleyear >= first_year & sampleyear <= last_year)

#data is in long format instead of wide, rotate table to get columns of parameters
waterbase_newformat<-waterbase_agg %>%
  tidyr::pivot_wider(id_cols = monitoringSiteIdentifier,
                    names_from = observedPropertyDeterminandCode,  #this puts parameters as columns
                     values_from= resultObservedValue,   #takes values and puts them in the rows 
                     values_fn= list(resultObservedValue = mean)) %>% #takes the means if there are multiple values for a site 
  rename(TN_ugpL = 'EEA_31615-01-7', TP_ugpL= "CAS_7723-14-0", Chla_ugpL= "EEA_3164-01-0", 
         Turbidity_Secchi_m = "EEA_3111-01-1", DOC_mgpL = "EEA_3133-05-9",
         pH='EEA_3152-01-0', TSS_mg = "EEA_31-02-7", 'Mean Temp_C'="EEA_3121-01-5")

#CAS_7723-14-0 = total phosphorus  (mg{P}/L)
#EEA_31615-01-7 = total nitrogen (mg{N}/L)
#EEA_31-02-7 = TSS (mg/L)
#EEA_3111-01-1 = Secchi depth (m)
#EEA_3121-01-5 = water temp (Cel)
#EEA_3133-05-9 = DOC (mg{C}/L) 
#EEA_3152-01-0 = pH ([pH])
#EEA_3164-01-0 = Chlorophyll a (ug/L)

#convert units- total phosphorus and total nitrogen from mg/L to ug/L, 
waterbase_newformat$TN_ugpL<-waterbase_newformat$TN_ugpL*1000 
waterbase_newformat$TP_ugpL<-waterbase_newformat$TP_ugpL*1000 

# merge nutrient data with lake info 
waterbase_dat<-left_join(waterbase_newformat, waterbase_info) 
#remove duplucated site IDs
waterbase_dat<-waterbase_dat %>% 
                    distinct(monitoringSiteIdentifier, .keep_all = TRUE) 

# create empty data frame with correct column names and add Waterbase data 
output_df <-data.frame(matrix(NA, nrow = length(waterbase_dat$monitoringSiteIdentifier), ncol = 44))
colnames(output_df) <- c("Author","Year", "Journal", "Volume/Issue", "DefinitionOfPond", "What do the authors call it?", "Location", "Latitude (decimal degree)", "Longitude (decimal degree)", 
                         "PondName", "Human Built and/or Manipulated", "Surface water connectivity", 
                         "Pond use", "Land Use", "Managed", 'Hydrology', "TrophicStatus", "Min depth_m", "Max depth_m", "Mean depth_m", 
                         "Min Surface area_m2", "Max surface area_m2", "Mean surface area_m2", "Volume_m3", "Min Temp_C", "Max Temp_C", "Mean Temp_C", 
                         "Fish presence", "Macrophytes Presence", "Macrophytes_percentcover", "pH", "Turbidity_Secchi_m", "TSS_mgpL", "Color", "Color_abs_440nm", "DOC_mgpL", "CDOM", "Chla_ugpL", 
                         "TP_ugpL", "TN_ugpL", "Cond_uSpcm", "Canopy cover_percentofpond", "OtherNotesAndUniquePoints", "Initials-WhoReviewedPaper")

#add data from Waterbase to the correct columns
output_df[,1] = "European Environment Agency"
output_df[,2] = 2019
output_df[,8] = waterbase_dat$lat
output_df[,9] = waterbase_dat$lon
output_df[,10] = waterbase_dat$waterBodyIdentifier
output_df[,27] =waterbase_dat$`Mean Temp_C`
output_df[,31] =waterbase_dat$pH
output_df[,32] =waterbase_dat$Turbidity_Secchi_m
output_df[,33] =waterbase_dat$TSS_mg
output_df[,36] =waterbase_dat$DOC_mgpL
output_df[,38] =waterbase_dat$Chla_ugpL
output_df[,39] =waterbase_dat$TP_ugpL
output_df[,40] =waterbase_dat$TN_ugpL

#write.csv(output_df,'RawData/SinglePonds/Waterbase.csv', row.names = FALSE)

