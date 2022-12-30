# This code compiles EPA Wetland Survey Data #
# Created by MAH on 19 October 2020 #


#Packages and libraries####
#*Checks if you have the package and if not, install it####
if(!require(plyr)){install.packages("plyr")}
if(!require(readr)){install.packages("readr")}
if(!require(dplyr)){install.packages("dplyr")}

#*Load libraries####
library(plyr)
library(readr)
library(dplyr)

#Data upload####
wetland_chem = read_csv("RawData/Wetlands/Wetlands_waterchem.csv") 
wetland_siteinfo1 = read_csv("RawData/Wetlands/Wetlands_siteinfo1.csv") 
wetland_siteinfo2 = read_csv("RawData/Wetlands/Wetlands_siteinfo2.csv") 
wetland_surfacewater = read_csv("RawData/Wetlands/Wetlands_surfacewater.csv") 
wetland_chla = read_csv("RawData/Wetlands/Wetlands_chla.csv") 


head(wetland_chem)
head(wetland_siteinfo1)
head(wetland_siteinfo2)
head(wetland_surfacewater)
head(wetland_chla)

str(wetland_siteinfo1)

#subset of the variables we want
wetland_chem1 <- wetland_chem[, c("UID", "SITE_ID", "SITE_USE", "STATE",
                              "COND", "NH3", "NO3NO2", "TKN", "TN", "TP")]
head(wetland_chem1)


wetland_siteinfo1a <- wetland_siteinfo1[, c("UID", "SITE_ID", "SITE_USE", "STATE",
                                            "AA_AREA", "DEPRESSION","HGM_CLASS")]
head(wetland_siteinfo1a)                                            

wetland_siteinfo2a <- wetland_siteinfo2[, c("UID", "SITE_ID", "SITE_USE", "STATE",
                                            "CLASS_FIELD_FWSST", "CLASS_DESIGN_FWSST",
                                            "CLASS_FIELD_HGM", "ECO_X_ST", "ECO_X_WETGRP")]
head(wetland_siteinfo2a)

wetland_surfacewater1 <- wetland_surfacewater[, c("UID", "SITE_ID", "SITE_USE", "STATE",
                                                  "CLASS_FIELD_FWSST","ECO_X_ST",
                                                  "AA_COVERAGE", "CLARITY_CLEAR", "CLARITY_MILKY",
                                                  "CLARITY_OTHER", "CLARITY_STAINED", "CLARITY_TURBID",
                                                  "CONDUCTIVITY", "DO", "PH", 
                                                  "SURFACE_WATER", "SW_DEPTH",
                                                  "TIDAL", "WATER_DEPTH", "WATER_PRESENCE", 
                                                  "WATER_SALINITY","WATER_TYPE_LAKE", 
                                                  "WATER_TYPE_OTHER", "WATER_TYPE_POND")]
head(wetland_surfacewater1)

wetland_chla1 <- wetland_chla[, c("UID", "SITE_ID", "SITE_USE", "STATE", "CHLA")]                                            
head(wetland_chla1)
  
  
#Merge datasets ####
wetland1 <- full_join(wetland_siteinfo1a, wetland_siteinfo2a) 
head(wetland1)
dim(wetland1) #1234,12
str(wetland1)

wetland2 <- full_join(wetland1, wetland_chem1) 
wetland3 <- full_join(wetland2, wetland_surfacewater1)
wetland <- full_join(wetland3, wetland_chla1)

dim(wetland) #1234,36
head(wetland)

#now wetland is a full dataset for all of the datasets



# remove sites without standing water
wetland_1 <- subset(wetland, WATER_PRESENCE == "Y")
dim(wetland_1) #822, 36

#select freshwater sites (removing salty)
wetland_2 <- subset(wetland_1, WATER_SALINITY=="FRESH")
dim(wetland_2) #465, 36

#remove tidal/river
str(wetland_2)
wetland_2$CLASS_FIELD_HGM <- as.factor(as.character(wetland_2$CLASS_FIELD_HGM))
levels(wetland_2$CLASS_FIELD_HGM)

wetland_3 <- subset(wetland_2, CLASS_FIELD_HGM=="DPRSS" | 
                      CLASS_FIELD_HGM=="FLATS" |
                      CLASS_FIELD_HGM=="FRINGE" |
                      CLASS_FIELD_HGM=="SLOPE")
dim(wetland_3) #295 sites

table(wetland_3$CLASS_FIELD_HGM)
table(wetland_3$CLASS_FIELD_FWSST)
#E2SS = estuarine shrub scrub-- need to remove*
#PEM = palustrine emergent
#PF = I think this is palustrine farmed
#PFO = palustrine forested
#PSS = palustrine scrub shrub 
#PUBPAB = Palustrine Unconsolidated Bottom/Aquatic Bed

wetland_4 <- subset(wetland_3, CLASS_FIELD_FWSST!="E2SS")
dim(wetland_4)#294, 37

wetland_4 %>% group_by(CLASS_FIELD_FWSST) %>% 
  summarize(meand_depth = mean(SW_DEPTH, na.rm = TRUE),
            meand_cond = mean(COND, na.rm = TRUE),
            meand_do = mean(DO, na.rm = TRUE),  #mg/L
            meand_ph = mean(PH, na.rm = TRUE),
            meand_tn = mean(TN, na.rm = TRUE),
            meand_tp = mean(TP, na.rm = TRUE),
            meand_chla = mean(CHLA, na.rm = TRUE),
            n=length(CLASS_FIELD_FWSST))

### Pond category
wetland_ponds <- subset(wetland_4, CLASS_FIELD_FWSST=="PUBPAB")
dim(wetland_ponds) #21, 37
21/1234 #1.7%

      