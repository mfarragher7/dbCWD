# Quality Assurance / Quality Control to systematically clean up raw DataMining 
# Created 22May2020 by MJF

#Packages and libraries ####
#*Checks for packages ####
if(!require(plyr)){install.packages("plyr")}
if(!require(readr)){install.packages("readr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(tm)){install.packages("tm")}

#*Load libraries ####
library(plyr)
library(readr)
library(dplyr)
library(tidyverse)
library(tm)


#To-DO list (copy-and-pasted from meeting notes) ####

#Checking the structure
#str(df) - look for character columns where there should be numeric columns
#as.numeric(), seeing what transition from character to NA
#Checking columns that should be factors from metadata and ensuring that there are not extra entries....
#that we did not specify, move ones that are obvious to the right category, move others to the ‘other’ category
#Look for extreme numbers (e.g., conversion errors): 
#summary(df)
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/summary
#Histogram for each column, especially fishy ones
#Duplicate ponds / lakes? Might be published in multiple studies
#Combination of redundant names and lat/long analyses (rounding)
#Mapping part (KH) - see code 06-Mapping


#Read in metadata ####
#*SinglePonds metadata ####
SingleMd = read.csv("RawData/OtherData/Metadata_DataMining_SinglePonds.csv", na.strings=c("","NA"))
colnames(SingleMd) = c(NA,"VariableType","CategoriesWithinVar")
SingleMd = setNames(data.frame(t(SingleMd[,-1])), SingleMd[,1]) #transpose but keep first row as colnames
SingleMd[2,5:6] = NA #delete text under DefinitionOfPond and WhatAuthorsCallIt
SingleMd = mutate_all(SingleMd,tolower) # convert everything to lowercase, except colnames :(
#fix colnames, no spaces and capitalized nicely
singlenames = c("Author","Year","Journal","VolumeIssue","DefinitionOfPond","WhatAuthorsCallIt","Location",
  "Latitude_decimal","Longitude_decimal","PondName","HumanBuilt_Manipulated","PondUse","LandUse","Managed",
  "SurfaceWaterConnectivity","Hydrology","TrophicStatus","MinDepth_m","MaxDepth_m","MeanDepth_m",
  "MinSurfaceArea_m2","MaxSurfaceArea_m2","MeanSurfaceArea_m2","Volume_m3","MinTemp_C","MaxTemp_C","MeanTemp_C",
  "FishPresence","MacrophytesPresence","Macrophytes_percentcover","pH","Turbidity_Secchi_m","TSS_mg",
  "Color","Color_abs_440nm","DOC_mgpL","CDOM","Chla_ugpL","TP_ugpL","TN_ugpL","Cond_uSpcm", 
  "CanopyCover_percentofpond","OtherNotesAndUniquePoints","Initials-WhoReviewedPaper")  
colnames(SingleMd) = singlenames

#*MultiplePonds metadata ####
MultipleMd = read.csv("RawData/OtherData/Metadata_DataMining_MultiplePonds.csv", na.strings=c("","NA"))
colnames(MultipleMd) = c(NA,"VariableType","CategoriesWithinVar")
MultipleMd = setNames(data.frame(t(MultipleMd[,-1])), MultipleMd[,1])
MultipleMd[2,5:6] = NA #delete text under DefinitionOfPond and WhatAuthorsCallIt
colnames(MultipleMd)[23] = "Standard deviation depth_m" #rename two columns, they were both "Standard deviation_m" and I think it was causing issues
colnames(MultipleMd)[28] = "Standard deviation surface area_m"
MultipleMd = mutate_all(MultipleMd,tolower) # convert to lowercase
#fix colnames, removed spaces
multinames = c("Author","Year","Journal","VolumeIssue","DefinitionOfPond","WhatAuthorsCallIt",     
  "Location","n","Latitude_decimal","Longitude_decimal","PondName","HumanBuilt_Manipulated",   
  "PondUse","LandUse","Managed","SurfaceWaterConnectivity","Hydrology","TrophicStatus","MinDepth_m","MaxDepth_m","MeanDepth_m",                     
  "MedianDepth_m","StdDepth_m","MinSurfaceArea_m","MaxSurfaceArea_m","MeanSurfaceArea_m","MedianSurfaceArea_m",
  "StdSurfaceArea_m","MinDOC_mgpL","MaxDOC_mgpL","MeanDOC_mgpL","MedianDOC_mgpL","StdDOC_mgpL","MinChla_ugpL",
  "MaxChla_ugpL","MeanChla_ugpL","MedianChla_ugpL","StdChla_ugpL","MinTemp_C","MaxTemp_C","MeanTemp_C","MedianTemp_C",
  "StdTemp_C","MinTP_ugpL","MaxTP_ugpL","MeanTP_ugpL","MedianTP_ugpL","StdTP_ugpL","MinTN_ugpL","MaxTN_ugpL",
  "MeanTN_ugpL","MedianTN_ugpL","StdTN_ugpL","MinCond_uSpcm","MaxCond_uSpcm","MeanCond_uSpcm","MedianCond_uSpcm",                
  "StdCond_uSpcm","MinpH","MaxpH","MeanpH","MedianpH","StdpH","MinTurbidity_Secchi_m","MaxTurbidity_Secchi_m",
  "MeanTurbidity_Secchi_m","StdTurbidity_Secchi_m","MinTurb_NTU","MaxTurb_NTU","MeanTurb_NTU","MedianTurb_NTU",
  "StdTurb_NTU","MinTSS_mgl","MaxTSS_mgpl","MedianTSS_mgpl","MeanTSS_mgpl","SD TSS_mgpl","FishPresence",                 
  "MacrophytesPresence","OtherNotesAndUniquePoints","Initials_WhoReviewedPaper")
colnames(MultipleMd) = multinames



#Read in data
#SinglePonds ####
SinglePonds = read.csv("RawData/CombinedDataframes/Combined_SinglePonds.csv")
SinglePonds = SinglePonds[-c(1)] #drop first column of duplicated row numbers
SinglePonds = SinglePonds[,c(1:11,13,14,15,12,16:44)] #renumber columns to match metadata
SinglePonds = mutate_all(SinglePonds,tolower) # convert to lowercase
colnames(SinglePonds) = singlenames  

#Number of unique values in each column
rapply(SinglePonds,function(x)length(unique(x)))

#*Check categorical columns ####
# str(df) look for character columns where there should be numeric columns
str(SinglePonds)

#** Human built / manipulation ####
#column 11
unique(SinglePonds$HumanBuilt_Manipulated) # a few cases of both, manipulated, human built
length(grep("y", SinglePonds$HumanBuilt_Manipulated)) # number of y = 430 

SinglePonds[grepl("both",SinglePonds$HumanBuilt_Manipulated),11]= "y" #change to y
SinglePonds[grepl("human built",SinglePonds$HumanBuilt_Manipulated),11]= "y" # change to y. now number of y = 464

# how to treat manipulated ponds? 
SinglePonds[grep("manip", SinglePonds$OtherNotesAndUniquePoints), ]
# ^ here's one paper that reviewer MLS identified as biomanipulated in notes. 


#**Pond use #### 
#column 12
unique(SinglePonds$PondUse) # 










#MultiplePonds ####
MultiplePonds = read.csv("RawData/CombinedDataframes/Combined_MultiplePonds.csv")
MultiplePonds = MultiplePonds[-c(1)] #drop first column of duplicated row numbers
MultiplePonds = MultiplePonds[,c(1:63,65:78,64,79:81)]
MultiplePonds = mutate_all(MultiplePonds,tolower) 
colnames(MultiplePonds) = multinames              


