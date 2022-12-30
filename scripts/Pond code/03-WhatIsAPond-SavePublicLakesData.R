#Compile raw data csvs for single and multiple ponds

# YOU ONLY NEED TO RUN CODE FOR THE PUBLIC DATA AT THE BOTTOM. 
# the other script 04-WhatIsAPond-QAQC.R pulls data from combined csvs that already exist in our git master. 


#Created 2020-05-22 to replace old 02_CompileCSVs script
#Use tiered levels for navigation - end comment headers with four #, if it is a sub-level, add * at the beginning

# Combined dataframes were written out to "RawData/CombinedDataframes" Folder
# Shouldn't need to use this code anymore!

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
#*single ponds databses #### 
#temp = "RawData/SinglePonds"
#data = list.files(path=temp, pattern="*.csv", full.names=TRUE)
#single = ldply(data, read_csv)
#single = single[c(1:44)]  # remove blank columns after Initials column
#single.filtered = single %>% filter_all(any_vars(complete.cases(.)))  #remove completely blank rows

#1797 rows in complete single.filtered dataframe. 
#Tested the length of "Author" column in single and single.filtered and other parameters, all matched up
#length(which(!is.na(single$Author)))  # = 1797, one 'NA' when I used "unique(single.filtered$Author)"...
# length(which(!is.na(single.filtered$Author)))  # = 1797
#write.csv(single.filtered, "RawData/CombinedDataframes/Combined_SinglePonds.csv")

#*multiple ponds databases ####
#temp2 = "RawData/MultiplePonds"
#data2 = list.files(path=temp2, pattern="*.csv", full.names=TRUE)
#multiple = ldply(data2, read_csv)
#multiple.filtered = multiple %>% filter_all(any_vars(complete.cases(.)))  #remove completely blank rows
#write.csv(multiple.filtered, "RawData/CombinedDataframes/Combined_MultiplePonds.csv")

#273 rows in complete multiple.filtered dataframe
#623379 total ponds 
#n = as.numeric(multiple.filtered$n) #nPonds per row as numeric
#sum(n, na.rm=TRUE) # = 623379

#Tested the length of "Author" column in single and single.filtered and other columns. all matched
# length(which(!is.na(multiple$Author))) = 273
# length(which(!is.na(multiple.filtered$Author))) = 273


#*Saving Combined public data csv locally ####
#commented out everything, need to write combined csv to save locally
temp3 = "RawData/Lakes"
data3 = list.files(path=temp3, pattern="*.csv", full.names=TRUE) #read in LAGOS, NLA, Waterbase csvs
three_databases = ldply(data3, read_csv)
hydrolakes = read.csv("RawData/Lakes/Public_data/Hydrolakes.csv") #read in Hydrolakes csv
colnames(hydrolakes) = c("PondName","Mean surface area_m2","Mean depth_m",
                      "Latitude (decimal degree)","Longitude (decimal degree)")
public = rbind.fill(three_databases, hydrolakes) #complete public dataframe. no blank rows

# save csv locally!
write.csv(public, "C:/Users/Matt/Documents/Ecology/GLEON/Ponding/PublicData/Combined_PublicData.csv") 





