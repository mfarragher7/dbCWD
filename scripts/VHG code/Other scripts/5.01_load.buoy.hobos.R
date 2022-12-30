#Load hobo data 
#updated 2020-12-01

#load libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)



#For deployment log, see SampleLog.xls

#Jordan Pond
files = "dbBuoy/buoyJP/Hobos/2020-10-12" #save filepath
data = list.files(path=files, pattern="*.csv", full.names=TRUE) #list hobo csvs in vector
fileSub = str_remove(basename(data), "\\.csv$") #save file names
jp.hobo = imap_dfr(setNames(data, fileSub), ~ read_csv(.x,skip=2) %>% mutate(file = .y)) #read all csvs, minus first 2 lines, paste file names in column
#make temp column, grep jp0X, grep depth
jp.hobo = jp.hobo %>% 
  mutate_if(is.character, str_to_lower) %>%   #lowercase
  mutate(file = str_replace(file, "\\s", "|")) %>%   #separate string at first white space
  separate(file, into = c("depth", "junk"), sep = "\\|") %>% 
  mutate(depth = str_replace(depth, "jp", "")) %>% #remove 'jp' from string 
  select(-4,-5,-6,-7,-8,-10) #delete columns
#more changes. convert temp, change date format, adjust depth
colnames(jp.hobo) = c("DateTime","TempF","Intensity","Depth")
jp.hobo$TempC =  5/9 * (jp.hobo$TempF - 32) #convert to celsius
jp.hobo$Depth = as.numeric(jp.hobo$Depth) #coerce
#round hours, split date/time subset dates. 
jp.hobo = jp.hobo %>% 
  mutate(DateTime = round_date(DateTime,unit='hour')) %>% 
  separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
str(jp.hobo)
#change datatime
jp.hobo$Date = as.Date(jp.hobo$Date)
jp.hobo$DateTime = as_datetime(jp.hobo$DateTime)
str(jp.hobo)
#remove before and after deployment
jp.hobo = jp.hobo %>% filter(Date>='2020-02-22' & Date<='2020-10-11')
#replace ~6 hours when buoy was removed and redeployed with NA
#spring download
jp.hobo = jp.hobo %>% mutate(TempC=replace(TempC,(DateTime>=ymd_hms("2020-06-13 10:00:00") & 
                                                    DateTime<=ymd_hms("2020-06-13 16:00:00")),NA))
#fall download (coming later)
#Fix depths. before 25-Jun 5pm, add 3m, after that, add 6m.
jp.hobo$Depth = jp.hobo$Depth + ifelse(jp.hobo$DateTime <= as_datetime('2020-06-25 17:00:00'), 3, 6)   
#adjust back again for oct -> removal

#add blank 25m hobo to interpolate

#add nps buoy data for upper layer







#Seal Cove

#feb to june
files = "dbBuoy/buoySC/Hobos/2020-06-14" 
data = list.files(path=files, pattern="*.csv", full.names=TRUE) 
fileSub = str_remove(basename(data), "\\.csv$") #save file names
sc.hobo1 = imap_dfr(setNames(data, fileSub), ~ read_csv(.x,skip=2) %>% mutate(file = .y)) 
str(sc.hobo1)
#make temp column, grep jp0X, grep depth
sc.hobo1 = sc.hobo1 %>% 
  mutate_if(is.character, str_to_lower) %>%   #lowercase
  mutate(file = str_replace(file, "\\s", "|")) %>%   #separate string at first white space
  separate(file, into = c("depth", "junk"), sep = "\\|") %>% #separate string after second character i think
  mutate(depth = str_replace(depth, "sc", "")) %>%  #remove 'sc' from string
  select(-4,-5,-6,-7,-9,-10,-11) #delete columns
#fix
colnames(sc.hobo1) = c("DateTime","TempF","Intensity","Depth")
sc.hobo1$TempC =  5/9 * (sc.hobo1$TempF - 32) #convert to celcius
sc.hobo1$Depth = as.numeric(sc.hobo1$Depth) #coerce
sc.hobo1$Depth = sc.hobo1$Depth + 1  #adjust for actual buoy depth
#subset dates, round to nearest hour
#round hours, split date/time subset dates. 
sc.hobo1 = sc.hobo1 %>% 
  mutate(DateTime = round_date(DateTime,unit='hour')) %>% 
  separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
str(sc.hobo1)
#change datetime
sc.hobo1$Date = as.Date(sc.hobo1$Date)
sc.hobo1$DateTime = as_datetime(sc.hobo1$DateTime)
#add downloadID
sc.hobo1$downloadID = 'june'
str(sc.hobo1)

#june to oct
files = "dbBuoy/buoySC/Hobos/2020-10-15" 
data = list.files(path=files, pattern="*.csv", full.names=TRUE) 
fileSub = str_remove(basename(data), "\\.csv$") #save file names
sc.hobo2 = imap_dfr(setNames(data, fileSub), ~ read_csv(.x,skip=2) %>% mutate(file = .y)) 
str(sc.hobo2)
#make temp column, grep jp0X, grep depth
sc.hobo2 = sc.hobo2 %>% 
  mutate_if(is.character, str_to_lower) %>%   #lowercase
  mutate(file = str_replace(file, "\\s", "|")) %>%   #separate string at first white space
  separate(file, into = c("depth", "junk"), sep = "\\|") %>% 
  mutate(depth = str_replace(depth, "sc", "")) %>%    #change to just depth
  select(-4,-5,-6,-7,-8,-10) #delete columns
#fix
colnames(sc.hobo2) = c("DateTime","TempF","Intensity","Depth")
sc.hobo2$TempC =  5/9 * (sc.hobo2$TempF - 32) #convert to celcius
sc.hobo2$Depth = as.numeric(sc.hobo2$Depth) #coerce
sc.hobo2$Depth = sc.hobo2$Depth + 1  #adjust for actual buoy depth
#subset dates, round to nearest hour
#round hours, split date/time subset dates. 
sc.hobo2 = sc.hobo2 %>% 
  mutate(DateTime = round_date(DateTime,unit='hour')) %>% 
  separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
str(sc.hobo2)
#change datetime
sc.hobo2$Date = as.Date(sc.hobo2$Date)
sc.hobo2$DateTime = as_datetime(sc.hobo2$DateTime)
#add downloadID
sc.hobo2$downloadID = 'oct'
str(sc.hobo2)
#remove feb to june readings from second download
sc.hobo2 = sc.hobo2 %>% filter(DateTime>=ymd_hms('2020-06-14 20:00:00'))

#merge sc hobos
sc.hobo = rbind(sc.hobo1,sc.hobo2)
#remove before and after deployment
sc.hobo = sc.hobo %>% filter(Date>='2020-02-22' & Date<='2020-10-14')
#replace ~6 hours when buoy was removed and redeployed with NA
#spring download
sc.hobo = sc.hobo %>% mutate(TempC=replace(TempC,(DateTime>=ymd_hms("2020-06-14 12:00:00") & 
                                                    DateTime<ymd_hms("2020-06-14 20:00:00")),NA))
#fall download (coming later)




#Witch Hole
#june download
files = "dbBuoy/buoyWH/hobos/2020-06-13" 
data = list.files(path=files, pattern="*.csv", full.names=TRUE) 
fileSub = str_remove(basename(data), "\\.csv$") #save file names
wh.hobo1 = imap_dfr(setNames(data, fileSub), ~ read_csv(.x,skip=2) %>% mutate(file = .y)) 
#make temp column, grep jp0X, grep depth
wh.hobo1 = wh.hobo1 %>% 
  mutate_if(is.character, str_to_lower) %>%   
  mutate(file = str_replace(file, "\\s", "|")) %>%
  separate(file, into = c("depth", "junk"), sep = "\\|") %>% 
  mutate(depth = str_replace(depth, "wh", "")) %>% 
  select(-4,-5,-6,-7,-8,-10)
#fix
colnames(wh.hobo1) = c("DateTime","TempF","Intensity","Depth")
wh.hobo1$TempC =  5/9 * (wh.hobo1$TempF - 32) #convert temp
wh.hobo1$Depth = as.numeric(wh.hobo1$Depth) #coerce
wh.hobo1$Depth = wh.hobo1$Depth + 1  #adjust for actual buoy depth
#subset dates, round to nearest hour
#round hours, split date/time subset dates. 
wh.hobo1 = wh.hobo1 %>% 
  mutate(DateTime = round_date(DateTime,unit='hour')) %>% 
  separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
str(wh.hobo1)
#change datetime
wh.hobo1$Date = as.Date(wh.hobo1$Date)
wh.hobo1$DateTime = as_datetime(wh.hobo1$DateTime)
str(wh.hobo1)

#october download
files = "dbBuoy/buoyWH/hobos/2020-10-15" 
data = list.files(path=files, pattern="*.csv", full.names=TRUE) 
fileSub = str_remove(basename(data), "\\.csv$") #save file names
wh.hobo2 = imap_dfr(setNames(data, fileSub), ~ read_csv(.x,skip=2) %>% mutate(file = .y)) 
#make temp column, grep jp0X, grep depth
wh.hobo2 = wh.hobo2 %>% 
  mutate_if(is.character, str_to_lower) %>%   
  mutate(file = str_replace(file, "\\s", "|")) %>%
  separate(file, into = c("depth", "junk"), sep = "\\|") %>% 
  mutate(depth = str_replace(depth, "wh", "")) %>% 
  select(-4,-5,-6,-7,-8,-10)
#fix
colnames(wh.hobo2) = c("DateTime","TempF","Intensity","Depth")
wh.hobo2$TempC =  5/9 * (wh.hobo2$TempF - 32) #convert temp
wh.hobo2$Depth = as.numeric(wh.hobo2$Depth) #coerce
wh.hobo2$Depth = wh.hobo2$Depth + 1  #adjust for actual buoy depth
#subset dates, round to nearest hour
#round hours, split date/time subset dates. 
wh.hobo2 = wh.hobo2 %>% 
  mutate(DateTime = round_date(DateTime,unit='hour')) %>% 
  separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
str(wh.hobo2)
#change datetime
wh.hobo2$Date = as.Date(wh.hobo2$Date)
wh.hobo2$DateTime = as_datetime(wh.hobo2$DateTime)
#remove feb to june readings from second download
wh.hobo2 = wh.hobo2 %>% filter(DateTime>=ymd_hms('2020-06-13 22:00:00'))
str(wh.hobo2)

#merge wh hobos
wh.hobo = rbind(wh.hobo1,wh.hobo2)
#remove before and after deployment
wh.hobo = wh.hobo %>% filter(Date>='2020-02-26' & Date<='2020-10-14')
#replace ~6 hours when buoy was removed and redeployed with NA
#spring download
wh.hobo = wh.hobo %>% mutate(TempC=replace(TempC,(DateTime>=ymd_hms("2020-06-13 14:00:00") & 
                                                    DateTime<ymd_hms("2020-06-13 22:00:00")),NA))
#fall download (coming later)




#BB temp string
bb.hobo = read.csv("dbBuoy/buoyBB/Bubb_Temp_full.csv")
#remove extra cols 
bb.hobo = bb.hobo %>% select(-Lake,-MIDAS,-ID)
#transpose
bb.hobo = bb.hobo %>% pivot_longer(-DateTime, names_to="Depth", values_to="TempC")
#format depth
bb.hobo = bb.hobo %>% mutate(Depth = str_replace(Depth, "wtr_", ""))
bb.hobo$Depth = as.numeric(bb.hobo$Depth)
str(bb.hobo)  
#fix datetime
bb.hobo$DateTime = as_datetime(bb.hobo$DateTime, format="%m/%d/%Y %H:%M")
#split datetime
bb.hobo = bb.hobo %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
bb.hobo$Date = as.Date(bb.hobo$Date)
str(bb.hobo)  














