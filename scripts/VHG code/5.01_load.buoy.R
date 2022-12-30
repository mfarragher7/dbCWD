#Load DO and PAR buoy data
#updated 2020-04-29

#load libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)




#DO ####
#JP 
#deployed Feb21 @ 15:00, download Jun13 @ 11:30, download again Oct12 at 2pm.
#DO top
jp.top = read.delim('dbBuoy/buoyJP/DOtop/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
jp.top = jp.top %>%  
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>% #UTC dateTime isn't the correct one
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='JPtop') %>% #add ID column
  slice(-1,) #delete 'units' row
#DO bottom
jp.bot = read.delim('dbBuoy/buoyJP/DObottom/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
jp.bot = jp.bot %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='JPbottom') %>% 
  slice(-1,) #delete 'units' row
#combine
jp.do = rbind(jp.top,jp.bot)
str(jp.do)
jp.do$DOmgpl = as.numeric(jp.do$DOmgpl)
jp.do$DOsat = as.numeric(jp.do$DOsat)
jp.do$Temp = as.numeric(jp.do$Temp)
#remove data download times, split Date and Time
jp.do = jp.do %>% 
  mutate(DOsat=replace(DOsat,(DateTime>=ymd_hms("2020-06-13 10:00:00")&DateTime<=ymd_hms("2020-06-13 16:00:00")),NA)) %>% 
  mutate(DOmgpl=replace(DOmgpl,(DateTime>=ymd_hms("2020-06-13 10:00:00")&DateTime<=ymd_hms("2020-06-13 16:00:00")),NA))
#trim leading whitespace
jp.do$DateTime = trimws(jp.do$DateTime, "l")
#split into date and time
jp.do = jp.do %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
jp.do$Date = as.Date(jp.do$Date)
str(jp.do)

#save csv
write.csv(jp.do,'library/buoy.jp.do.csv',row.names=F)



#SC
#deployed Feb21 @ 11:00, download Jun14 @ 14:00, download again Oct15 @ 11
#DO top
sc.top = read.delim('dbBuoy/buoySC/DOtop/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
sc.top = sc.top %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='SCtop') %>% 
  slice(-1,) 
#DO bottom
sc.bot = read.delim('dbBuoy/buoySC/DObottom/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
sc.bot = sc.bot %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='SCbottom') %>% 
  slice(-1,) 
#combine
sc.do = rbind(sc.top,sc.bot)
str(sc.do)
sc.do$DOmgpl = as.numeric(sc.do$DOmgpl)
sc.do$DOsat = as.numeric(sc.do$DOsat)
sc.do$Temp = as.numeric(sc.do$Temp)
#remove download times
sc.do = sc.do %>% 
  mutate(DOsat=replace(DOsat,(DateTime>=ymd_hms("2020-06-14 12:00:00")&DateTime<=ymd_hms("2020-06-14 20:00:00")),NA)) %>% 
  mutate(DOmgpl=replace(DOmgpl,(DateTime>=ymd_hms("2020-06-14 12:00:00")&DateTime<=ymd_hms("2020-06-14 20:00:00")),NA))
#trim leading whitespace
sc.do$DateTime = trimws(sc.do$DateTime, "l")
#split into date and time
sc.do = sc.do %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
sc.do$Date = as.Date(sc.do$Date)
str(sc.do)

#save csv
write.csv(sc.do,'library/buoy.sc.do.csv',row.names=F)


#WH
#deployed Feb25 @ 15:00, download Jun13 @ 16:00
#DO top
wh.top = read.delim('dbBuoy/buoyWH/DOtop/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
wh.top = wh.top %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='WHtop') %>% 
  slice(-1,) 
#DO bottom
wh.bot = read.delim('dbBuoy/buoyWH/DObottom/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
wh.bot = wh.bot %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='WHbottom') %>% 
  slice(-1,) 
#combine
wh.do = rbind(wh.top,wh.bot)
str(wh.do)
wh.do$DOmgpl = as.numeric(wh.do$DOmgpl)
wh.do$DOsat = as.numeric(wh.do$DOsat)
wh.do$Temp = as.numeric(wh.do$Temp)
#remove download times
wh.do = wh.do %>% 
  mutate(DOsat=replace(DOsat,(DateTime>=ymd_hms("2020-06-13 14:00:00")&DateTime<=ymd_hms("2020-06-13 22:00:00")),NA)) %>% 
  mutate(DOmgpl=replace(DOmgpl,(DateTime>=ymd_hms("2020-06-13 14:00:00")&DateTime<=ymd_hms("2020-06-13 22:00:00")),NA)) 
#trim leading whitespace
wh.do$DateTime = trimws(wh.do$DateTime, "l")
#split into date and time
wh.do = wh.do %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
wh.do$Date = as.Date(wh.do$Date)
str(wh.do)

#save csv
write.csv(wh.do,'library/buoy.wh.do.csv',row.names=F)




#BB DO
bb.do = read.csv('sampling/buoyBB/Bubb_DO_full.csv')
bb.do = bb.do %>% 
  select(-Lake,-MIDAS,-ID)
#separate top
bb.do.top = bb.do %>% 
  select(-2,-5,-6,-7,-8,-9,-10,-11,-12,-13) %>% 
  dplyr::rename('DOmgpl'='DO_1m') %>% 
  dplyr::rename('Temp'='Temp_1m') %>% 
  mutate(ID ='BBtop')
#separate middle
bb.do.mid = bb.do %>% 
  select(-2,-3,-4,-5,-6,-9,-10,-11,-12,-13) %>% 
  dplyr::rename('DOmgpl'='DO_7m') %>% 
  dplyr::rename('Temp'='Temp_7m') %>% 
  mutate(ID ='BBmid')
#separate top
bb.do.bot = bb.do %>% 
  select(-2,-3,-4,-5,-6,-7,-8,-9,-10,-13) %>% 
  dplyr::rename('DOmgpl'='DO_11m') %>% 
  dplyr::rename('Temp'='Temp_11m') %>% 
  mutate(ID ='BBbot')
#merge
bb.do2 = rbind(bb.do.top,bb.do.mid,bb.do.bot)

#save
write.csv(wh.do,'library/buoy.bb.do.csv',row.names=F)








#### PAR ############################

#JP
jp.par = read.delim('dbBuoy/buoyJP/PAR/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
jp.par = jp.par %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Acceleration.X,-Acceleration.Y,-Acceleration.Z) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='JPpar') %>% 
  slice(-1,)
#coerce
str(jp.par)
jp.par$PAR = as.numeric(jp.par$PAR)
jp.par$Temp = as.numeric(jp.par$Temp)
#remove data download times
jp.par = jp.par %>%
  mutate(PAR=replace(PAR,(DateTime>=ymd_hms("2020-06-13 10:00:00")&DateTime<=ymd_hms("2020-06-13 16:00:00")),NA))
#trim leading whitespace
jp.par$DateTime = trimws(jp.par$DateTime, "l")
#split into date and time
jp.par = jp.par %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
jp.par$Date = as.Date(jp.par$Date)
str(jp.par)
#label 'before/after demonic intrusion'\
jp.par = jp.par %>% mutate(BDI = ifelse(DateTime >= "2020-06-25 18:00:00",'y','n'))
#select only time range 10am - 2pm
#jp.par = jp.par %>% mutate(PAR=replace(PAR,Time < "10:00:00" | Time > "14:00:00",NA))

#save csv
write.csv(jp.par,'library/buoy.jp.par.csv',row.names=F)






#SC
sc.par = read.delim('dbBuoy/buoySC/PAR/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
sc.par = sc.par %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Acceleration.X,-Acceleration.Y,-Acceleration.Z) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='SCpar') %>% 
  slice(-1,)
#coerce
str(sc.par)
sc.par$PAR = as.numeric(sc.par$PAR)
sc.par$Temp = as.numeric(sc.par$Temp)
#remove download times
sc.par = sc.par %>%
  mutate(PAR=replace(PAR,(DateTime>=ymd_hms("2020-06-14 12:00:00")&DateTime<=ymd_hms("2020-06-14 20:00:00")),NA)) 
#trim leading whitespace
sc.par$DateTime = trimws(sc.par$DateTime, "l")
#split into date and time
sc.par = sc.par %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
sc.par$Date = as.Date(sc.par$Date)
str(sc.par)
#select only time range 10am - 2pm
#sc.par = sc.par %>% mutate(PAR=replace(PAR,Time < "10:00:00" | Time > "14:00:00",NA))

#save csv
write.csv(sc.par,'library/buoy.sc.par.csv',row.names=F)






#WH
wh.par = read.delim('dbBuoy/buoyWH/PAR/full/Cat.txt',sep=',')
#delete and dplyr::rename columns
wh.par = wh.par %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Acceleration.X,-Acceleration.Y,-Acceleration.Z) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('Temp'='Temperature') %>% 
  mutate(ID='SCpar') %>% 
  slice(-1,)
#coerce
str(wh.par)
wh.par$PAR = as.numeric(wh.par$PAR)
wh.par$Temp = as.numeric(wh.par$Temp)
#remove download times
wh.par = wh.par %>% 
  mutate(PAR=replace(PAR,(DateTime>=ymd_hms("2020-06-13 14:00:00")&DateTime<=ymd_hms("2020-06-13 22:00:00")),NA))
#trim leading whitespace
wh.par$DateTime = trimws(wh.par$DateTime, "l")
#split into date and time
wh.par = wh.par %>% separate(DateTime, c("Date", "Time"), sep = " ", remove=F)
#coerce date
wh.par$Date = as.Date(wh.par$Date)
str(wh.par)
#select only time range 10am - 2pm
#wh.par = wh.par %>% mutate(PAR=replace(PAR,Time < "10:00:00" | Time > "14:00:00",NA))

#save csv
write.csv(wh.par,'library/buoy.wh.par.csv',row.names=F)







#Hobos ####



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


write.csv(jp.hobo,'library/buoy.jp.hobo.csv',row.names=F)





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

write.csv(sc.hobo,'library/buoy.sc.hobo.csv',row.names=F)





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

write.csv(wh.hobo,'library/buoy.wh.hobo.csv',row.names=F)








#BB temp string
bb.hobo = read.csv("dbBuoy/buoyBB/Bubb_Temp_full.csv")
#remove extra cols 
bb.hobo = bb.hobo %>% select(-Lake,-MIDAS,-ID)
#transpose
bb.hobo = bb.hobo %>% pivot_longer(-DateTime, names_to="Depth", values_to="Temp")
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

write.csv(bb.hobo,'library/buoy.bb.temp.csv',row.names=F)















