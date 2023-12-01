####################################################
#IMPORTANT, please read !!!!

#   I updated this code to save exported dataframes in folder "CWD_2023" 
#   Full filepath for that is "C:\Users\CWD2-Matt\OneDrive\Database\dbCWD\db.raw\db.cwd\survey123"
#   After you have exported the excel workbook from Survey123... (into folder ‘OneDrive > Documents > dbCWD > survey123output’)
#       ....you need to replace the date listed below on line 39 "S123_2023-03-29.xlsx"
#       ....with the date from the new file (for example, if you exported February 14 your file should be named S123_2024-02-14.xlsx)
#       ....so then line 39 should read: xlfile = "S123_2024-02-14.xlsx"
#       ....also change the date the same way for line XXXXX
#   Great, now you can hold Ctrl+A to highlight this entire script, then hold Ctrl+Enter to run the code!





#libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(hms)
library(ggplot2)


#Loading... ########
#load raw excel file 
#format names
#save parameters


#last export from survey123 - March 29 2023
xlfile = "S123_2023-03-29.xlsx"



sites = read_xlsx("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/survey123output/S123_2023-03-29.xlsx", sheet = "Form_1_0",
                  col_types = c("numeric",   # ObjectID
                                "text",      # GlobalID
                                "numeric",   # MIDAS
                                "text",      # First 4 digits of lake name	
                                "numeric",   # Station Number
                                "text",      # STATION
                                "text",      # Station desc. if other
                                "date",      # Date
                                "guess",     # Time
                                "numeric",   # wind speed
                                "numeric",   # wind Direction 
                                "text",      # Sky conditions 
                                "skip",      # GLOEO: visual aid
                                "numeric",   # GLOEO: refer to visual aid
                                "text",      # Monitor 1 
                                "text",      # Monitor 2 
                                "text",      # Additional Monitors 
                                "text",      # meter used 
                                "text",      # meter description if other                              
                                "text",      # meter calibrated
                                "text",      # DO/Temp Units
                                "numeric",   # Bottom depth   (meters)               
                                "numeric",   # Core Depth 
                                "text",      # SCUM
                                "text",      # Zooplankton collected
                                "numeric",   # Number of Zooplankton Tows
                                "numeric",   # Depth of Tows
                                "text",      # Net ID
                                "text",      # Notes	
                                "text",      # Phytoplankton collected
                                "numeric",   # Number of cores
                                "numeric",   # Depth of cores
                                "text",      # phyto Notes
                                "skip",      # Sediments collected
                                "skip",      # Number of Cores
                                "skip",      # Odor?	
                                "skip",      # Worm Tubes?	
                                "skip",      # Notes	
                                "skip",      # eDNA collected
                                "skip",      # Number of eDNA samps	
                                "skip",      # Depths of eDNA samps
                                "skip",      # eDNA Sample Description if Other	
                                "skip",      # eDNA Notes	
                                "text",      # SCUM
                                "text",      # Notes (access, conditions, weirdness, loons etc…)
                                "text",      # Entered By:
                                "text",      # CreationDate	
                                "text",      # Creator
                                "text",      # EditDate
                                "text",      # Editor
                                "numeric",   # Latitude
                                "numeric",   # Longitude	
                                "text",      # Project
                                "numeric",   # x	
                                "numeric"))  # y
                                      
#rename
names(sites)
#remove spaces
names(sites) = gsub(x = names(sites), pattern = "\\ ", replacement = "_") 
#make everything lowercase
sites = sites %>% set_names(~ str_to_lower(.))
#fix other names
names(sites)
sites = sites %>% 
  rename(ObjectID = 'objectid') %>% 
  rename(GlobalID = 'globalid') %>% 
  rename(MIDAS = midas) %>% 
  rename(lakename_first4 = first_4_digits_of_lake_name) %>% 
  rename(station_type = station) %>% 
  rename(station_description = station_description_if_other) %>% 
  rename(gloeo_rank = 'gloeo:_refer_to_visual_aid') %>% 
  rename(do_temp_units = 'do_/_temp_units') %>% 
  rename(bottom_depth_m = 'bottom_depth_(meters)') %>% 
  rename(scum1 = 'scum...23') %>% 
  rename(zoop_tow_n = number_of_zooplankton_tows) %>% 
  rename(zoop_tow_depth = depth_of_tows) %>% 
  rename(zoop_netID = net_id) %>% 
  rename(zoop_notes = 'notes...28') %>% 
  rename(phyto_cores_n = number_of_cores) %>% 
  rename(phyto_cores_depth = depth_of_cores) %>% 
  rename(phyto_notes = 'notes...32') %>% 
  rename(scum2 = 'scum...33') %>% 
  rename(notes_general = "notes_(access,_conditions,_weirdness,_loons_etc…)") %>% 
  rename(entered_by = "entered_by:")
#check
names(sites)

#format date
sites$date = ymd_hms(sites$date)
sites$date = date(sites$date)
str(sites)

#split
secchi = read_xlsx(paste0("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/survey123output/",xlfile), sheet = "SECCHI_DATA_1")
dotemp = read_xlsx(paste0("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/survey123output/",xlfile), sheet = "DOTEMP_2")
qc = read_xlsx(paste0("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/survey123output/",xlfile), sheet = "QC_3")
tp_grabs = read_xlsx(paste0("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/survey123output/",xlfile), sheet = "Tpgrab_4")
cyanos = read_xlsx(paste0("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/survey123output/",xlfile), sheet = "Cyano_5") 
chem = read_xlsx(paste0("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/survey123output/",xlfile), sheet = "CHEMISTRY_6")



#Build dfs #######
#format data tables following DEP

#* General ##############

#load lake metadata
lakeinv = read.csv('C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/lakemd.csv',header=T)
lakeinv = lakeinv %>% 
  rename(MIDAS = midas) %>% 
  rename(LAKE = lake)


sites$AGENCY = 'CW'
sites$PROJECT = 3
sites$SURVEYOR4 = NA


General = left_join(sites, lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") 
General = left_join(General, secchi, by =c("GlobalID" = "ParentGlobalID"))

#names for gen df
gen_names = c("LAKNAM",
              "MIDAS",
              "STATION",
              "SAMPDATE",
              "AGENCY",
              "PROJECT",
              "TIME",
              "SURVEYOR1",
              "SURVEYOR2",	
              "SURVEYOR3",
              "SURVEYOR4",
              "WINDVEL",
              "WINDDIR",
              "CLOUDCVR",
              "SECCHI",
              "SECCBOT",
              "SCOPE",
              "REP",
              "QA_CERT",
              "GLOEO",
              'LAT',
              'LONG',
              "COMMENTS")

names(General)

#match df to desired colnames
General = General %>% 
  select(LAKE, 
         MIDAS, 
         station_number, 
         date,
         AGENCY,
         PROJECT,
         time,
         monitor_1,
         monitor_2, 
         additional_monitors,
         SURVEYOR4,
         wind_speed,
         wind_direction, 
         sky_conditions,
         SECCHI, 
         "Secchi on bottom?",
         "Scope Type", 
         "Reading #",
         "QA Certification #",
         gloeo_rank, 
         latitude,
         longitude,
         notes_general)

names(General)
colnames(General) = gen_names

#save output in raw df folder
write.csv(General, 
          file = paste("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.cwd/survey123/CWD_2023_GENERAL.csv" , 
                       sep = ""),
          row.names = FALSE)






#* DO / TEMP ###########
#DO and temperature df

DO_names =c("LAKNAM","MIDAS","STATION","SAMPDATE","AGENCY","PROJECT",
            "DEPTH","TEMP","OXYGEN","OXYMETH","METER","CALIB")
names(sites)

DO_TEMP_PROFILES = left_join(sites[,c('GlobalID', 'MIDAS', 'station_number', 'date', 'AGENCY', 'PROJECT', 
                                      'meter_used', 'meter_calibrated')],
                                lakeinv[,c('MIDAS', 'LAKE')], by = "MIDAS")
DO_TEMP_PROFILES = left_join(DO_TEMP_PROFILES, dotemp, by =c("GlobalID" = "ParentGlobalID"))

DO_TEMP_PROFILES = DO_TEMP_PROFILES %>% 
  mutate(OXYMETH = "M") %>% 
  select("LAKE", "MIDAS", "station_number", "date", "AGENCY","PROJECT", "Depth", 
         "Temp", "DO", "OXYMETH", "meter_used","meter_calibrated")

colnames(DO_TEMP_PROFILES) = DO_names

#save 
write.csv(DO_TEMP_PROFILES, 
          file = paste("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.cwd/survey123/CWD_2023/CWD_2023_DO_TEMP_PROFILES.csv",
                       sep = ""),
          row.names = FALSE)





#* DO QC ###########
#do/temp QC table

DO_QC_names = c("LAKNAM","MIDAS","STATION","SAMPDATE",
                "METER","DEPTH","QC_TEMP","QC_OXYGEN")
names(sites)

DO_QC = left_join(sites[,c("GlobalID","MIDAS", 'station_number', 'date',
                           'meter_used')],
                  lakeinv[,c("MIDAS","LAKE")], by = "MIDAS")
DO_QC = left_join(DO_QC, qc[,2:6], by = c("GlobalID" = "ParentGlobalID"))

DO_QC = DO_QC %>% 
  select("LAKE",
         "MIDAS", 
         "station_number",
         "date",
         "meter_used",
         "QC Depth", 
         "QCTemp",
         "QCDO") %>% 
  rename(DEPTH = "QC Depth")

colnames(DO_QC) = DO_QC_names

#save 
write.csv(DO_QC, 
          file = paste("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.cwd/survey123/CWD_2023/CWD_2023_DO_QC.csv",
                       sep = ""),
          row.names = FALSE)




#* TP Grabs  ########
#TP grab tables

TP_grab_names = c("LAKNAM","MIDAS","STATION","SAMPDATE","DEPTH")

TP_Grabs = left_join(sites[,c("GlobalID","MIDAS", "station_number", "date")],
                    lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") %>% 
  left_join(.,tp_grabs[,3:4],by =c("GlobalID" = "ParentGlobalID")) %>% 
  select("LAKE", "MIDAS", "station_number", "date", "TP Grab Depth" ) %>% 
  rename(DEPTH = "TP Grab Depth") 

colnames(TP_Grabs) = TP_grab_names

# Remove records without TP grab
TP_Grabs = TP_Grabs[!is.na(TP_Grabs$DEPTH),]    

# Create column for data input
TP_Grabs$PHOS = NA

#save
write.csv(TP_Grabs, 
          file = paste("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.cwd/survey123/CWD_2023/CWD_2023_TP_Grabs.csv", 
                       sep = ""), 
          row.names = FALSE)






#* Cyano  #######
#HAB stuff

cyano_names = c("LAKNAM","MIDAS","STATION","SAMPDATE","Sample","Lat", "Lon", "Time")

Cyanos = left_join(sites[,c("GlobalID","MIDAS", "station_number", "date")],
                  lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") %>% 
  left_join(. ,cyanos[,3:7],by =c("GlobalID" = "ParentGlobalID")) %>% 
  select("LAKE", "MIDAS", "station_number", "date", 6, 8, 7, 9) %>% 
  rename(Lat = 6,
         Lon = 7,
         STATION = 3,
         Sample = 5)

colnames(Cyanos) = cyano_names

# Remove Lake surveys without Cyano Samples
Cyanos = Cyanos[!is.na(Cyanos$Sample),]    

# Create column for data input
Cyanos$Chl = NA
Cyanos$Phc = NA

#save
write.csv(Cyanos, 
          file = paste("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.cwd/survey123/CWD_2023/CWD_2023_Cyanos.csv",
                       sep = ""), 
          row.names = FALSE)






#* Chemistry  ########

names(sites)
Chem = left_join(sites[,c("GlobalID","MIDAS", "station_number", "date", 
                         "core_depth")],
                lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") %>% 
  left_join(. ,chem[,3:28],by =c("GlobalID" = "ParentGlobalID")) 


Chem = Chem %>% select('LAKE', 'MIDAS', 'station_number', 'date', 5:31) 

chem_names = c("LAKNAM","MIDAS","STATION","SAMPDATE",
               'Core_depth', 'Sample_depth', 'Units', 'Type',
               'pH', 'pH_Meth', 'pH_Lab', 
               'Color', 'A_T', 'Color_Meth', 'Color_Lab',
               'Cond', 'Cond_Meth', 'Cond_Lab', 'Notes',
               'Alkalinity', 'Alk_Meth', 'Alk_Lab', 
               'Labwork_by', 'TP_Label', 'TP_ppb', 'TP_Rep', 'TP_Lab', 
               'CHL_ppb', 'CHL_Rep', 'CHL_Lab')


colnames(Chem) = chem_names

#save
write.csv(Chem, 
          file =paste("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.cwd/survey123/CWD_2023/CWD_2023_Chem.csv", 
                      sep = ""), 
          row.names = FALSE)


#end








