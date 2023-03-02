library(readxl)
library(tidyverse)
library(lubridate)
library(hms)
library(ggplot2)


#######################################

  ### Bring in and Format Data ###

#######################################

lakeinv<-read.csv("C:/R/LakeGeneralInfo/LKINDEX.csv", stringsAsFactors = FALSE)

xlfile<-"Lake_Survey_Data_12132022.xlsx"

sites<-read_xlsx("C:/R/LakeData123/RawDataDownloads/Lake_Survey_Data_12132022.xlsx", sheet = "Lake_Survey_Data_0",
                 col_types = c("numeric",    # ObjectID
                              "text",      # GlobalID
                              "numeric",   # MIDAS
                              "text",      # First 4 cha
                              "numeric",   # Station Number
                              "text",      # Station description
                              "text",      # Station desc. if other
                              "date",      # Date
                              "guess",      # Time
                              "numeric",   # Wind Speed
                              "numeric",   # wind direction 
                              "text",      # sky conditions 
                              "skip",      # Gloeo visual aid 
                              "numeric",   # Gloeo rating
                              "text",      # Monitor 1 
                              "text",      # Monitor 2 
                              "text",      # Additional Monitors 
                              "text",      # meter used 
                              "text",      # meter description if other                              "text",      # Monitor 1 
                              "text",      # meter calibrated
                              "text",      # DO/Temp Units
                              "numeric",   # bottom depth                 
                              "numeric",   # core depth 
                              "guess", "guess","guess", "guess","guess", "guess","guess", "guess",
                              "guess", "guess","guess", "guess","guess", "guess","guess", "guess",
                              "guess", "guess","guess", "guess","guess", "guess", "guess","guess", 
                              "guess","guess","guess","guess", "guess", "guess"))      

#rename AccessNotes:              

sites<-sites %>%   rename(AccessNotes = `Notes (access, conditions, weirdness, loons etc…)`)

# format time
sites$Time <- ymd_hms(sites$Date)
sites$Time <- as_hms(sites$Time)


# format date
sites$Date <- ymd_hms(sites$Date)
sites$Date <- date(sites$Date)


# 2022 Only

sites<-sites %>% filter(year(Date) >= 2022)



secchi<-read_xlsx(paste0("C:/R/LakeData123/RawDataDownloads/",xlfile), sheet = "SECCHI_DATA_1") %>% 
  select(c(1,9,3:8)) 


dotemp<-read_xlsx(paste0("C:/R/LakeData123/RawDataDownloads/",xlfile), sheet = "DOTEMP_2") %>% 
  select(c(1,8,3:5))


qc<-read_xlsx(paste0("C:/R/LakeData123/RawDataDownloads/",xlfile), sheet = "QC_3") %>% 
  select(c(1,6,3:5))


tp_grabs<-read_xlsx(paste0("C:/R/LakeData123/RawDataDownloads/",xlfile), sheet = "Tpgrab_4") %>% 
  select(c(1,4,3))


cyanos<-read_xlsx(paste0("C:/R/LakeData123/RawDataDownloads/",xlfile), sheet = "Cyano_5") %>% 
  select(c(1,7,3:6))

chemistry<-read_xlsx(paste0("C:/R/LakeData123/RawDataDownloads/",xlfile), sheet = "CHEMISTRY_6") %>% 
  select(c(1,28,3:27))




#######################################

      ### Build Data Tables ###

#######################################


    ### GENERAL table ###


  # GENERAL colnames:

# LAKNAM	MIDAS	STATION	SAMPDATE	AGENCY	PROJECT	TIME	SURVEYOR1	SURVEYOR2	
# SURVEYOR3	SURVEYOR4	WINDVEL	WINDDIR	CLOUDCVR	SECCHI	SECCBOT	SCOPE	REP	
# QA_CERT	GLOEO	COMMENTS	

### (JD Columns): Year	Month	Day	SampLocID	SampDayID


gen_names<-c("LAKNAM","MIDAS","STATION","SAMPDATE","AGENCY","PROJECT","TIME","SURVEYOR1","SURVEYOR2",	
"SURVEYOR3","SURVEYOR4","WINDVEL","WINDDIR","CLOUDCVR","SECCHI","SECCBOT","SCOPE","REP","QA_CERT","GLOEO","COMMENTS")


agency <- "EI"  # Enter Agency Code. "EI" for DEP.
project <- 2    # Enter Project Code. "2" for Lake Assessment (?)

General<-left_join(sites[,c(2:16,39)], lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") %>% 
  left_join(.,secchi, by =c("GlobalID" = "ParentGlobalID")) %>% 
  mutate(Agency = agency,
         Project = project,
         SURVEYOR4 = as.character(NA)) %>% 
  select("LAKE", "MIDAS","Station Number","Date","Agency","Project","Time","Monitor 1",
         "Monitor 2", "additional monitors", "SURVEYOR4", "wind speed","Wind Direction", "Sky conditions",
         "SECCHI","Secchi on bottom?", "Scope Type", "Reading #", "QA Certification #",
         "GLOEO: refer to visual aid", "AccessNotes") %>% 
  filter(year(Date) >= 2022)

colnames(General)<-gen_names


write.csv(General, file =paste("C:/R/LakeData123/FormattedData/GENERAL_" , Sys.Date(),".csv", sep = ""), row.names = FALSE)






### DO_TEMP_PROFILES table ###


  # DO_TEMP_PROFILES colnames:

  # LAKNAM	MIDAS	STATION	SAMPDATE	AGENCY	PROJECT	DEPTH	TEMP	OXYGEN	OXYMETH	METER	CALIB
	

    ### (JD Columns): DEP	MONTH	DAY	YEAR	SampLocID	ProfileID	DDepth	30yCount


DO_names<-c("LAKNAM","MIDAS","STATION","SAMPDATE","AGENCY",
            "PROJECT","DEPTH","TEMP","OXYGEN","OXYMETH","METER","CALIB")


DO_TEMP_PROFILES_123<-left_join(sites[,c("GlobalID","MIDAS", "Station Number", "Date", "Meter Used", "Meter Calibrated")],
                            lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") %>% 
            left_join(.,dotemp[,2:5],by =c("GlobalID" = "ParentGlobalID")) %>% 
            mutate(AGENCY = agency, PROJECT = project, OXYMETH = "M") %>% 
            select("LAKE", "MIDAS", "Station Number", "Date", "AGENCY","PROJECT", "Depth", 
                   "Temp", "DO", "OXYMETH", "Meter Used","Meter Calibrated") %>%
            rename(Station = "Station Number") %>% 
  arrange(MIDAS,Station,Date,Depth) %>% 
  filter(!is.na(Depth))  # remove records with no data - where recorded with Eureka

        

colnames(DO_TEMP_PROFILES_123)<-DO_names


# Bring in Eureka Data

erk<-read.csv("C:/R/Eureka/Eureka/All_erk_2022.csv") %>% 
  mutate(MIDAS = as.numeric(MIDAS)) 




  ## Match to "DO_names":
    ## "LAKNAM"   "MIDAS"    "STATION"  "SAMPDATE" "AGENCY"   "PROJECT"  "DEPTH"    "TEMP"     "OXYGEN"   "OXYMETH"  "METER"    "CALIB"   

erkMerge<-erk %>% 
  left_join(., lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") %>% 
  mutate(AGENCY = "EI",
         PROJECT = 2,
         OXYMETH = "E",
         METER = if_else(manta == "MT01162166", "Eureka1", "Eureka2"),
         CALIB = "Y") %>% 
  select(LAKE,MIDAS,Station,Date,AGENCY,PROJECT,Depth,Temp_deg_C,HDO_mg.l,OXYMETH,METER, CALIB)


colnames(erkMerge)<-DO_names

erkMerge<-erkMerge %>% 
  mutate(SAMPDATE = ymd(SAMPDATE))



### Remove manually collected Profile data from 123 if data were stored on Eureka:

DO_TEMP_PROFILES_123_clean<- anti_join(DO_TEMP_PROFILES_123, erkMerge, by = c("MIDAS","STATION","SAMPDATE"))


# Merge DO Data:

DO_TEMP_PROFILES <- rbind(DO_TEMP_PROFILES_123_clean, erkMerge) %>% 
  arrange(MIDAS,STATION,SAMPDATE,DEPTH) 
  



write.csv(DO_TEMP_PROFILES, file =paste("C:/R/LakeData123/FormattedData/DO_TEMP_PROFILES_" , Sys.Date(),".csv", sep = ""), row.names = FALSE)



do_long<-gather(DO_TEMP_PROFILES, "Legend", "measurement", TEMP:OXYGEN) %>% 
  filter(!is.na(measurement))
# do_long$LakeMidas<- paste(do_long$LAKNAM,do_long$MIDAS, sep = ": ")



# ggplot(do_long, aes(y=DEPTH, x=measurement))+
#   geom_path(aes(y=DEPTH, x=measurement, color=Legend, linetype=Legend), size = 1)+
#   scale_color_manual(values=c("blue","red"),labels=c("Dissolved Oxygen (mg/L)","Temperature (\u00B0C)"))+
#   scale_linetype_manual(values = c(1,2), labels=c("Dissolved Oxygen (mg/L)","Temperature (\u00B0C)"))+
#   scale_y_reverse()+  #lim=c((max(do_long$DEPTH)+2),0))+
#   theme_bw()+
#   scale_x_continuous(position = "top")+
#   # expand_limits(x=0)+
#   theme(plot.title = element_text(hjust = 0.5))+
#   theme(plot.title = element_text(color = 73))+
#   theme(legend.position = "bottom")+
#   theme(axis.title.x=element_blank())+
#   guides(fill = guide_legend(keywidth = 1, keyheight = 1), 
#          linetype=guide_legend(keywidth = 2, keyheight = 2),
#          colour=guide_legend(keywidth = 2, keyheight = 1)) +
#   # theme(aspect.ratio=12/5)+
#   #legend.justification = c(1, 0))+
#   ylab("Depth (m)")+
#   # facet_wrap(. ~ MIDAS, scales = "free_y")

  



       


    
    #if you want all graphs in a pdf, run the 2 lines directly below,
    #then do not run the *png* command in the loop below and take dev.off() out of loop
    
    # dir.create('C:/R/Trends/NewTrends/Update/Plots/IL/Resid0')
    # setwd('C:/R/Trends/NewTrends/Update/Plots/IL/Resid0')
    # 
    

    do_long$unique<-paste(do_long$MIDAS,do_long$STATION,
                          do_long$SAMPDATE,do_long$METER, sep = ".")
    
    
      names<-unique( do_long$unique)  # change the field to be the one for your data
      num<-length(names)
      w=1

      pdf(file='C:/R/LakeData123/FormattedData/DO_plots.pdf',  paper='letter', width=7, height=10.5, onefile=T)
      
      par(mfrow = c(2,2))    
      

    for(w in 1:num)  {

      s<-subset(do_long,subset=unique==names[w])  # enter your data set name in the code

      titles<-s$LAKNAM[which(s$unique==names[w])] [1]
      titles2<-s$MIDAS[which(s$unique==names[w])] [1]
      titles3<-s$STATION[which(s$unique==names[w])][1]
      titles4<-s$SAMPDATE[which(s$unique==names[w])][1]
      

       print(ggplot(s, aes(y=DEPTH, x=measurement))+
               geom_path(aes(y=DEPTH, x=measurement, color=Legend, linetype=Legend), size = 1)+
               scale_color_manual(values=c("blue","red"),labels=c("Dissolved Oxygen (mg/L)","Temperature (\u00B0C)"))+
               scale_linetype_manual(values = c(1,2), labels=c("Dissolved Oxygen (mg/L)","Temperature (\u00B0C)"))+
               scale_y_reverse()+  #lim=c((max(do_long$DEPTH)+2),0))+
               theme_bw()+
               scale_x_continuous(limits = c(0,29), position = "top")+
               # expand_limits(x=0)+
               theme(plot.title = element_text(hjust = 0.5,size = 12,color = 73))+
              theme(plot.subtitle = element_text(hjust = 0.5,size = 10,color = 73))+
               theme(legend.position = "bottom",
                    legend.text = element_text(size=8),
                    legend.title = element_blank())+
               theme(axis.title.x=element_blank())+
              labs(title = titles, 
                subtitle = (paste("MIDAS",titles2,"- Station",titles3, "-", titles4)))+
              ylab("Depth (m)")+
               guides(linetype=guide_legend(keywidth = 2, keyheight = 2),
                      colour=guide_legend(nrow =2,keywidth = 2, keyheight = 1),
                      fill = guide_legend(keywidth = 1, keyheight = 1))  )

       
       }
      
      

      
dev.off()
dev.off()
dev.off()

      
            
            
            
            
            
            
                  
      
      
###########################################
    #############################################
    
    
    ### DO QC Table ###
    
  
    
    DO_QC_names<-c("LAKNAM","MIDAS","STATION","SAMPDATE","METER","DEPTH","QC_TEMP","QC_OXYGEN")
    
    
    DO_QC<-left_join(sites[,c("GlobalID","MIDAS", "Station Number", "Date", "Meter Used")],
                                lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") %>% 
      left_join(.,qc[,2:5],by =c("GlobalID" = "ParentGlobalID")) %>% 
      select("LAKE", "MIDAS", "Station Number", "Date",  "Meter Used", "QC Depth", 
             "QCTemp", "QCDO") %>% 
      rename(DEPTH = "QC Depth") %>% 
      arrange(MIDAS,Date,DEPTH)
    
    
    colnames(DO_QC)<-DO_QC_names
    
    
    
    
    ### Eureka-stored QC data:
    
    
    erkQA1<- read.csv("C:/R/Eureka/Eureka/All_erk_QA_2022.csv") %>% 
      mutate(MIDAS = as.numeric(MIDAS)) 
    
  
    
    ## Match to "DO_QC_names":
    #"LAKNAM"    "MIDAS"     "STATION"   "SAMPDATE"  "METER"     "DEPTH"     "QC_TEMP"  
    #"QC_OXYGEN"   
  
    
    
    erkQA<-erkQA1 %>% 
      left_join(., lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") %>% 
      mutate(METER = if_else(manta == "MT01162166", "Eureka1", "Eureka2"),
             Date = ymd(Date))%>%
      select(LAKE, MIDAS, Station, Date, METER, Depth, Temp_deg_C,HDO_mg.l)  
      
    
    colnames(erkQA)<-DO_QC_names
    
    
    
    DO_QC_exp <- rbind(DO_QC, erkQA) %>% 
      arrange(MIDAS,STATION,SAMPDATE,DEPTH) %>% 
      filter(!is.na(DEPTH))
    
    
    
    
    
    write.csv(DO_QC_exp, file =paste("C:/R/LakeData123/FormattedData/DO_QC_" , Sys.Date(),".csv", sep = ""), row.names = FALSE)
    
    
    

###########################################
    #############################################
    
    
    ### TP Grabs  ###
    
    
    TP_grab_names<-c("LAKNAM","MIDAS","STATION","SAMPDATE","DEPTH")
    
    
    TP_Grabs<-left_join(sites[,c("GlobalID","MIDAS", "Station Number", "Date")],
                     lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") %>% 
      left_join(.,tp_grabs[,2:3],by =c("GlobalID" = "ParentGlobalID")) %>% 
      select("LAKE", "MIDAS", "Station Number", "Date", "TP Grab Depth" ) %>% 
      rename(DEPTH = "TP Grab Depth") %>% 
      arrange(MIDAS,Date, DEPTH)
      
      colnames(TP_Grabs)<-TP_grab_names
      
# Remove records without TP grab
TP_Grabs<-TP_Grabs[!is.na(TP_Grabs$DEPTH),]    

# Create column for data input
TP_Grabs$PHOS    <- NA
    
  
write.csv(TP_Grabs, file =paste("C:/R/LakeData123/FormattedData/TP_Grabs_" , Sys.Date(),".csv", sep = ""), row.names = FALSE)


###########################################
#############################################


### Cyanos  ###


cyano_names<-c("LAKNAM","MIDAS","STATION","SAMPDATE","Sample","Lat", "Lon", "Time")


Cyanos<-left_join(sites[,c("GlobalID","MIDAS", "Station Number", "Date")],
                    lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") %>% 
  left_join(.,cyanos[,2:6],by =c("GlobalID" = "ParentGlobalID")) %>% 
  select("LAKE", "MIDAS", "Station Number", "Date", 6, 8,7,9) %>% 
  rename(Lat = 6,
         Lon = 7,
         STATION = 3,
         Sample = 5) %>% 
  arrange(MIDAS,Date, Sample)

colnames(Cyanos)<-cyano_names

# Remove Lake surveys without Cyano Samples
Cyanos<-Cyanos[!is.na(Cyanos$Sample),]    

# Create column for data input
Cyanos$Chl    <- NA
Cyanos$Phc    <- NA

write.csv(Cyanos, file =paste("C:/R/LakeData123/FormattedData/Cyanos_" , Sys.Date(),".csv", sep = ""), row.names = FALSE)




###########################################
#############################################


### Chemistry  ###



Chem<-left_join(sites[,c("GlobalID","MIDAS", "Station Number", "Date", 
                         "Core Depth")],
                  lakeinv[,c("MIDAS","LAKE")], by = "MIDAS") %>% 
  left_join(.,chemistry[,2:27],by =c("GlobalID" = "ParentGlobalID")) %>% 
  select("LAKE", "MIDAS", "Station Number", "Date", 5:30) %>% 
  rename(STATION = 3,
         pH_Meth = Method...7,
         pH_Lab = Lab...8,
         Color_Meth = Method...11,
         Color_Lab = Lab...12,
         Cond_Meth = Method...14,
         Cond_Lab = Lab...15,
         Alk_Meth = Method...18,
         Alk_Lab = Lab...19,
         TP_Rep = "Rep #...23",
         TP_Lab = Lab...24,
         CHL_Rep = "Rep #...26") %>% 
  arrange(MIDAS,STATION,Date)





write.csv(Chem, file =paste("C:/R/LakeData123/FormattedData/Chem_" , Sys.Date(),".csv", sep = ""), row.names = FALSE)



