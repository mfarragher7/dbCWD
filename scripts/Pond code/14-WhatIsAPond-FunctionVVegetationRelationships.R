# Create some theoretical and empirical figures based on our pond definition ideas
# This one carries script 11 forward by doing the analysis on vegeatation
# Created 11Mar2022 by Dave Richardson (hereafter: DCR)

#Things to do####
  #Export all AICc's 
  #Select optimal model for each
  #Create large figure
  #Export AICC results
  #summarize breakpoints (if they exist)

#Packages and libraries ####
#*Checks for packages ####
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(scales)){install.packages("scales")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(gtools)){install.packages("gtools")}
if(!require(mgcv)){install.packages("mgcv")}
if(!require(gratia)){install.packages("gratia")}
if(!require(strucchange)){install.packages("strucchange")}
if(!require(segmented)){install.packages("segmented")}
if(!require(patchwork)){install.packages("patchwork")}
if(!require(paleoMAS)){install.packages("paleoMAS")}
if(!require(nlme)){install.packages("nlme")}
if(!require(pryr)){install.packages("pryr")}


#*Load libraries ####
library(tidyverse)
library(ggpubr)
library(scales)
library(dplyr)
library(ggplot2)
library(car) # Levene's test 
library(agricolae) #LSD test
library(gtools)
library(mgcv) ##GAM models
library(gratia) ##GAM models
library(strucchange) #segmented regressions
library(segmented)
library(patchwork) #laying out multipanel plots with the same size
library(paleoMAS) #for AIC for LOESS
library(nlme) #for losgitic self starting functions

#Pond/Lake/Wetland from our literature survey####
#*load cleaned pond data ####
singleponds <- read.csv("Data_cleaned/Singleponds_Cleaned.csv")

#remove large/deep lakes. n=1364
singleponds <- singleponds %>% 
  filter((max_depth_m<=9 | is.na(max_depth_m)==T) &
           (mean_depth_m<=9 | is.na(mean_depth_m)==T) &
           (max_surfacearea_m2<=200000 | is.na(max_surfacearea_m2)==T) &
           (mean_surfacearea_m2<=200000 | is.na(mean_surfacearea_m2)==T))
singleponds$type<-"pond"
  #histogram(singleponds$macrophytes_percentcover)

#*load in NLA lake data ####
NLA_info<-read.csv("RawData/Lakes/Public_data/nla2012_wide_siteinfo_08232016.csv") %>% 
  dplyr::select(UID, EVAL_NAME, LAT_DD83, LON_DD83, AREA_HA, LAKE_ORIGIN, STATUS, SITETYPE, INDEX_NLA) %>% 
  rename(pondname = EVAL_NAME, latitude= LAT_DD83, longitude = LON_DD83, mean_surfacearea_ha = AREA_HA, humanbuilt_manipulated =LAKE_ORIGIN) %>%
  filter(STATUS == "Target_Sampled") %>% #lakes that were actually sampled 
  filter(SITETYPE == "HAND" | INDEX_NLA == "Y") %>% #remove duplicate lakes, HAND are the reference lakes and PROB (probability) lakes with index Y were used in the NLA analysis
  mutate(mean_surfacearea_m2=mean_surfacearea_ha* 10000,  #convert area ha to m2 
         humanbuilt_manipulated= case_when(humanbuilt_manipulated == 'MAN_MADE' ~ 'y',
                                           humanbuilt_manipulated == 'NATURAL' ~ 'n' )  #change names to match ponds database
          ) %>% 
  dplyr::select(-c(STATUS, SITETYPE, INDEX_NLA))

NLA_chem<-read.csv("RawData/Lakes/Public_data/nla2012_waterchem_wide.csv")  %>% 
  dplyr::select(UID, COND_RESULT, DOC_RESULT, PH_RESULT, PTL_RESULT, NTL_RESULT, TSS_RESULT) %>% #select columns Cond (uS/cm @ 25C), DOC (mg/L), pH, TP (ug/L), TN (mg/L), TSS (mg/L) 
  rename(cond_uspcm= COND_RESULT, doc_mgpl= DOC_RESULT, ph=PH_RESULT, tp_ugpl= PTL_RESULT, tn_mg= NTL_RESULT, tss_mgpl = TSS_RESULT) %>%
  mutate(tn_ugpl = tn_mg*1000) #convert TN from mg to ug 

NLA_chla<-read.csv("RawData/Lakes/Public_data/nla2012_keyvariables_data.csv")  %>% #chla (ug/L), depth (m)
  dplyr::select(UID, INDEX_SITE_DEPTH, CHLX_RESULT, TROPHIC_STATE, AMITOTAL) %>%
  rename(max_depth_m=INDEX_SITE_DEPTH, chla_ugpl = CHLX_RESULT, trophic_status = TROPHIC_STATE, macrophytes_percentcover=AMITOTAL) %>%
  mutate(macrophytes_percentcover = macrophytes_percentcover*100) %>%  #convert from proportion to percent 
  mutate(Trophic_status = case_when(trophic_status == 'EUTROPHIC' ~ 'eutrophic',   #if ~ then  #change names to match pond database
                                    trophic_status == 'MESOTROPHIC' ~ 'mesotrophic',
                                    trophic_status == 'OLIGOTROPHIC' ~ 'oligotrophic',
                                    trophic_status == 'HYPEREUTROPHIC' ~ 'hypereutrophic'))
NLA_chla$macrophytes_percentcover[NLA_chla$macrophytes_percentcover > 100] <- 100  #can't have over 100% coverage

#add more detailed veg data fractional cover of macrophytes
NLA_veg<- read.csv("RawData/Lakes/Public_data/nla2012_wide_phabmet_10202016.csv") %>% 
  dplyr::select(UID, AMFCEMERGENT, AMFCFLOATING, AMFCSUBMERGENT)%>%
  mutate(emergent=AMFCEMERGENT*100, 
         floating = AMFCFLOATING*100, 
         submergent = AMFCSUBMERGENT*100) #convert from proportion to %

#join tables 
NLA_dat<-left_join(NLA_info, NLA_chem) %>%
  left_join(NLA_chla) %>% 
  left_join(NLA_veg) %>%
  mutate(type= "lake")


#*load in wetlands data ####
wetland_siteinfo<- read.csv("RawData/Wetlands/Wetlands_siteinfo2.csv")  %>%
  dplyr::select(UID, SITE_USE, AA_CENTER_LAT, AA_CENTER_LON, CLASS_FIELD_FWSST)
wetland_surfacewater <- read.csv("RawData/Wetlands/Wetlands_watercharacteristic.csv") #depth (cm)
wetland_chem <- read.csv("RawData/Wetlands/Wetlands_waterchem.csv") %>%
  dplyr::select('UID', 'COND', 'NH3', 'NO3NO2', 'PH', 'TKN', 'TN', 'TP', 'CONDUCTIVITY_FIELD', 'PH_FIELD') #TP (ug/L), TN (mg/L)
wetland_chla <- read.csv("RawData/Wetlands/Wetlands_chla.csv") %>%
  dplyr::select(UID, CHLA) #Chla (ug/L)
wetland_veg<-read.csv("RawData/Wetlands/Wetlands_vegtype.csv", header = TRUE) %>%
  dplyr::select("UID", "WATER_AQVEG", "WATER_EMERGVEG")
#group by UID and average at the site for each vegetation type (all are % covers)
wetland_veg_mean<-wetland_veg%>%
  group_by(UID)%>%
  summarise(mean_AQVEG = mean(WATER_AQVEG, na.rm=TRUE), 
            mean_EMERGVEG = mean(WATER_EMERGVEG, na.rm=TRUE)) %>% 
  mutate(macrophytes_percentcover= mean_EMERGVEG + mean_AQVEG ) #add up the submerged, floating, and emergent veg

wetland_veg_mean$macrophytes_percentcover[wetland_veg_mean$macrophytes_percentcover > 100] <- 100  #can't have over 100% coverage

#join tables  
wetland_dat<-left_join(wetland_siteinfo, wetland_surfacewater) %>%
  left_join(wetland_chem) %>%
  left_join(wetland_chla) %>%
  left_join(wetland_veg_mean) %>%
  filter(SITE_USE != "NWCA_REVISITS" ) %>% #select on sample from wetlands 
  filter(WATER_SALINITY == "FRESH") %>% #select freshwater #458 sites 
  drop_na(TP) %>% #select the ones with enough water for chem samples #400 sites 
  mutate(type = "wetland", 
         max_depth_m= SW_DEPTH/100,  #convert cm to m 
          tn_ugpl= TN * 1000 ) #convert mg/L to ug/L


### select out comparable variables to compare all 3 "types" 
#not using the ponds data at the moment 
#ponds<-dplyr::select(singleponds, pondname, max_depth_m, mean_depth_m, max_surfacearea_m2, mean_surfacearea_m2, ph, doc_mgpl, chla_ugpl, tp_ugpl, tn_ugpl, cond_uspcm, macrophytes_percentcover, type) %>%
 # rename(siteid = pondname)

wetlands<-dplyr::select(wetland_dat, UID, max_depth_m, COND, PH, tn_ugpl, TP, CHLA, macrophytes_percentcover, mean_EMERGVEG, CLASS_FIELD_FWSST, type ) %>%
  rename(siteid=UID, cond_uspcm=COND, ph=PH, tp_ugpl=TP, chla_ugpl= CHLA, emergent = mean_EMERGVEG) %>%
  mutate(field_class= ifelse(CLASS_FIELD_FWSST == "PUBPAB", "pond", "wetland")) %>% # "ponds" (PUBPAB n=22)
  dplyr::select(-c(CLASS_FIELD_FWSST))
lakes<-dplyr::select(NLA_dat, pondname, max_depth_m, cond_uspcm, ph, tn_ugpl, tp_ugpl, chla_ugpl, macrophytes_percentcover, emergent, type) %>%
  rename(siteid = pondname) %>%
  mutate(field_class = "lake")
both_ecos<-gtools::smartbind( wetlands, lakes)

#*plots ####
plot(lakes$chla_ugpl~lakes$macrophytes_percentcover,log="y")
plot(wetlands$chla_ugpl~wetlands$emergent,log="y")
plot(both_ecos$tn_ugpl ~both_ecos$macrophytes_percentcover,log="y")

#quick boxplot to look at the "ponds" (PUBPAB n=22) vs wetlands vs lakes 
ggplot(both_ecos, aes(x = field_class, y = emergent)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = field_class), width = 0.2, size = 1) 

#quick test and post-hoc for differences in mean emergent veg
anova(lm(emergent ~ field_class, data = both_ecos, na.action = na.omit ))
LSD<-LSD.test(both_ecos$emergent, both_ecos$field_class, 1512, 281.3, alpha=0.05)  #specify the DF and MSE of the residuals
LSD


#both_ecos has ponds and wetlands included in wetlands but also lakes with emergent veg####
#Convert both_ecos to tibble####
both_ecos<-tibble(both_ecos) 

#Things2do####
#Run breakpoint analysis on the following variables
#cond_uspcm, ph, tn_ugpl tp_ugpl chla_ugpl vs. emergent
# do this for all of both_ecos and both_ecos%>%filter(type=="wetland") 


#Run analyses for both_ecos####
#Initialize aicc.df.emergent for all####
aicc.df.emerg<-tibble(type=character(),flat=numeric(),linear=numeric(),segmented=numeric(),logistic=numeric(),segmented.cutoff=numeric(), logistic.cutoff=numeric(), segmented.cutoff.SE=numeric(), logistic.cutoff.SE=numeric())

#pH analysis for depth####
#*ph: trim and order to only non NA values####
ph.trim.emerg<-both_ecos%>%dplyr::select(ph,emergent)%>%arrange(emergent)%>%filter(!is.na(ph))%>%filter(!is.na(emergent))%>%filter(ph>0)%>%mutate(log_emergent=log10(emergent+1))

#*pH: Flat model####
ph.flat.emerg<-lm(ph~1,data=ph.trim.emerg)
summary(ph.flat.emerg)

#*ph: linear model####
ph.lm.emerg<-lm(ph~log_emergent,data=ph.trim.emerg)
summary(ph.lm.emerg)

#*ph: Segmented regression####
#Try the segmented package version: https://rpubs.com/MarkusLoew/12164#
ph.segmented.emerg<-segmented(lm(ph ~ log_emergent,data=ph.trim.emerg),seg.z=emergent,psi=c(0.3)) #initial guess makes a difference
summary(ph.segmented.emerg) 
10^ph.segmented.emerg$psi[,"Est."] #back calculate breakpoint
slope(ph.segmented.emerg) #get the slopes  
ph.segmented.emerg.fitted<-fitted(ph.segmented.emerg) #get the fitted data
ph.segmented.emergModel<-tibble(emergent=10^ph.trim.emerg$log_emergent,log_emergent=ph.trim.emerg$log_emergent,ph.segmented.emerg.fitted=ph.segmented.emerg.fitted) #calculate the fit with the back transformed x axis

#Get both y=mx+b equations
paste("y=",round(as.numeric(ph.segmented.emerg$coefficients["log_emergent"]),2),"x+",round(as.numeric(ph.segmented.emerg$coefficients["(Intercept)"]),2),sep="")
paste("y=",round(as.numeric(ph.segmented.emerg$coefficients["U1.log_emergent"]),2),"x+",round(as.numeric(ph.segmented.emerg$coefficients["log_emergent"])*as.numeric(ph.segmented.emerg$psi[,"Est."]) + as.numeric(ph.segmented.emerg$coefficients["(Intercept)"])-as.numeric(ph.segmented.emerg$psi[,"Est."])*as.numeric(ph.segmented.emerg$coefficients["U1.log_emergent"]),2),sep="")




#*ph: fit a nonlinear logistic curve####
ph.logistic.emerg<-nls(ph~A+(B-A)/(1+exp((xmid-log_emergent)/scal)),
                       start=list(A=7.2,B=7.9,xmid=1.5,scal=-0.1),data=ph.trim.emerg,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
summary(ph.logistic.emerg)
#Get the cutoff from the logistic parameter as the xmid inflection point
ph.logistic.emerg.cutoff<-10^ph.logistic.emerg$m$getAllPars()[3]

#**ph: flat model AICc####
ph.flat.emerg.aicc<-AIC(ph.flat.emerg)+ (2 * 1^2 + 2 * 1)/(length(ph.flat.emerg$residuals) - 1 - 1)

#**ph: linear model AICc####
ph.lm.emerg.aicc<-AIC(ph.lm.emerg)+ (2 * 2^2 + 2 * 2)/(length(ph.flat.emerg$residuals) - 2 - 1)

#**ph: Segmented model AICc####  
ph.segmented.emerg.aicc<-AIC(ph.segmented.emerg)+ (2 * 4^2 + 2 * 4)/(length(ph.flat.emerg$residuals) - 4 - 1)

#**ph: logistic model AICc#### 
ph.logistic.emerg.aicc<-AIC(ph.logistic.emerg)+ (2 * 4^2 + 2 * 4)/(length(ph.flat.emerg$residuals) - 4 - 1)

#**ph: Store aicc from each model as a row####
#Keep adding to this with each analysis
aicc.df.emerg<-aicc.df.emerg%>%add_row(type="ph",flat=ph.flat.emerg.aicc,linear=ph.lm.emerg.aicc,segmented=ph.segmented.emerg.aicc,logistic=ph.logistic.emerg.aicc,
                                       segmented.cutoff=10^ph.segmented.emerg$psi[,"Est."],logistic.cutoff=10^ph.logistic.emerg$m$getAllPars()[3], 
                                       segmented.cutoff.SE=10^ph.segmented.emerg$psi[,"St.Err"], logistic.cutoff.SE=10^coef(summary(ph.logistic.emerg))[,"Std. Error"][3])

#*ph: plot models overlaid####
ggplot(data=ph.trim.emerg,aes(y=ph, x=emergent))+geom_point()+
  geom_hline(yintercept=mean(ph.trim.emerg$ph,na.rm=TRUE),color="orange")+
  geom_smooth(method='lm', se=FALSE,color="red")+
  geom_line(data=ph.segmented.emergModel,aes(y=ph.segmented.emerg.fitted,x=emergent),color="blue")+ #This is the segmented regression
  geom_line(data=tibble(logistic_predict=predict(ph.logistic.emerg),emergent=ph.trim.emerg$emergent), aes(y=logistic_predict,x=emergent),color="purple")+
  geom_text(data=data.frame(aicc=round(ph.flat.emerg.aicc,1)), aes(label = paste("flat=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 1)+
  geom_text(data=data.frame(aicc=round(ph.lm.emerg.aicc,1)), aes(label = paste("linear=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 3)+
  geom_text(data=data.frame(aicc=round(ph.segmented.emerg.aicc,1)), aes(label = paste("segmented=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 5)+
  geom_text(data=data.frame(aicc=round(ph.logistic.emerg.aicc,1)), aes(label = paste("logistic=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 7)+
  theme_bw()

#*ph: plot optimal fit#### 
gg.ph.emerg.scatter<-ggplot(data=ph.trim.emerg,aes(x=emergent+1,y=ph))+
  geom_vline(xintercept=10^ph.segmented.emerg$psi[,"Est."],color="black",linetype=1,size=1)+ #vertical line indicating cutoff from optimal model if it exists, change that here
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(1,100),trans="log10",breaks=c(1,4,11,31,101),labels=c(0,3,10,30,100))+
  #scale_x_continuous(limits=c(0,100),breaks=c(0,25,50,75,100))+
  #geom_smooth(aes(x=emergent,y=ph),method="lm",se=FALSE,color="black",size=1.2)+ #linear model
  #geom_line(data=tibble(logistic_predict=predict(ph.logistic.emerg),log_max_emerg_m=ph.trim.emerg$log_max_emerg_m), aes(y=logistic_predict,x=10^log_max_emerg_m),color="black",size=1.2)+ #add the logistic fit
  geom_line(data=ph.segmented.emergModel,aes(y=ph.segmented.emerg.fitted,x=emergent),color="black",size=1.2)+ #This is the segmented regression, have to raise the y variable 10^ if it has been transformed
  #geom_line(aes(y=predict),color="black",size=1.2)+ #flat
  #scale_y_log10()+ #comment this out if the y axis is not transformed
  labs(y=bquote(pH),x=bquote(Emergent~veg.~('%')))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
  )

#**pH: FIGURE export as jpg####
#ggsave("plots/WIAP-FigureX-pHScatter-emergent.jpg", plot=gg.ph.emerg.scatter, width=3, height=3,units="in", dpi=300)

#chla analysis for depth####
#*chla: trim and order to only non NA values####
chla.trim.emerg<-both_ecos%>%dplyr::select(chla_ugpl,emergent)%>%arrange(emergent)%>%filter(!is.na(chla_ugpl))%>%filter(!is.na(emergent))%>%filter(chla_ugpl>0)%>%mutate(log_emergent=log10(emergent+1),log_chla_ugpl=log10(chla_ugpl))
  
#ggplot(data=chla.trim.emerg,aes(y=log_chla_ugpl,x=log_emergent))+geom_point() #plot to check if data needs to be log transformed 

#*chla: Flat model####
chla.flat.emerg<-lm(log_chla_ugpl~1,data=chla.trim.emerg)
summary(chla.flat.emerg)

#*chla: linear model####
chla.lm.emerg<-lm(log_chla_ugpl~log_emergent,data=chla.trim.emerg)
summary(chla.lm.emerg)

#*chla: Segmented regression####
#Try the segmented package version: https://rpubs.com/MarkusLoew/12164#
chla.segmented.emerg<-segmented(lm(log_chla_ugpl ~ log_emergent,data=chla.trim.emerg),seg.z=log_emergent,psi=c(0.7)) #initial guess makes a difference
summary(chla.segmented.emerg) 
10^chla.segmented.emerg$psi[,"Est."] #back calculate breakpoint
slope(chla.segmented.emerg) #get the slopes  
chla.segmented.emerg.fitted<-fitted(chla.segmented.emerg) #get the fitted data
chla.segmented.emergModel<-tibble(emergent=10^chla.trim.emerg$log_emergent,log_emergent=chla.trim.emerg$log_emergent,chla.segmented.emerg.fitted=chla.segmented.emerg.fitted) #calculate the fit with the back transformed x axis

#*chla: fit a nonlinear logistic curve####
chla.logistic.emerg<-nls(log_chla_ugpl~A+(B-A)/(1+exp((xmid-log_emergent)/scal)),
                         start=list(A=1.5,B=0.5,xmid=1,scal=-0.003),data=chla.trim.emerg,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
summary(chla.logistic.emerg)
#Get the cutoff from the logistic parameter as the xmid inflection point
10^chla.logistic.emerg$m$getAllPars()[3]

#**chla: flat model AICc####
chla.flat.emerg.aicc<-AIC(chla.flat.emerg)+ (2 * 1^2 + 2 * 1)/(length(chla.flat.emerg$residuals) - 1 - 1)

#**chla: linear model AICc####
chla.lm.emerg.aicc<-AIC(chla.lm.emerg)+ (2 * 2^2 + 2 * 2)/(length(chla.flat.emerg$residuals) - 2 - 1)

#**chla: Segmented model AICc####  
chla.segmented.emerg.aicc<-AIC(chla.segmented.emerg)+ (2 * 4^2 + 2 * 4)/(length(chla.flat.emerg$residuals) - 4 - 1)

#**chla: logistic model AICc#### 
chla.logistic.emerg.aicc<-AIC(chla.logistic.emerg)+ (2 * 4^2 + 2 * 4)/(length(chla.flat.emerg$residuals) - 4 - 1)

#**chla: Store aicc from each model as a row####
#Keep adding to this with each analysis
aicc.df.emerg<-aicc.df.emerg%>%add_row(type="chla",flat=chla.flat.emerg.aicc,linear=chla.lm.emerg.aicc,segmented=chla.segmented.emerg.aicc,logistic=chla.logistic.emerg.aicc,
                                       segmented.cutoff=NA,logistic.cutoff=10^chla.logistic.emerg$m$getAllPars()[3], 
                                       segmented.cutoff.SE=NA, logistic.cutoff.SE=10^coef(summary(chla.logistic.emerg))[,"Std. Error"][3])

#*chla: plot models overlaid####
ggplot(data=chla.trim.emerg,aes(y=log_chla_ugpl, x=log_emergent))+geom_point()+
  geom_hline(yintercept=mean(chla.trim.emerg$log_chla_ugpl,na.rm=TRUE),color="orange")+
  geom_smooth(method='lm', se=FALSE,color="red")+
  geom_line(data=chla.segmented.emergModel,aes(y=chla.segmented.emerg.fitted,x=log_emergent),color="blue")+ #This is the segmented regression
  geom_line(data=tibble(logistic_predict=predict(chla.logistic.emerg),log_emergent=chla.trim.emerg$log_emergent), aes(y=logistic_predict,x=log_emergent),color="purple")+
  geom_text(data=data.frame(aicc=round(chla.flat.emerg.aicc,1)), aes(label = paste("flat=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 1)+
  geom_text(data=data.frame(aicc=round(chla.lm.emerg.aicc,1)), aes(label = paste("linear=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 3)+
  geom_text(data=data.frame(aicc=round(chla.segmented.emerg.aicc,1)), aes(label = paste("segmented=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 5)+
  geom_text(data=data.frame(aicc=round(chla.logistic.emerg.aicc,1)), aes(label = paste("logistic=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 7)+
  theme_bw()

#*chla: plot optimal fit#### 
gg.chla.emerg.scatter<-ggplot(data=chla.trim.emerg,aes(x=emergent+1,y=chla_ugpl))+
  #geom_vline(xintercept=chla.logistic.depth.cutoff,color="black",linetype=1,size=1)+ #vertical line indicating cutoff from optimal model if it exists, change that here
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(1,100),trans="log10",breaks=c(1,4,11,31,101),labels=c(0,3,10,30,100))+
  #geom_line(data=tibble(logistic_predict=predict(chla.logistic.depth),log_max_depth_m=chla.trim.depth$log_max_depth_m), aes(y=10^logistic_predict,x=10^log_max_depth_m),color="black",size=1.2)+ #add the logistic fit, have to raise the y variable 10^ if it has been transformed
  #geom_line(data=chla.segmented.depthModel,aes(y=chla.segmented.depth.fitted,x=10^log_max_depth_m),color="black",size=1.2)+ #This is the segmented regression, have to raise the y variable 10^ if it has been transformed
  geom_line(data=tibble(predict_linear=predict(chla.lm.emerg),log_emergent=chla.trim.emerg$log_emergent),aes(y=10^predict_linear,x=10^log_emergent),color="black",size=1.2)+ #add the logistic fit, have to raise the y variable 10^ if it has been transformed)
  #scale_y_log10(limits=c(0.1,1000),breaks=c(0.1,1,10,100,1000),labels=c(bquote(10^-1),bquote(10^0),bquote(10^1),bquote(10^2),bquote(10^3)))+
  scale_y_continuous(limits=c(0.1,1000),trans="log10",breaks=c(0.1,1,10,100,1000),labels=trans_format("log10",math_format(10^.x)))+
  labs(y=bquote(Chl~italic(a)~(mu*g~L^-1)),x=bquote(Emergent~veg.~('%')))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
  )
#**chla: FIGURE export as jpg####
#ggsave("plots/WIAP-FigureX-chlaScatter-emergent.jpg", plot=gg.chla.emerg.scatter, width=3, height=3,units="in", dpi=300)

#tn analysis for emergent####
#*tn: trim and order to only non NA values####
tn.trim.emerg<-both_ecos%>%dplyr::select(tn_ugpl,emergent)%>%arrange(emergent)%>%filter(!is.na(tn_ugpl))%>%filter(!is.na(emergent))%>%filter(tn_ugpl>0)%>%mutate(log_emergent=log10(emergent+1),log_tn_ugpl=log10(tn_ugpl))
#ggplot(data=tn.trim.emerg,aes(y=log_tn_ugpl,x=log_emergent))+geom_point() #plot to check if data needs to be log transformed 

#*tn: Flat model####
tn.flat.emerg<-lm(log_tn_ugpl~1,data=tn.trim.emerg)
summary(tn.flat.emerg)

#*tn: linear model####
tn.lm.emerg<-lm(log_tn_ugpl~log_emergent,data=tn.trim.emerg)
summary(tn.lm.emerg)

#*tn: Segmented regression####
#Try the segmented package version: https://rpubs.com/MarkusLoew/12164#
tn.segmented.emerg<-segmented(lm(log_tn_ugpl ~ log_emergent,data=tn.trim.emerg),seg.z=log_emergent,psi=c(1)) #initial guess makes a difference
summary(tn.segmented.emerg) 
10^tn.segmented.emerg$psi[,"Est."] #back calculate breakpoint
slope(tn.segmented.emerg) #get the slopes  
tn.segmented.emerg.fitted<-fitted(tn.segmented.emerg) #get the fitted data
tn.segmented.emergModel<-tibble(emergent=10^tn.trim.emerg$log_emergent,log_emergent=tn.trim.emerg$log_emergent,tn.segmented.emerg.fitted=tn.segmented.emerg.fitted) #calculate the fit with the back transformed x axis

#Get both y=mx+b equations
paste("y=",round(as.numeric(tn.segmented.emerg$coefficients["log_emergent"]),2),"x+",round(as.numeric(tn.segmented.emerg$coefficients["(Intercept)"]),2),sep="")
paste("y=",round(as.numeric(tn.segmented.emerg$coefficients["U1.log_emergent"]),2),"x+",round(as.numeric(tn.segmented.emerg$coefficients["log_emergent"])*as.numeric(tn.segmented.emerg$psi[,"Est."]) + as.numeric(tn.segmented.emerg$coefficients["(Intercept)"])-as.numeric(tn.segmented.emerg$psi[,"Est."])*as.numeric(tn.segmented.emerg$coefficients["U1.log_emergent"]),2),sep="")


#*tn: fit a nonlinear logistic curve####
tn.logistic.emerg<-nls(log_tn_ugpl~A+(B-A)/(1+exp((xmid-log_emergent)/scal)),
                       start=list(A=2.5,B=3.5,xmid=1,scal=0.2),data=tn.trim.emerg,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
summary(tn.logistic.emerg)
#Get the cutoff from the logistic parameter as the xmid inflection point
10^tn.logistic.emerg$m$getAllPars()[3]

#**tn: flat model AICc####
tn.flat.emerg.aicc<-AIC(tn.flat.emerg)+ (2 * 1^2 + 2 * 1)/(length(tn.flat.emerg$residuals) - 1 - 1)

#**tn: linear model AICc####
tn.lm.emerg.aicc<-AIC(tn.lm.emerg)+ (2 * 2^2 + 2 * 2)/(length(tn.flat.emerg$residuals) - 2 - 1)

#**tn: Segmented model AICc####  
tn.segmented.emerg.aicc<-AIC(tn.segmented.emerg)+ (2 * 4^2 + 2 * 4)/(length(tn.flat.emerg$residuals) - 4 - 1)

#**tn: logistic model AICc#### 
tn.logistic.emerg.aicc<-AIC(tn.logistic.emerg)+ (2 * 4^2 + 2 * 4)/(length(tn.flat.emerg$residuals) - 4 - 1)

#**tn: Store aicc from each model as a row####
#Keep adding to this with each analysis
aicc.df.emerg<-aicc.df.emerg%>%add_row(type="tn",flat=tn.flat.emerg.aicc,linear=tn.lm.emerg.aicc,segmented=tn.segmented.emerg.aicc,logistic=tn.logistic.emerg.aicc,
                                       segmented.cutoff=10^tn.segmented.emerg$psi[,"Est."],logistic.cutoff=10^tn.logistic.emerg$m$getAllPars()[3], 
                                       segmented.cutoff.SE=10^tn.segmented.emerg$psi[,"St.Err"], logistic.cutoff.SE=10^coef(summary(tn.logistic.emerg))[,"Std. Error"][3])

#*tn: plot models overlaid####
ggplot(data=tn.trim.emerg,aes(y=log_tn_ugpl, x=log_emergent))+geom_point()+
  geom_hline(yintercept=mean(tn.trim.emerg$log_tn_ugpl,na.rm=TRUE),color="orange")+
  geom_smooth(method='lm', se=FALSE,color="red")+
  geom_line(data=tn.segmented.emergModel,aes(y=tn.segmented.emerg.fitted,x=log_emergent),color="blue")+ #This is the segmented regression
  geom_line(data=tibble(logistic_predict=predict(tn.logistic.emerg),log_emergent=tn.trim.emerg$log_emergent), aes(y=logistic_predict,x=log_emergent),color="purple")+
  geom_text(data=data.frame(aicc=round(tn.flat.emerg.aicc,1)), aes(label = paste("flat=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 1)+
  geom_text(data=data.frame(aicc=round(tn.lm.emerg.aicc,1)), aes(label = paste("linear=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 3)+
  geom_text(data=data.frame(aicc=round(tn.segmented.emerg.aicc,1)), aes(label = paste("segmented=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 5)+
  geom_text(data=data.frame(aicc=round(tn.logistic.emerg.aicc,1)), aes(label = paste("logistic=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 7)+
  theme_bw()

#*tn: plot optimal fit#### 
gg.tn.emerg.scatter<-ggplot(data=tn.trim.emerg,aes(x=emergent+1,y=tn_ugpl))+
  geom_vline(xintercept=10^tn.segmented.emerg$psi[,"Est."],color="black",linetype=1,size=1)+ #vertical line indicating cutoff from optimal model if it exists, change that here, either 10^tn.segmented.depth$psi[,"Est."] for segemented or tn.logistic.depth.cutoff for logistic
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(1,100),trans="log10",breaks=c(1,4,11,31,101),labels=c(0,3,10,30,100))+
  #geom_line(data=tibble(logistic_predict=predict(tn.logistic.depth),log_max_depth_m=tn.trim.depth$log_max_depth_m), aes(y=10^logistic_predict,x=10^log_max_depth_m),color="black",size=1.2)+ #add the logistic fit, have to raise the y variable 10^ if it has been transformed
  geom_line(data=tn.segmented.emergModel,aes(y=10^tn.segmented.emerg.fitted,x=10^log_emergent),color="black",size=1.2)+ #This is the segmented regression, have to raise the y variable 10^ if it has been transformed
  #geom_line(aes(y=predict),color="black",size=1.2)+
  #geom_line(data=methanefluxes.loess.fit,aes(x=10^log_area_ha,y=loess_fit),color="purple")+
  #geom_line(data=temp,aes(x=area_ha,y=loess_fit),color="green")+
  scale_y_continuous(limits=c(30,10000),trans="log10",breaks=c(10,100,1000,10000),labels=trans_format("log10",math_format(10^.x)))+
  labs(y=bquote(TN~(mu*g~L^-1)),x=bquote(Emergent~veg.~('%')))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
  )
#**tn: FIGURE export as jpg####
#ggsave("plots/WIAP-FigureX-tnScatter-emerg.jpg", plot=gg.tn.emerg.scatter, width=3, height=3,units="in", dpi=300)


#tp analysis for emergent####
#*tp: trim and order to only non NA values####
tp.trim.emerg<-both_ecos%>%dplyr::select(tp_ugpl,emergent)%>%arrange(emergent)%>%filter(!is.na(tp_ugpl))%>%filter(!is.na(emergent))%>%filter(tp_ugpl>0)%>%mutate(log_emergent=log10(emergent+1),log_tp_ugpl=log10(tp_ugpl))
#ggplot(data=tp.trim.emerg,aes(y=log_tp_ugpl,x=log_emergent))+geom_point() #plot to check if data needs to be log transformed 

#*tp: Flat model####
tp.flat.emerg<-lm(log_tp_ugpl~1,data=tp.trim.emerg)
summary(tp.flat.emerg)

#*tp: linear model####
tp.lm.emerg<-lm(log_tp_ugpl~log_emergent,data=tp.trim.emerg)
summary(tp.lm.emerg)

#*tp: Segmented regression####
#Try the segmented package version: https://rpubs.com/MarkusLoew/12164#
tp.segmented.emerg<-segmented(lm(log_tp_ugpl ~ log_emergent,data=tp.trim.emerg),seg.z=log_emergent,psi=c(1)) #initial guess makes a difference
summary(tp.segmented.emerg) 
10^tp.segmented.emerg$psi[,"Est."] #back calculate breakpoint
slope(tp.segmented.emerg) #get the slopes  
tp.segmented.emerg.fitted<-fitted(tp.segmented.emerg) #get the fitted data
tp.segmented.emergModel<-tibble(emergent=10^tp.trim.emerg$log_emergent,log_emergent=tp.trim.emerg$log_emergent,tp.segmented.emerg.fitted=tp.segmented.emerg.fitted) #calculate the fit with the back transformed x axis

#Get both y=mx+b equations
paste("y=",round(as.numeric(tp.segmented.emerg$coefficients["log_emergent"]),2),"x+",round(as.numeric(tp.segmented.emerg$coefficients["(Intercept)"]),2),sep="")
paste("y=",round(as.numeric(tp.segmented.emerg$coefficients["U1.log_emergent"]),2),"x+",round(as.numeric(tp.segmented.emerg$coefficients["log_emergent"])*as.numeric(tp.segmented.emerg$psi[,"Est."]) + as.numeric(tp.segmented.emerg$coefficients["(Intercept)"])-as.numeric(tp.segmented.emerg$psi[,"Est."])*as.numeric(tp.segmented.emerg$coefficients["U1.log_emergent"]),2),sep="")


#*tp: fit a nonlinear logistic curve####
tp.logistic.emerg<-nls(log_tp_ugpl~A+(B-A)/(1+exp((xmid-log_emergent)/scal)),
                       start=list(A=2.5,B=3.5,xmid=1,scal=0.2),data=tp.trim.emerg,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
summary(tp.logistic.emerg)
#Get the cutoff from the logistic parameter as the xmid inflection point
10^tp.logistic.emerg$m$getAllPars()[3]

#**tp: flat model AICc####
tp.flat.emerg.aicc<-AIC(tp.flat.emerg)+ (2 * 1^2 + 2 * 1)/(length(tp.flat.emerg$residuals) - 1 - 1)

#**tp: linear model AICc####
tp.lm.emerg.aicc<-AIC(tp.lm.emerg)+ (2 * 2^2 + 2 * 2)/(length(tp.flat.emerg$residuals) - 2 - 1)

#**tp: Segmented model AICc####  
tp.segmented.emerg.aicc<-AIC(tp.segmented.emerg)+ (2 * 4^2 + 2 * 4)/(length(tp.flat.emerg$residuals) - 4 - 1)

#**tp: logistic model AICc#### 
tp.logistic.emerg.aicc<-AIC(tp.logistic.emerg)+ (2 * 4^2 + 2 * 4)/(length(tp.flat.emerg$residuals) - 4 - 1)

#**tp: Store aicc from each model as a row####
#Keep adding to this with each analysis
aicc.df.emerg<-aicc.df.emerg%>%add_row(type="tp",flat=tp.flat.emerg.aicc,linear=tp.lm.emerg.aicc,segmented=tp.segmented.emerg.aicc,logistic=tp.logistic.emerg.aicc,
                                       segmented.cutoff=10^tp.segmented.emerg$psi[,"Est."],logistic.cutoff=10^tp.logistic.emerg$m$getAllPars()[3], 
                                       segmented.cutoff.SE=10^tp.segmented.emerg$psi[,"St.Err"], logistic.cutoff.SE=10^coef(summary(tp.logistic.emerg))[,"Std. Error"][3])

#*tp: plot models overlaid####
ggplot(data=tp.trim.emerg,aes(y=log_tp_ugpl, x=log_emergent))+geom_point()+
  geom_hline(yintercept=mean(tp.trim.emerg$log_tp_ugpl,na.rm=TRUE),color="orange")+
  geom_smooth(method='lm', se=FALSE,color="red")+
  geom_line(data=tp.segmented.emergModel,aes(y=tp.segmented.emerg.fitted,x=log_emergent),color="blue")+ #This is the segmented regression
  geom_line(data=tibble(logistic_predict=predict(tp.logistic.emerg),log_emergent=tp.trim.emerg$log_emergent), aes(y=logistic_predict,x=log_emergent),color="purple")+
  geom_text(data=data.frame(aicc=round(tp.flat.emerg.aicc,1)), aes(label = paste("flat=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 1)+
  geom_text(data=data.frame(aicc=round(tp.lm.emerg.aicc,1)), aes(label = paste("linear=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 3)+
  geom_text(data=data.frame(aicc=round(tp.segmented.emerg.aicc,1)), aes(label = paste("segmented=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 5)+
  geom_text(data=data.frame(aicc=round(tp.logistic.emerg.aicc,1)), aes(label = paste("logistic=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 7)+
  theme_bw()

#*tp: plot optimal fit#### 
gg.tp.emerg.scatter<-ggplot(data=tp.trim.emerg,aes(x=emergent+1,y=tp_ugpl))+
  geom_vline(xintercept=10^tp.segmented.emerg$psi[,"Est."],color="black",linetype=1,size=1)+ #vertical line indicating cutoff from optimal model if it exists, change that here, either 10^tp.segmented.depth$psi[,"Est."] for segemented or tp.logistic.depth.cutoff for logistic
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(1,100),trans="log10",breaks=c(1,4,11,31,101),labels=c(0,3,10,30,100))+
  #geom_line(data=tibble(logistic_predict=predict(tp.logistic.depth),log_max_depth_m=tp.trim.depth$log_max_depth_m), aes(y=10^logistic_predict,x=10^log_max_depth_m),color="black",size=1.2)+ #add the logistic fit, have to raise the y variable 10^ if it has been transformed
  geom_line(data=tp.segmented.emergModel,aes(y=10^tp.segmented.emerg.fitted,x=10^log_emergent),color="black",size=1.2)+ #This is the segmented regression, have to raise the y variable 10^ if it has been transformed
  #geom_line(aes(y=predict),color="black",size=1.2)+
  #geom_line(data=methanefluxes.loess.fit,aes(x=10^log_area_ha,y=loess_fit),color="purple")+
  #geom_line(data=temp,aes(x=area_ha,y=loess_fit),color="green")+
  scale_y_continuous(limits=c(3,10000),trans="log10",breaks=c(10,100,1000,10000),labels=trans_format("log10",math_format(10^.x)))+
  labs(y=bquote(TP~(mu*g~L^-1)),x=bquote(Emergent~veg.~('%')))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
  )
#**tp: FIGURE export as jpg####
#ggsave("plots/WIAP-FigureX-tpScatter-emerg.jpg", plot=gg.tp.emerg.scatter, width=3, height=3,units="in", dpi=300)

#Do stats on selected models####
#First combine the cutoffs based on which was selected for segmented vs. logistic
#This adds in a mutual column called cutoff
aicc.df.emerg.summary<-bind_rows(aicc.df.emerg%>%filter(type=="tn"|type=="tp"|type=="ph")%>%dplyr::select(type,segmented.cutoff,segmented.cutoff.SE)%>%mutate(cutoff=segmented.cutoff))%>%arrange(cutoff)
#Now summarize all cutoffs for manuscript
aicc.df.emerg.summary%>%ungroup()%>%summarize(mean=mean(cutoff),stderr=sd(cutoff)/sqrt(length(cutoff)),median=median(cutoff),stderr_propogated=sqrt(sum(segmented.cutoff.SE^2)))                        

#Export the aicc table for supplemental####
write_csv(aicc.df.emerg,"outputs/WIAP-SupplementalTable-aicc-Emergent.csv")


#goes left to right
#panel letter size
panel.size<-10
List<-list(gg.chla.emerg.scatter+
             theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             #scale_y_continuous(limits=c(10,10000),trans="log10",breaks=c(10,100,1000,10000),labels=trans_format("log10",math_format(10^.x)))+
             #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="d",fontface="bold"))+
             ggtitle("(a)"),
          gg.tn.emerg.scatter+
             theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             #scale_y_continuous(limits=c(1,10000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
             ggtitle("(b)"),
          gg.tp.emerg.scatter+
             theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             #scale_y_continuous(breaks=c(3,6,9))+ 
             #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="b",fontface="bold"))+
             ggtitle("(c)"),
          gg.ph.emerg.scatter+
            theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
            #scale_y_continuous(limits=c(1,1200),trans="log10",breaks=c(1,10,100,1000),labels=trans_format("log10",math_format(10^.x)))+ 
            ggtitle("(d)")
)

#Plot them using patchwork####
(gg.largefigureemerg.patchwork.2x2<-wrap_plots(List,ncol = 2,nrow = 2)&theme(plot.margin = unit(c(3,3,3,3),"pt")))
#Could do a 3x3 with width 6, height = 5
ggsave("plots/WIAP-FigureX-EcosystemFunctionVEmergent2x2.jpg", plot=gg.largefigureemerg.patchwork.2x2, width=4, height=3.8,units="in", dpi=300)
