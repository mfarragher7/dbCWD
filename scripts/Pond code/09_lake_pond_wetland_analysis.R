#Pond vs.lake vs. wetland data analysis
#created 2020-Dec-16 by KK

#### load libraries and data #### 
#check for required packages
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(car)){install.packages("car")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(gtools)){install.packages("gtools")}
if(!require(cowplot)){install.packages("cowplot")}
if(!require(raster)){install.packages("raster")}

#load libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(car) # Levene's test 
library(agricolae) #LSD test
library(gtools)
library(cowplot)
library(raster)

#*load cleaned pond data ####
singleponds <- read.csv("Data_cleaned/Singleponds_Cleaned.csv")

#remove out large/deep lakes. n=1364
singleponds <- singleponds %>% 
  filter((max_depth_m<=9 | is.na(max_depth_m)==T) &
           (mean_depth_m<=9 | is.na(mean_depth_m)==T) &
           (max_surfacearea_m2<=200000 | is.na(max_surfacearea_m2)==T) &
           (mean_surfacearea_m2<=200000 | is.na(mean_surfacearea_m2)==T))
singleponds$type<-"pond"

#*load in public lake data ####
lagos<- read.csv("RawData/Lakes/LAGOS.csv")
nla<- read.csv("RawData/Lakes/NLA.csv")
waterbase<- read.csv("RawData/Lakes/Waterbase.csv")
#hydrolakes <- read.csv("RawData/Lakes/Public_data/Hydrolakes.csv") %>% # only has area and depth so not needed for this analysis
    #              rename('Latitude..decimal.degree.'='Latitude..decimal.degrees.', 'Longitude..decimal.degree.'='Longitude..decimal.degrees.')
all_lakes<-gtools::smartbind(lagos, nla, waterbase) #add in hydrolakes if you want to look at depth/area plots
all_lakes$type<-"lake"

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
                     mean_EMERGVEG = mean(WATER_EMERGVEG, na.rm=TRUE))

wetland_veg_mean$macrophytes_percentcover<-wetland_veg_mean$mean_EMERGVEG + wetland_veg_mean$mean_AQVEG  #add up the submerged, floating, and emergent veg
wetland_veg_mean$macrophytes_percentcover[wetland_veg_mean$macrophytes_percentcover > 100] <- 100  #can't have over 100% coverage

#join tables  
merged_WL<-left_join(wetland_siteinfo, wetland_surfacewater) %>%
  left_join(wetland_chem) %>%
  left_join(wetland_chla) %>%
  left_join(wetland_veg_mean) 

#select wetlands that were not re-sampled, that are freshwater, and that have enough water for a water chem sample  
WL_singlevisit<-filter(merged_WL, SITE_USE != "NWCA_REVISITS" ) #remove repeat samples 
WL_fresh<-filter(WL_singlevisit, WATER_SALINITY == "FRESH") #458 sites with freshwater 
wetland_dat<-WL_fresh[!(is.na(WL_fresh$TP)),] # 400 sites with enough water for chem measures 
wetland_dat$type<-"wetland"
wetland_dat$max_depth_m<-wetland_dat$SW_DEPTH/100  #convert cm to m 
wetland_dat$tn_ugpl<-wetland_dat$TN * 1000  #convert mg/L to ug/L

### select out comparable variables to compare all 3 "types" 
ponds<-dplyr::select(singleponds, pondname, max_depth_m, mean_depth_m, max_surfacearea_m2, mean_surfacearea_m2, ph, doc_mgpl, chla_ugpl, tp_ugpl, tn_ugpl, cond_uspcm, macrophytes_percentcover, type) %>%
  rename(siteid = pondname)
wetlands<-dplyr::select(wetland_dat, UID, max_depth_m, COND, PH, tn_ugpl, TP, CHLA, macrophytes_percentcover, type ) %>%
  rename(siteid=UID, cond_uspcm=COND, ph=PH, tp_ugpl=TP, chla_ugpl= CHLA)
lakes<-dplyr::select(all_lakes, PondName, Max.depth_m, Mean.depth_m, Max.surface.area_m2, Mean.surface.area_m2, pH, DOC_mgpL, Chla_ugpL, TP_ugpL, TN_ugpL, Cond_uSpcm, Macrophytes_percentcover, type) %>%
  rename(siteid = PondName, max_depth_m= Max.depth_m, mean_depth_m=Mean.depth_m, max_surfacearea_m2=Max.surface.area_m2, mean_surfacearea_m2=Mean.surface.area_m2, ph=pH, doc_mgpl = DOC_mgpL, chla_ugpl= Chla_ugpL, tp_ugpl= TP_ugpL, tn_ugpl=TN_ugpL, cond_uspcm=Cond_uSpcm, macrophytes_percentcover=Macrophytes_percentcover)

all_ecos<-gtools::smartbind(ponds, lakes, wetlands)

#change char to factors and numeric 
all_ecos$type<-as.factor(all_ecos$type)
all_ecos$siteid<-as.factor(all_ecos$siteid)
all_ecos<-all_ecos %>%
  mutate_if(is.character,as.numeric) 

#*merge mean and max surface area, and take max if there are both ####
all_ecos <- all_ecos %>%
  mutate(
    suface_area = case_when(
      is.na(all_ecos$max_surfacearea_m2) ~ all_ecos$mean_surfacearea_m2, #if ~ then
      is.na(all_ecos$mean_surfacearea_m2) ~ all_ecos$max_surfacearea_m2, #if ~ then 
      TRUE      ~ all_ecos$max_surfacearea_m2))  ### the else part 

#create a new colomn for hectares 
all_ecos$surfacearea_ha<-all_ecos$suface_area/10000

#### lakes, wetlands, streams violin plots for manuscript #### 

# *visualizing TP ####

# ANOVA 
hist(all_ecos$tp_ugpl) 
hist(log(all_ecos$tp_ugpl))

tp_out<-anova(lm(log(tp_ugpl+ .0001) ~ type, data = all_ecos, na.action = na.omit ))

LSD_tp<-LSD.test(log(all_ecos$tp_ugpl+ .0001), all_ecos$type, 10346, 1.54, alpha=0.05)  #specify the DF and MSE of the residuals
LSD_tp_means<-as.data.frame(LSD_tp$means)
names(LSD_tp_means)[1] <- c("mean")

#violin plots 
tp_plot<-ggplot(all_ecos, aes(x = type, y = tp_ugpl, fill=type)) + 
  geom_violin(trim=TRUE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="Waterbody", y = "Total phosphorus (ug/L)") + 
  scale_fill_manual(values=c("#51A9F7", "#E8D86D", "#0C8655")) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position="none") +
  scale_y_continuous(trans='log10',
                     breaks=c(1,10,100,1000, 100000),
                     labels=function(x)format(x,scientific=F)) + 
  coord_flip() + 
  annotate("text", x = 1, y = 0.05, label = "b", fontface=2) + #lake
  annotate("text", x = 2, y = 0.05, label = "a", fontface=2) + #pond
  annotate("text", x = 3, y = 0.05, label = "a", fontface=2) #wetland 

#Levene's test is statistically significant, then the null hypothesis, that the groups have equal variances, is rejected. i.e. you have unequal variances 
lav_tp<-car::leveneTest(log(tp_ugpl+ .0001) ~ type, data= all_ecos)

#coeff of variation 
cv_out_tp <- 
  all_ecos %>%
  group_by(type)%>%
  summarise(cv = cv(tp_ugpl, na.rm = TRUE))

# *visualize data for TN ####
# ANOVA 
hist(log(all_ecos$tn_ugpl))

tn_out<-anova(lm(log(tn_ugpl+ .0001) ~ type, data = all_ecos, na.action = na.omit ))
 
LSD_tn<-LSD.test(log(all_ecos$tn_ugpl+ .0001), all_ecos$type, 5649, 0.81, alpha=0.05)  #specify the DF and MSE of the residuals
LSD_tn_means<-as.data.frame(LSD_tn$means)
names(LSD_tn_means)[1] <- c("mean")

#violin plots 
tn_plot<-ggplot(all_ecos, aes(x = type, y = tn_ugpl, fill=type)) + 
  geom_violin(trim=TRUE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="Waterbody", y = "Total nitrogen (ug/L)") + 
  scale_fill_manual(values=c("#51A9F7", "#E8D86D", "#0C8655")) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position="none") +  
  scale_y_continuous(trans='log10',
                     breaks=c(0.1,10,100,1000,10000, 70000),
                     labels=function(x)format(x,scientific=F)) + 
  coord_flip() + 
  annotate("text", x = 1, y = 9, label = "c", fontface=2) + #lake
  annotate("text", x = 2, y = 9, label = "a", fontface=2) + #pond
  annotate("text", x = 3, y = 9, label = "b", fontface=2) #wetland 


#Levene's test is statistically significant, then the null hypothesis, that the groups have equal variances, is rejected. i.e. you have unequal variances 
lav_tn<-car::leveneTest(log(tn_ugpl+ .0001) ~ type, data= all_ecos)


#coeff of variation 
cv_tn <- 
  all_ecos %>%
  group_by(type)%>%
  summarise(cv = cv(tn_ugpl, na.rm = TRUE))

# *visualize data for pH ####
#remove pH>100
ph_dat<-filter(all_ecos, ph <100)

# ANOVA 
hist(ph_dat$ph)
ph_out<-anova(lm(ph ~ type, data = ph_dat, na.action = na.omit ))

LSD_pH<-LSD.test(ph_dat$ph, ph_dat$type, 4690, 0.822, alpha=0.05)  #specify the DF and MSE of the residuals
LSD_pH_means<-as.data.frame(LSD_pH$means)
names(LSD_pH_means)[1] <- c("mean")

ph_plot<-ggplot(ph_dat, aes(x = type, y = ph, fill=type)) + 
  geom_violin(trim=TRUE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="Waterbody", y = "pH") + 
  scale_fill_manual(values=c("#51A9F7", "#E8D86D", "#0C8655")) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position="none") +  
  coord_flip() + 
  annotate("text", x = 1, y = 2, label = "a", fontface=2) + #lake
  annotate("text", x = 2, y = 2, label = "b", fontface=2) + #pond
  annotate("text", x = 3, y = 2, label = "c", fontface=2) #wetland 

#coeff of variation 
cv_ph <- 
  ph_dat %>%
  group_by(type)%>%
  summarise(cv = cv(ph, na.rm = TRUE))

#Levene's test is statistically significant, then the null hypothesis, that the groups have equal variances, is rejected. i.e. you have unequal variances 
lav_ph<-car::leveneTest(ph ~ type, data= ph_dat)

# *visualize data for conductivity ####
# ANOVA 
hist(log(all_ecos$cond_uspcm))
out_cond<-anova(lm(log(cond_uspcm) ~ type, data = all_ecos, na.action = na.omit ))

#not significant ANOVA - but need means to add to the table
LSD_cond<-LSD.test(log(all_ecos$cond_uspcm), all_ecos$type, 1961, 2.486016, alpha=0.05)  #specify the DF and MSE of the residuals
LSD_cond_means<-as.data.frame(LSD_cond$means)
names(LSD_cond_means)[1] <- c("mean")

#violin plots 
cond_plot<-ggplot(all_ecos, aes(x = type, y = cond_uspcm, fill=type)) + 
  geom_violin(trim=TRUE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="Waterbody", y = "Conductivity") + 
  scale_fill_manual(values=c("#51A9F7", "#E8D86D", "#0C8655")) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none") +  
  scale_y_continuous(trans='log10',
                     breaks=c(1,10,100,1000,60000),
                     labels=function(x)format(x,scientific=F)) + 
  coord_flip()  + 
  annotate("text", x = 1, y = 0.01, label = "a", fontface=2) + #lake
  annotate("text", x = 2, y = 0.01, label = "a", fontface=2) + #pond
  annotate("text", x = 3, y = 0.01, label = "a", fontface=2) #wetland 

#Levene's test is statistically significant, then the null hypothesis, that the groups have equal variances, is rejected. i.e. you have unequal variances 
lav_cond<-car::leveneTest(log(cond_uspcm) ~ type, data= all_ecos)

cv_cond <- 
  all_ecos %>%
  group_by(type)%>%
  summarise(cv = cv(cond_uspcm, na.rm = TRUE)) 

#* TN:TP molar ratio #### 
#indicative of which nutrient would become limiting for growth
#1 μg N/l = 0.071394 μmol N/l
#1 μg P/l = 0.032285 μmol P/l
all_ecos$tn_tp_mol_ratio<-(all_ecos$tn_ugpl/(28.014*1000))/(all_ecos$tp_ugpl/(123.88*1000))

ratio_dat <- all_ecos %>% 
  filter_all(all_vars(!is.infinite(.))) #remove Inf value
out_ratio<-anova(lm(log(tn_tp_mol_ratio) ~ type, data = ratio_dat, na.action = na.omit ))

LSD_ratio<-LSD.test(log(ratio_dat$tn_tp_mol_ratio), ratio_dat$type, 5051, 0.649, alpha=0.05)  #specify the DF and MSE of the residuals from ANOVA
LSD_ratio_means<-as.data.frame(LSD_ratio$means)
names(LSD_ratio_means)[1] <- c("mean")

#violin plots 
tptn_plot<-ggplot(ratio_dat, aes(x = type, y = tn_tp_mol_ratio, fill=type)) + 
  geom_violin(trim=TRUE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="Waterbody", y = "TN:TP") + 
  scale_fill_manual(values=c("#51A9F7", "#E8D86D", "#0C8655")) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position="none") +  
  scale_y_continuous(trans='log10',
                     breaks=c(1,10,100,1000,4000),
                     labels=function(x)format(x,scientific=F)) + 
  coord_flip()  + 
  annotate("text", x = 1, y = 0.1, label = "a", fontface=2) + #lake
  annotate("text", x = 2, y = 0.1, label = "b", fontface=2) + #pond
  annotate("text", x = 3, y = 0.1, label = "c", fontface=2) #wetland 


lav_ratio<-car::leveneTest(log(tn_tp_mol_ratio) ~ type, data= ratio_dat)

cv_ratio <- 
  ratio_dat %>%
  group_by(type)%>%
  summarise(cv = cv(tn_tp_mol_ratio, na.rm = TRUE)) 

# *visualize data for CHLA ####
#ANOVA
hist(log(all_ecos$chla_ugpl))
out_chl<-anova(lm(log(chla_ugpl + 0.001) ~ type, data = all_ecos, na.action = na.omit ))

LSD_chl<-LSD.test(log(all_ecos$chla_ugpl+ 0.001), all_ecos$type, 10983, 2.065, alpha=0.05)  #specify the DF and MSE of the residuals
LSD_chl_means<-as.data.frame(LSD_chl$means)
names(LSD_chl_means)[1] <- c("mean")

#violin plots 
chl_plot<-ggplot(all_ecos, aes(x = type, y = chla_ugpl, fill=type)) + 
  geom_violin(trim=TRUE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="Waterbody", y = "Chlorophyll a (ug/l)") + 
  scale_fill_manual(values=c("#51A9F7", "#E8D86D", "#0C8655")) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position="none") +   
  scale_y_continuous(trans='log10',
                     breaks=c(1,10,100,1000),
                     labels=function(x)format(x,scientific=F)) + 
  coord_flip()+ 
  annotate("text", x = 1, y = 0.001, label = "a", fontface=2) + #lake
  annotate("text", x = 2, y = 0.001, label = "a", fontface=2) + #pond
  annotate("text", x = 3, y = 0.001, label = "b", fontface=2) #wetland 

#Levene's test is statistically significant, then the null hypothesis, that the groups have equal variances, is rejected. i.e. you have unequal variances 
lav_chl<-car::leveneTest(log(chla_ugpl+ 0.001) ~ type, data= all_ecos)

cv_chl <- 
  all_ecos %>%
  group_by(type)%>%
  summarise(cv = cv(chla_ugpl, na.rm = TRUE)) 


#*save plot #### 
LPW_plot<-cowplot::plot_grid(tp_plot, tn_plot,
                              ph_plot, chl_plot,
                             labels = c('(a)', '(b)', '(c)', '(d)'), 
                             ncol = 2)
LPW_plot
cowplot::save_plot("Plots/Obj3.jpg", LPW_plot, base_width = 6.6,
                   base_height = 8, dpi=300)

#*combine tables for supplement of manu #### 
anova.df<-bind_rows(tp_out, tn_out, ph_out, out_cond, out_ratio, out_chl) #ANOVA table results
LSD.df<-bind_rows(LSD_tp_means, LSD_tn_means, LSD_pH_means, LSD_cond_means, LSD_ratio_means, LSD_chl_means) #keep adding to this table 
lav.df<-bind_rows(lav_tp, lav_tn, lav_ph, lav_cond, lav_ratio, lav_chl) #lavenes test results
cv.df<-bind_rows(cv_out_tp, cv_tn, cv_ph, cv_cond, cv_ratio, cv_chl) # cv results 
write.csv(anova.df,'outputs/anova.csv', row.names = FALSE)
write.csv(LSD.df,'outputs/LSD.csv', row.names = FALSE)
write.csv(lav.df,'outputs/lav.csv', row.names = FALSE)
write.csv(cv.df,'outputs/cv.csv', row.names = FALSE)

#### OTHER plots #### 
# *visualize data for area ####
#violin plots 
area_plot<-ggplot(all_ecos, aes(x = type, y = surfacearea_ha, fill=type)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="type", y = "surface area (ha)") + 
  scale_fill_manual(values=c("#51A9F7", "#E8D86D", "#0C8655")) + 
  theme_classic() + 
  theme(legend.position="none") + 
  scale_y_continuous(trans='log10',
                     breaks=c(0.1, 1,10,100,1000, 3000000),
                     labels=function(x)format(x,scientific=F)) + 
  coord_flip()

## distribution plots by groups
a <- ggplot(all_ecos, aes(x = log(mean_surfacearea_m2)))
a+geom_density(aes(fill = type), alpha = 0.4) +
  scale_fill_manual(values = c("deepskyblue", "green3", "mediumpurple"))

anova(lm(log(mean_surfacearea_m2) ~ type, data = all_ecos, na.action = na.omit ))

LSD_div<-LSD.test(log(all_ecos$mean_surfacearea_m2), all_ecos$type, 1571283, 1.5, alpha=0.05)  #specify the DF and MSE of the residuals
LSD_div 

car::leveneTest(log(mean_surfacearea_m2) ~ type, data= all_ecos)

# *boxplots to visualize data for max_depth ####
ggplot(all_ecos, aes(x = type, y = log(max_depth_m))) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = type, shape = type), width = 0.2, size = 1) + 
  scale_color_manual(values = c("deepskyblue", "green3", "mediumpurple" )) + 
  labs(x = NULL)   # Remove x axis label

#violin plots 
depth_plot<-ggplot(all_ecos, aes(x = type, y = max_depth_m, fill=type)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="type", y = "max depth (m)") + 
  scale_fill_manual(values=c("deepskyblue", "#E69F00", "#33a02c")) + 
  theme_classic() + 
  theme(legend.position="none") + 
  scale_y_continuous(trans='log10',
                     breaks=c(0.1,1,10,200),
                     labels=function(x)format(x,scientific=F)) + 
  coord_flip()

## distribution plots by groups
a <- ggplot(all_ecos, aes(x = log(max_depth_m)))
a+geom_density(aes(fill = type), alpha = 0.4) +
  scale_fill_manual(values = c("deepskyblue", "green3", "mediumpurple"))

anova(lm(log(max_depth_m) ~ type, data = all_ecos, na.action = na.omit ))

LSD_div<-LSD.test(log(all_ecos$max_depth_m), all_ecos$type, 14653, 0.76, alpha=0.05)  #specify the DF and MSE of the residuals
LSD_div 

car::leveneTest(log(max_depth_m) ~ type, data= all_ecos)

# *visualize data for Macrophyte cover ####
hist(all_ecos$macrophytes_percentcover)
all_ecos$macrophytes_percentcover[all_ecos$macrophytes_percentcover > 100] <- 100  #can't have over 100% coverage
hist(all_ecos$macrophytes_percentcover)

ggplot(all_ecos, aes(x = type, y = macrophytes_percentcover)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = type, shape = type), width = 0.2, size = 1) + 
  scale_color_manual(values = c("deepskyblue", "green3", "mediumpurple" )) + 
  labs(x = NULL)   # Remove x axis label


## distribution plots by groups
a <- ggplot(all_ecos, aes(x = macrophytes_percentcover))
a+geom_density(aes(fill = type), alpha = 0.4) +
  scale_fill_manual(values = c("deepskyblue", "green3", "mediumpurple"))

#ANOVA

hist(all_ecos$macrophytes_percentcover)
anova(lm(all_ecos$macrophytes_percentcover ~ type, data = all_ecos, na.action = na.omit ))

LSD_div<-LSD.test(log(all_ecos$macrophytes_percentcover+ 0.001), all_ecos$type, 1639, 12.52, alpha=0.05)  #specify the DF and MSE of the residuals
LSD_div 
car::leveneTest(macrophytes_percentcover ~ type, data= all_ecos)

# *boxplots to visualize data for DOC #### note none for wetland 
hist(all_ecos$doc_mgpl)
hist(log(all_ecos$doc_mgpl))

ggplot(all_ecos, aes(x = type, y = log(doc_mgpl))) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = type, shape = type), width = 0.2, size = 1) + 
  scale_color_manual(values = c("deepskyblue", "green3", "mediumpurple" )) + 
  labs(x = NULL)   # Remove x axis label

## distribution plots by groups
a <- ggplot(all_ecos, aes(x = log(doc_mgpl)))
a+geom_density(aes(fill = type), alpha = 0.4) +
  scale_fill_manual(values = c("deepskyblue", "green3", "mediumpurple"))

#ANOVA
anova(lm(log(doc_mgpl) ~ type, data = all_ecos, na.action = na.omit ))

LSD_div<-LSD.test(log(all_ecos$doc_mgpl+ 0.001), all_ecos$type, 4008, 0.507, alpha=0.05)  #specify the DF and MSE of the residuals
LSD_div 

car::leveneTest(log(doc_mgpl) ~ type, data= all_ecos)

##* plot depth to area #### 
lake_pond<-filter(all_ecos, type=="lake" | type=="pond") #just lakes and ponds

ggplot(lake_pond, aes(x=log(max_depth_m), y=log(mean_surfacearea_m2), color=type))+
  scale_color_manual(values = c("deepskyblue", "black" )) + 
  geom_point(aes(fill=type), alpha = 0.4)

##* plot depth to chla #### 
ggplot(all_ecos, aes(x=chla_ugpl, y=max_depth_m, color=type))+
  scale_color_manual(values = c("deepskyblue", "green3", "mediumpurple" )) + 
  geom_point(aes(fill=type), alpha = 0.4)

#* DOC:Chla ratio #### 
## distribution plots by groups
lake_pond<-filter(all_ecos, type=="lake" | type=="pond")
lake_pond$doc_chl_ratio<-lake_pond$doc_mgpl / lake_pond$chla_ugpl

a <- ggplot(lake_pond, aes(x = doc_chl_ratio))
a+geom_density(aes(fill = type), alpha = 0.4) +
  scale_fill_manual(values = c("deepskyblue", "green3", "mediumpurple"))

anova(lm(log(doc_chl_ratio) ~ type, data = lake_pond, na.action = na.omit ))

LSD_div<-LSD.test(log(lake_pond$doc_chl_ratio), lake_pond$type, 14653, 0.76, alpha=0.05)  #specify the DF and MSE of the residuals
LSD_div 

car::leveneTest(log(mean_surfacearea_m2) ~ type, data= all_ecos)


####### Depth/ Area vs. nutients/ chla ########### 
#* make function for stats ####
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
#REMOVE rows with NAs - runs raster 
tp_dat<-all_ecos[!(is.na(all_ecos$tp_ugpl)),]
tn_dat<-all_ecos[!(is.na(all_ecos$tn_ugpl)),]
chl_dat<-all_ecos[!(is.na(all_ecos$chla_ugpl)),]
#* area vs. TP ####
fit1 <- lm(log(tp_ugpl) ~ log(mean_surfacearea_m2), data = all_ecos)
summary(fit1)
ggplotRegression(fit1)

ggplot(tp_dat, aes(x =surfacearea_ha, y=tp_ugpl, col=type)) + 
  geom_point() +
  scale_color_manual(values=c("deepskyblue", "#E69F00", "darkgreen")) +
  stat_smooth(method = "loess", col = "black") + 
  scale_x_continuous(limits=c(0.01,150000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x))) +
  scale_y_continuous(limits=c(1,500000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))

#look at just ponds 
tp_pond<-filter(tp_dat, type =="pond")
ggplot(tp_pond, aes(x = surfacearea_ha, y = tp_ugpl, col=type)) + 
  geom_point() +
  scale_color_manual(values=c( "#E69F00")) +
  stat_smooth(method = "loess", col = "black") + 
  scale_x_continuous(limits=c(0.01,200),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x))) +
  scale_y_continuous(limits=c(1,500000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))

#* area vs. TN ####
ggplot(tn_dat, aes(x = surfacearea_ha, y=tn_ugpl, col=type)) + 
  geom_point() +
  scale_color_manual(values=c("deepskyblue", "#E69F00", "darkgreen")) +
  stat_smooth(method = "loess", col = "black") + 
  scale_x_continuous(limits=c(0.01,150000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x))) +
  scale_y_continuous(limits=c(1,70000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))

#* area vs. CHL ####
ggplot(chl_dat, aes(x =surfacearea_ha, y = chla_ugpl, col=type)) + 
  geom_point() +
  scale_color_manual(values=c("deepskyblue", "#E69F00", "darkgreen")) +
  stat_smooth(method = "loess", col = "black") + 
  scale_x_continuous(limits=c(0.01,150000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x))) +
  scale_y_continuous(limits=c(1,2000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))


#* area vs. DOC ####
ggplot(all_ecos, aes(x = log(mean_surfacearea_m2), y = log(doc_mgpl))) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") 


#* depth vs. TP ####
ggplot(tp_dat, aes(x = max_depth_m, y = tp_ugpl, col=type)) + 
  geom_point() +
  scale_color_manual(values=c("deepskyblue", "#E69F00", "darkgreen")) +
  stat_smooth(method = "loess", col = "black") +
  scale_x_continuous(limits=c(0.01,200),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x))) +
  scale_y_continuous(limits=c(1,500000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))


#* depth vs. TN ####
ggplot(tn_dat, aes(x = max_depth_m, y = tn_ugpl, col=type)) + 
  geom_point() +
  scale_color_manual(values=c("deepskyblue", "#E69F00", "darkgreen")) +
  stat_smooth(method = "loess", col = "black") +
  scale_x_continuous(limits=c(0.01,200),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x))) +
  scale_y_continuous(limits=c(1,70000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))


#* depth vs. CHL ####

ggplot(chl_dat, aes(x=max_depth_m, y=chla_ugpl, col=type)) + 
  geom_point() +
  scale_color_manual(values=c("deepskyblue", "#E69F00", "darkgreen")) +
  stat_smooth(method = "loess", col = "black") +
  scale_x_continuous(limits=c(0.01,200),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x))) +
  scale_y_continuous(limits=c(0.001,2000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))

#* depth vs. DOC ####
ggplot(all_ecos, aes(x = log(max_depth_m), y = log(doc_mgpl), col=type)) + 
  geom_point() +
  scale_color_manual(values=c("deepskyblue", "#E69F00", "darkgreen")) +
  stat_smooth(method = "loess", col = "black")


#### PCA analysis #### 
library(factoextra) #library for great visualization of PCA results

#Remove rows with na
all_ecos$logchl<-log(all_ecos$chla_ugpl)
all_ecos$logtp<-log(all_ecos$tp_ugpl)
all_ecos$logtn<-log(all_ecos$tn_ugpl)
all_ecos$logcond<-log(all_ecos$cond_uspcm)
all_ecos$logtntp<-log(all_ecos$tn_tp_mol_ratio)

log_dat <- all_ecos %>% 
  filter_all(all_vars(!is.infinite(.))) #remove Inf value
pca_dat<-dplyr::select(log_dat, ph, logchl, logtp, logtn, logcond, logtntp, type) %>%
  na.omit() 

pca<-prcomp(pca_dat [, -7], scale=TRUE) #perform PCA without the type 


#eigenvalues
fviz_eig(pca)  ###eigen values associated with each PC (how much does each explain the variation in the data)
eig.val_trans<-get_eigenvalue(pca) ## table of the eigenvalues 

# graph results with the type grouping
fviz_pca_ind(pca, label="none", 
             habillage=pca_dat$type,  # group by the  type
             addEllipses=TRUE, ellipse.level=0.75)
#PCAallsites_allvar +ylim(-4,5)+xlim(-7.5, 4.0) #adjust axis if needed

#graph vectors of PCA
fviz_pca_var(pca,
                         col.var = "contrib", # Color by contributions to the PC
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE 
                         )     # Avoid text overlapping


#graph both 
fviz_pca_biplot(pca, 
                label="var", 
                habillage=pca_dat$type,  # group by the  type
                addEllipses=TRUE, ellipse.level=0.95
                )

#shows the top variables contributing to the 1st and 2nd axis
#red dotted line shows expected average contribution. 
#If the contribution of the variables were uniform, the expected value would be 1/length(variables) = 1/10 = 10%. 
#For a given component, a variable with a contribution larger than this cutoff could be considered as important in contributing to the component.
fviz_contrib(pca, 
             choice = c("var"), 
             axes = 1:2, #both PCA axis 
             sort.val = c("desc"), #descending order 
             top = 10) #show top 10 
fviz_contrib(allsites.pca, 
             choice = c("var"), 
             axes = 1, #1st PCA axis 
             sort.val = c("desc"), #descending order 
             top = 10) #show top 10
fviz_contrib(allsites.pca, 
             choice = c("var"), 
             axes = 2, #2nd PCA axis 
             sort.val = c("desc"), #descending order 
             top = 10) #show top 10
