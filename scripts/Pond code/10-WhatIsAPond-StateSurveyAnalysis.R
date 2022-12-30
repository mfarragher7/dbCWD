#Code to read in state survey datasets  
#created by Kathryn Hoffman (KH) on Jun 4, 2020


#peach #F7AC8A
#other colors:
#light blue #0072B2
#mustard yellow #FFBF1D
#darker mustard #FFB700
#lighter yellow #EEDC82
#Packages and libraries####
#Checks if you have the package and if not, installs it
if(!require(cowplot)){install.packages("cowplot")}
if(!require(googleway)){install.packages("googleway")} 
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggrepel)){install.packages("ggrepel")}
if(!require(ggspatial)){install.packages("ggspatial")}
#if(!require(libwgeom)){install.packages("libwgeom")} #no package called libwgeom
if(!require(sf)){install.packages("sf")}
if(!require(rnaturalearth)){install.packages("rnaturalearth")}
if(!require(rnaturalearthdata)){install.packages("rnaturalearthdata")}
if(!require(rgeos)){install.packages("rgeos")}
if(!require(rnaturalearthhires)){install.packages("rnaturalearthhires")} #necessary for large scale map, 
#maybe necessary for this
devtools::install_github("UrbanInstitute/urbnmapr")
#rnaturalearthhires not available for 4.0.0 - possibly unnecessary
if(!require(maps)){install.packages("maps")}
library(maps)
library(tidyverse)
#Load all libraries
library(ggplot2)
library(sf) #simple feature package for mapping, sp is another option, does not work with ggplot2
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(tidyverse)
library(urbnmapr)



statesurvey<-read.csv('RawData/StateSurvey/StateSurvey_useme.csv') #read in csv
dim(statesurvey) #42, 16
head(statesurvey)
df <- count(statesurvey, wetland)
df
df <- count(statesurvey, pond)
df
df <- count(statesurvey, lake)
df
#Ggplot can work well with maps package
library(maps)
library(tidyverse)

#Mapping####
#Can look at data with maps
usa<-map_data('usa')
canada<-map_data('world')%>%filter(region=="Canada")

#Read in state data
state<-map_data('state')  
head(state)

setwd("~/What_is_a_pond")

survey.results<-read_csv("~/What_is_a_pond/RawData/StateSurvey/StateSurveyResponses.csv")
survey.results
StatesWithSurveys<-pull(survey.results%>%filter(responded=="Y")%>%dplyr::select(us_state)%>%mutate(us_state=tolower(us_state)))
StatesWithSurveys
StatesWithNoSurveys<-pull(survey.results%>%filter(responded=="N")%>%dplyr::select(us_state)%>%mutate(us_state=tolower(us_state)))
StatesWithNoSurveys
#Pull the data for non-contigious states (Alaska, Hawaii)
noncontigiousStats<-map_data("world")%>%filter(subregion=="Hawaii"|subregion=="Alaska")%>%mutate(subregion=tolower(subregion))

#Map of contiguous USA with green states as survey results###
# ggplot()+
#   geom_polygon(data=usa,aes(x=long,y=lat,group=group),fill='white',color='black')+
#   geom_polygon(data=state,aes(x=long,y=lat,group=group),fill='white',color='black')+
#   geom_polygon(data=state[state$region%in%StatesWithSurveys,],aes(x=long,y=lat,group=group),fill='blue',color='black')+
#   geom_polygon(data=state[state$region%in%StatesWithNoSurveys,],aes(x=long,y=lat,group=group),fill='orange',color='black')
# 
# ggplot()+
#   geom_polygon(data=states_sf,aes(x=long,y=lat,group=group),fill='white',color='black')+
#   geom_polygon(data=state,aes(x=long,y=lat,group=group),fill='white',color='black')+
#   geom_polygon(data=state[state$region%in%StatesWithSurveys,],aes(x=long,y=lat,group=group),fill='blue',color='black')+
#   geom_polygon(data=state[state$region%in%StatesWithNoSurveys,],aes(x=long,y=lat,group=group),fill='orange',color='black')
library(tidyverse)
library(urbnmapr)

states_sf <- get_urbn_map("states", sf = TRUE)

states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")

#merge states_sf with survey.results
states_sf1 <- states_sf %>% 
  left_join(survey.results, by = c("state_name" = "us_state"))
states_sf1
states_sf1<-(na.omit(states_sf1))  
#plot state responses
states_sf1 %>% 
    ggplot(aes(fill = responded)) +scale_fill_manual(values=c("lightgrey", "#49B192"))+
    geom_sf()
#ggsave("~/What_is_a_pond/Plots/StateSurveyAnalysis/map_stateresponses.png", width = 20, height = 13, units = "in")

#scale_fill_manual(values=c("lightgrey", "#E18035", "#49B192"))

#state survey data analysis####
statesurvey<-read.csv('RawData/StateSurvey/StateSurvey_useme.csv') #read in csv
dim(statesurvey) #42, 16
head(statesurvey)
#join map with survey data
states_sf_results <- states_sf %>% 
  left_join(statesurvey, by = c("state_name" = "state"))
dim(states_sf_results) #51,19
head(states_sf_results)

statesurvey<-read.csv('RawData/StateSurvey/Figureupdate1.csv') #read in csv
dim(statesurvey) #42, 16
head(statesurvey)
#join map with survey data
states_sf_results <- states_sf %>% 
  left_join(statesurvey, by = c("state_name" = "state"))
dim(states_sf_results) #51,19
head(states_sf_results)

states_sf_results$inclusive_pond2 = states_sf_results$inclusive_pond
states_sf_results[is.na(states_sf_results$inclusive_pond2)]<-"no"
states_sf_results1<-states_sf_results
states_sf_results1[is.na(states_sf_results1)]<-"no"
states_sf_results1$pond
head(states_sf_results)
#map with pond definition
states_sf_results %>% 
  ggplot(aes(fill = pond)) +  scale_fill_manual(values=c("#EEDC82", "#0072B1", "lightgrey"
  ))+
  geom_sf()+theme_bw()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
#ggsave("~/What_is_a_pond/Plots/StateSurveyAnalysis/map_ponddefflax.png", width = 20, height = 14, units = "in")

#map with lake definition
states_sf_results %>% 
  ggplot(aes(fill = lake)) +
  geom_sf()
#ggsave("~/What_is_a_pond/Plots/StateSurveyAnalysis/map_lakedef.png", width = 20, height = 10, units = "in")

#map with wetland definition
states_sf_results %>% 
  ggplot(aes(fill = wetland)) +
  geom_sf()
#ggsave("~/What_is_a_pond/Plots/StateSurveyAnalysis/map_wetlanddef.png", width = 20, height = 10, units = "in")

# # heatColor
# heatColor <- c("peru", "peru", "orchid", 
#                 "deepskyblue", "mediumseagreen")
# 
# # Generate plot
# scale_color_manual(labels=c("NA","no","both",
#                             "lake","waters","wetland") ,values = heatColor)
#map with possible pond definition
states_sf_results1 %>% 
  ggplot(aes(fill=inclusive_pond)) + scale_color_brewer(palette = "Dark2")+
 geom_sf() 
  
# state_categorical <- statedata %>% 
#   mutate(cat_var = paste0("Group ",
#                           sample(1:4, nrow(statedata), replace = TRUE))) %>% 
#   left_join(get_urbn_map(map = "states", sf = TRUE), by = "state_name")
# 
# 
# ggplot() +
#   geom_sf(state_categorical, mapping = aes(fill = cat_var),
#           color = "#ffffff") +
#   scale_fill_discrete() +
#   coord_sf(datum = NA) +
#   labs(fill = "Categorical variable")
# + scale_color_manual(labels=c("NA","no","both",
#                                           "lake","waters","wetland") ,values = heatColor)
# #ggsave("~/What_is_a_pond/Plots/StateSurveyAnalysis/map_defaultponddef.png", width = 20, height = 10, units = "in")

states_sf_results

category<-c("Pond","Lake", "Wetland", "Use Pond in Legislation")  
YES<- c(1,11,30,21)
NO<- c(41,31,12,21)
NR <-c(8,8,8,8)
barplottime<- data.frame(category, YES, NO, NR)
barplottime
barplottime$category <- factor(barplottime$category,levels = c("Wetland", "Lake", "Pond", "Use Pond in Legislation"))

barplot <- reshape2::melt(barplottime, id = "category")
barplot
ggplot(barplot, aes(x = category, y = value, fill = variable, label = value)) + 
  geom_bar(stat = "identity") +   
  geom_text(size = 3, position= position_stack(vjust = 0.5))

ggplot(barplot, aes(x = category, y = value, fill = factor(variable, levels=c("NR", "NO", "YES")))) +
  geom_bar(stat = "identity") + labs(fill="Survey Response", x="  ", y="Number of State Responses")+
  scale_x_discrete(labels=c("Wetland", "Lake", "Pond", "Use Pond \n in Legislation")) +
  theme(plot.title = element_text(size = 20, face = "bold"),   legend.title=element_text(size=20), 
  legend.text=element_text(size=50), axis.text = element_text(size=20), axis.title = element_text(size=20))+
  scale_fill_manual(values=c("#DCDCDC", "#F7AC8A", "#0072B1"
                            ))  + #geom_bar(stat = "identity") +   
  #geom_text((label = "value"), size = 3, position= position_stack(vjust = 0.5)) +
  geom_text(aes(label = paste0(format(round((value), 1)))),size = 5, position = position_stack(vjust = 0.5))+
theme_bw()+
   theme( panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18, face = "bold"),   legend.title=element_text(size=12), legend.position = "top", 
                legend.text=element_text(size=12), axis.text = element_text(size=14), axis.title = element_text(size=14))
#ggsave("~/What_is_a_pond/Plots/StateSurveyAnalysis/SSAbarplotpeach.png", width = 6, height = 4.5, units = "in")

#SUPPLEMENTAL INFORMATION FIGURE #3
ggplot(barplot, aes(x = category, y = value, fill = factor(variable, levels=c("NR", "NO", "YES")))) +
  geom_bar(stat = "identity") + labs(fill="Survey Response", x="  ", y="Number of State Responses")+
  scale_x_discrete(labels=c("Wetland", "Lake", "Pond", "Use Pond \n in Legislation")) +
  theme(plot.title = element_text(size = 20, face = "bold"),   legend.title=element_text(size=20), 
        legend.text=element_text(size=50), axis.text = element_text(size=20), axis.title = element_text(size=20))+
  scale_fill_manual(values=c("#DCDCDC", "#EEDC82", "#0072B1"
  ))  + #geom_bar(stat = "identity") +   
  #geom_text((label = "value"), size = 3, position= position_stack(vjust = 0.5)) +
  geom_text(aes(label = paste0(format(round((value), 1)))),size = 5, position = position_stack(vjust = 0.5))+
  theme_bw()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         plot.title = element_text(size = 18, face = "bold"),   legend.title=element_text(size=12), legend.position = "top", 
         legend.text=element_text(size=12), axis.text = element_text(size=14), axis.title = element_text(size=14))
ggsave("~/What_is_a_pond/Plots/StateSurveyAnalysis/SSAbarplotyellow.png", width = 6, height = 4.5, units = "in")


#geom_text(aes(label = paste0(format(round((value/42*100), 1)), nsmall = 0,"%")),size = 3, position = position_stack(vjust = 0.5))

#retry mapping
library(ggplot2)
library(maps)

# Get all states data
all_states <- map_data("state")
all_states
# Merge two set of dataframes
heatTotal <- merge(all_states, survey.results,by="region")
heatTotal <- all_states %>% 
  left_join(statesurvey, by = c("region" = "state"))
heatTotal
# heatColor
heatColor <- c("peru", "hotpink", "orchid", 
               "mediumpurple", "deepskyblue", "cyan3")

# Generate plot
usHeatMap <- ggplot(data = heatTotal) + 
  geom_polygon(aes(x = long, y = lat, fill = wetland, group = group)) + 
  coord_fixed(1.3) + 
  labs(title = "2018-19 Influenza Season Week 4",
       x = "Longitude", y="Latitude", color="Heat level") + 
  scale_color_manual(labels=c("NA","no","both", "lake","waters","wetland") ,values = heatColor)
usHeatMap

