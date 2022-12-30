library(tidyverse)
library(lubridate)


library(plotly)
library(isoband)
library(reshape2)
library(ggplot2)

andro21<-read.csv("C:/R/Misc/30mile/AndroDO_2021_SDT.csv", stringsAsFactors = FALSE) %>% 
  select(Sample.Date,DEPTH,TEMP,OXYGEN,SDT) %>%
  mutate(SAMPDATE = ymd(Sample.Date))%>% 
  select(SAMPDATE, DEPTH, TEMP, OXYGEN, SDT) %>% 
  filter(SAMPDATE < "2021-12-01")


head(andro21)



do_plot <-
  ggplot(andro21, aes(x = SAMPDATE, y = DEPTH, z = OXYGEN))+
  geom_contour_filled(aes(fill = stat(level)), show.legend = NA)+
  scale_fill_brewer(palette = rev("RdYlBu"))+
  labs(fill = "Dissolved \nOxygen (mg/L)")+
  geom_hline(aes(yintercept = 2, linetype = "Algae Bloom \nThreshold \n(2 m SDT)"), color= "green", size = 1.5)+
  geom_vline(aes(xintercept  = SAMPDATE),  color= "grey80", size = .5, linetype = "dashed")+
 geom_point(aes(x = SAMPDATE, y = SDT, color = "SDT"), size =7, pch = 10, stroke = 1.5)+
  scale_color_manual(name = "",
                     labels = "SDT (m)",
                     values = c("SDT"= "black"))+
  # scale_fill_manual()+
    scale_linetype_manual(name = "", values = c("dashed", "dashed"))+
    
  scale_y_reverse()+
  # scale_linetype_manual(name = "", #values = c("dashed", "dashed"),
  #   labels=c("Algae Bloom \nThreshold \n(2 m SDT)" ="Algae Bloom \nThreshold \n(2 m SDT)", 
  #            "Profile Events" = "Profile Events"),
  #   values=c("Algae Bloom \nThreshold \n(2 m SDT)" ="dashed", "Profile Events" = "dashed"))+
  # guide = guide_legend(order = 2))+


  ylab("Depth (m)")+ xlab("Sample Date")+
  theme_bw()



# Temperature Heat Map

t_plot<- ggplot(andro21, aes(x = SAMPDATE, y = DEPTH, z = TEMP))+
  geom_contour_filled(aes(fill = stat(level)))+
  labs(fill = "Water \nTemperature \n(°C)")+
  scale_fill_brewer(palette = "Spectral", direction = -1)+
  # Add the SDT data:
  geom_vline(aes(xintercept  = SAMPDATE),  color= "grey80", size = .5, linetype = "dashed")+
  
  geom_point(aes(x = SAMPDATE, y = SDT, color = "SDT"), size =7, pch = 10, stroke = 1.5)+
  scale_color_manual(name = "",
                     labels = "SDT (m)",
                     values = c("SDT"= "black"))+
  scale_y_reverse()+
  geom_hline(aes(yintercept = 2, linetype = "Algae Bloom \nThreshold \n(2 m SDT)"), color= "green", size = 1.5)+
  scale_linetype_manual(name = "", values = "dashed")+
  # Set axis labels:
  ylab("Depth (m)")+ xlab("Sample Date")+
  
  #Set Theme:
  theme_bw()

# library(ggpubr)


ggarrange(do_plot, t_plot,   ncol = 1, nrow = 2,labels = "")

ggsave("C:/R/Misc/30mile/Andro_DO_Temp_Heatmaps.png", width = 12, height = 12, units = "in")















# ggplot(andro21, aes(x = SAMPDATE, y = DEPTH, z = OXYGEN))+
#   geom_contour_filled(aes(fill = stat(level)), binwidth =  1)+
#   scale_fill_brewer(palette = "RdYlBu")+
#   labs(fill = "Dissolved \nOxygen (mg/L)")+
#   
# 
#   # call the pH data:
#   geom_point(aes(x = SAMPDATE, y = SDT, color = "SDT"), size =7, pch = 10, stroke = 1.5)+
#   # scale_linetype_manual(name = "", values = c("SDT" = "solid"))+
#   # scale_shape_manual(name = "SDT", values = "white")+
#   scale_color_manual(name = "",
#                     labels = "SDT (m)",
#                     values = c("SDT"= "black"))+
#   # Reverse the primary axis and create a secondary axis for pH:
#   # scale_y_reverse(sec.axis =dup_axis(name = "SDT",~(.-12.5)*-1 ))+
#   scale_y_reverse()+
#   
#   
#   geom_hline(aes(yintercept = 2, linetype = "Algae Bloom \nThreshold \n(2 m SDT)"), color= "green", size = 1.5)+
#   
#   # Set axis labels:
#   ylab("Depth (m)")+ xlab("2021 Sample Date")+
#   
#   #Set Theme:
#   theme_bw()







# junk<-read.csv("C:/R/Misc/MadeUpData/MadeUpProfiles.csv", stringsAsFactors = FALSE)
# 
# junk$DATE<-mdy(junk$FixDate)
# junk$Depth<-round(junk$Depth.m,0)
# 
# junk<-junk %>% 
#   arrange(as.Date(DATE), Depth)
# 









andro21<-read.csv("C:/R/Misc/30mile/AndroDO_2021.csv", stringsAsFactors = FALSE) %>% 
  # select(Sample.Date,DEPTH,TEMP,OXYGEN,SDT) %>% 
  mutate(SAMPDATE = ymd(Sample.Date))


ggplot(andro21, aes(x = SAMPDATE, y = DEPTH, z =TEMP))+
  geom_contour_filled(aes(fill = stat(level)))+
  scale_fill_brewer(palette = "RdYlBu")+
  labs(fill = "Temp") +
  
  
  
  # call the Temp Thresh data:
  geom_contour(aes(x = SAMPDATE, y = DEPTH, z = TempThresh, color = "SDT"), size =7, pch = 10, stroke = 1.5)+
  # scale_linetype_manual(name = "", values = c("SDT" = "solid"))+
  # scale_shape_manual(name = "SDT", values = "white")+
  scale_color_manual(name = "",
                     labels = "SDT (m)",
                     values = c("SDT"= "black"))+
  # Reverse the primary axis and create a secondary axis for pH:
  # scale_y_reverse(sec.axis =dup_axis(name = "SDT",~(.-12.5)*-1 ))+
  scale_y_reverse()+
  
  
  geom_hline(aes(yintercept = 2, linetype = "Algae Bloom \nThreshold \n(2 m SDT)"), color= "green", size = 1.5)+
  scale_linetype_manual(name = "", values = "dashed")+
  
  # Set axis labels:
  ylab("Depth (m)")+ xlab("2021 Sample Date")+
  
  #Set Theme:
  theme_bw()
