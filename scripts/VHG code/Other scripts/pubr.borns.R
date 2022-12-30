#compare under-ice and summer dates
#SPECIFICALLY compare buoy deployment dates and July 20-21 dates
#updated 2020-08-27

#libraries
library(dplyr)
library(ggplot2)
library(ggpubr)

#Subset data ####
#subset exo
seasons.exo = full.exo %>% 
  mutate(lakemonth = ifelse(lakedate=='JP_2020-02-21','Jordan Pond - Feb 2020',NA))%>%  #make lakemonth column
  mutate(lakemonth = replace(lakemonth,lakedate=='SC_2020-02-21','Seal Cove Pond - Feb 2020')) %>%   #add lakemonth for relevant samples
  mutate(lakemonth = replace(lakemonth,lakedate=='WH_2020-02-25','Witch Hole Pond - Feb 2020')) %>%   
  mutate(lakemonth = replace(lakemonth,lakedate=='JP_2020-07-21','Jordan Pond - July 2020')) %>%   
  mutate(lakemonth = replace(lakemonth,lakedate=='SC_2020-07-20','Seal Cove Pond - July 2020')) %>%   
  mutate(lakemonth = replace(lakemonth,lakedate=='WH_2020-07-20','Witch Hole Pond - July 2020')) %>% 
  filter(!is.na(lakemonth)) #remove the rest

#subset c3
seasons.c3 = full.c3 %>% 
  mutate(lakemonth = ifelse(lakedate=='JP_2020-02-21','Jordan Pond - Feb 2020',NA))%>%  #make lakemonth column
  mutate(lakemonth = replace(lakemonth,lakedate=='SC_2020-02-21','Seal Cove Pond - Feb 2020')) %>%   #add lakemonth for relevant samples
  mutate(lakemonth = replace(lakemonth,lakedate=='WH_2020-02-25','Witch Hole Pond - Feb 2020')) %>%   
  mutate(lakemonth = replace(lakemonth,lakedate=='JP_2020-07-21','Jordan Pond - July 2020')) %>%   
  mutate(lakemonth = replace(lakemonth,lakedate=='SC_2020-07-20','Seal Cove Pond - July 2020')) %>%   
  mutate(lakemonth = replace(lakemonth,lakedate=='WH_2020-07-20','Witch Hole Pond - July 2020')) %>% 
  filter(!is.na(lakemonth)) #remove the rest

#subset chl
seasons.chl = full.samples %>% 
  mutate(lakemonth = ifelse(lakedate=='JP_2020-02-21','Jordan Pond - Feb 2020',NA))%>%  #make lakemonth column
  mutate(lakemonth = replace(lakemonth,lakedate=='SC_2020-02-21','Seal Cove Pond - Feb 2020')) %>%   #add lakemonth for relevant samples
  mutate(lakemonth = replace(lakemonth,lakedate=='WH_2020-02-25','Witch Hole Pond - Feb 2020')) %>%   
  mutate(lakemonth = replace(lakemonth,lakedate=='JP_2020-07-21','Jordan Pond - July 2020')) %>%   
  mutate(lakemonth = replace(lakemonth,lakedate=='SC_2020-07-20','Seal Cove Pond - July 2020')) %>%   
  mutate(lakemonth = replace(lakemonth,lakedate=='WH_2020-07-20','Witch Hole Pond - July 2020')) %>% 
  filter(!is.na(lakemonth)) #remove the rest


#Plots ####
#color pairs
b = c('cadetblue1','deepskyblue4')
g = c('darkolivegreen2','darkolivegreen4')
r = c('indianred1','indianred4')
bgr = c(b,g,r)

#oxygen
plot.compare.exo.dosat = ggplot(seasons.exo, aes(x=Depth, y=DO_sat, color=lakemonth)) +
  geom_point(size=2) +
  geom_path(linetype=2) +
  scale_x_reverse(n.breaks=6) +
  labs(title="Dissolved Oxygen", color="Lake - Date", x="Depth (m)", y="Dissolved Oxygen (% Saturation)") +
  scale_color_manual(values=bgr) +
  theme_classic() +
  coord_flip()
plot.compare.exo.dosat

#fDOM
plot.compare.exo.fdom = ggplot(seasons.exo, aes(x=Depth, y=fDOM_corr, color=lakemonth)) +
  geom_point(size=2) +
  geom_path(linetype=2) +
  scale_x_reverse(n.breaks=6) +
  labs(title="fDOM", color="Lake - Date", x="Depth (m)", y="fDOM (RFU)") +
  scale_color_manual(values=bgr) +
  theme_classic() +
  coord_flip()
plot.compare.exo.fdom

#temp
plot.compare.c3.temp = ggplot(seasons.c3, aes(x=Depth, y=Temp, color=lakemonth)) +
  geom_point(size=2, shape=1) +
  scale_x_reverse(limits=c(28,1), n.breaks=6) +
  labs(title="Temperature", color="Lake - Date", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  scale_color_manual(values=bgr) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() +
  coord_flip()
plot.compare.c3.temp

#chla
plot.compare.c3.chl = ggplot(seasons.c3, aes(x=Depth, y=Chlorophyll_a, color=lakemonth)) +
  geom_point(size=2, shape=1) +
  scale_x_reverse(limits=c(28,1), n.breaks=6) +
  labs(title=expression("Chlorophyll"~italic(a)), color="Lake - Date", x="Depth (m)", y=expression("Chlorophyll"~italic(a)~"(RFU)")) +
  scale_color_manual(values=bgr) +
  theme_classic() +
  coord_flip()
plot.compare.c3.chl

#phyco
plot.compare.c3.phyco = ggplot(seasons.c3, aes(x=Depth, y=Phycocyanin, color=lakemonth)) +
  geom_point(size=2, shape=1) +
  stat_smooth(method = "loess",  size=1, se=F, show.legend=FALSE) +
  scale_x_reverse(limits=c(28,1), n.breaks=6) +
  ylim(0,20) +
  labs(title="Phycocyanin", color="Lake - Date", x="Depth (m)", y="Phycocyanin (RFU)") +
  scale_color_manual(values=bgr) +
  theme_classic() +
  coord_flip()
plot.compare.c3.phyco

#CDOM
plot.compare.c3.cdom = ggplot(seasons.c3, aes(x=Depth, y=CDOM, color=lakemonth)) +
  geom_point(size=2) +
  scale_x_reverse(limits=c(28,1), n.breaks=6) +
  ylim(0,1500) +
  labs(title="cDOM", color="Lake", x="Depth (m)", y="cDOM (RFU)") +
  scale_color_manual(values=bgr) +
  theme_classic() +
  coord_flip()
plot.compare.c3.cdom


plot.compare.feb.to.july = ggarrange(plot.compare.c3.temp,
                                    plot.compare.c3.chl,
                                    plot.compare.exo.dosat, 
                                    plot.compare.exo.fdom,
                                    ncol=2,nrow=2,common.legend=TRUE,legend='bottom')
plot.compare.feb.to.july






# Borns Symposium Fig ####
#Compare under-ice chl profiles to summer profiles for 3 lakes

#color pairs
b = c('cadetblue1','deepskyblue4')
g = c('darkolivegreen2','darkolivegreen4')
r = c('indianred1','indianred4')
bgr = c(b,g,r)


plot.borns = ggplot(seasons.c3, aes(x=Depth_unfixed, y=Chlorophyll_a, color=lakemonth)) +
  geom_point(size=2, shape=1) +
  stat_smooth(method = "loess",  size=1, se=F, show.legend=FALSE) +
  scale_x_reverse(limits=c(28,1), n.breaks=6) +
  scale_y_continuous(limits=c(0,550), n.breaks=6) +
  scale_color_manual(values=bgr) +
  labs(title=" ", color="Pond - Date", x="Depth (m)", y=expression("Chlorophyll"~italic(a)~"(RFU)")) +
  #guides(fill = guide_legend(label.position = "bottom")) + #SHOULD WORK BUT DOESNT
  guides(color = guide_legend(override.aes=list(shape=19))) +
  coord_flip() + 
  theme_classic(base_size = 17)
plot.borns

plot.borns = ggarrange(plot.borns, legend = 'bottom')
plot.borns


use theme()
 #like this

combinedPAR.19 + theme(axis.title.x = element_blank(),
                       axis.title.y = element_text(face='plain',size=12,vjust=1),
                       axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.5,vjust=0.5),
                       axis.text.y = element_text(face='plain',size=10,color='black',hjust=1), 
                       strip.background = element_blank(),
                       legend.position=c(0.08,0.9),
                       legend.background = element_blank(),
                       legend.title = element_blank(),
                       axis.line.y = element_line(size = 0.25, colour = "black"),
                       axis.line.x = element_line(size = 0.25, colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.background =  element_blank())





