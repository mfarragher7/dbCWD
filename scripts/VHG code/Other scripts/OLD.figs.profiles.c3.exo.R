#generate figures for c3 and exo profiles
#*** run load.everything.R first to load data
#updated 2020-08-18



# this code is getting sorta outdated....
#STILL USEFUL to look at all profiles for each lake for each sample date


#load libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(lubridate)
library(ggpubr)


#EXO plots ####
#*Jordan Pond ####

#save colors
brownblues = c( '#DFC27D',   #2
                '#BF812D',   #3
                '#8C510A',   #4
                '#000000',   #7
                '#01665E',   #10
                '#35978F',   #11
                '#80CDC1')   #12

#DO Sat
plot.jp.exo.dosat = ggplot(jp.exo, aes(x=DO_sat, y=Depth, color=factor(Date))) + 
  geom_point() +
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=6) +
  xlim(85,105) +
  scale_color_manual(values=brownblues) +
  labs(color="Date", x="Dissolved Oxygen (% Saturation)", y="Depth (m)") +
  theme_classic() + 
  ggtitle("Jordan Pond - Oxygen profiles")
plot.jp.exo.dosat 
  
#temp
plot.jp.exo.t = ggplot(jp.exo, aes(x=Temp, y=Depth, color=factor(Date))) + 
  geom_point() +
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=6) +
  xlim(0,25) +
  labs(color="Date", x="Temperature (deg C)", y="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Jordan Pond - Temperature (EXO)")
plot.jp.exo.t

#sp cond
plot.jp.exo.cond = ggplot(jp.exo, aes(x=SpCond, y=Depth, color=factor(Date))) + 
  geom_point() +
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=6) +
  scale_x_continuous(limits=c(29,35),n.breaks=10) +
  labs(color="Date", x="Specific Conductivity", y="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Jordan Pond - Specific Conductivity")
plot.jp.exo.cond

#chl RFU
plot.jp.exo.Chl.rfu = ggplot(jp.exo, aes(x=Chl_RFU, y=Depth, color=factor(Date))) + 
  geom_point() +
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=6) +
  xlim(0,0.05) +
  labs(color="Date", x="Chlorophyll RFU", y="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Jordan Pond - Chlorophyll RFU")
plot.jp.exo.chl.rfu

#chl raw
plot.jp.exo.chl2 = ggplot(jp.exo, aes(x=Chl_ugpl, y=Depth, color=factor(Date))) + 
  geom_point() +
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=6) +
  xlim(0,0.5) +
  labs(color="Date", x="Chlorophyll", y="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Jordan Pond - Chlorophyll")
plot.jp.exo.chl2

#fdom rfu corrected
plot.jp.exo.fdom.corr = ggplot(jp.exo, aes(x=fDOM_corr, y=Depth, color=factor(Date))) + 
  geom_point() +
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=6) +
  labs(color="Date", x="fDOM RFU", y="Depth (m)") +
  #scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Jordan Pond - fDOM corrected")
plot.jp.exo.fdom.corr

#fdom raw
plot.jp.exo.fdom.raw = ggplot(jp.exo, aes(x=fDOM_raw, y=Depth, color=factor(Date))) + 
  geom_point() +
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=6) +
  xlim(0,15) +
  labs(color="Date", x="fDOM raw", y="Depth (m)") +
 # scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Jordan Pond - fDOM raw")
plot.jp.exo.fdom.raw



#*Seal Cove ####
#DO Sat
plot.sc.exo.dosat = ggplot(sc.exo, aes(x=DO_sat, y=Depth, color=factor(Date))) + 
  geom_point() + 
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=11) +
  labs(color="Date", x="Dissolved Oxygen (% Saturation)", y="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Seal Cove - Oxygen profiles")
plot.sc.exo.dosat 

#temp
plot.sc.exo.t = ggplot(sc.exo, aes(x=Temp, y=Depth, color=factor(Date))) + 
  geom_point() + 
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=11) +
  labs(color="Date", x="Temperature (deg C)", y="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Seal Cove - Temperature (EXO)")
plot.sc.exo.t

#spcond
plot.sc.exo.cond = ggplot(sc.exo, aes(x=SpCond, y=Depth, color=factor(Date))) + 
  geom_point() + 
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=11) +
  labs(color="Date", x="Specific Conductivity", y="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Seal Cove - Specific Conductivity")
plot.sc.exo.cond

#chl rfu
plot.sc.exo.Chl_RFU = ggplot(sc.exo, aes(x=Chl_RFU, y=Depth, color=factor(Date))) + 
  geom_point() + 
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=11) +
  labs(color="Date", x="Chlorophyll RFU", y="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Seal Cove - Chlorophyll RFU")
plot.sc.exo.Chl_RFU

#chl 2
plot.sc.exo.chl2 = ggplot(sc.exo, aes(x=Chl_ugpl, y=Depth, color=factor(Date))) + 
  geom_point() + 
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=11) +
  labs(color="Date", x="Chlorophyll", y="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Seal Cove - Chlorophyll")
plot.sc.exo.chl2

#fdom rfu
plot.sc.exo.fdom.rfu = ggplot(sc.exo, aes(x=fDOM_RFU, y=Depth, color=factor(Date))) + 
  geom_point() + 
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=11) +
  labs(color="Date", x="fDOM RFU", y="Depth (m)") +
  #scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Seal Cove - fDOM RFU")
plot.sc.exo.fdom.rfu

#fdom raw
plot.sc.exo.fdom.raw = ggplot(sc.exo, aes(x=fDOM_raw, y=Depth, color=factor(Date))) + 
  geom_point() + 
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=11) +
  labs(color="Date", x="fDOM raw", y="Depth (m)") +
  #scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Seal Cove - fDOM raw")
plot.sc.exo.fdom.raw

#fdom raw
plot.sc.exo.fdom.corr = ggplot(sc.exo, aes(x=fDOM_corr, y=Depth, color=factor(Date))) + 
  geom_point() + 
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=11) +
  labs(color="Date", x="fDOM corr", y="Depth (m)") +
  #scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Seal Cove - fDOM corrected")
plot.sc.exo.fdom.corr







#*Bubble Pond ####
plot.bb.exo.dosat = ggplot(bb.exo, aes(x=DO_sat, y=Depth, color=factor(Date))) + 
  geom_point() +  
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=11) +
  #xlim(80,105) +
  labs(color="Date", x="Dissolved Oxygen (% Saturation)", y="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Bubble Pond - Oxygen profiles")
plot.bb.exo.dosat 



#*Witch Hole ####
plot.wh.exo.dosat = ggplot(wh.exo, aes(x=DO_sat, y=Depth, color=factor(Date))) + 
  geom_point() +  
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=9) +
  #xlim(80,105) +
  labs(color="Date", x="Dissolved Oxygen (% saturation)", y="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Witch Hole - Oxygen profiles")
plot.wh.exo.dosat 

#FDOM corrected
plot.wh.exo.fdom.corr = ggplot(wh.exo, aes(x=fDOM_corr, y=Depth, color=factor(Date))) + 
  geom_point() +  
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=9) +
  #xlim(80,105) +
  labs(color="Date", x="fDOM_corr", y="Depth (m)") +
  theme_classic() + 
  ggtitle("Witch Hole - fDOM corr")
plot.wh.exo.fdom.corr

#FDOM raw
plot.wh.exo.fdom.raw = ggplot(wh.exo, aes(x=fDOM_raw, y=Depth, color=factor(Date))) + 
  geom_point() +  
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=9) +
  #xlim(80,105) +
  labs(color="Date", x="fDOM raw", y="Depth (m)") +
  theme_classic() + 
  ggtitle("Witch Hole - fDOM raw")
plot.wh.exo.fdom.raw






#C3 plots ####

#*Jordan Pond ####
#chlorophyll
plot.jp.c3.chl = ggplot(jp.c3, aes(x=Depth, y=Chlorophyll_a, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 2, se=F) +
  scale_x_reverse(limits=c(27,1),n.breaks=6) +
  ylim(0,165) +
  labs(color="Date", y="Chlorophyll RFU", x="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Jordan Pond -  Chlorophyll Relative Fluorescence ") + 
  coord_flip() #plotted reversed x/y, then flipped :)
plot.jp.c3.chl

#phyco
plot.jp.c3.phy = ggplot(jp.c3, aes(x=Depth, y=Phycocyanin, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 2) +
  scale_x_reverse(limits=c(27,1),n.breaks=6) +
  ylim(0,10) +
  labs(color="Date", y="Phycocyanin RFU", x="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Jordan Pond - Phycocyanin") + 
  coord_flip() 
plot.jp.c3.phy

#temp
plot.jp.c3.t = ggplot(jp.c3, aes(x=Depth, y=Temp, color=factor(Date))) + 
  geom_point(shape=1,size=2) +  
  scale_x_reverse(limits=c(27,1),n.breaks=6) +
  ylim(0,25) +
  labs(color="Date", y="Temperature deg C", x="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Jordan Pond - Temperature") + 
  coord_flip() 
plot.jp.c3.t

#Combined ####
plot.jp.profiles = grid.arrange(plot.jp.exo.dosat,plot.jp.c3.t,plot.jp.c3.chl,plot.jp.c3.phy)
plot.jp.profiles



#*Bubble Pond ####
#chlorophyll
plot.bb.c3.chl = ggplot(bb.c3, aes(x=Depth, y=Chlorophyll_a, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 2, se=F) +
  scale_x_reverse(limits=c(10,1),n.breaks=10) +
  ylim(0,400) +
  labs(color="Date", y="Chlorophyll RFU", x="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Bubble Pond - Chlorophyll Relative Fluorescence") + 
  coord_flip() 
plot.bb.c3.chl

#phyco
plot.bb.c3.phy = ggplot(bb.c3, aes(x=Depth, y=Phycocyanin, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 2) +
  scale_x_reverse(limits=c(10,1),n.breaks=10) +
  ylim(0,10) +
  labs(color="Date", y="Phycocyanin RFU", x="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Bubble Pond - Phycocyanin") + 
  coord_flip()
plot.bb.c3.phy

#temp
plot.bb.c3.t = ggplot(bb.c3, aes(x=Depth, y=Temp, color=factor(Date))) + 
  geom_point(shape=16,size=2) +  
  scale_x_reverse(limits=c(10,1),n.breaks=10) +
  ylim(10,25) +
  labs(color="Date", y="Temperature deg C", x="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Bubble Pond - Temperature") + 
  coord_flip() 
plot.bb.c3.t

#Combined ####
plot.bb.profiles = grid.arrange(plot.bb.exo.dosat,plot.bb.c3.t,plot.bb.c3.chl,plot.bb.c3.phy)
plot.bb.profiles



#*Witch Hole ####

wh.cols = c('#BF812D',   #3
            '#8C510A',   #4
            '#000000',   #7
            '#01665E',   #10
            '#35978F')


#chlorophyll
plot.wh.c3.chl = ggplot(wh.c3, aes(x=Depth, y=Chlorophyll_a, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 2, se=F) +
  scale_x_reverse(limits=c(9,1),n.breaks=10) +
  ylim(0,500) +
  labs(color="Date", y="Chlorophyll RFU", x="Depth (m)") +
  scale_color_manual(values = wh.cols) +
  theme_classic() + 
  ggtitle("Witch Hole Pond - Chlorophyll Relative Fluorescence") + 
  coord_flip() 
plot.wh.c3.chl

#cdom. Zeros
plot.wh.c3.cdom = ggplot(wh.c3, aes(x=Depth, y=CDOM, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 2) +
  scale_x_reverse(limits=c(9,1),n.breaks=10) +
  labs(color="Date", y="CDOM RFU", x="Depth (m)") +
  scale_color_brewer(palette = "Paired") +
  theme_classic() + 
  ggtitle("Witch Hole Pond - CDOM") + 
  coord_flip()
plot.wh.c3.cdom

#phyco
plot.wh.c3.phy = ggplot(wh.c3, aes(x=Depth, y=Phycocyanin, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 2) +
  scale_x_reverse(limits=c(9,1),n.breaks=10) +
  ylim(0,15) +
  labs(color="Date", y="Phycocyanin RFU", x="Depth (m)") +
  scale_color_brewer(palette = "Paired") +
  theme_classic() + 
  ggtitle("Witch Hole Pond - Phycocyanin") + 
  coord_flip() 
plot.wh.c3.phy

#temp
plot.wh.c3.t = ggplot(wh.c3, aes(x=Depth, y=Temp, color=factor(Date))) + 
  geom_point(shape=16,size=2) +  
  scale_x_reverse(limits=c(9,1),n.breaks=10) +
  ylim(0,25) +
  labs(color="Date", y="Temperature deg C", x="Depth (m)") +
  scale_color_brewer(palette = "Paired") +
  theme_classic() + 
  ggtitle("Witch Hole Pond - Temperature") + 
  coord_flip() 
plot.wh.c3.t

#Combined ####
plot.wh.profiles = grid.arrange(plot.wh.exo.dosat,plot.wh.c3.t,plot.wh.c3.chl,plot.wh.c3.phy,plot.wh.c3.cdom)
plot.wh.profiles











            

#*Seal Cove ####

sc.cols = c('#BF812D',   #3
            '#8C510A',   #4
            '#543005',   #5
            '#000000',   #7
            '#003C30',   #9
            '#01665E',   #10
            '#35978F')

#chlorophyll
plot.sc.c3.chl = ggplot(sc.c3, aes(x=Depth, y=Chlorophyll_a, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 2, se=F) +
  scale_x_reverse(limits=c(12,1),n.breaks=10) +
  ylim(0,500) +
  labs(color="Date", y="Chlorophyll RFU", x="Depth (m)") +
  scale_color_manual(values = sc.cols) +
  theme_classic() +
  ggtitle("Seal Cove Pond - Chlorophyll Relative Fluorescence") + 
  coord_flip() 
plot.sc.c3.chl

#cdom. Zeros
plot.sc.c3.cdom = ggplot(sc.c3, aes(x=Depth, y=CDOM, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 1.5) +
  scale_x_reverse(limits=c(12,1),n.breaks=10) +
  ylim(0,2000) +
  labs(color="Date", y="CDOM RFU", x="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Seal Cove Pond - CDOM") + 
  coord_flip()
plot.sc.c3.cdom

#phyco
plot.sc.c3.phy = ggplot(sc.c3, aes(x=Depth, y=Phycocyanin, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 2) +
  scale_x_reverse(limits=c(12,1),n.breaks=10) +
  ylim(0,10) +
  labs(color="Date", y="Phycocyanin RFU", x="Depth (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_classic() + 
  ggtitle("Seal Cove Pond - Phycocyanin") + 
  coord_flip()
plot.sc.c3.phy

#temp
plot.sc.c3.t = ggplot(sc.c3, aes(x=Depth, y=Temp, color=factor(Date))) + 
  geom_point(shape=16,size=2) +  
  scale_x_reverse(limits=c(12,0),n.breaks=10) +
  ylim(0,25) +
  labs(color="Date", y="Temperature deg C", x="Depth (m)") +
  scale_color_brewer(palette = "Paired") +
  theme_classic() + 
  ggtitle("Seal Cove Pond - Temperature") + 
  coord_flip() 
plot.sc.c3.t

#Combined ####
plot.sc.profiles = grid.arrange(plot.sc.exo.dosat,plot.sc.c3.t,plot.sc.c3.chl,plot.sc.c3.phy,plot.sc.c3.cdom)
plot.sc.profiles



#other combos
plot.sc.c3.exo.comb = grid.arrange(plot.sc.c3.chl,plot.sc.exo.chl,plot.sc.c3.phy,plot.sc.exo.chl2)
plot.sc.c3.exo.comb

plot.sc.c3.exo.comb2 = ggarrange(plot.sc.c3.t,plot.sc.exo.t,ncol=2,nrow=1)
plot.sc.c3.exo.comb2







