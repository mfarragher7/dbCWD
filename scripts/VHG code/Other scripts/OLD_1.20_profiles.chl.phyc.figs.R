
#compare 4L chl profiles nicely
#created 2020-08-28 04:00

#load.everything.R first

#libs
library(ggplot2)
library(ggpubr)
library(dplyr)


#Jordan
#colors
brownblues = c('#DFC27D','#BF812D','#8C510A','#000000','#003C30','#01665E','#35978F','#80CDC1')
#plot
plot.jp.c3.chl = ggplot(filter(full.c3,LakeID=='JP'), aes(x=Depth, y=Chlorophyll_a, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 1, se=F, show.legend = FALSE) +
  scale_x_reverse(limits=c(27,1),n.breaks=6) +
  ylim(0,165) +
  labs(color="Date", title="Jordan Pond", y=expression("Chlorophyll"~italic(a)~"(RFU)"), x="Depth (m)") +
  scale_color_manual(values=brownblues) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() + 
  coord_flip() 
plot.jp.c3.chl

#Seal Cove
#colors
brownblues = c('#DFC27D','#BF812D','#8C510A','#543005','#000000','#003C30','#01665E','#35978F','#80CDC1')
#plot
plot.sc.c3.chl = ggplot(filter(full.c3,LakeID=='SC'), aes(x=Depth, y=Chlorophyll_a, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 1, se=F, show.legend = FALSE) +
  scale_x_reverse(limits=c(13,1),n.breaks=7) +
  ylim(0,500) +
  labs(color="Date", title="Seal Cove Pond", y=expression("Chlorophyll"~italic(a)~"(RFU)"), x="Depth (m)") +
  scale_color_manual(values=brownblues) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() + 
  coord_flip() 
plot.sc.c3.chl

#Witch Hole?
#colors
brownblues = c('#DFC27D','#BF812D','#8C510A','#000000','#01665E','#35978F','#80CDC1')
#plot
plot.wh.c3.chl = ggplot(filter(full.c3,LakeID=='WH'), aes(x=Depth, y=Chlorophyll_a, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 1, se=F, show.legend = FALSE) +
  scale_x_reverse(limits=c(9,1),n.breaks=7) +
  ylim(0,500) +
  labs(color="Date", title="Witch Hole Pond", y=expression("Chlorophyll"~italic(a)~"(RFU)"), x="Depth (m)") +
  scale_color_manual(values=brownblues) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() + 
  coord_flip() 
plot.wh.c3.chl

#Bub
#colors
brownblues = c('#BF812D','#8C510A','#000000','#01665E','#35978F','#80CDC1')
#plot
plot.bb.c3.chl = ggplot(filter(full.c3,LakeID=='BB'), aes(x=Depth, y=Chlorophyll_a, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 1, se=F, show.legend = FALSE) +
  scale_x_reverse(limits=c(11,1),n.breaks=7) +
  ylim(0,500) +
  labs(color="Date", title="Bubble Pond", y=expression("Chlorophyll"~italic(a)~"(RFU)"), x="Depth (m)") +
  scale_color_manual(values=brownblues) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() + 
  coord_flip() 
plot.bb.c3.chl

#combine
plot.comb.c3.chl = ggarrange(plot.jp.c3.chl, 
                             plot.sc.c3.chl, 
                             plot.bb.c3.chl,
                             plot.wh.c3.chl, 
                             nrow=2, ncol=2)
plot.comb.c3.chl



# Phyco ####
#Jordan
#colors
brownblues = c('#DFC27D','#BF812D','#8C510A','#000000','#003C30','#01665E','#35978F','#80CDC1')
#plot
plot.jp.c3.phy = ggplot(filter(full.c3,LakeID=='JP'), aes(x=Depth, y=Phycocyanin, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 1, se=F, show.legend = FALSE) +
  scale_x_reverse(limits=c(27,1),n.breaks=6) +
  ylim(0,20) +
  labs(color="Date", title="Jordan Pond", y="Phycocyanin (RFU)", x="Depth (m)") +
  scale_color_manual(values=brownblues) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() + 
  coord_flip() 
plot.jp.c3.phy

#Seal Cove
#colors
brownblues = c('#DFC27D','#BF812D','#8C510A','#543005','#000000','#003C30','#01665E','#35978F','#80CDC1')
#plot
plot.sc.c3.phy = ggplot(filter(full.c3,LakeID=='SC'), aes(x=Depth, y=Phycocyanin, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 1, se=F, show.legend = FALSE) +
  scale_x_reverse(limits=c(13,1),n.breaks=7) +
  ylim(0,20) +
  labs(color="Date", title="Seal Cove Pond", y="Phycocyanin (RFU)", x="Depth (m)") +
  scale_color_manual(values=brownblues) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
theme_classic() + 
  coord_flip() 
plot.sc.c3.phy

#Witch Hole?
#colors
brownblues = c('#DFC27D','#BF812D','#8C510A','#000000','#01665E','#35978F','#80CDC1')
#plot
plot.wh.c3.phy = ggplot(filter(full.c3,LakeID=='WH'), aes(x=Depth, y=Phycocyanin, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 1, se=F, show.legend = FALSE) +
  scale_x_reverse(limits=c(9,1),n.breaks=7) +
  ylim(0,20) +
  labs(color="Date", title="Witch Hole Pond", y="Phycocyanin (RFU)", x="Depth (m)") +
  scale_color_manual(values=brownblues) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() + 
  coord_flip() 
plot.wh.c3.phy

#Bub
#colors
brownblues = c('#BF812D','#8C510A','#000000','#01665E','#35978F','#80CDC1')
#plot
plot.bb.c3.phy = ggplot(filter(full.c3,LakeID=='BB'), aes(x=Depth, y=Phycocyanin, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 1, se=F, show.legend = FALSE) +
  scale_x_reverse(limits=c(11,1),n.breaks=7) +
  ylim(0,20) +
  labs(color="Date", title="Bubble Pond",y="Phycocyanin (RFU)", x="Depth (m)") +
  scale_color_manual(values=brownblues) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() + 
  coord_flip() 
plot.bb.c3.phy

#combine
plot.comb.c3.phy = ggarrange(plot.jp.c3.phy, 
                             plot.sc.c3.phy, 
                             plot.bb.c3.phy,
                             plot.wh.c3.phy, 
                             nrow=2, ncol=2)
plot.comb.c3.phy






