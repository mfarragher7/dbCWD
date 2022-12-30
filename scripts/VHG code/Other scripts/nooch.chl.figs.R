#plot extracted chlorophyll 
#***run load.everything.R first
#updated 2020-08-28

#libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)

#plot chl for each lake depth, epi meta & hypo
color.code = c('#80CDC1','#35978F','#003C30')

#remove NA chla_ugpl rows 
extracted.chl = full.samples %>% filter(!is.na(chla_ugpl))
#format date
#extracted.chl$Date_short = format(as.Date(extracted.chl$Date), "%b-%d")



#tried to use as.Date.factor(), and then scale_x_date(). not bad i think. specific sample date doesn't matter


#Jordan
plot.jp.chla = ggplot(filter(extracted.chl,LakeID=='JP'), aes(x=as.Date.factor(Date), y=chla_ugpl, color=Site)) + 
  geom_point(position=position_dodge(width=0.2) , size=5) +
  labs(title="Jordan Pond", color="Sample Depth", x="Date", y=expression("Chlorophyll"~italic(a)~"(ug/L)")) +
  scale_color_manual(values=color.code, limits=c("Epi", "Meta","Hypo")) +
  #scale_x_discrete(guide=guide_axis(n.dodge=2)) +
  scale_x_date(date_labels = "%b-%d") +
  theme_classic()
plot.jp.chla

#Seal Cove
plot.sc.chla = ggplot(filter(extracted.chl,LakeID=='SC'), aes(x=factor(Date), y=chla_ugpl, color=Site)) + 
  geom_point(position=position_dodge(width=0.2) , size=5) +
  labs(title="Seal Cove Pond", color="Sample Depth", x="Date", y=expression("Chlorophyll"~italic(a)~"(ug/L)")) +
  scale_color_manual(values=color.code, limits=c("Epi", "Meta","Hypo")) +
  scale_x_discrete(guide=guide_axis(n.dodge=2)) +
  theme_classic()
plot.sc.chla

#Witch Hole
plot.wh.chla = ggplot(filter(extracted.chl,LakeID=='WH'), aes(x=factor(Date), y=chla_ugpl, color=Site)) + 
  geom_point(position=position_dodge(width=0.2) , size=5) +
  labs(title="Witch Hole Pond", color="Sample Depth", x="Date", y=expression("Chlorophyll"~italic(a)~"(ug/L)")) +
  scale_color_manual(values=color.code, limits=c("Epi", "Meta","Hypo")) +
  scale_y_continuous(limits=c(1,6), n.breaks=4) +
  scale_x_discrete(guide=guide_axis(n.dodge=2)) +
  theme_classic()
plot.wh.chla

#Bubble... maybe later! need more data
plot.bb.chla = ggplot(filter(extracted.chl,LakeID=='BB'), aes(x=factor(Date), y=chla_ugpl, color=Site)) + 
  geom_point(position=position_dodge(width=0.2) , size=5) +
  labs(title="Bubble Pond", color="Sample Depth", x="Date", y=expression("Chlorophyll"~italic(a)~"(ug/L)")) +
  scale_color_manual(values=color.code, limits=c("Epi", "Meta","Hypo")) +
  theme_classic()
plot.bb.chla

#combine
plot.comb.chla = ggarrange(plot.jp.chla, plot.sc.chla, plot.bb.chla, plot.wh.chla, 
                           nrow=2, ncol=2, common.legend=TRUE, legend='bottom')
plot.comb.chla







#OLD Plots ####
#*JP 
#by depth
# IMPORTANT - leave date as character, so ggplot doesn't space out x axis unevenly
#chl a
plot.jp.chla = ggplot(filter(extracted.chl,LakeID=='JP'), aes(x=Date, y=Depth, color=chla_ugpl)) + 
  geom_point(size=5) +
  scale_y_reverse(n.breaks=6) +
  labs(color="Chl a (ug/L)", x="Date", y="Depth (m)") +
  scale_color_gradient2(limits=c(0,2),midpoint=1,high="green",mid="gold",low="red") +
  theme_classic() + 
  ggtitle("Jordan Pond - Chlorophyll a (ug/L)")
plot.jp.chla

#*SC 
#chl a
plot.sc.chla = ggplot(filter(extracted.chl,LakeID=='SC'),  aes(x=Date, y=Depth, color=chla_ugpl)) + 
  geom_point(size=5) +
  scale_y_reverse(n.breaks=10) +
  labs(color="Chl a (ug/L)", x="Date", y="Depth (m)") +
  scale_color_gradient2(limits=c(0,7),midpoint=3.5,high="green",mid="gold",low="red") +
  theme_classic() + 
  ggtitle("Seal Cove Pond - Chlorophyll a (ug/L)")
plot.sc.chla

#*BB
#chl a
plot.bb.chla = ggplot(filter(extracted.chl,LakeID=='BB'), aes(x=Date, y=Depth, color=chla_ugpl)) + 
  geom_point(size=5) +
  scale_y_reverse(n.breaks=10) +
  labs(color="Chl a (ug/L)", x="Date", y="Depth (m)") +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,high="green",mid="gold",low="red") +
  theme_classic() + 
  ggtitle("Bubble Pond - Chlorophyll a (ug/L)")
plot.bb.chla

#*WH 
#chl a
plot.wh.chla = ggplot(filter(extracted.chl,LakeID=='WH'), aes(x=Date, y=Depth, color=chla_ugpl)) + 
  geom_point(size=5) +
  scale_y_reverse(n.breaks=10) +
  labs(color="Chl a (ug/L)", x="Date", y="Depth (m)") +
  scale_color_gradient2(limits=c(0,6),midpoint=3,high="green",mid="gold",low="red") +
  theme_classic() + 
  ggtitle("Witch Hole Pond - Chlorophyll a (ug/L)")
plot.wh.chla





