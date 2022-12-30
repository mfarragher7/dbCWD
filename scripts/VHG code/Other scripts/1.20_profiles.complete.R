#Temp, chl, and oxygen profiles from (mostly) every sample for each lake
#updated 2021-04-26



#load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)


#load data
exo = read.csv("library/db.exo.csv",header=T)
c3 = read.csv("library/db.c3.csv",header=T)


#sort seasons
s1=c('winter1','spring3','spring4')
s2=c('summer1','summer2','summer3','summer4')
s3=c('fall1','fall2','fall3','fall4')

#subset c3 dfs
c3s1=c3 %>% filter(seasonID %in% s1)
c3s2=c3 %>% filter(seasonID %in% s2)
c3s3=c3 %>% filter(seasonID %in% s3)

#subset exo dfs
exos1=exo %>% filter(seasonID %in% s1)
exos2=exo %>% filter(seasonID %in% s2)
exos3=exo %>% filter(seasonID %in% s3)

#color codes
cc2=c('#003C30','#35978F') #2colors
cc3=c('#003C30','#01665E','#35978F') #3colors
cc4=c('#003C30','#01665E','#35978F','#80CDC1') #4colors



#JORDAN
#temperature winter/spring
jptemp1=ggplot(filter(c3s1,LakeID=='JP'),aes(x=Depth,y=Temp,color=as.factor(Date))) + 
  geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) + 
  scale_color_manual(values=cc3) +
  labs(title=" ", color="Winter/Spring", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) + 
  coord_flip() + 
  theme_classic(); jptemp1 

#summer temp
jptemp2=ggplot(filter(c3s2,LakeID=='JP'),aes(x=Depth,y=Temp,color=as.factor(Date))) + 
  geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) +
  scale_color_manual(values=cc4) +
  labs(title=" ", color="Summer", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) +
  coord_flip() +
  theme_classic(); jptemp2

#fall temp
jptemp3=ggplot(filter(c3s3,LakeID=='JP'),aes(x=Depth,y=Temp,color=as.factor(Date))) +
  geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) +
  scale_color_manual(values=cc4) +
  labs(title=" ", color="Fall", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) +
  coord_flip() + 
  theme_classic(); jptemp3

#chlorophyll winter/spring 
jpchl1=ggplot(filter(c3s1,LakeID=='JP'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) +
  geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) +
  scale_color_manual(values=cc3) + 
  ylim(0,150) + labs(title=" ", color="Winter/Spring", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  coord_flip() + 
  theme_classic(); jpchl1 

#chl summer
jpchl2=ggplot(filter(c3s2,LakeID=='JP'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) +
  geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) +
  scale_x_reverse(limits=c(27,0),n.breaks=6) + 
  scale_color_manual(values=cc4) + 
  ylim(0,150) + labs(title=" ", color="Summer", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  coord_flip() +
  theme_classic(); jpchl2

#chl fall
jpchl3=ggplot(filter(c3s3,LakeID=='JP'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) +
  geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) +
  scale_color_manual(values=cc4) + 
  ylim(0,150) + labs(title=" ", color="Fall", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) + 
  coord_flip() +
  theme_classic(); jpchl3

#oxygen winter/spring
jpdo1=ggplot(filter(exos1,LakeID=='JP'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) +
  geom_point(size=1,shape=1) + 
  geom_path(linetype=2) +
  scale_x_reverse(limits=c(27,0),n.breaks=6) +
  scale_color_manual(values=cc3) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Winter/Spring", x=" ", y='Dissolved Oxygen Saturation %') + 
  coord_flip() + 
  theme_classic(); jpdo1

#oxygen summer
jpdo2=ggplot(filter(exos2,LakeID=='JP'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) +
  geom_point(size=1,shape=1) + 
  geom_path(linetype=2) +
  scale_x_reverse(limits=c(27,0),n.breaks=6) +
  scale_color_manual(values=cc4) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Summer", x=" ", y='Dissolved Oxygen Saturation %') +
  coord_flip() + 
  theme_classic(); jpdo2

#oxygen fall
jpdo3=ggplot(filter(exos3,LakeID=='JP'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) + 
  geom_point(size=1,shape=1) + 
  geom_path(linetype=2) +
  scale_x_reverse(limits=c(27,0),n.breaks=6) + 
  scale_color_manual(values=cc4) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Fall", x=" ", y='Dissolved Oxygen Saturation %') + 
  coord_flip() + 
  theme_classic(); jpdo3

#combine
jps1=ggarrange(jptemp1,jpchl1,jpdo1,nrow=1,ncol=3,common.legend=T,legend='right'); jps1
jps2=ggarrange(jptemp2,jpchl2,jpdo2,nrow=1,ncol=3,common.legend=T,legend='right'); jps2
jps3=ggarrange(jptemp3,jpchl3,jpdo3,nrow=1,ncol=3,common.legend=T,legend='right'); jps3
jp = ggarrange(jps1,jps2,jps3,nrow=3,ncol=1)
ggsave("jp.profiles.png", plot = jp)


#SEAL COVE
#temperature winter/spring
sctemp1=ggplot(filter(c3s1,LakeID=='SC'),aes(x=Depth,y=Temp,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(13,0),n.breaks=7) + scale_color_manual(values=cc3) +
  labs(title=" ", color="Winter/Spring", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) + coord_flip() + theme_classic(); sctemp1 
#summer temp
sctemp2=ggplot(filter(c3s2,LakeID=='SC'),aes(x=Depth,y=Temp,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(13,0),n.breaks=7) + scale_color_manual(values=cc4) +
  labs(title=" ", color="Summer", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) + coord_flip() + theme_classic(); sctemp2
#fall temp
sctemp3=ggplot(filter(c3s3,LakeID=='SC'),aes(x=Depth,y=Temp,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(13,0),n.breaks=7) + scale_color_manual(values=cc4) +
  labs(title=" ", color="Fall", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) + coord_flip() + theme_classic(); sctemp3
#chlorophyll winter/spring 
scchl1=ggplot(filter(c3s1,LakeID=='SC'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) + scale_x_reverse(limits=c(13,0),n.breaks=7) + scale_color_manual(values=cc3) + 
  ylim(0,400) + labs(title=" ", color="Winter/Spring", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) + coord_flip() + theme_classic(); scchl1 
#chl summer
scchl2=ggplot(filter(c3s2,LakeID=='SC'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) + scale_x_reverse(limits=c(13,0),n.breaks=7) + scale_color_manual(values=cc4) + 
  ylim(0,400) + labs(title=" ", color="Summer", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) + coord_flip() + theme_classic(); scchl2
#chl fall
scchl3=ggplot(filter(c3s3,LakeID=='SC'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) + scale_x_reverse(limits=c(13,0),n.breaks=7) + scale_color_manual(values=cc4) + 
  ylim(0,400) + labs(title=" ", color="Fall", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) + coord_flip() + theme_classic(); scchl3
#oxygen winter/spring
scdo1=ggplot(filter(exos1,LakeID=='SC'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  geom_path(linetype=2) + scale_x_reverse(limits=c(13,0),n.breaks=7) + scale_color_manual(values=cc3) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Winter/Spring", x=" ", y='Dissolved Oxygen Saturation %') + coord_flip() + theme_classic(); scdo1
#oxygen summer
scdo2=ggplot(filter(exos2,LakeID=='SC'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  geom_path(linetype=2) + scale_x_reverse(limits=c(13,0),n.breaks=7) + scale_color_manual(values=cc4) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Summer", x=" ", y='Dissolved Oxygen Saturation %') + coord_flip() + theme_classic(); scdo2
#oxygen fall
scdo3=ggplot(filter(exos3,LakeID=='SC'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  geom_path(linetype=2) + scale_x_reverse(limits=c(13,0),n.breaks=7) + scale_color_manual(values=cc4) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Fall", x=" ", y='Dissolved Oxygen Saturation %') + coord_flip() + theme_classic(); scdo3
#combine
scs1=ggarrange(sctemp1,scchl1,scdo1,nrow=1,ncol=3,common.legend=T,legend='right'); scs1
scs2=ggarrange(sctemp2,scchl2,scdo2,nrow=1,ncol=3,common.legend=T,legend='right'); scs2
scs3=ggarrange(sctemp3,scchl3,scdo3,nrow=1,ncol=3,common.legend=T,legend='right'); scs3
sc = ggarrange(scs1,scs2,scs3,nrow=3,ncol=1)
ggsave("sc.profiles.png", plot = sc)


#BUBBLE
#temperature winter/spring
bbtemp1=ggplot(filter(c3s1,LakeID=='BB'),aes(x=Depth,y=Temp,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(11,0), n.breaks=7) + scale_color_manual(values=cc2) +
  labs(title=" ", color="Winter/Spring", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) + coord_flip() + theme_classic(); bbtemp1 
#summer temp
bbtemp2=ggplot(filter(c3s2,LakeID=='BB'),aes(x=Depth,y=Temp,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(11,0), n.breaks=7) +  scale_color_manual(values=cc4) +
  labs(title=" ", color="Summer", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) + coord_flip() + theme_classic(); bbtemp2
#fall temp
bbtemp3=ggplot(filter(c3s3,LakeID=='BB'),aes(x=Depth,y=Temp,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(11,0), n.breaks=7) + scale_color_manual(values=cc4) +
  labs(title=" ", color="Fall", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) + coord_flip() + theme_classic(); bbtemp3
#chlorophyll winter/spring 
bbchl1=ggplot(filter(c3s1,LakeID=='BB'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) + scale_x_reverse(limits=c(11,0), n.breaks=7) + scale_color_manual(values=cc3) + 
  ylim(0,400) + labs(title=" ", color="Winter/Spring", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) + coord_flip() + theme_classic(); bbchl1 
#chl summer
bbchl2=ggplot(filter(c3s2,LakeID=='BB'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) + scale_x_reverse(limits=c(11,0), n.breaks=7) + scale_color_manual(values=cc4) + 
  ylim(0,400) + labs(title=" ", color="Summer", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) + coord_flip() + theme_classic(); bbchl2
#chl fall
bbchl3=ggplot(filter(c3s3,LakeID=='BB'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) + scale_x_reverse(limits=c(11,0), n.breaks=7) +  scale_color_manual(values=cc4) + 
  ylim(0,400) + labs(title=" ", color="Fall", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) + coord_flip() + theme_classic(); bbchl3
#oxygen winter/spring
bbdo1=ggplot(filter(exos1,LakeID=='BB'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  geom_path(linetype=2) + scale_x_reverse(limits=c(11,0), n.breaks=7) + scale_color_manual(values=cc3) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Winter/Spring", x=" ", y='Dissolved Oxygen Saturation %') + coord_flip() + theme_classic(); bbdo1
#oxygen summer
bbdo2=ggplot(filter(exos2,LakeID=='BB'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  geom_path(linetype=2) + scale_x_reverse(limits=c(11,0), n.breaks=7) + scale_color_manual(values=cc4) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Summer", x=" ", y='Dissolved Oxygen Saturation %') + coord_flip() + theme_classic(); bbdo2
#oxygen fall
bbdo3=ggplot(filter(exos3,LakeID=='BB'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  geom_path(linetype=2) + scale_x_reverse(limits=c(11,0), n.breaks=7) + scale_color_manual(values=cc4) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Fall", x=" ", y='Dissolved Oxygen Saturation %') + coord_flip() + theme_classic(); bbdo3
#combine
bbs1=ggarrange(bbtemp1,bbchl1,bbdo1,nrow=1,ncol=3,common.legend=T,legend='right'); bbs1
bbs2=ggarrange(bbtemp2,bbchl2,bbdo2,nrow=1,ncol=3,common.legend=T,legend='right'); bbs2
bbs3=ggarrange(bbtemp3,bbchl3,bbdo3,nrow=1,ncol=3,common.legend=T,legend='right'); bbs3
bb = ggarrange(bbs1,bbs2,bbs3,nrow=3,ncol=1)
ggsave("bb.profiles.png", plot = bb)

#WITCH HOLE
#temperature winter/spring
whtemp1=ggplot(filter(c3s1,LakeID=='WH'),aes(x=Depth,y=Temp,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(9,0), n.breaks=6) + scale_color_manual(values=cc3) +
  labs(title=" ", color="Winter/Spring", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) + coord_flip() + theme_classic(); whtemp1 
#summer temp
whtemp2=ggplot(filter(c3s2,LakeID=='WH'),aes(x=Depth,y=Temp,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(9,0), n.breaks=6) +  scale_color_manual(values=cc4) +
  labs(title=" ", color="Summer", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) + coord_flip() + theme_classic(); whtemp2
#fall temp
whtemp3=ggplot(filter(c3s3,LakeID=='WH'),aes(x=Depth,y=Temp,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  scale_x_reverse(limits=c(9,0), n.breaks=6) + scale_color_manual(values=cc4) +
  labs(title=" ", color="Fall", x="Depth (m)", y=expression("Temperature " ( degree*C))) +
  guides(color=guide_legend(override.aes=list(shape=19))) + coord_flip() + theme_classic(); whtemp3
#chlorophyll winter/spring 
whchl1=ggplot(filter(c3s1,LakeID=='WH'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) + scale_x_reverse(limits=c(9,0), n.breaks=6) + scale_color_manual(values=cc3) + 
  ylim(0,1000) + labs(title=" ", color="Winter/Spring", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) + coord_flip() + theme_classic(); whchl1 
#chl summer
whchl2=ggplot(filter(c3s2,LakeID=='WH'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) + scale_x_reverse(limits=c(9,0), n.breaks=6) +  scale_color_manual(values=cc4) + 
  ylim(0,1000) + labs(title=" ", color="Summer", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) + coord_flip() + theme_classic(); whchl2
#chl fall
whchl3=ggplot(filter(c3s3,LakeID=='WH'),aes(x=Depth,y=Chlorophyll_a,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  stat_smooth(method="loess",size=1,se=F) + scale_x_reverse(limits=c(9,0), n.breaks=6) + scale_color_manual(values=cc4) + 
  ylim(0,1000) + labs(title=" ", color="Fall", x=" ", y='Chlorophyll RFU') + 
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) + coord_flip() + theme_classic(); whchl3
#oxygen winter/spring
whdo1=ggplot(filter(exos1,LakeID=='WH'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  geom_path(linetype=2) + scale_x_reverse(limits=c(9,0), n.breaks=6) + scale_color_manual(values=cc3) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Winter/Spring", x=" ", y='Dissolved Oxygen Saturation %') + coord_flip() + theme_classic(); whdo1
#oxygen summer
whdo2=ggplot(filter(exos2,LakeID=='WH'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  geom_path(linetype=2) + scale_x_reverse(limits=c(9,0), n.breaks=6) + scale_color_manual(values=cc4) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Summer", x=" ", y='Dissolved Oxygen Saturation %') + coord_flip() + theme_classic(); whdo2
#oxygen fall
whdo3=ggplot(filter(exos3,LakeID=='WH'),aes(x=Depth,y=DO_sat,color=as.factor(Date))) + geom_point(size=1,shape=1) + 
  geom_path(linetype=2) + scale_x_reverse(limits=c(9,0), n.breaks=6) + scale_color_manual(values=cc4) +
  guides(color=guide_legend(override.aes=list(linetype=NA,shape=19))) +
  labs(title=" ", color="Fall", x=" ", y='Dissolved Oxygen Saturation %') + coord_flip() + theme_classic(); whdo3
#combine
whs1=ggarrange(whtemp1,whchl1,whdo1,nrow=1,ncol=3,common.legend=T,legend='right'); whs1
whs2=ggarrange(whtemp2,whchl2,whdo2,nrow=1,ncol=3,common.legend=T,legend='right'); whs2
whs3=ggarrange(whtemp3,whchl3,whdo3,nrow=1,ncol=3,common.legend=T,legend='right'); whs3
wh = ggarrange(whs1,whs2,whs3,nrow=3,ncol=1)
ggsave("wh.profiles.png", plot = wh)





#Summary figs #####


pro = read.csv('library/profiles.all.csv',header=T)

#libraries
library(dplyr)
library(ggplot2)
library(ggpubr)

#Thermocline####
#Compare thermocline depths from EXO and C3
#JP
a=ggplot(filter(pro,LakeID=='JP'),
         aes(x=Date,y=thermocline_depth_exo)) + 
  geom_point(size=2,shape=19) +
  scale_y_reverse() +
  labs(title="JP thermocline depth", y='Depth') +
  geom_point(data=filter(pro,LakeID=='JP'),
             aes(x=Date, y=thermocline_depth_c3),size=3,shape=1);a

#SC
b=ggplot(filter(pro,LakeID=='SC'),aes(x=Date,y=thermocline_depth_exo)) + 
  geom_point(size=2,shape=19) +
  scale_y_reverse() +
  labs(title="SC thermocline depth", y='Depth') + 
  geom_point(data=filter(pro,LakeID=='SC'),aes(x=Date, y=thermocline_depth_c3),size=3,shape=1);b

#BB
c=ggplot(filter(pro,LakeID=='BB'),aes(x=Date,y=thermocline_depth_exo)) +
  geom_point(size=2,shape=19) + 
  scale_y_reverse() +
  labs(title="BB thermocline depth", y='Depth') +
  geom_point(data=filter(pro,LakeID=='BB'),aes(x=Date, y=thermocline_depth_c3),size=3,shape=1);c

#WH
d=ggplot(filter(pro,LakeID=='WH'),aes(x=Date,y=thermocline_depth_exo)) + 
  geom_point(size=2,shape=19) + 
  scale_y_reverse() +
  labs(title="WH thermocline depth", y='Depth') +
  geom_point(data=filter(pro,LakeID=='WH'),aes(x=Date, y=thermocline_depth_c3),size=3,shape=1);d

#combine
z=ggarrange(a,b,c,d,ncol=2,nrow=2);z






#Depth Max DO ####
#JP
a=ggplot(filter(pro,LakeID=='JP'),aes(x=Date,y=do_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() + 
  labs(title="JP max DO depth", y='Depth') + 
  theme_classic();a

#SC
b=ggplot(filter(pro,LakeID=='SC'),aes(x=Date,y=do_max_depth)) +
  geom_point(size=2,shape=19) +
  geom_line(linetype=3) + 
  scale_y_reverse() + 
  labs(title="SC max DO depth", y='Depth') +
  theme_classic();b

#BB
c=ggplot(filter(pro,LakeID=='BB'),aes(x=Date,y=do_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() +
  labs(title="BB max DO depth", y='Depth') + 
  theme_classic();c

#WH
d=ggplot(filter(pro,LakeID=='WH'),aes(x=Date,y=do_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() +
  labs(title="WH max DO depth", y='Depth') + 
  theme_classic();d

#combine
z=ggarrange(a,b,c,d,ncol=2,nrow=2);z



#Mean DO ####
a=ggplot(filter(pro,LakeID=='JP'),aes(x=Date,y=do_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  scale_y_reverse() + 
  labs(title="JP", y='DO saturation') + 
  theme_classic();a

#SC
b=ggplot(filter(pro,LakeID=='SC'),aes(x=Date,y=do_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() + 
  labs(title="SC", y='DO saturation') + 
  theme_classic();b

#BB
c=ggplot(filter(pro,LakeID=='BB'),aes(x=Date,y=do_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  scale_y_reverse() + 
  labs(title="BB", y='DO saturation') + 
  theme_classic();c
#WH
d=ggplot(filter(pro,LakeID=='WH'),aes(x=Date,y=do_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  scale_y_reverse() +
  labs(title="WH", y='DO saturation') + 
  theme_classic();d

#combine
z=ggarrange(a,b,c,d,ncol=2,nrow=2);z



#DCM ####
#JP
a=ggplot(filter(pro,LakeID=='JP'),aes(x=Date,y=chl_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() + 
  labs(title="JP", y='Depth') +
  theme_classic();a

#SC
b=ggplot(filter(pro,LakeID=='SC'),aes(x=Date,y=chl_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  scale_y_reverse() + 
  labs(title="SC", y='Depth') +
  theme_classic();b

#BB
c=ggplot(filter(pro,LakeID=='BB'),aes(x=Date,y=chl_max_depth)) + 
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() + 
  labs(title="BB", y='Depth') +
  theme_classic();c

#WH
d=ggplot(filter(pro,LakeID=='WH'),aes(x=Date,y=chl_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  scale_y_reverse() +
  labs(title="WH", y='Depth') +
  theme_classic();d

#combine
z=ggarrange(a,b,c,d,ncol=2,nrow=2);z



#mean chl
#JP
a=ggplot(filter(pro,LakeID=='JP'),aes(x=Date,y=chl_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  labs(title="JP", y='Chl a RFU') +
  theme_classic();a

#SC
b=ggplot(filter(pro,LakeID=='SC'),aes(x=Date,y=chl_mean)) + 
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  labs(title="SC", y='Chl a RFU') + 
  theme_classic();b

#BB
c=ggplot(filter(pro,LakeID=='BB'),aes(x=Date,y=chl_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  labs(title="BB", y='Chl a RFU') + 
  theme_classic();c

#WH
d=ggplot(filter(pro,LakeID=='WH'),aes(x=Date,y=chl_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  labs(title="WH", y='Chl a RFU') +
  theme_classic();d

#combine
z=ggarrange(a,b,c,d,ncol=2,nrow=2);z














