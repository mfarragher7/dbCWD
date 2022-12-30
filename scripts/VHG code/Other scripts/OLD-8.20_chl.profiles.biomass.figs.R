#combining c3, and extracted chlorophyll
#16-ish panels. arranged lakes vertically, seasons horizontally 
#updated 2020-12-04

#load survey data 
full.nooch = read.csv("library/full.nooch.csv",header=T)
full.secchi = read.csv("library/full.secchi.csv",header=T)
#load nps secchi



#load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)

#make special combined secchi dataframe
winter.secchi = full.secchi %>% 
  filter(Date <= "2020-03-01") %>% #get my winter secchi for fun
  select(-Notes) #drop notes col
nps.secchi = lt.secchi %>% #get nps data for the other dates
  filter(Date >= "2020-01-01") %>% #get 2020 data
  mutate(seasonID = ifelse(Date >= "2020-10-01", "fall4", NA)) %>% #ID october dates as fall 4
  mutate(seasonID = replace(seasonID, Date > "2020-07-01" & Date < "2020-07-31", "summer3")) %>% #closest dates to my summer3 dates
  mutate(seasonID = replace(seasonID, Date > "2020-06-01" & Date < "2020-06-30", "spring3")) #closest dates to my spring3 dates
#combine
secchi = rbind(winter.secchi,nps.secchi)

#select seasons. 
szns=c('winter1','spring3','summer3','fall4')
c3 = full.c3 %>% filter(seasonID %in% szns)
chl = full.nooch %>% filter(seasonID %in% szns)
secchi = secchi %>% filter(seasonID %in% szns)


#JORDAN ####
#winter
jp.winter=ggplot(filter(c3,lakedate=='JP_2020-02-21'),
                 aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) +
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='JP_2020-02-21'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) +  #add extracted chl. use mapping=aes()' to avoid error
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(data=filter(secchi,LakeID=="JP"&seasonID=="winter1"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab("Depth (m)") +
  ylab(" ") +
  coord_flip() + 
  theme_classic(); jp.winter 

#spring3
jp.spring3=ggplot(filter(c3,lakedate=='JP_2020-05-28'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='JP_2020-05-28'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) + 
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="JP"&seasonID=="spring3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(" ") +
  ylab(" ") +
  coord_flip() + 
  theme_classic(); jp.spring3

#summer3
jp.summer3=ggplot(filter(c3,lakedate=='JP_2020-08-04'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='JP_2020-08-04'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) + 
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="JP"&seasonID=="summer3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(" ") +
  ylab(" ") +
  coord_flip() + 
  theme_classic(); jp.summer3 

#fall4
jp.fall4=ggplot(filter(c3,lakedate=='JP_2020-10-31'),
                aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='JP_2020-10-31'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) + 
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="JP"&seasonID=="fall4"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(" ") +
  ylab(" ") +
  coord_flip() + 
  theme_classic(); jp.fall4


#BUBBLE ####
#winter
#coming soon?
#spring3
bb.spring3=ggplot(filter(c3,lakedate=='BB_2020-05-26'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(11,0),n.breaks=7) +
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='BB_2020-05-26'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="BB"&seasonID=="spring3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab("Depth (m)") +
  ylab(" ") +
  coord_flip() +
  theme_classic(); bb.spring3

#summer3
bb.summer3=ggplot(filter(c3,lakedate=='BB_2020-08-04'),
                  aes(x=Depth,y=Chlorophyll_a)) +
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(11,0),n.breaks=7) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='BB_2020-08-04'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="BB"&seasonID=="summer3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(" ") +
  ylab(" ") +
  coord_flip() + 
  theme_classic(); bb.summer3 

#fall4
bb.fall4=ggplot(filter(c3,lakedate=='BB_2020-10-31'),
                aes(x=Depth,y=Chlorophyll_a)) +
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(11,0),n.breaks=7) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='BB_2020-10-31'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="BB"&seasonID=="fall4"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(" ") +
  ylab(" ") +
  coord_flip() + 
  theme_classic(); bb.fall4


#SEAL COVE ####
#winter
sc.winter=ggplot(filter(c3,lakedate=='SC_2020-02-21'),
                 aes(x=Depth,y=Chlorophyll_a)) +
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(12,0),n.breaks=7) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='SC_2020-02-21'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="SC"&seasonID=="winter1"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab("Depth (m)") +
  ylab(" ") +
  coord_flip() +
  theme_classic(); sc.winter 

#spring3
sc.spring3=ggplot(filter(c3,lakedate=='SC_2020-05-28'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(12,0),n.breaks=7) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='SC_2020-05-28'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="SC"&seasonID=="spring3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(" ") +
  ylab(" ") +
  coord_flip() + 
  theme_classic(); sc.spring3

#summer3
sc.summer3=ggplot(filter(c3,lakedate=='SC_2020-08-03'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(12,0),n.breaks=7) +
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='SC_2020-08-03'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) + 
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="SC"&seasonID=="summer3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(" ") +
  ylab(" ") +
  coord_flip() + 
  theme_classic(); sc.summer3 

#fall4
sc.fall4=ggplot(filter(c3,lakedate=='SC_2020-11-06'),
                aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(12,0),n.breaks=7) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='SC_2020-11-06'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="SC"&seasonID=="fall4"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(" ") +
  ylab(" ") +
  coord_flip() + 
  theme_classic(); sc.fall4


#WITCH HOLE ####
#winter
wh.winter=ggplot(filter(c3,lakedate=='WH_2020-02-25'),
                 aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(9,0),n.breaks=6) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='WH_2020-02-25'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,10),midpoint=5,
                        breaks=c(0,2,4,6,8,10),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="WH"&seasonID=="winter1"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab("Depth (m)") +
  ylab(bquote("Chl"~italic("a")~"(RFU)")) +
  coord_flip() + 
  theme_classic(); wh.winter

#spring3
wh.spring3=ggplot(filter(c3,lakedate=='WH_2020-05-26'),
                  aes(x=Depth,y=Chlorophyll_a)) +
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(9,0),n.breaks=6) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='WH_2020-05-26'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,10),midpoint=5,
                        breaks=c(0,2,4,6,8,10),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="WH"&seasonID=="spring3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(" ") +
  ylab(bquote("Chl"~italic("a")~"(RFU)")) +
  coord_flip() + 
  theme_classic(); wh.spring3

#summer3
wh.summer3=ggplot(filter(c3,lakedate=='WH_2020-08-03'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(9,0),n.breaks=6) +
  ylim(0,1000) +
  geom_point(filter(chl,lakedate=='WH_2020-08-03'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=975),size=5) + 
  scale_color_gradient2(limits=c(0,10),midpoint=5,
                        breaks=c(0,2,4,6,8,10),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="WH"&seasonID=="summer3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=925),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(" ") +
  ylab(bquote("Chl"~italic("a")~"(RFU)")) +
  coord_flip() + 
  theme_classic(); wh.summer3 

#fall4
wh.fall4=ggplot(filter(c3,lakedate=='WH_2020-10-31'),
                aes(x=Depth,y=Chlorophyll_a)) +
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(9,0),n.breaks=6) +
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='WH_2020-10-31'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,10),midpoint=5,
                        breaks=c(0,2,4,6,8,10),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="WH"&seasonID=="fall4"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(" ") +
  ylab(bquote("Chl"~italic("a")~"(RFU)")) +
  coord_flip() +
  theme_classic(); wh.fall4


#Combine ####
jp = ggarrange(jp.winter,jp.spring3,jp.summer3,jp.fall4,
               nrow=1,ncol=4,common.legend=TRUE,legend='right'); jp
#bubble
bb = ggarrange(NA,bb.spring3,bb.summer3,bb.fall4,
               nrow=1,ncol=4,common.legend=TRUE,legend='right'); bb
#seal cove
sc = ggarrange(sc.winter,sc.spring3,sc.summer3,sc.fall4,
               nrow=1,ncol=4,common.legend=TRUE,legend='right'); sc
#which hole
wh = ggarrange(wh.winter,wh.spring3,wh.summer3,wh.fall4,
               nrow=1,ncol=4,common.legend=TRUE,legend='right'); wh
#combine all
seasonal.chl = ggarrange(jp,bb,sc,wh,nrow=4,ncol=1)
seasonal.chl

#format
seasonal.chl.formatted=seasonal.chl +
  theme(title=element_text(size=4,hjust=0.5),
        legend.title=element_text(size=3.5),
        axis.title=element_text(size=4),
        axis.text=element_text(size=4))

#save
ggsave(filename="seasonal.chl.png", plot=seasonal.chl.formatted, device="png",
       height=8, width=12, units="in", dpi=800)





