#combining c3, and extracted chlorophyll
#version 2
#2021-03-26



#load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)



#load survey data and nps secchi data
c3 = read.csv("library/db.c3.csv",header=T)
#drop < 0.5m
c3 = c3 %>% filter(Depth > 0.5)
nooch = read.csv("library/db.nooch.csv",header=T)
secchi = read.csv("library/db.secchi.csv",header=T)
nps.secchi = read.csv("library/lt.secchi.csv",header=T)


#combined secchi dataframe
#get my winter secchi for fun
secchi = secchi %>% 
  filter(Date <= "2020-03-01") %>% 
  select(-Notes) 
#get nps data for the other dates
nps.secchi = nps.secchi %>% 
  filter(Date >= "2020-01-01") %>% #get 2020 data
  mutate(seasonID = ifelse(Date >= "2020-10-01", "fall4", NA)) %>% #ID october dates as fall 4
  #closest dates to my summer3 dates
  mutate(seasonID = replace(seasonID, Date > "2020-07-01" & Date < "2020-07-31", "summer3")) %>% 
  #closest dates to my spring3 dates
  mutate(seasonID = replace(seasonID, Date > "2020-06-01" & Date < "2020-06-30", "spring3")) 
#combine
secchi = rbind(secchi,nps.secchi)

#select seasons from each df
szns = c('winter1','spring3','summer3','fall4')
c3 = c3 %>% filter(seasonID %in% szns)
chl = nooch %>% filter(seasonID %in% szns)
secchi = secchi %>% filter(seasonID %in% szns)




#JORDAN ####
#annotation
jp.text = ggplot(c3,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Jordan Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); jp.text


#winter
jp.winter=ggplot(filter(c3,lakedate=='JP_2020-02-21'),
                 aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) +
  ylim(0,450) +
  #add extracted chl. use mapping=aes()' to avoid error
  geom_point(filter(chl,lakedate=='JP_2020-02-21'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) +  
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(data=filter(secchi,LakeID=="JP"&seasonID=="winter1"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(title="Winter",color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab("Depth (m)") +
  ylab(NULL) +
  coord_flip() + 
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        legend.position="none"); jp.winter 

#spring3
jp.spring3=ggplot(filter(c3,lakedate=='JP_2020-05-28'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='JP_2020-05-28'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) + 
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="JP"&seasonID=="spring3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(title="Spring", color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() + 
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        legend.position="none"); jp.spring3

#summer3
jp.summer3=ggplot(filter(c3,lakedate=='JP_2020-08-04'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='JP_2020-08-04'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) + 
  scale_color_gradient2(limits=c(0,3),
                        midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="JP"&seasonID=="summer3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(title="Summer",color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() + 
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        legend.position="none"); jp.summer3 

#fall4
jp.fall4=ggplot(filter(c3,lakedate=='JP_2020-10-31'),
                aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='JP_2020-10-31'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) + 
  scale_color_gradient2(limits=c(0,3),
                        midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="JP"&seasonID=="fall4"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(title="Autumn", color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() + 
  theme_classic() +
  theme(plot.title=element_text(size=rel(1))); jp.fall4





#BUBBLE ####

#annotation
bb.text = ggplot(c3,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Bubble Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); bb.text


#winter - NA
#making sure its the exact same size
bb.winter = ggplot(filter(c3,lakedate=='BB_2020-05-26'),
                   aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(16,13),n.breaks=7) +
  ylim(-450,0) +
  geom_point(filter(chl,lakedate=='BB_2020-05-26'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(3,6),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  ylab(NULL) +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  theme(legend.position="none",
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),        
        panel.background=element_blank()); bb.winter



#spring3
bb.spring3=ggplot(filter(c3,lakedate=='BB_2020-05-26'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(11,0),n.breaks=7) +
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='BB_2020-05-26'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="BB"&seasonID=="spring3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  theme_classic() +
  theme(legend.position="none"); bb.spring3

#summer3
bb.summer3=ggplot(filter(c3,lakedate=='BB_2020-08-04'),
                  aes(x=Depth,y=Chlorophyll_a)) +
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(11,0),n.breaks=7) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='BB_2020-08-04'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="BB"&seasonID=="summer3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() + 
  theme_classic() +
  theme(legend.position="none"); bb.summer3 

#fall4
bb.fall4=ggplot(filter(c3,lakedate=='BB_2020-10-31'), #remove 2 high fchl points
                aes(x=Depth,y=Chlorophyll_a)) +
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(11,0),n.breaks=7) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='BB_2020-10-31'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="BB"&seasonID=="fall4"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() + 
  theme_classic(); bb.fall4


#SEAL COVE ####
#annotation
sc.text = ggplot(c3,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Seal Cove Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); sc.text


#winter
sc.winter=ggplot(filter(c3,lakedate=='SC_2020-02-21'),
                 aes(x=Depth,y=Chlorophyll_a)) +
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(12,0),n.breaks=7) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='SC_2020-02-21'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="SC"&seasonID=="winter1"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab("Depth (m)") +
  ylab(NULL) +
  coord_flip() +
  theme_classic() +
  theme(legend.position="none"); sc.winter 

#spring3
sc.spring3=ggplot(filter(c3,lakedate=='SC_2020-05-28'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(12,0),n.breaks=7) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='SC_2020-05-28'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="SC"&seasonID=="spring3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() + 
  theme_classic() +
  theme(legend.position="none"); sc.spring3

#summer3
sc.summer3=ggplot(filter(c3,lakedate=='SC_2020-08-03'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(12,0),n.breaks=7) +
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='SC_2020-08-03'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) + 
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="SC"&seasonID=="summer3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() + 
  theme_classic() +
  theme(legend.position="none"); sc.summer3 

#fall4
sc.fall4=ggplot(filter(c3,lakedate=='SC_2020-11-06'),
                aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(12,0),n.breaks=7) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='SC_2020-11-06'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="SC"&seasonID=="fall4"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() + 
  theme_classic(); sc.fall4



#WITCH HOLE ####
#annotation
wh.text = ggplot(c3,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Witch Hole Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); wh.text


#winter
wh.winter=ggplot(filter(c3,lakedate=='WH_2020-02-25'),
                 aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(10,0),n.breaks=6) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='WH_2020-02-25'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
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
  theme_classic() +
  theme(legend.position="none"); wh.winter

#spring3
wh.spring3=ggplot(filter(c3,lakedate=='WH_2020-05-26'),
                  aes(x=Depth,y=Chlorophyll_a)) +
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(10,0),n.breaks=6) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='WH_2020-05-26'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,10),midpoint=5,
                        breaks=c(0,2,4,6,8,10),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="WH"&seasonID=="spring3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(bquote("Chl"~italic("a")~"(RFU)")) +
  coord_flip() + 
  theme_classic() +
  theme(legend.position="none"); wh.spring3

#summer3
wh.summer3=ggplot(filter(c3,lakedate=='WH_2020-08-03'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(10,0),n.breaks=6) +
  ylim(0,1000) +
  geom_point(filter(chl,lakedate=='WH_2020-08-03'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=975),size=5) + 
  scale_color_gradient2(limits=c(0,10),midpoint=5,
                        breaks=c(0,2,4,6,8,10),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="WH"&seasonID=="summer3"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=925),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(bquote("Chl"~italic("a")~"(RFU)")) +
  coord_flip() + 
  theme_classic() +
  theme(legend.position="none"); wh.summer3 

#fall4
wh.fall4=ggplot(filter(c3,lakedate=='WH_2020-10-31'),
                aes(x=Depth,y=Chlorophyll_a)) +
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(10,0),n.breaks=6) +
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='WH_2020-10-31'),
             mapping=aes(x=depth,y=chla.ugpl,color=chla.ugpl),
             position=position_nudge(y=400),size=5) +
  scale_color_gradient2(limits=c(0,10),midpoint=5,
                        breaks=c(0,2,4,6,8,10),
                        low="green",mid='gold',high="red") +
  geom_segment(filter(secchi,LakeID=="WH"&seasonID=="fall4"),
               mapping=aes(x=Secchi_m,xend=Secchi_m,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  xlab(NULL) +
  ylab(bquote("Chl"~italic("a")~"(RFU)")) +
  coord_flip() +
  theme_classic(); wh.fall4



#Combine ####
#jp
jp = wrap_plots(jp.winter,
                jp.spring3,
                jp.summer3,
                jp.fall4,
                nrow=1,
                ncol=4); jp



#bubble
bb = wrap_plots(bb.winter,
                bb.spring3,
                bb.summer3,
                bb.fall4,
                nrow=1,
                ncol=4); bb

#seal cove
sc = wrap_plots(sc.winter,
                sc.spring3,
                sc.summer3,
                sc.fall4,
                nrow=1,
                ncol=4); sc

#which hole
wh = wrap_plots(wh.winter,
                wh.spring3,
                wh.summer3,
                wh.fall4,
                nrow=1,
                ncol=4);wh

#annotations
text = wrap_plots(jp.text,
                  bb.text,
                  sc.text,
                  wh.text,
                  nrow=4,
                  ncol=1); text

#combine all
comb = wrap_plots(jp,bb,sc,wh,nrow=4,ncol=1); comb

comb2=wrap_plots(text,comb,nrow=1,ncol=2,widths=c(1,5)); comb2



#save
ggsave(plot=comb2, 
       filename="plots/formatted/seasonal.chl.png",
       device="png",
       height=8, 
       width=14, 
       units="in", 
       dpi=600)


#save
ggsave(plot=comb2, 
       filename="plots/formatted/seasonal.chl2.png",
       device="png",
       height=6, 
       width=10, 
       units="in", 
       dpi=600)










#HABs version ####


#annotations
text = wrap_plots(jp.text,
                  sc.text,
                  wh.text,
                  nrow=3,
                  ncol=1); text

#combine all
comb = wrap_plots(jp,
                  sc,
                  wh,
                  nrow=3,
                  ncol=1); comb

comb2 = wrap_plots(text,
                   comb,
                   nrow=1,
                   ncol=2,
                   widths=c(1,5)); comb2



#save
ggsave(plot=comb2, 
       filename="HABs/figures/seasonal.chl.jpg",
       device="jpg",
       height=8, 
       width=12, 
       units="in", 
       dpi=600)





