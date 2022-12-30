#combining c3, and extracted chlorophyll
#16-ish panels. arranged lakes vertically, seasons horizontally 
#updated 2020-12-04

#load survey data 
source('Rscripts/1.01_load.survey.R')
#load nps secchi
source('Rscripts/4.01_load.longterm.R')

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
do = full.exo %>% filter(seasonID %in% szns)


#JORDAN ####
#winter
#temp
jp.w.t=ggplot(filter(c3,lakedate=='JP_2020-02-21'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(27,0),n.breaks=6) +
  geom_segment(data=filter(secchi,LakeID=="JP"&seasonID=="winter1"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=5),
               linetype=6,color="gray60") +
  ylab("Depth (m)") + xlab(" ") + xlim(0,5) +
  theme_classic(); jp.w.t 
#DO
jp.w.o=ggplot(filter(do,lakedate=='JP_2020-02-21'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(27,0),n.breaks=6) +
  geom_segment(data=filter(secchi,LakeID=="JP"&seasonID=="winter1"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,105) +
  theme_classic(); jp.w.o 
#spring3
#temp
jp.sp.t=ggplot(filter(c3,lakedate=='JP_2020-05-28'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(27,0),n.breaks=6) +
  geom_segment(data=filter(secchi,LakeID=="JP"&seasonID=="spring3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=20),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,20) +
  theme_classic(); jp.sp.t 
#DO
jp.sp.o=ggplot(filter(do,lakedate=='JP_2020-05-28'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(27,0),n.breaks=6) +
  geom_segment(data=filter(secchi,LakeID=="JP"&seasonID=="spring3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,105) +
  theme_classic(); jp.sp.o 
#summer3
#temp
jp.su.t=ggplot(filter(c3,lakedate=='JP_2020-08-04'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(27,0),n.breaks=6) +
  geom_segment(data=filter(secchi,LakeID=="JP"&seasonID=="summer3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=30),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,30) +
  theme_classic(); jp.su.t 
#DO
jp.su.o=ggplot(filter(do,lakedate=='JP_2020-08-04'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(27,0),n.breaks=6) +
  geom_segment(data=filter(secchi,LakeID=="JP"&seasonID=="summer3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,105) +
  theme_classic(); jp.su.o 
#fall4
#temp
jp.f.t=ggplot(filter(c3,lakedate=='JP_2020-10-31'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(27,0),n.breaks=6) +
  geom_segment(data=filter(secchi,LakeID=="JP"&seasonID=="fall4"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=15),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,15) +
  theme_classic(); jp.f.t 
#DO
jp.f.o=ggplot(filter(do,lakedate=='JP_2020-10-31'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(27,0),n.breaks=6) +
  geom_segment(data=filter(secchi,LakeID=="JP"&seasonID=="fall4"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  xlim(0,105) + ylab(" ") + xlab(" ") +
  theme_classic(); jp.f.o


#BUBBLE ####
#spring3
#temp
bb.sp.t=ggplot(filter(c3,lakedate=='BB_2020-05-26'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(11,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="BB"&seasonID=="spring3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=20),
               linetype=6,color="gray60") +
  ylab("Depth (m)") + xlab(" ") + xlim(0,20) +
  theme_classic(); bb.sp.t 
#DO
bb.sp.o=ggplot(filter(do,lakedate=='BB_2020-05-26'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(11,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="BB"&seasonID=="spring3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,105) +
  theme_classic(); bb.sp.o 
#summer3
#temp
bb.su.t=ggplot(filter(c3,lakedate=='BB_2020-08-04'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(11,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="BB"&seasonID=="summer3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=30),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,30) +
  theme_classic(); bb.su.t 
#DO
bb.su.o=ggplot(filter(do,lakedate=='BB_2020-08-04'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(11,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="BB"&seasonID=="summer3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,105) +
  theme_classic(); bb.su.o 
#fall4
#temp
bb.f.t=ggplot(filter(c3,lakedate=='BB_2020-10-31'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(11,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="BB"&seasonID=="fall4"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=15),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,15) +
  theme_classic(); bb.f.t 
#DO
bb.f.o=ggplot(filter(do,lakedate=='BB_2020-10-31'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(11,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="BB"&seasonID=="fall4"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  xlim(0,105) + ylab(" ") + xlab(" ") +
  theme_classic(); bb.f.o



#SEAL COVE ####
#winter
#temp
sc.w.t=ggplot(filter(c3,lakedate=='SC_2020-02-21'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(12,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="SC"&seasonID=="winter1"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=5),
               linetype=6,color="gray60") +
  ylab("Depth (m)") + xlab(" ") + xlim(0,5) +
  theme_classic(); sc.w.t 
#DO
sc.w.o=ggplot(filter(do,lakedate=='SC_2020-02-21'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(12,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="SC"&seasonID=="winter1"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,105) +
  theme_classic(); sc.w.o 
#spring3
#temp
sc.sp.t=ggplot(filter(c3,lakedate=='SC_2020-05-28'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(12,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="SC"&seasonID=="spring3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=20),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,20) +
  theme_classic(); sc.sp.t 
#DO
sc.sp.o=ggplot(filter(do,lakedate=='SC_2020-05-28'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(12,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="SC"&seasonID=="spring3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,105) +
  theme_classic(); sc.sp.o 
#summer3
#temp
sc.su.t=ggplot(filter(c3,lakedate=='SC_2020-08-03'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(12,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="SC"&seasonID=="summer3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=30),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,30) +
  theme_classic(); sc.su.t 
#DO
sc.su.o=ggplot(filter(do,lakedate=='SC_2020-08-03'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(12,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="SC"&seasonID=="summer3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,105) +
  theme_classic(); sc.su.o 
#fall4
#temp
sc.f.t=ggplot(filter(c3,lakedate=='SC_2020-11-06'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(12,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="SC"&seasonID=="fall4"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=15),
               linetype=6,color="gray60") +
  ylab(" ") + xlab(" ") + xlim(0,15) +
  theme_classic(); sc.f.t 
#DO
sc.f.o=ggplot(filter(do,lakedate=='SC_2020-11-06'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(12,0),n.breaks=7) +
  geom_segment(data=filter(secchi,LakeID=="SC"&seasonID=="fall4"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  xlim(0,105) + ylab(" ") + xlab(" ") +
  theme_classic(); sc.f.o




#WITCH HOLE ####
#winter
#temp
WH.w.t=ggplot(filter(c3,lakedate=='WH_2020-02-25'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(9,0),n.breaks=6) + 
  geom_segment(data=filter(secchi,LakeID=="WH"&seasonID=="winter1"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=5),
               linetype=6,color="gray60") +
  ylab("Depth (m)") + xlab("Temp C") + xlim(0,5) +
  theme_classic(); WH.w.t 
#DO
WH.w.o=ggplot(filter(do,lakedate=='WH_2020-02-25'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(9,0),n.breaks=6) + 
  geom_segment(data=filter(secchi,LakeID=="WH"&seasonID=="winter1"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  xlim(0,105) + xlab("DO sat%") + ylab(" ") +
  theme_classic(); WH.w.o 
#spring3
#temp
WH.sp.t=ggplot(filter(c3,lakedate=='WH_2020-05-26'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(9,0),n.breaks=6) + 
  geom_segment(data=filter(secchi,LakeID=="WH"&seasonID=="spring3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=20),
               linetype=6,color="gray60") +
  ylab(" ") + xlab("Temp C") + xlim(0,20) +
  theme_classic(); WH.sp.t 
#DO
WH.sp.o=ggplot(filter(do,lakedate=='WH_2020-05-26'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(9,0),n.breaks=6) + 
  geom_segment(data=filter(secchi,LakeID=="WH"&seasonID=="spring3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  xlim(0,105) + xlab("DO sat%") + ylab(" ") +
  theme_classic(); WH.sp.o 
#summer3
#temp
WH.su.t=ggplot(filter(c3,lakedate=='WH_2020-08-03'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(9,0),n.breaks=6) + 
  geom_segment(data=filter(secchi,LakeID=="WH"&seasonID=="summer3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=30),
               linetype=6,color="gray60") +
  ylab(" ") + xlab("Temp C") + xlim(0,30) +
  theme_classic(); WH.su.t 
#DO
WH.su.o=ggplot(filter(do,lakedate=='WH_2020-08-03'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(9,0),n.breaks=6) + 
  geom_segment(data=filter(secchi,LakeID=="WH"&seasonID=="summer3"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  xlim(0,105) + xlab("DO sat%") + ylab(" ") +
  theme_classic(); WH.su.o 
#fall4
#temp
WH.f.t=ggplot(filter(c3,lakedate=='WH_2020-10-31'),aes(y=Depth,x=Temp)) + 
  geom_point(shape=1,size=1,color='firebrick3') +  
  scale_y_reverse(limits=c(9,0),n.breaks=6) + 
  geom_segment(data=filter(secchi,LakeID=="WH"&seasonID=="fall4"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=15),
               linetype=6,color="gray60") +
  ylab(" ") + xlab("Temp C") + xlim(0,15) +
  theme_classic(); WH.f.t 
#DO
WH.f.o=ggplot(filter(do,lakedate=='WH_2020-10-31'),aes(y=Depth,x=DO_sat)) + 
  geom_point(shape=1,size=1,color='skyblue4') + 
  geom_path(linetype=2,color='skyblue4') +
  scale_y_reverse(limits=c(9,0),n.breaks=6) + 
  geom_segment(data=filter(secchi,LakeID=="WH"&seasonID=="fall4"),
               mapping=aes(y=Secchi_m,yend=Secchi_m,x=0,xend=105),
               linetype=6,color="gray60") +
  xlim(0,105) + xlab("DO sat%") + ylab(" ") +
  theme_classic(); WH.f.o









#Combine ####
jp = ggarrange(jp.w.t,jp.w.o,jp.sp.t,jp.sp.o,
               jp.su.t,jp.su.o, jp.f.t,jp.f.o,nrow=1,ncol=8); jp
#bubble
bb = ggarrange(NA,NA,bb.sp.t,bb.sp.o,
               bb.su.t,bb.su.o, bb.f.t,bb.f.o,nrow=1,ncol=8); bb

#seal cove
sc = ggarrange(sc.w.t,sc.w.o,sc.sp.t,sc.sp.o,
               sc.su.t,sc.su.o, sc.f.t,sc.f.o,nrow=1,ncol=8); sc
#which hole
wh = ggarrange(WH.w.t,WH.w.o,WH.sp.t,WH.sp.o,
               WH.su.t,WH.su.o, WH.f.t,WH.f.o,nrow=1,ncol=8); wh



#combine all
comb = ggarrange(jp,bb,sc,wh,nrow=4,ncol=1)
comb

#format
comb.f=comb +
  theme(title=element_text(size=4,hjust=0.5),
        legend.title=element_text(size=3.5),
        axis.title=element_text(size=4),
        axis.text=element_text(size=4))

#save
ggsave(filename="seasonal.do.temp.png", plot=comb.f, device="png",
       height=8, width=14, units="in", dpi=500)





