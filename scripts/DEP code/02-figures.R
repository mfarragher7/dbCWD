#Generate figures
#created 2022-09-19



#figure ideas:
#seperate each year for each lake
#1. heatmap with thermocline and meta depths 
#2. DO with 2mg and 5mg
#3. weather - precip
#4. weather - wind speed
#5. weather - air temp, min,max,mean
#export pdf for each sset of lake figs

#libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)
library(ggpubr)




#Temp ######
#thermal structure

temp.df = read.csv('library/summary.daily.temp.csv')


#bubble
ggplot(filter(temp.df, lake=='Bubble'), 
       aes(x=jday, y=thermo.depth)) +
  geom_point(shape=1, alpha=0.5) + 
  # stat_smooth(method="loess", size=1, span=0.35, se=F, show.legend=F) +
  facet_wrap(~year, nrow=3, ncol=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  scale_y_reverse(limits=c(12,0),breaks=c(0,4,8,12)) +
  #scale_color_manual(labels=c("Power consumed", "Power generated"), values=c("red", "blue")) +
  labs(x='Date',
       y='Thermocline Depth (m)') +
  #guides(color=guide_legend(override.aes=list(shape=19, alpha=1))) +
  theme_bw()




#Temp profiles ##### 
#using interpolated values for some dates
depths.df = read.csv('library/summary.daily.depth.temp.int.csv')
str(depths.df)

#* modify dfs ######
#replace negative temps with zeros
depths.df = depths.df %>% 
  mutate(temp.mean=replace(temp.mean,temp.mean<0,0))

#get thermocline values. matches values from other dataframe
depths.df = depths.df %>% mutate(lakedate=paste(lake, date, sep="_"))
depths.df$thermo.depth = NA
ld = depths.df$lakedate

for (i in 1:length(ld)){ #for every unique lakedate,
  td = depths.df[depths.df$lakedate == ld[i], ] #temp dataframe that subsets lakes df by each lakedate
  thermo = thermo.depth(td$temp, td$depth, seasonal=FALSE, index=FALSE, mixed.cutoff=1) #run thermo.depth function
  depths.df[i,8] = thermo[1] }    #paste thermocline depth in 10th column of row i*j == i 


#add meta layers
depths.df$meta.top = NA
depths.df$meta.bottom = NA

for (i in 1:length(ld)){ #for every unique lakehr,
  td = depths.df[depths.df$lakedate == ld[i], ] #temp dataframe that subsets lakes df by each lakedate
  td = td[!duplicated(td$depth), ] #remove duplicate depths
  meta = meta.depths(td$temp, td$depth, slope=0.1, seasonal=F, mixed.cutoff=1) 
  depths.df[i,9] = meta[1]   #top
  depths.df[i,10] = meta[2] } #bottom     



#clean up layer values

#remove NaNs.
depths.df = depths.df %>%  mutate_all(~ifelse(is.nan(.), NA, .))

#remove faslse thermocline
#anything before may 1st, and after dec 1
depths.df = depths.df %>% 
  mutate(thermo.depth=replace(thermo.depth,jday<121,NA)) %>% 
  mutate(thermo.depth=replace(thermo.depth,jday>334,NA)) %>% 
  #less than 2m
  mutate(thermo.depth=replace(thermo.depth,thermo.depth<2,NA))
  
#if thermocline is NA, remove meta depths
depths.df = depths.df %>%  
  mutate(meta.top=replace(meta.top,is.na(thermo.depth),NA)) %>% 
  mutate(meta.bottom=replace(meta.bottom,is.na(thermo.depth),NA))

#make new df, pivoted for depth layers
dl = depths.df %>% 
  pivot_longer(cols=c(thermo.depth,meta.top,meta.bottom), 
               names_to="layer", values_to="value") %>% 
  mutate(layer=replace(layer,layer=='thermo.depth','Thermocline')) %>% 
  mutate(layer=replace(layer,layer=='meta.top','Meta top')) %>% 
  mutate(layer=replace(layer,layer=='meta.bottom','Meta bottom'))

dl$layer = factor(dl$layer, levels=c('Thermocline','Meta top','Meta bottom'))







#Figures ########

#* tile plot ######
#old version
cols = c('#2E0769','#07766F','#59A64D','#E9CE28','#F94343')

#version zero
v = ggplot(filter(depths.df, lake=='Bubble'),
       aes(x=jday,
           y=depth,
           fill=temp.mean)) +
  geom_tile() +
  facet_wrap(~year,ncol=2,nrow=3) +
  scale_y_reverse(limits=c(12,0), n.breaks=4) +
  scale_x_continuous(limits=c(0,366),
                    breaks=c(60,182,305),
                    labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  scale_fill_gradientn(colors=cols,
                       limits=c(0,30), 
                       na.value="white") +
  labs(title='Bubble Pond',
       color="Temp C",
       x="Date",
       y="Depth (m)") +
  theme_bw() +
  theme(title=element_text(size=10),
        legend.title=element_blank()); v

ggsave(plot=v,
       filename="figures/temp.contours.bb.alt.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)  


#* contour plots ######
#** Bubble ####

ggplot(filter(depths.df, lake=='Bubble'),
       aes(x=jday,
           y=depth,
           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  facet_wrap(~year,ncol=2,nrow=3) +
  scale_y_reverse(limits=c(12,0), n.breaks=4) +
  labs(title='Bubble Pond',
       color="Temp C",
       x="Date",
       y="Depth (m)")
  

#geom_contour plus thermocline  

p1 = ggplot(filter(depths.df, lake=='Bubble'),
       aes(x=jday, 
           y=depth, 
           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=thermo.depth),
             shape=1,
             size=0.5) +
  facet_wrap(~year,ncol=2,nrow=3) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  labs(title='Bubble Pond',
       color="Temp C",
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); p1



ggsave(plot=p1,
       filename="figures/temp.contours.bb.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)




#thermocline and meta depths

bb.contour = ggplot(filter(dl, lake=='Bubble'),
            aes(x=jday, 
                y=depth, 
                z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  facet_wrap(~year,ncol=2,nrow=3) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Bubble Pond',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.contour



ggsave(plot=bb.contour,
       filename="figures/temp.contours.layers.bb.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)













#** LSB #####

p2 = ggplot(filter(depths.df, lake=='Lower South Branch'),
            aes(x=jday, 
                y=depth, 
                z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=thermo.depth),
             shape=1,
             size=0.5) +
  facet_wrap(~year,ncol=2,nrow=3) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(18,0), 
                  n.breaks=6) +
  labs(title='Lower South Branch Pond',
       color="Temp C",
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); p2



ggsave(plot=p2,
       filename="figures/temp.contours.lsb.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)

#tile version
t= ggplot(filter(depths.df, lake=='Lower South Branch'),
          aes(x=jday,
              y=depth,
              fill=temp.mean)) +
  geom_tile() +
  facet_wrap(~year,ncol=2,nrow=3) +
  scale_y_reverse(limits=c(18,0), n.breaks=5) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  scale_fill_gradientn(colors=cols,
                       limits=c(0,30),
                       na.value="white") +
  labs(title='LSB Pond',
       color="Temp C",
       x="Date",
       y="Depth (m)") +
  theme_bw() +
  theme(title=element_text(size=10),
        legend.title=element_blank()); t


ggsave(plot=t,
       filename="figures/temp.contours.lsb.alt.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400) 


#thermocline and meta depths
lsb.sub = dl %>%
  filter(lake=='Lower South Branch') %>% 
  filter(depth>16) %>% 
  filter(year==2021)

#temperature at 18m is double 17m in summer months in every year....
#plot without bottom meter

lsb.contour = ggplot(filter(dl, lake=='Lower South Branch'),
                    aes(x=jday, 
                        y=depth, 
                        z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  facet_wrap(~year,ncol=2,nrow=3) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(18,0), n.breaks=5) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Lower South Branch Pond',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.contour


ggsave(plot=lsb.contour,
       filename="figures/temp.contours.layers.lsb.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)







#** Ellis #####
p3 = ggplot(filter(depths.df, lake=='Ellis'),
            aes(x=jday, 
                y=depth, 
                z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=thermo.depth),
             shape=1,
             size=0.5) +
  facet_wrap(~year,ncol=2,nrow=3) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  labs(title='Ellis Pond',
       color="Temp C",
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); p3



ggsave(plot=p3,
       filename="figures/temp.contours.ellis.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)


#thermocline and meta layers
el.contour = ggplot(filter(dl, lake=='Ellis'),
                     aes(x=jday, 
                         y=depth, 
                         z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  facet_wrap(~year,ncol=2,nrow=3) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Ellis Pond',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.contour


ggsave(plot=el.contour,
       filename="figures/temp.contours.layers.el.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)











#** Wood #####

p4 = ggplot(filter(depths.df, lake=='Wood'),
            aes(x=jday, 
                y=depth, 
                z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=thermo.depth),
             shape=1,
             size=0.5) +
  facet_wrap(~year,ncol=2,nrow=3) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(4,0), 
                  n.breaks=4) +
  labs(title='Wood Pond',
       color="Temp C",
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); p4



ggsave(plot=p4,
       filename="figures/temp.contours.wood.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)




#thermocline and meta layers
wd.contour = ggplot(filter(dl, lake=='Wood'),
                    aes(x=jday, 
                        y=depth, 
                        z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  facet_wrap(~year,ncol=2,nrow=3) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(4,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Wood Pond',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.contour


ggsave(plot=el.contour,
       filename="figures/temp.contours.layers.wd.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)
 










#DO #############

#load DO

do = read.csv('library/summary.do.daily.csv')
str(do)
#make depth factors
do$depth.factor = paste(do$depth,"m",sep="")
unique(do$depth.factor)
#split
bbdo = do %>% filter(lake=='Bubble')
bbdo$depth.factor = factor(bbdo$depth.factor,levels=c("1m","7m","11m"))
str(bbdo)



#** Bubble #####

bb.do = ggplot(bbdo,
       aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  facet_wrap(~year ,nrow=3, ncol=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=2))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed","dashed")) +
  labs(title='Bubble Pond',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.do


ggsave(plot=bb.do,
       filename="figures/do.points.bb.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)

#v2

n = ggplot(bbdo,
       aes(x=jday, 
           y=depth, 
           z=do.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  facet_wrap(~year,nrow=3,ncol=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  labs(title='Bubble Pond',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); n

ggsave(plot=n,
       filename="figures/do.contour.bb.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)




#split
lsb = do %>% filter(lake=='Lower South Branch')
lsb$depth.factor = factor(lsb$depth.factor,levels=c("1m","10m","17m"))
str(lsb)



#** LSB #####

g = ggplot(lsb,
       aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  facet_wrap(~year ,nrow=3, ncol=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=3))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed","dashed")) +
  labs(title='LSB Pond',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); g



ggsave(plot=g,
       filename="figures/do.points.lsb.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)






#v2

o = ggplot(lsb,
           aes(x=jday, 
               y=depth, 
               z=do.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  facet_wrap(~year,nrow=3,ncol=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(18,0), 
                  n.breaks=5) +
  labs(title='LSB Pond',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); o

ggsave(plot=o,
       filename="figures/do.contour.lsb.jpg",
       device="jpg",
       height=6, 
       width=10, 
       units="in", 
       dpi=400)





#Weather ###############
w = read.csv('library/weather.csv')

#Acadia

#precip
ggplot(filter(w,lake=='Bubble and Wood' & precip>0),
       aes(x=jday, y=precip)) +
  geom_point(shape=1,alpha=0.5,size=1) +
  facet_wrap(~year,nrow=3) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Macfarland Hill weather station',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10))


#wind
ggplot(filter(w,lake=='Ellis'),
       aes(x=jday, y=wind.mean)) +
  geom_point(shape=1,alpha=0.5,size=1) +
  facet_wrap(~year,nrow=3) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Greenville weather station',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10))


#temperature
#pivot for temp categories. mean min max
wl = w %>% 
  pivot_longer(cols=c(temp.mean,temp.min,temp.max), 
               names_to='temp.var', 
               values_to='temp.value') %>% 
  mutate(temp.var=replace(temp.var,temp.var=='temp.mean','Temp mean')) %>% 
  mutate(temp.var=replace(temp.var,temp.var=='temp.min','Temp min')) %>% 
  mutate(temp.var=replace(temp.var,temp.var=='temp.max','Temp max'))

wl$temp.var = factor(wl$temp.var, levels=c('Temp mean','Temp min','Temp max'))
str(wl)


ggplot(filter(wl,lake=='Bubble and Wood'),
       aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,alpha=0.5,size=1) +
  facet_wrap(~year,nrow=3) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('gray30','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Bar Harbor weather station',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10))


  





#SAVE PLOTS ########


#* Bubble ####

#*** temp ####
#2018
bb.contour.18 = ggplot(filter(dl, lake=='Bubble' & year==2018),
                    aes(x=jday, 
                        y=depth, 
                        z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  guides(fill=guide_legend(override.aes=list(size=0.5))) +
  labs(title='Bubble Pond Temperature 2018',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.contour.18

#2019
bb.contour.19 = ggplot(filter(dl, lake=='Bubble' & year==2019),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Bubble Pond Temperature 2019',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.contour.19

#2020
bb.contour.20 = ggplot(filter(dl, lake=='Bubble' & year==2020),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Bubble Pond Temperature 2020',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.contour.20

#2021
bb.contour.21 = ggplot(filter(dl, lake=='Bubble' & year==2021),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Bubble Pond Temperature 2021',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.contour.21

#2022
bb.contour.22 = ggplot(filter(dl, lake=='Bubble' & year==2022),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Bubble Pond Temperature 2022',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.contour.22




#*** DO ######
#DO - 2018
bb.do.18 = ggplot(filter(bbdo, year==2018),
               aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=3))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed", "dashed")) +
  labs(title='Bubble Pond Dissolved Oxygen 2018',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.do.18

#DO - 2019
bb.do.19 = ggplot(filter(bbdo, year==2019),
                  aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=3))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed", "dashed")) +
  labs(title='Bubble Pond Dissolved Oxygen 2019',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.do.19


#DO - 2020
bb.do.20 = ggplot(filter(bbdo, year==2020),
                  aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=3))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed", "dashed")) +
  labs(title='Bubble Pond Dissolved Oxygen 2020',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.do.20

#DO - 2021
bb.do.21 = ggplot(filter(bbdo, year==2021),
                  aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=3))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed", "dashed")) +
  labs(title='Bubble Pond Dissolved Oxygen 2021',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.do.21

#DO - 2022
bb.do.22 = ggplot(filter(bbdo, year==2022),
                  aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=3))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed", "dashed")) +
  labs(title='Bubble Pond Dissolved Oxygen 2022',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bb.do.22



#*** weather #####
#2018

#precip
#2018
bbwd.precip.18 = ggplot(filter(w,lake=='Bubble and Wood' & precip>0 & year==2018),
                         aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Bubble Pond Precipitation 2018',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.precip.18

#2019
bbwd.precip.19 = ggplot(filter(w,lake=='Bubble and Wood' & precip>0 & year==2019),
                         aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Bubble Pond Precipitation 2019',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.precip.19


#2020
bbwd.precip.20 = ggplot(filter(w,lake=='Bubble and Wood' & precip>0 & year==2020),
                         aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Bubble Pond Precipitation 2020',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.precip.20


#2021
bbwd.precip.21 = ggplot(filter(w,lake=='Bubble and Wood' & precip>0 & year==2021),
                         aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Bubble Pond Precipitation 2021',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.precip.21


#2022

#no 2022 precip data


#wind
#pull in separate csv
bbwdwind = read.csv('library/bb.wd.wind.csv')
str(bbwdwind)

#2018
bbwd.wind.18 = ggplot(filter(bbwdwind, year==2018),
       aes(x=jday, y=wind)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Bubble Pond Wind Speed 2018',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.wind.18

#2019
bbwd.wind.19 = ggplot(filter(bbwdwind, year==2019),
                      aes(x=jday, y=wind)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Bubble Pond Wind Speed 2019',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.wind.19

#2020
bbwd.wind.20 = ggplot(filter(bbwdwind, year==2020),
                      aes(x=jday, y=wind)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Bubble Pond Wind Speed 2020',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.wind.20


#2021
bbwd.wind.21 = ggplot(filter(bbwdwind, year==2021),
                      aes(x=jday, y=wind)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Bubble Pond Wind Speed 2021',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.wind.21

#2022
#no wind data




#temperature
#pivot for temp categories. mean min max
wl = w %>% 
  pivot_longer(cols=c(temp.mean,temp.min,temp.max), 
               names_to='temp.var', 
               values_to='temp.value') %>% 
  mutate(temp.var=replace(temp.var,temp.var=='temp.mean','Temp mean')) %>% 
  mutate(temp.var=replace(temp.var,temp.var=='temp.min','Temp min')) %>% 
  mutate(temp.var=replace(temp.var,temp.var=='temp.max','Temp max'))

wl$temp.var = factor(wl$temp.var, levels=c('Temp mean','Temp min','Temp max'))
str(wl)

#2018
bbwd.at.18 = ggplot(filter(wl,lake=='Bubble and Wood' & year==2018),
       aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Bubble Pond Air Temp 2018',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.at.18

#2019
bbwd.at.19 = ggplot(filter(wl,lake=='Bubble and Wood' & year==2019),
       aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Bubble Pond Air Temp 2019',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.at.19

#2020
bbwd.at.20 = ggplot(filter(wl,lake=='Bubble and Wood' & year==2020),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Bubble Pond Air Temp 2020',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.at.20

#2021
bbwd.at.21 = ggplot(filter(wl,lake=='Bubble and Wood' & year==2021),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Bubble Pond Air Temp 2021',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.at.21

#2022
bbwd.at.22 = ggplot(filter(wl,lake=='Bubble and Wood' & year==2022),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Bubble Pond Air Temp 2022',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); bbwd.at.22

#save
ggsave(plot=bb.contour.18,
       filename="figures/bubble/bb.contour.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bb.do.18,
       filename="figures/bubble/bb.do.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.precip.18,
       filename="figures/bubble/bb.precip.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.wind.18,
       filename="figures/bubble/bb.wind.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.at.18,
       filename="figures/bubble/bb.at.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)


#2019
ggsave(plot=bb.contour.19,
       filename="figures/bubble/bb.contour.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bb.do.19,
       filename="figures/bubble/bb.do.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.precip.19,
       filename="figures/bubble/bb.precip.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.wind.19,
       filename="figures/bubble/bb.wind.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.at.19,
       filename="figures/bubble/bb.at.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2020
ggsave(plot=bb.contour.20,
       filename="figures/bubble/bb.contour.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bb.do.20,
       filename="figures/bubble/bb.do.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.precip.20,
       filename="figures/bubble/bb.precip.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.wind.20,
       filename="figures/bubble/bb.wind.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.at.20,
       filename="figures/bubble/bb.at.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2021
ggsave(plot=bb.contour.21,
       filename="figures/bubble/bb.contour.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bb.do.21,
       filename="figures/bubble/bb.do.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.precip.21,
       filename="figures/bubble/bb.precip.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.wind.21,
       filename="figures/bubble/bb.wind.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bbwd.at.21,
       filename="figures/bubble/bb.at.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2022
ggsave(plot=bb.contour.22,
       filename="figures/bubble/bb.contour.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=bb.do.22,
       filename="figures/bubble/bb.do.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)


ggsave(plot=bbwd.at.22,
       filename="figures/bubble/bb.at.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)


#**COMBINE and SAVE #####
#combine temp and do plots
#bb
bb.temp.do.18 = ggarrange(bb.contour.18, bb.do.18, nrow=2, ncol=1, heights=c(7,3)); bb.temp.do.18
bb.temp.do.19 = ggarrange(bb.contour.19, bb.do.19, nrow=2, ncol=1, heights=c(7,3)); bb.temp.do.19
bb.temp.do.20 = ggarrange(bb.contour.20, bb.do.20, nrow=2, ncol=1, heights=c(7,3)); bb.temp.do.20
bb.temp.do.21 = ggarrange(bb.contour.21, bb.do.21, nrow=2, ncol=1, heights=c(7,3)); bb.temp.do.21
bb.temp.do.22 = ggarrange(bb.contour.22, bb.do.22, nrow=2, ncol=1, heights=c(7,3)); bb.temp.do.22
#combine weather plots
#bb
bb.weather.18 = ggarrange(bbwd.precip.18, bbwd.wind.18, bbwd.at.18, nrow=3, ncol=1); bb.weather.18
bb.weather.19 = ggarrange(bbwd.precip.19, bbwd.wind.19, bbwd.at.19, nrow=3, ncol=1); bb.weather.19
bb.weather.20 = ggarrange(bbwd.precip.20, bbwd.wind.20, bbwd.at.20, nrow=3, ncol=1); bb.weather.20
bb.weather.21 = ggarrange(bbwd.precip.21, bbwd.wind.21, bbwd.at.21, nrow=3, ncol=1); bb.weather.21

#save all plots in list
bb.list = list(bb.temp.do.18, bb.weather.18,
               bb.temp.do.19, bb.weather.19,
               bb.temp.do.20, bb.weather.20,
               bb.temp.do.21, bb.weather.21,
               bb.temp.do.22, bbwd.at.22)


#save pdf of all combined plots
pdf("figures/Combined_plots_Bubble.pdf")

for (i in 1:length(bb.list)) {
  print(bb.list[[i]])
}

dev.off() 






#* LSB #########

#*** temp ####
#2018
lsb.contour.18 = ggplot(filter(dl, lake=='Lower South Branch' & year==2018),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(18,0), 
                  n.breaks=5) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  guides(fill=guide_legend(override.aes=list(size=0.5))) +
  labs(title='Lower South Branch Pond Temperature 2018',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.contour.18

#2019
lsb.contour.19 = ggplot(filter(dl, lake=='Lower South Branch' & year==2019),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(18,0), 
                  n.breaks=5) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Lower South Branch Pond Temperature 2019',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.contour.19

#2020
lsb.contour.20 = ggplot(filter(dl, lake=='Lower South Branch' & year==2020),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(18,0), 
                  n.breaks=5) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Lower South Branch Pond Temperature 2020',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.contour.20

#2021
lsb.contour.21 = ggplot(filter(dl, lake=='Lower South Branch' & year==2021),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(18,0), 
                  n.breaks=5) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Lower South Branch Pond Temperature 2021',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.contour.21

#2022
lsb.contour.22 = ggplot(filter(dl, lake=='Lower South Branch' & year==2022),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(18,0), 
                  n.breaks=5) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Lower South Branch Pond Temperature 2022',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.contour.22




#*** DO ######
#DO - 2018
lsb.do.18 = ggplot(filter(lsb, year==2018),
                  aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=3))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed", "dashed")) +
  labs(title='Lower South Branch Pond Dissolved Oxygen 2018',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.do.18

#DO - 2019
lsb.do.19 = ggplot(filter(lsb, year==2019),
                  aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=3))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed", "dashed")) +
  labs(title='Lower South Branch Pond Dissolved Oxygen 2019',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.do.19


#DO - 2020
lsb.do.20 = ggplot(filter(lsb, year==2020),
                  aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=3))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed", "dashed")) +
  labs(title='Lower South Branch Pond Dissolved Oxygen 2020',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.do.20

#DO - 2021
lsb.do.21 = ggplot(filter(lsb, year==2021),
                  aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=3))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed", "dashed")) +
  labs(title='Lower South Branch Pond Dissolved Oxygen 2021',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.do.21

#DO - 2022
lsb.do.22 = ggplot(filter(lsb, year==2022),
                  aes(x=jday, y=do.mean, color=depth.factor)) +
  geom_point(shape=1, alpha=0.5, size=2) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('green4','red','darkblue'),
                     guide=guide_legend("Depth",override.aes=list(shape=16,size=3))) +
  geom_hline(aes(yintercept=2,linetype='Anoxia < 2 mg/L'),color="gray30",size=0.7) + #no do
  geom_hline(aes(yintercept=5,linetype='Hypoxia < 5 mg/L'),color="gray30",size=0.7) + #low do
  scale_linetype_manual(name=NULL,values=c("dashed", "dashed")) +
  labs(title='Lower South Branch Pond Dissolved Oxygen 2022',
       x='Date',
       y='DO (mg)') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.do.22



#*** weather #####
#2018

#precip
#2018
lsb.precip.18 = ggplot(filter(w,lake=='Lower South Branch' & precip>0 & year==2018),
                        aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Lower South Branch Pond Precipitation 2018',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.precip.18

#2019
lsb.precip.19 = ggplot(filter(w,lake=='Lower South Branch' & precip>0 & year==2019),
                        aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Lower South Branch Pond Precipitation 2019',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.precip.19


#2020
lsb.precip.20 = ggplot(filter(w,lake=='Lower South Branch' & precip>0 & year==2020),
                        aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Lower South Branch Pond Precipitation 2020',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.precip.20


#2021
lsb.precip.21 = ggplot(filter(w,lake=='Lower South Branch' & precip>0 & year==2021),
                        aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Lower South Branch Pond Precipitation 2021',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.precip.21


#2022
lsb.precip.22 = ggplot(filter(w,lake=='Lower South Branch' & precip>0 & year==2022),
                        aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Lower South Branch Pond Precipitation 2022',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.precip.22


#wind

#2018
lsb.wind.18 = ggplot(filter(w, year==2018),
                      aes(x=jday, y=wind.mean)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Lower South Branch Pond Wind Speed 2018',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.wind.18

#2019
lsb.wind.19 = ggplot(filter(w, year==2019),
                      aes(x=jday, y=wind.mean)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Lower South Branch Pond Wind Speed 2019',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.wind.19

#2020
lsb.wind.20 = ggplot(filter(w, year==2020),
                      aes(x=jday, y=wind.mean)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Lower South Branch Pond Wind Speed 2020',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.wind.20


#2021
lsb.wind.21 = ggplot(filter(w, year==2021),
                      aes(x=jday, y=wind.mean)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Lower South Branch Pond Wind Speed 2021',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.wind.21

#2022
lsb.wind.22 = ggplot(filter(w, year==2022),
                      aes(x=jday, y=wind.mean)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Lower South Branch Pond Wind Speed 2022',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.wind.22




#temperature

#2018
lsb.at.18 = ggplot(filter(wl,lake=='Lower South Branch' & year==2018),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Lower South Branch Pond Air Temp 2018',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.at.18

#2019
lsb.at.19 = ggplot(filter(wl,lake=='Lower South Branch' & year==2019),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Lower South Branch Pond Air Temp 2019',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.at.19

#2020
lsb.at.20 = ggplot(filter(wl,lake=='Lower South Branch' & year==2020),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Lower South Branch Pond Air Temp 2020',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.at.20

#2021
lsb.at.21 = ggplot(filter(wl,lake=='Lower South Branch' & year==2021),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Lower South Branch Pond Air Temp 2021',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.at.21

#2022
lsb.at.22 = ggplot(filter(wl,lake=='Lower South Branch' & year==2022),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Lower South Branch Pond Air Temp 2022',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); lsb.at.22

#*** save ####
ggsave(plot=lsb.contour.18,
       filename="figures/lowersouthbranch/lsb.contour.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.do.18,
       filename="figures/lowersouthbranch/lsb.do.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.precip.18,
       filename="figures/lowersouthbranch/lsb.precip.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.wind.18,
       filename="figures/lowersouthbranch/lsb.wind.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.at.18,
       filename="figures/lowersouthbranch/lsb.at.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)


#2019
ggsave(plot=lsb.contour.19,
       filename="figures/lowersouthbranch/lsb.contour.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.do.19,
       filename="figures/lowersouthbranch/lsb.do.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.precip.19,
       filename="figures/lowersouthbranch/lsb.precip.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.wind.19,
       filename="figures/lowersouthbranch/lsb.wind.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.at.19,
       filename="figures/lowersouthbranch/lsb.at.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2020
ggsave(plot=lsb.contour.20,
       filename="figures/lowersouthbranch/lsb.contour.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.do.20,
       filename="figures/lowersouthbranch/lsb.do.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.precip.20,
       filename="figures/lowersouthbranch/lsb.precip.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.wind.20,
       filename="figures/lowersouthbranch/lsb.wind.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.at.20,
       filename="figures/lowersouthbranch/lsb.at.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2021
ggsave(plot=lsb.contour.21,
       filename="figures/lowersouthbranch/lsb.contour.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.do.21,
       filename="figures/lowersouthbranch/lsb.do.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.precip.21,
       filename="figures/lowersouthbranch/lsb.precip.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.wind.21,
       filename="figures/lowersouthbranch/lsb.wind.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.at.21,
       filename="figures/lowersouthbranch/lsb.at.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2022
ggsave(plot=lsb.contour.22,
       filename="figures/lowersouthbranch/lsb.contour.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.do.22,
       filename="figures/lowersouthbranch/lsb.do.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.precip.22,
       filename="figures/lowersouthbranch/lsb.precip.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.wind.22,
       filename="figures/lowersouthbranch/lsb.wind.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=lsb.at.22,
       filename="figures/lowersouthbranch/lsb.at.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#**COMBINE and SAVE####
#combine temp and do
lsb.temp.do.18 = ggarrange(lsb.contour.18, lsb.do.18, nrow=2, ncol=1, heights=c(7,3)); lsb.temp.do.18
lsb.temp.do.19 = ggarrange(lsb.contour.19, lsb.do.19, nrow=2, ncol=1, heights=c(7,3)); lsb.temp.do.19
lsb.temp.do.20 = ggarrange(lsb.contour.20, lsb.do.20, nrow=2, ncol=1, heights=c(7,3)); lsb.temp.do.20
lsb.temp.do.21 = ggarrange(lsb.contour.21, lsb.do.21, nrow=2, ncol=1, heights=c(7,3)); lsb.temp.do.21
lsb.temp.do.22 = ggarrange(lsb.contour.22, lsb.do.22, nrow=2, ncol=1, heights=c(7,3)); lsb.temp.do.22
#combine weather plots
lsb.weather.18 = ggarrange(lsb.precip.18, lsb.wind.18, lsb.at.18, nrow=3, ncol=1); lsb.weather.18
lsb.weather.19 = ggarrange(lsb.precip.19, lsb.wind.19, lsb.at.19, nrow=3, ncol=1); lsb.weather.19
lsb.weather.20 = ggarrange(lsb.precip.20, lsb.wind.20, lsb.at.20, nrow=3, ncol=1); lsb.weather.20
lsb.weather.21 = ggarrange(lsb.precip.21, lsb.wind.21, lsb.at.21, nrow=3, ncol=1); lsb.weather.21
lsb.weather.22 = ggarrange(lsb.precip.22, lsb.wind.22, lsb.at.22, nrow=3, ncol=1); lsb.weather.22

#save plots in list
lsb.list = list(lsb.temp.do.18, lsb.weather.18,
                lsb.temp.do.19, lsb.weather.19,
                lsb.temp.do.20, lsb.weather.20,
                lsb.temp.do.21, lsb.weather.21,
                lsb.temp.do.22, lsb.weather.22)

#save pdf of all combined plots
pdf("figures/Combined_plots_LSB.pdf")

for (i in 1:length(lsb.list)) {
  print(lsb.list[[i]])
}

dev.off() 






#* Ellis ########


#*** temp ####
#2018
el.contour.18 = ggplot(filter(dl, lake=='Ellis' & year==2018),
                        aes(x=jday, 
                            y=depth, 
                            z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  guides(fill=guide_legend(override.aes=list(size=0.5))) +
  labs(title='Ellis Pond Temperature 2018',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.contour.18

#2019
el.contour.19 = ggplot(filter(dl, lake=='Ellis' & year==2019),
                        aes(x=jday, 
                            y=depth, 
                            z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Ellis Pond Temperature 2019',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.contour.19

#2020
el.contour.20 = ggplot(filter(dl, lake=='Ellis' & year==2020),
                        aes(x=jday, 
                            y=depth, 
                            z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Ellis Pond Temperature 2020',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.contour.20

#2021
el.contour.21 = ggplot(filter(dl, lake=='Ellis' & year==2021),
                        aes(x=jday, 
                            y=depth, 
                            z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Ellis Pond Temperature 2021',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.contour.21

#2022
el.contour.22 = ggplot(filter(dl, lake=='Ellis' & year==2022),
                        aes(x=jday, 
                            y=depth, 
                            z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  geom_point(mapping=aes(x=jday, y=value, color=layer),
             shape=1, size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(12,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Ellis Pond Temperature 2022',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.contour.22


#*** weather #####
#2018

#precip
#2018
el.precip.18 = ggplot(filter(w,lake=='Ellis' & precip>0 & year==2018),
                       aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Ellis Pond Precipitation 2018',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.precip.18

#2019
el.precip.19 = ggplot(filter(w,lake=='Ellis' & precip>0 & year==2019),
                       aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Ellis Pond Precipitation 2019',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.precip.19


#2020
el.precip.20 = ggplot(filter(w,lake=='Ellis' & precip>0 & year==2020),
                       aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Ellis Pond Precipitation 2020',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.precip.20


#2021
el.precip.21 = ggplot(filter(w,lake=='Ellis' & precip>0 & year==2021),
                       aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Ellis Pond Precipitation 2021',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.precip.21


#2022
el.precip.22 = ggplot(filter(w,lake=='Ellis' & precip>0 & year==2022),
                       aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Ellis Pond Precipitation 2022',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.precip.22


#wind

#2018
el.wind.18 = ggplot(filter(w, year==2018),
                     aes(x=jday, y=wind.mean)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Ellis Pond Wind Speed 2018',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.wind.18

#2019
el.wind.19 = ggplot(filter(w, year==2019),
                     aes(x=jday, y=wind.mean)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Ellis Pond Wind Speed 2019',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.wind.19

#2020
el.wind.20 = ggplot(filter(w, year==2020),
                     aes(x=jday, y=wind.mean)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Ellis Pond Wind Speed 2020',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.wind.20


#2021
el.wind.21 = ggplot(filter(w, year==2021),
                     aes(x=jday, y=wind.mean)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Ellis Pond Wind Speed 2021',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.wind.21

#2022
el.wind.22 = ggplot(filter(w, year==2022),
                     aes(x=jday, y=wind.mean)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Ellis Pond Wind Speed 2022',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.wind.22




#temperature

#2018
el.at.18 = ggplot(filter(wl,lake=='Ellis' & year==2018),
                   aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Ellis Pond Air Temp 2018',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.at.18

#2019
el.at.19 = ggplot(filter(wl,lake=='Ellis' & year==2019),
                   aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Ellis Pond Air Temp 2019',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.at.19

#2020
el.at.20 = ggplot(filter(wl,lake=='Ellis' & year==2020),
                   aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Ellis Pond Air Temp 2020',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.at.20

#2021
el.at.21 = ggplot(filter(wl,lake=='Ellis' & year==2021),
                   aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Ellis Pond Air Temp 2021',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.at.21

#2022
el.at.22 = ggplot(filter(wl,lake=='Ellis' & year==2022),
                   aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Ellis Pond Air Temp 2022',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); el.at.22

#*** save ####
ggsave(plot=el.contour.18,
       filename="figures/ellis/el.contour.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.precip.18,
       filename="figures/ellis/el.precip.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.wind.18,
       filename="figures/ellis/el.wind.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.at.18,
       filename="figures/ellis/el.at.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)


#2019
ggsave(plot=el.contour.19,
       filename="figures/ellis/el.contour.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.precip.19,
       filename="figures/ellis/el.precip.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.wind.19,
       filename="figures/ellis/el.wind.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.at.19,
       filename="figures/ellis/el.at.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2020
ggsave(plot=el.contour.20,
       filename="figures/ellis/el.contour.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.precip.20,
       filename="figures/ellis/el.precip.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.wind.20,
       filename="figures/ellis/el.wind.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.at.20,
       filename="figures/ellis/el.at.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2021
ggsave(plot=el.contour.21,
       filename="figures/ellis/el.contour.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.precip.21,
       filename="figures/ellis/el.precip.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.wind.21,
       filename="figures/ellis/el.wind.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.at.21,
       filename="figures/ellis/el.at.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2022
ggsave(plot=el.contour.22,
       filename="figures/ellis/el.contour.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.precip.22,
       filename="figures/ellis/el.precip.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.wind.22,
       filename="figures/ellis/el.wind.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=el.at.22,
       filename="figures/ellis/el.at.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#**COMBINE and SAVE####
#combine weather plots
el.weather.18 = ggarrange(el.precip.18, el.wind.18, el.at.18, nrow=3, ncol=1); el.weather.18
el.weather.19 = ggarrange(el.precip.19, el.wind.19, el.at.19, nrow=3, ncol=1); el.weather.19
el.weather.20 = ggarrange(el.precip.20, el.wind.20, el.at.20, nrow=3, ncol=1); el.weather.20
el.weather.21 = ggarrange(el.precip.21, el.wind.21, el.at.21, nrow=3, ncol=1); el.weather.21
el.weather.22 = ggarrange(el.precip.22, el.wind.22, el.at.22, nrow=3, ncol=1); el.weather.22

#save plots in list
el.list = list(el.contour.18, el.weather.18,
               el.contour.19, el.weather.19,
               el.contour.20, el.weather.20,
               el.contour.21, el.weather.21,
               el.contour.22, el.weather.22)

#save pdf of all combined plots
pdf("figures/Combined_plots_Ellis.pdf")

for (i in 1:length(el.list)) {
  print(el.list[[i]])
}

dev.off() 





#* Wood ########


#*** temp ####
#2018
wd.contour.18 = ggplot(filter(dl, lake=='Wood' & year==2018),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(4,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  guides(fill=guide_legend(override.aes=list(size=0.5))) +
  labs(title='Wood Pond Temperature 2018',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.contour.18

#2019
wd.contour.19 = ggplot(filter(dl, lake=='Wood' & year==2019),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(4,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Wood Pond Temperature 2019',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.contour.19

#2020
wd.contour.20 = ggplot(filter(dl, lake=='Wood' & year==2020),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(4,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Wood Pond Temperature 2020',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.contour.20

#2021
wd.contour.21 = ggplot(filter(dl, lake=='Wood' & year==2021),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(4,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Wood Pond Temperature 2021',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.contour.21

#2022
wd.contour.22 = ggplot(filter(dl, lake=='Wood' & year==2022),
                       aes(x=jday, 
                           y=depth, 
                           z=temp.mean)) +
  geom_contour_filled(aes(fill=stat(level))) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_y_reverse(limits=c(4,0), 
                  n.breaks=4) +
  scale_color_manual(values=c('red3','gray30','gray15'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Wood Pond Temperature 2022',
       x="Date",
       y="Depth (m)",
       fill='Temp C') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.contour.22


#*** weather #####
#2018

#precip
#2018
wd.precip.18 = ggplot(filter(w,lake=='Bubble and Wood' & precip>0 & year==2018),
                        aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Wood Pond Precipitation 2018',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.precip.18

#2019
wd.precip.19 = ggplot(filter(w,lake=='Bubble and Wood' & precip>0 & year==2019),
                        aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Wood Pond Precipitation 2019',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.precip.19


#2020
wd.precip.20 = ggplot(filter(w,lake=='Bubble and Wood' & precip>0 & year==2020),
                        aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Wood Pond Precipitation 2020',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.precip.20


#2021
wd.precip.21 = ggplot(filter(w,lake=='Bubble and Wood' & precip>0 & year==2021),
                        aes(x=jday, y=precip)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Wood Pond Precipitation 2021',
       x="Date",
       y="Precipitation (mm)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.precip.21


#2022

#no 2022 precip data


#wind

#2018
wd.wind.18 = ggplot(filter(bbwdwind, year==2018),
                      aes(x=jday, y=wind)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Wood Pond Wind Speed 2018',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.wind.18

#2019
wd.wind.19 = ggplot(filter(bbwdwind, year==2019),
                      aes(x=jday, y=wind)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Wood Pond Wind Speed 2019',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.wind.19

#2020
wd.wind.20 = ggplot(filter(bbwdwind, year==2020),
                      aes(x=jday, y=wind)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Wood Pond Wind Speed 2020',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.wind.20


#2021
wd.wind.21 = ggplot(filter(bbwdwind, year==2021),
                      aes(x=jday, y=wind)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  labs(title='Wood Pond Wind Speed 2021',
       x="Date",
       y="Wind speed (m/s)") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.wind.21

#2022
#no wind data




#temperature

#2018
wd.at.18 = ggplot(filter(wl,lake=='Bubble and Wood' & year==2018),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Wood Pond Air Temp 2018',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.at.18

#2019
wd.at.19 = ggplot(filter(wl,lake=='Bubble and Wood' & year==2019),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Wood Pond Air Temp 2019',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.at.19

#2020
wd.at.20 = ggplot(filter(wl,lake=='Bubble and Wood' & year==2020),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Wood Pond Air Temp 2020',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.at.20

#2021
wd.at.21 = ggplot(filter(wl,lake=='Bubble and Wood' & year==2021),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Wood Pond Air Temp 2021',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.at.21

#2022
wd.at.22 = ggplot(filter(wl,lake=='Bubble and Wood' & year==2022),
                    aes(x=jday, y=temp.value, color=temp.var)) +
  geom_point(shape=1,size=1) +
  scale_x_continuous(limits=c(0,366),
                     breaks=c(60,182,305),
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%b")) +
  scale_color_manual(values=c('black','blue','red'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Wood Pond Air Temp 2022',
       x="Date",
       y="Temp C") +
  theme_bw()+
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.grid.minor=element_line(color='gray90'),
        panel.grid.major=element_line(color='gray90'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        title=element_text(size=10)); wd.at.22


#save
ggsave(plot=wd.contour.18,
       filename="figures/wood/wd.contour.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.precip.18,
       filename="figures/wood/wd.precip.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.wind.18,
       filename="figures/wood/wd.wind.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.at.18,
       filename="figures/wood/wd.at.18.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)


#2019
ggsave(plot=wd.contour.19,
       filename="figures/wood/wd.contour.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.precip.19,
       filename="figures/wood/wd.precip.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.wind.19,
       filename="figures/wood/wd.wind.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.at.19,
       filename="figures/wood/wd.at.19.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2020
ggsave(plot=wd.contour.20,
       filename="figures/wood/wd.contour.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.precip.20,
       filename="figures/wood/wd.precip.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.wind.20,
       filename="figures/wood/wd.wind.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.at.20,
       filename="figures/wood/wd.at.20.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2021
ggsave(plot=wd.contour.21,
       filename="figures/wood/wd.contour.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.precip.21,
       filename="figures/wood/wd.precip.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.wind.21,
       filename="figures/wood/wd.wind.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.at.21,
       filename="figures/wood/wd.at.21.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#2022
ggsave(plot=wd.contour.22,
       filename="figures/wood/wd.contour.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

ggsave(plot=wd.at.22,
       filename="figures/wood/wd.at.22.jpg",
       device="jpg",
       height=6, 
       width=6, 
       units="in", 
       dpi=400)

#**COMBINE and SAVE ########
#wd
wd.weather.18 = ggarrange(wd.precip.18, wd.wind.18, wd.at.18, nrow=3, ncol=1); wd.weather.18
wd.weather.19 = ggarrange(wd.precip.19, wd.wind.19, wd.at.19, nrow=3, ncol=1); wd.weather.19
wd.weather.20 = ggarrange(wd.precip.20, wd.wind.20, wd.at.20, nrow=3, ncol=1); wd.weather.20
wd.weather.21 = ggarrange(wd.precip.21, wd.wind.21, wd.at.21, nrow=3, ncol=1); wd.weather.21

#save in list
wd.list = list(wd.contour.18, wd.weather.18,
               wd.contour.19, wd.weather.19,
               wd.contour.20, wd.weather.20,
               wd.contour.21, wd.weather.21,
               wd.contour.22, wd.at.22)


#save pdf of all combined plots
pdf("figures/Combined_plots_Wood.pdf")

for (i in 1:length(wd.list)) {
  print(wd.list[[i]])
}

dev.off() 




# end