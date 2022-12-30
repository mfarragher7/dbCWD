#analyze and plot chl profiles after Lofton2020 
#2021-04-27



#libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(metR)
library(patchwork)
library(viridis)
library(reshape2)


#load profile stats
pro = read.csv('library/profiles.all.csv',header=T)
pro$Date = as.Date(pro$Date)
str(pro)
pro = pro %>% dplyr::select(lakedate,LakeID,Date,thermocline.depth.c3,z1)
#melt profiles
pmelt = melt(pro, 
              id.vars = c('lakedate','LakeID','Date'), 
              variable.name = 'var',
              value.name = 'val')

#load c3
c3 = read.csv('library/db.c3.csv',header=T)
#remove top 0.5m
c3 = c3 %>% filter(Depth > 0.5)

#test
ggplot(filter(c3,lakedate=='SC_2020-08-03'),
       aes(x=Chlorophyll_a,y=Depth)) +
  geom_point() +
  scale_y_reverse() +
  xlim(50,300)

ggplot(filter(c3,lakedate=='SC_2020-08-03'),
       aes(y=Chlorophyll_a,x=Depth)) +
  geom_point() +
  stat_smooth(method='loess') +
  scale_x_reverse() +
  coord_flip() +
  ylim(50,300)

# make contours
#get means for each 0.5m segment
c3$depth.bin = plyr::round_any(c3$Depth, 0.5, f=floor)

c3.bin = ddply(c3, .(lakedate,depth.bin,LakeID,Date,seasonID), summarize, 
               t.bin = mean(Temp),
               chl.bin = mean(Chlorophyll_a))
         


      
#Contour plots ####
#Jordan 
jp = ggplot(filter(c3.bin,LakeID=='JP'),
            aes(x=as.Date(Date),
                y=depth.bin)) +
  #add contours
  geom_contour_fill(na.fill=T,
                    mapping=aes(z=chl.bin)) +
  #add segments
  geom_segment(linetype=5,         
               color='black',
               alpha=0.01,
               mapping=aes(x=as.Date(Date),
                           xend=as.Date(Date),
                           y=0.5,
                           yend=Inf)) +
  #add profile vars
  geom_point(data=filter(pmelt,LakeID=='JP'),
             mapping=aes(x=as.Date(Date),y=val,
             shape=var,
             color=var,
             size=2)) +
  #scales
  scale_x_date(limits=as.Date(c('2020-02-20','2020-11-10')),
               date_breaks="1 month",date_labels="%b") +
  scale_y_reverse(limits=c(27,0),
                  n.breaks=6) +
  scale_fill_gradient2(low='navy',
                       mid='aquamarine',
                       high='orangered',
                       midpoint=500,
                       limits=c(0,1000)) + 
  scale_color_manual(values=c('goldenrod','green'),
                     labels=c(bquote(Z['mix']), bquote(Z['1%PAR'])),
                     guide=guide_legend('Vertical gradients')) + 
  scale_shape_manual(values = c(2,6),
                     labels=c(bquote(Z['mix']), bquote(Z['1%PAR'])),
                     guide=guide_legend('Vertical gradients')) + 
  scale_size(guide='none') +
  labs(title='Jordan Pond', 
       y='Depth (m)',  
       x=NULL,
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        legend.text=element_text(size=rel(1))); jp



  
  
  

#Seal cove
sc = ggplot(filter(c3.bin,LakeID=='SC'),
              aes(x=as.Date(Date),
                  y=depth.bin)) +
  geom_contour_fill(na.fill=T,
                      mapping=aes(z=chl.bin)) +
  #add segments
  geom_segment(linetype=5,         
               color='black',
               alpha=0.01,
               mapping=aes(x=as.Date(Date),
                           xend=as.Date(Date),
                           y=0.5,
                           yend=Inf)) +
  #add profile vars
  geom_point(data=filter(pmelt,LakeID=='SC'),
             mapping=aes(x=as.Date(Date),y=val,
                         shape=var,
                         color=var,
                         size=2)) +
  scale_x_date(limits=as.Date(c('2020-02-20','2020-11-10')),
               date_breaks="1 month",date_labels="%b") +
  scale_y_reverse(limits=c(13,0),
                  n.breaks=7) +
  scale_fill_gradient2(low='navy',
                       mid='aquamarine',
                       high='orangered',
                       midpoint=500,
                       limits=c(0,1000)) + 
  scale_color_manual(values=c('goldenrod','green'),
                     labels=c(bquote(Z['mix']), bquote(Z['1%PAR'])),
                     guide=guide_legend('Vertical gradients')) + 
  scale_shape_manual(values = c(2,6),
                     labels=c(bquote(Z['mix']), bquote(Z['1%PAR'])),
                     guide=guide_legend('Vertical gradients')) + 
  scale_size(guide='none') +
  labs(title='Seal Cove Pond', 
       y=NULL,  
       x=NULL,
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        legend.text=element_text(size=rel(1))); sc



#Bubble
bb = ggplot(filter(c3.bin,LakeID=='BB'),
            aes(x=as.Date(Date),
                y=depth.bin)) +
  geom_contour_fill(na.fill=T,
                    mapping=aes(z=chl.bin)) +
  #add segments
  geom_segment(linetype=5,         
               color='black',
               alpha=0.01,
               mapping=aes(x=as.Date(Date),
                           xend=as.Date(Date),
                           y=0.5,
                           yend=Inf)) +
  #add profile vars
  geom_point(data=filter(pmelt,LakeID=='BB'),
             mapping=aes(x=as.Date(Date),y=val,
                         shape=var,
                         color=var,
                         size=2)) +
  scale_x_date(limits=as.Date(c('2020-02-20','2020-11-10')),
               date_breaks="1 month",date_labels="%b") +
  scale_y_reverse(limits=c(11,0),
                  n.breaks=8) +
  scale_fill_gradient2(low='navy',
                       mid='aquamarine',
                       high='orangered',
                       midpoint=500,
                       limits=c(0,1000)) + 
  scale_color_manual(values=c('goldenrod','green'),
                     labels=c(bquote(Z['mix']), bquote(Z['1%PAR'])),
                     guide=guide_legend('Vertical gradients')) + 
  scale_shape_manual(values = c(2,6),
                     labels=c(bquote(Z['mix']), bquote(Z['1%PAR'])),
                     guide=guide_legend('Vertical gradients')) + 
  scale_size(guide='none') +
  labs(title='Bubble Pond', 
       y='Depth (m)',  
       x='Date',
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        legend.text=element_text(size=rel(1))); bb




#Witch hole
wh = ggplot(filter(c3.bin,LakeID=='WH'),
            aes(x=as.Date(Date),
                y=depth.bin)) +
  geom_contour_fill(na.fill=T,
                    mapping=aes(z=chl.bin)) +
  #add segments
  geom_segment(linetype=5,         
               color='black',
               alpha=0.01,
               mapping=aes(x=as.Date(Date),
                           xend=as.Date(Date),
                           y=0.5,
                           yend=Inf)) +
  #add profile vars
  geom_point(data=filter(pmelt,LakeID=='WH'),
             mapping=aes(x=as.Date(Date),y=val,
                         shape=var,
                         color=var,
                         size=2)) +
  scale_x_date(limits=as.Date(c('2020-02-20','2020-11-10')),
               date_breaks="1 month",date_labels="%b") +
  scale_y_reverse(limits=c(8.5,0),
                  n.breaks=7) +
  scale_fill_gradient2(low='navy',
                       mid='aquamarine',
                       high='orangered',
                       midpoint=500,
                       limits=c(0,1000)) + 
  scale_color_manual(values=c('goldenrod','green'),
                     labels=c(bquote(Z['mix']), bquote(Z['1%PAR'])),
                     guide=guide_legend('Vertical gradients')) + 
  scale_shape_manual(values = c(2,6),
                     labels=c(bquote(Z['mix']), bquote(Z['1%PAR'])),
                     guide=guide_legend('Vertical gradients')) + 
  scale_size(guide='none') +
  labs(title='Witch Hole Pond',
       y=NULL,
       x='Date',
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        legend.text=element_text(size=rel(1))); wh




#combine and save
comb = wrap_plots(jp, 
                  sc, 
                  bb, 
                  wh,
                  ncol=2,
                  nrow=2,
                  guides='collect'); comb

#save
ggsave(plot=comb, 
       filename="plots/formatted/dcm.jpg",
       device="jpg",
       height=6, 
       width=11, 
       units="in", 
       dpi=600)











#Contour plots no VHGs ####
#Jordan 
jp = ggplot(filter(c3.bin,LakeID=='JP'),
            aes(x=as.Date(Date),
                y=depth.bin)) +
  #add contours
  geom_contour_fill(na.fill=T,
                    mapping=aes(z=chl.bin)) +
  #add segments
  geom_segment(linetype=5,         
               color='black',
               alpha=0.01,
               mapping=aes(x=as.Date(Date),
                           xend=as.Date(Date),
                           y=0.5,
                           yend=Inf)) +
  #scales
  scale_x_date(limits=as.Date(c('2020-02-20','2020-11-10')),
               date_breaks="1 month",date_labels="%b") +
  scale_y_reverse(limits=c(27,0),
                  n.breaks=6) +
  scale_fill_gradient2(low='navy',
                       mid='aquamarine',
                       high='orangered',
                       midpoint=500,
                       limits=c(0,1000)) + 
  scale_size(guide='none') +
  labs(title='Jordan Pond', 
       y='Depth (m)',  
       x=NULL,
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        legend.text=element_text(size=rel(1))); jp



#Seal cove
sc = ggplot(filter(c3.bin,LakeID=='SC'),
            aes(x=as.Date(Date),
                y=depth.bin)) +
  geom_contour_fill(na.fill=T,
                    mapping=aes(z=chl.bin)) +
  #add segments
  geom_segment(linetype=5,         
               color='black',
               alpha=0.01,
               mapping=aes(x=as.Date(Date),
                           xend=as.Date(Date),
                           y=0.5,
                           yend=Inf)) +
  scale_x_date(limits=as.Date(c('2020-02-20','2020-11-10')),
               date_breaks="1 month",date_labels="%b") +
  scale_y_reverse(limits=c(13,0),
                  n.breaks=7) +
  scale_fill_gradient2(low='navy',
                       mid='aquamarine',
                       high='orangered',
                       midpoint=500,
                       limits=c(0,1000)) + 
  scale_size(guide='none') +
  labs(title='Seal Cove Pond', 
       y=NULL,  
       x=NULL,
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        legend.text=element_text(size=rel(1))); sc



#Bubble
bb = ggplot(filter(c3.bin,LakeID=='BB'),
            aes(x=as.Date(Date),
                y=depth.bin)) +
  geom_contour_fill(na.fill=T,
                    mapping=aes(z=chl.bin)) +
  #add segments
  geom_segment(linetype=5,         
               color='black',
               alpha=0.01,
               mapping=aes(x=as.Date(Date),
                           xend=as.Date(Date),
                           y=0.5,
                           yend=Inf)) +
  scale_x_date(limits=as.Date(c('2020-02-20','2020-11-10')),
               date_breaks="1 month",date_labels="%b") +
  scale_y_reverse(limits=c(11,0),
                  n.breaks=8) +
  scale_fill_gradient2(low='navy',
                       mid='aquamarine',
                       high='orangered',
                       midpoint=500,
                       limits=c(0,1000)) + 
  scale_size(guide='none') +
  labs(title='Bubble Pond', 
       y='Depth (m)',  
       x='Date',
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        legend.text=element_text(size=rel(1))); bb




#Witch hole
wh = ggplot(filter(c3.bin,LakeID=='WH'),
            aes(x=as.Date(Date),
                y=depth.bin)) +
  geom_contour_fill(na.fill=T,
                    mapping=aes(z=chl.bin)) +
  #add segments
  geom_segment(linetype=5,         
               color='black',
               alpha=0.01,
               mapping=aes(x=as.Date(Date),
                           xend=as.Date(Date),
                           y=0.5,
                           yend=Inf)) +
  scale_x_date(limits=as.Date(c('2020-02-20','2020-11-10')),
               date_breaks="1 month",date_labels="%b") +
  scale_y_reverse(limits=c(8.5,0),
                  n.breaks=7) +
  scale_fill_gradient2(low='navy',
                       mid='aquamarine',
                       high='orangered',
                       midpoint=500,
                       limits=c(0,1000)) + 
  scale_size(guide='none') +
  labs(title='Witch Hole Pond',
       y=NULL,
       x='Date',
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        legend.text=element_text(size=rel(1))); wh




#combine and save
comb = wrap_plots(jp, 
                  sc, 
                  bb, 
                  wh,
                  ncol=2,
                  nrow=2,
                  guides='collect'); comb

#save
ggsave(plot=comb, 
       filename="plots/formatted/dcm.no.vhg.jpg",
       device="jpg",
       height=6, 
       width=11, 
       units="in", 
       dpi=600)




























#OLD SHIT ######
#add mixing depth
#geom_point(filter(pro,LakeID=='JP'), 
#          mapping=aes(x=as.Date(Date),
#                     y=thermocline.depth.c3)) +

#add depth 1%PAR
#geom_point(filter(pro,LakeID=='JP'), 
#          mapping=aes(x=as.Date(Date),
#                     y=z1)) +

#add DCM method 1
#geom_point(filter(pro,LakeID=='JP'), 
#          mapping=aes(x=as.Date(Date),
#                     y=fchl.max.depth),
#        color='RED') +

#add DCM method 2
#geom_point(filter(pro,LakeID=='JP'), 
#          mapping=aes(x=as.Date(Date),
#                     y=dcm.depth.curvefit),
#        color='green') +













#same but viridis

 # scale_fill_viridis(option='mako',limits=c(0,1000)) + 



#combine and save
#comb = wrap_plots(jp, sc, bb, wh,
                  ncol=2,nrow=2,
                  guides='collect'); comb

#save
#ggsave(plot=comb, 
 #      filename="plots/dcm/dcm.contour.mako.jpg",
  #     device="jpg",
   #    height=8, 
    #   width=14, 
     #  units="in", 
      # dpi=600)
















# TILE PLOTS #####

#get means from each 0.5m
c3$depth.bin = plyr::round_any(c3$Depth, 0.5, f=floor)
c3.bin = ddply(c3, .(lakedate,depth.bin,LakeID,Date,seasonID), 
               summarize, chl.bin = mean(Chlorophyll_a))


#JP
jp = ggplot(filter(c3.bin,LakeID=='JP')) +
  geom_raster(aes(x=as.Date(Date),
                  y=depth.bin,
                  fill=chl.bin)) +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_reverse(limits=c(30,0),
                  n.breaks=6) +
  scale_fill_viridis(limits=c(0,1000),
                     option='viridis') +
  labs(title='Jordan Pond', 
       y='Depth (m)',  
       x=NULL,
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme(plot.title=element_text(size=rel(1))); jp

#SC
sc = ggplot(filter(c3.bin,LakeID=='SC')) +
  geom_raster(aes(x=as.Date(Date),
                  y=depth.bin,
                  fill=chl.bin)) +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_reverse(limits=c(13,0),
                  n.breaks=7) +
  scale_fill_viridis(limits=c(0,1000),
                     option='viridis') +
  labs(title='Seal Cove Pond', 
       y=NULL,  
       x=NULL,
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme(plot.title=element_text(size=rel(1))); sc

#BB
bb = ggplot(filter(c3.bin,LakeID=='BB')) +
  geom_raster(aes(x=as.Date(Date),
                  y=depth.bin,
                  fill=chl.bin)) +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_reverse(limits=c(11,0),
                  n.breaks=7) +
  scale_fill_viridis(limits=c(0,1000),
                     option='viridis') +
  labs(title='Bubble Pond', 
       y='Depth (m)',  
       x='Date',
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme(plot.title=element_text(size=rel(1))); bb
  
  

wh = ggplot(filter(c3.bin,LakeID=='WH')) +
  geom_raster(aes(x=as.Date(Date),
                  y=depth.bin,
                  fill=chl.bin)) +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_reverse(limits=c(10,0),
                  n.breaks=7) +
  scale_fill_viridis(limits=c(0,1000),
                     option='viridis') +
  labs(title='Witch Hole Pond',
       y=NULL,
       x="Date",
       fill=bquote("Chl"~italic("a")~"(RFU)")) +
  theme(plot.title=element_text(size=rel(1))); wh


#combine and save
comb = wrap_plots(jp,sc,bb,wh,nrow=2,ncol=2,guides='collect'); comb

#save
ggsave(plot=comb, 
       filename="plots/dcm/dcm.tiles.jpg",
       device="jpg",
       height=8, 
       width=12, 
       units="in", 
       dpi=600)












#HABS version ####

#load profiles
c3 = read.csv('library/db.c3.csv',header=T)
#remove top 0.5m
c3 = c3 %>% filter(Depth > 0.5)



#get means from each 0.5m
c3$depth.bin = plyr::round_any(c3$Depth, 0.5, f=floor)
c3.bin = ddply(c3, .(lakedate,depth.bin,LakeID,Date,seasonID), 
               summarize, phy.bin = mean(Phycocyanin))





#JP
jp = ggplot(filter(c3.bin,LakeID=='JP')) +
  geom_raster(aes(x=as.Date(Date),
                  y=depth.bin,
                  fill=phy.bin)) +
  geom_segment(linetype=5,
               color='black',
               alpha=0.01,
               mapping=aes(x=as.Date(Date),
                           xend=as.Date(Date),
                           y=0,
                           yend=Inf)) +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_reverse(limits=c(30,0),
                  n.breaks=6) +
  scale_fill_viridis(limits=c(0,20),
                     option='viridis') +
  labs(title='Jordan Pond', 
       y='Depth (m)',  
       x=NULL,
       fill='Phyco RFU') +
  theme(plot.title=element_text(size=rel(1))); jp

#SC
sc = ggplot(filter(c3.bin,LakeID=='SC')) +
  geom_raster(aes(x=as.Date(Date),
                  y=depth.bin,
                  fill=phy.bin)) +
  geom_segment(linetype=5,
               color='black',
               alpha=0.01,
               mapping=aes(x=as.Date(Date),
                           xend=as.Date(Date),
                           y=0,
                           yend=Inf)) +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_reverse(limits=c(13,0),
                  n.breaks=7) +
  scale_fill_viridis(limits=c(0,20),
                     option='viridis') +
  labs(title='Seal Cove Pond', 
       y=NULL,  
       x=NULL,
       fill='Phyco RFU') +
  theme(plot.title=element_text(size=rel(1))); sc


#WH
wh = ggplot(filter(c3.bin,LakeID=='WH')) +
  geom_raster(aes(x=as.Date(Date),
                  y=depth.bin,
                  fill=phy.bin)) +
  geom_segment(linetype=5,
               color='black',
               alpha=0.01,
               mapping=aes(x=as.Date(Date),
                           xend=as.Date(Date),
                           y=0,
                           yend=Inf)) +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_reverse(limits=c(10,0),
                  n.breaks=7) +
  scale_fill_viridis(limits=c(0,20),
                     option='viridis') +
  labs(title='Witch Hole Pond',
       y=NULL,
       x="Date",
       fill='Phyco RFU') +
  theme(plot.title=element_text(size=rel(1))); wh




#combine and save
comb = wrap_plots(jp,sc,wh,nrow=2,ncol=2,guides='collect'); comb

#save
ggsave(plot=comb, 
       filename="HABs/figures/dpm.tiles2.jpg",
       device="jpg",
       height=8, 
       width=12, 
       units="in", 
       dpi=600)


















# exploratory plots ####

# compare dcm depths #####

ggplot(pro,aes(x=Date,y=fchl_max_depth,color=LakeID)) +
  geom_point(shape=1,size=2) +
  geom_point(aes(x=Date,y=dcm.depth.curvefit),shape=2,size=2)






#overview plots


#pro curvefit depth vs dcm depth manual
ggplot(pro, aes(x=dcm.depth.curvefit,y=dcm.depth.manual)) +
  geom_point()

#use dcm.depth.curvefit and peak widths and/or breadth top/bottom




#JP
jp = 
  ggplot(filter(pro,LakeID=='JP'),
         aes(x=Date,y=dcm.depth.curvefit)) +
  geom_point(shape=19,size=3) +
  geom_line(color='gray30',
            linetype=4,
            inherit.aes=T) +
  geom_errorbar(aes(ymin=dcm.breadth.top,
                    ymax=dcm.breadth.bottom),
                width=5) +
  scale_x_date(limits=c(as.Date('2020-02-01'),
                        as.Date('2020-11-06')),
               breaks='2 months',
               date_labels="%d-%b") +
  scale_y_reverse(limits=c(30,0),n.breaks=6) +
  labs(title="Jordan Pond"); jp


sc = 
  ggplot(filter(pro,LakeID=='SC'),
         aes(x=Date,y=dcm.depth.curvefit)) +
  geom_point(shape=19,size=3) +
  geom_line(color='gray30',
            linetype=4,
            inherit.aes=T) +
  geom_errorbar(aes(ymin=dcm.breadth.top,
                    ymax=dcm.breadth.bottom),
                width=5) +
  scale_x_date(limits=c(as.Date('2020-02-01'),
                        as.Date('2020-11-06')),
               breaks='2 months',
               date_labels="%d-%b") +
  scale_y_reverse(n.breaks=7) +
  labs(title="Seal Cove Pond"); sc

bb = 
  ggplot(filter(pro,LakeID=='BB'),
         aes(x=Date,y=dcm.depth.curvefit)) +
  geom_point(shape=19,size=3) +
  geom_line(color='gray30',
            linetype=4,
            inherit.aes=T) +
  geom_errorbar(aes(ymin=dcm.breadth.top,
                    ymax=dcm.breadth.bottom),
                width=5) +
  scale_x_date(limits=c(as.Date('2020-02-01'),
                        as.Date('2020-11-06')),
               breaks='2 months',
               date_labels="%d-%b") +
  scale_y_reverse(limits=c(11,0),n.breaks=7) +
  labs(title="Bubble Pond"); bb

wh = 
  ggplot(filter(pro,LakeID=='WH'),
         aes(x=Date,y=dcm.depth.curvefit)) +
  geom_point(shape=19,size=3) +
  geom_line(color='gray30',
            linetype=4,
            inherit.aes=T) +
  geom_errorbar(aes(ymin=dcm.breadth.top,
                    ymax=dcm.breadth.bottom),
                width=5) +
  scale_x_date(limits=c(as.Date('2020-02-01'),
                        as.Date('2020-11-06')),
               breaks='2 months',
               date_labels="%d-%b") +
  scale_y_reverse() +
  labs(title="Witch Hole Pond"); wh


#save
comb = wrap_plots(jp,sc,bb,wh,nrow=2,ncol=2); comb

ggsave(plot=comb,
       filename="plots/dcm/dcm.QL.jpg",
       device="jpg",
       height=8, 
       width=12, 
       units="in", 
       dpi=600)







# More explotatory plots ####
ggplot(pro, aes(x=Date, y=dcm.depth.curvefit)) +
  geom_point() +
  facet_wrap(~LakeID, nrow=2, ncol=2) +
  scale_x_date(limits=c(as.Date('2020-02-01'),
                        as.Date('2020-11-06')),
               breaks='2 months',
               date_labels="%d-%b")




ggplot(pro, aes(x=Date, y=dcm.r2.fit)) +
  geom_point() +
  facet_wrap(~LakeID, nrow=2, ncol=2) +
                        as.Date('2020-11-06')),
               breaks='2 months',


ggplot(pro, aes(x=Date, y=peak.width)) +
  geom_point() +
  facet_wrap(~LakeID, nrow=2, ncol=2) +
  scale_x_date(limits=c(as.Date('2020-02-01'),
                        as.Date('2020-11-06')),
               breaks='2 months',
               date_labels="%d-%b")


ggplot(pro, aes(x=Date, y=stand.peak.width)) +
  geom_point() +
  facet_wrap(~LakeID, nrow=2, ncol=2) +
  scale_x_date(limits=c(as.Date('2020-02-01'),
                        as.Date('2020-11-06')),
               breaks='2 months',
               date_labels="%d-%b")








# DCM : Z 1%PAR

#set colors
pro$LakeID = factor(pro$LakeID,levels=c("JP","BB","SC","WH"))
#4 colors pairs
color.code = c('#80CDC1','#01665E','#BF812D','#543005')

ggplot(pro,aes(x=dcm.depth.curvefit,y=z1,color=LakeID)) +
  geom_point(shape=19,size=3) +
  scale_x_continuous(limits=c(0,21),n.breaks=5) +
  scale_y_continuous(limits=c(0,21),n.breaks=5) +
  scale_color_manual(values=color.code) +
  geom_segment(mapping=aes(x=0,xend=21,y=0,yend=21),linetype=2,color="gray20") +
  labs(title='DCM : 1%PAR Lofton2020',color="Lake",x="DCM",y="z 1% PAR") 

#compare methods
ggplot(pro,aes(x=fchl_max_depth,y=z1,color=LakeID)) +
  geom_point(shape=19,size=3) +
  scale_x_continuous(limits=c(0,21),n.breaks=5) +
  scale_y_continuous(limits=c(0,21),n.breaks=5) +
  scale_color_manual(values=color.code) +
  geom_segment(mapping=aes(x=0,xend=21,y=0,yend=21),linetype=2,color="gray20") +
  labs(title='DCM : 1%PAR',color="Lake",x="DCM",y="z 1% PAR") 










#add column
pro$ratio.dcm.z1 = pro$dcm.depth.curvefit / pro$z1

ggplot(pro,aes(x=as.Date(Date),y=ratio.dcm.z1,color=LakeID)) +
  geom_point(size=3) +
  geom_line(show.legend=F) +
  geom_segment(color="gray20",
               linetype=2,
               mapping=aes(x=as.Date("2020-02-15"),
                           xend=as.Date("2020-11-12"),
                           y=1,yend=1)) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-12')),
               date_breaks="1 month",
               date_labels="%d-%b") +
  scale_color_manual(values=color.code)
  
#my method
ggplot(pro,aes(x=as.Date(Date),y=(fchl_max_depth/z1),color=LakeID)) +
  geom_point(size=3) +
  geom_line(show.legend=F) +
  geom_segment(color="gray20",
               linetype=2,
               mapping=aes(x=as.Date("2020-02-15"),
                           xend=as.Date("2020-11-12"),
                           y=1,yend=1)) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-12')),
               date_breaks="1 month",
               date_labels="%d-%b") +
  scale_color_manual(values=color.code)




#test
ggplot(filter(c3,lakedate=='BB_2020-06-13'),
       aes(x=Chlorophyll_a,y=Depth)) +
  geom_point() +
  scale_y_reverse() +
  xlim(0,300)







ggplot(pro,aes(x=as.Date(Date),y=dcm.shape,color=LakeID)) +
  
  geom_point(size=3) +
  
  geom_line(show.legend=F) +
  

  scale_color_manual(values=color.code) +

  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-12')),
               date_breaks="1 month",date_labels="%d-%b")
  

#test
ggplot(filter(c3,lakedate=='WH_2020-02-25'),
       aes(x=Chlorophyll_a,y=Depth)) +
  geom_point() +
  scale_y_reverse() +
  xlim(0,400)



c3$Date = as.Date(c3$Date)


#boxplots
ggplot(filter(c3,LakeID=='JP'),aes(x=Date,y=Chlorophyll_a,group=Date)) +
  geom_boxplot() +
  geom_jitter(width=0.2) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-12')),date_breaks="1 month",date_labels="%d-%b")





#add color fill by merging c3 with profiles df to get depth_max_chl
dcm = pro %>% select(lakedate,dcm.depth.curvefit,fchl_max_depth)
c3 = join(c3,dcm,by='lakedate')



#violin plots ####
#JP
jp=ggplot(filter(c3,LakeID=='JP'),aes(x=Date,y=Chlorophyll_a,group=Date)) +
  geom_violin(aes(fill=fchl_max_depth),trim=T) +
  scale_fill_gradient2(limits=c(0,27),
                       breaks=c(0,5,10,15,20,25),
                       midpoint=15,
                       high='navy',
                       mid='turquoise',
                       low='gold') +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_continuous(limits=c(0,1000)) +
  guides(fill=guide_colorbar(reverse=T)) +
  labs(title='Jordan Pond',x=NULL,y='Chl a (RFU)',fill='DCM'); jp

#SC
sc=ggplot(filter(c3,LakeID=='SC'),aes(x=Date,y=Chlorophyll_a,group=Date)) +
  geom_violin(aes(fill=fchl_max_depth),trim=T) +
  scale_fill_gradient2(limits=c(0,13),
                       breaks=c(0,3,6,9,12),
                       midpoint=7,
                       high='navy',
                       mid='turquoise',
                       low='gold') +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_continuous(limits=c(0,1000)) +
  guides(fill=guide_colorbar(reverse=T)) +
  labs(title='Seal Cove Pond',x=NULL,y=NULL,fill='DCM'); sc

#BB
bb=ggplot(filter(c3,LakeID=='BB'),aes(x=Date,y=Chlorophyll_a,group=Date)) +
  geom_violin(aes(fill=fchl_max_depth),trim=T) +
  scale_fill_gradient2(limits=c(0,11),
                       breaks=c(0,2,4,6,8,10),
                       midpoint=6,
                       high='navy',
                       mid='turquoise',
                       low='gold') +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_continuous(limits=c(0,1000)) +
  guides(fill=guide_colorbar(reverse=T)) +
  labs(title='Bubble Pond',x='Date',y='Chl a (RFU)',fill='DCM'); bb


#WH
wh=ggplot(filter(c3,LakeID=='WH'),aes(x=Date,y=Chlorophyll_a,group=Date)) +
  geom_violin(aes(fill=fchl_max_depth),trim=T) +
  scale_fill_gradient2(limits=c(0,10),
                       breaks=c(0,2,4,6,8,10),
                       midpoint=5,
                       high='navy',
                       mid='turquoise',
                       low='gold') +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_continuous(limits=c(0,1000)) +
  guides(fill=guide_colorbar(reverse=T)) +
  labs(title='Witch Hole Pond',x='Date',y=NULL,fill='DCM'); wh




comb = wrap_plots(jp,sc,bb,wh,nrow=2,ncol=2); comb




ggsave(
  filename="plots/dcm/dcm.violins.jpg",
  plot=comb, 
  device="jpg",
  height=8, 
  width=14, 
  units="in", 
  dpi=400)





#Lofton Method ####

#violin plots
#JP
jp=ggplot(filter(c3,LakeID=='JP'),aes(x=Date,y=Chlorophyll_a,group=Date)) +
  geom_violin(aes(fill=dcm.depth.curvefit),trim=T) +
  scale_fill_gradient2(limits=c(0,27),
                       breaks=c(0,5,10,15,20,25),
                       midpoint=15,
                       high='navy',
                       mid='turquoise',
                       low='gold') +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_continuous(limits=c(0,1000)) +
  guides(fill=guide_colorbar(reverse=T)) +
  labs(title='Jordan Pond',x=NULL,y='Chl a (RFU)',fill='DCM'); jp

#SC
sc=ggplot(filter(c3,LakeID=='SC'),aes(x=Date,y=Chlorophyll_a,group=Date)) +
  geom_violin(aes(fill=dcm.depth.curvefit),trim=T) +
  scale_fill_gradient2(limits=c(0,13),
                       breaks=c(0,3,6,9,12),
                       midpoint=7,
                       high='navy',
                       mid='turquoise',
                       low='gold') +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_continuous(limits=c(0,1000)) +
  guides(fill=guide_colorbar(reverse=T)) +
  labs(title='Seal Cove Pond',x=NULL,y=NULL,fill='DCM'); sc

#BB
bb=ggplot(filter(c3,LakeID=='BB'),aes(x=Date,y=Chlorophyll_a,group=Date)) +
  geom_violin(aes(fill=dcm.depth.curvefit),trim=T) +
  scale_fill_gradient2(limits=c(0,11),
                       breaks=c(0,2,4,6,8,10),
                       midpoint=6,
                       high='navy',
                       mid='turquoise',
                       low='gold') +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_continuous(limits=c(0,1000)) +
  guides(fill=guide_colorbar(reverse=T)) +
  labs(title='Bubble Pond',x='Date',y='Chl a (RFU)',fill='DCM'); bb


#WH
wh=ggplot(filter(c3,LakeID=='WH'),aes(x=Date,y=Chlorophyll_a,group=Date)) +
  geom_violin(aes(fill=dcm.depth.curvefit),trim=T) +
  scale_fill_gradient2(limits=c(0,10),
                       breaks=c(0,2,4,6,8,10),
                       midpoint=5,
                       high='navy',
                       mid='turquoise',
                       low='gold') +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_continuous(limits=c(0,1000)) +
  guides(fill=guide_colorbar(reverse=T)) +
  labs(title='Witch Hole Pond',x='Date',y=NULL,fill='DCM'); wh







comb = wrap_plots(jp,sc,bb,wh,nrow=2,ncol=2); comb

ggsave(
  filename="plots/dcm/dcm.violins.lofton2020.jpg",
  plot=comb, 
  device="jpg",
  height=8, 
  width=14, 
  units="in", 
  dpi=400)












#HABs ####

comb = wrap_plots(jp,sc,wh,nrow=3,ncol=1); comb




ggsave(
  filename="HABs/figures/dcm.chl.violins.lofton.jpg",
  plot=comb, 
  device="jpg",
  height=10, 
  width=8, 
  units="in", 
  dpi=400)







pro = read.csv('HABs/library/profiles_c3loess.csv',header=T)
phy = read.csv('HABs/library/profiles_phy_dcm.csv',header=T)

c3 = read.csv('library/db.c3.csv',header=T)
#remove top 0.5m
c3 = c3 %>% filter(Depth > 0.5)

# merge c3 with phy profiles df to get depth_max_chl
phy = phy %>% select(lakedate,DCM.depth.curvefit)
#remember DCM is really for phyco here
pro = pro %>% select(lakedate,phyco_max_depth)



c3 = join(c3,phy,by='lakedate')

c3 = join(c3,pro,by='lakedate')





#violin plots
#JP
jp=ggplot(filter(c3,LakeID=='JP'),aes(x=as.Date(Date),y=Phycocyanin,group=Date)) +
  geom_violin(aes(fill=phyco_max_depth),trim=T) +
  scale_fill_gradient2(limits=c(0,27),
                       breaks=c(0,5,10,15,20,25),
                       midpoint=15,
                       high='navy',
                       mid='turquoise',
                       low='gold') +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_continuous(limits=c(0,50)) +
  guides(fill=guide_colorbar(reverse=T)) +
  labs(title='Jordan Pond',x=NULL,y='Phycocyanin (RFU)',fill='CPM'); jp

#SC
sc=ggplot(filter(c3,LakeID=='SC'),aes(x=as.Date(Date),y=Phycocyanin,group=Date)) +
  geom_violin(aes(fill=phyco_max_depth),trim=T) +
  scale_fill_gradient2(limits=c(0,13),
                       breaks=c(0,3,6,9,12),
                       midpoint=7,
                       high='navy',
                       mid='turquoise',
                       low='gold') +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_continuous(limits=c(0,50)) +
  guides(fill=guide_colorbar(reverse=T)) +
  labs(title='Seal Cove Pond',x=NULL,y=NULL,fill='CPM'); sc


#WH
wh=ggplot(filter(c3,LakeID=='WH'),aes(x=as.Date(Date),y=Phycocyanin,group=Date)) +
  geom_violin(aes(fill=phyco_max_depth),trim=T) +
  scale_fill_gradient2(limits=c(0,10),
                       breaks=c(0,2,4,6,8,10),
                       midpoint=5,
                       high='navy',
                       mid='turquoise',
                       low='gold') +
  scale_x_date(limits=as.Date(c('2020-02-10','2020-11-16')),
               date_breaks="1 month",date_labels="%d-%b") +
  scale_y_continuous(limits=c(0,50)) +
  guides(fill=guide_colorbar(reverse=T)) +
  labs(title='Witch Hole Pond',x='Date',y=NULL,fill='CPM'); wh



comb = wrap_plots(jp,sc,wh,nrow=3,ncol=1); comb




ggsave(
  filename="HABs/figures/dcm.phy.violins.lofton2020.jpg",
  plot=comb, 
  device="jpg",
  height=10, 
  width=8, 
  units="in", 
  dpi=400)

































