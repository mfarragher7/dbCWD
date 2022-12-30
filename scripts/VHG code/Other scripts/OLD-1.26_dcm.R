#analyze and plot chl profiles after Lofton2020 
#2021-02-15

#libraries
library(dplyr)
library(ggplot2)


#load profile stats
profile.summary = read.csv('library/profiles_c3loess.csv',header=T)
profile.summary$Date = as.Date(profile.summary$Date)
str(profile.summary)
#load DCM peak width
chl.width = read.csv('library/profiles_chl_peak_width_results.csv',header=T)
chl.width$Date = as.Date(chl.width$Date)
str(chl.width)




#dcm curvefit depth vs dcm  depth manual
a = ggplot(chl.width, aes(x=DCM.depth.curvefit,y=DCM.depth.manual)) +
  geom_point(); a

#use DCM.depth.curvefit and peak widths and/or bredth top/bottom





#JP
jp = 
  ggplot(filter(chl.width,LakeID=='JP'),aes(x=Date,y=DCM.depth.curvefit)) +
  geom_point(shape=19,size=3) +
  geom_line(color='gray30',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=DCM.breadth.top,ymax=DCM.breadth.bottom),width=5) +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b") +
  scale_y_reverse(n.breaks=6) +
  labs(title="Jordan Pond") + 
  theme_classic(); jp

sc = 
  ggplot(filter(chl.width,LakeID=='SC'),aes(x=Date,y=DCM.depth.curvefit)) +
  geom_point(shape=19,size=3) +
  geom_line(color='gray30',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=DCM.breadth.top,ymax=DCM.breadth.bottom),width=5) +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b") +
  scale_y_reverse(n.breaks=7) +
  labs(title="Seal Cove Pond") + 
  theme_classic(); sc

bb = 
  ggplot(filter(chl.width,LakeID=='BB'),aes(x=Date,y=DCM.depth.curvefit)) +
  geom_point(shape=19,size=3) +
  geom_line(color='gray30',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=DCM.breadth.top,ymax=DCM.breadth.bottom),width=5) +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b") +
  scale_y_reverse(limits=c(11,0),n.breaks=7) +
  labs(title="Bubble Pond") + 
  theme_classic(); bb

wh = 
  ggplot(filter(chl.width,LakeID=='WH'),aes(x=Date,y=DCM.depth.curvefit)) +
  geom_point(shape=19,size=3) +
  geom_line(color='gray30',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=DCM.breadth.top,ymax=DCM.breadth.bottom),width=5) +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b") +
  scale_y_reverse() +
  labs(title="Witch Hole Pond") + 
  theme_classic(); wh


library(ggpubr)
comb = ggarrange(jp,sc,bb,wh,nrow=2,ncol=2); comb


#test plots


a = ggplot(chl.width, aes(x=Date, y=DCM.depth.curvefit)) +
  geom_point() +
  facet_wrap(~LakeID, nrow=2, ncol=2) +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b"); a



a = ggplot(chl.width, aes(x=Date, y=DCM.r2.fit)) +
  geom_point() +
  facet_wrap(~LakeID, nrow=2, ncol=2) +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b"); a


a = ggplot(chl.width, aes(x=Date, y=peak.width)) +
  geom_point() +
  facet_wrap(~LakeID, nrow=2, ncol=2) +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b"); a


a = ggplot(chl.width, aes(x=Date, y=stand.peak.width)) +
  geom_point() +
  facet_wrap(~LakeID, nrow=2, ncol=2) +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b"); a







#JP
buh = 
  ggplot(filter(dcm_z,LakeID=='JP'),aes(x=Date,y=chl_mean,color=chl_max_depth)) +
  geom_point(shape=19,size=3) +
  geom_line(color='honeydew4',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=chl_mean-chl_sd,ymax=chl_mean+chl_sd),width=5) +
  scale_color_gradient2(limits=c(10,22),breaks=c(10,12,14,16,18,20,22),midpoint=16,
                        low="green",mid='gold',high="red") +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b") +
  
  ylim(0,400) +
  
  guides(color=guide_colorbar(reverse=T)) + #reverse color/number order
  
  labs(title='Jordan Pond',color="Depth max fChl (m)",x="",y="Mean fChl") + 
  
  theme_classic(); buh