#Nutrient overview figures.
#updated 2020-11-25

#load survey data 
source('Rscripts/1.01_load.survey.R')

library(dplyr)
library(ggplot2)
library(ggpubr)

#reorder layers
full.nooch$Site = factor(full.nooch$Site, levels = c("Epi", "Meta", "Hypo"))
#colors
cols = c('#03C66C','#C0A603','#470528')

#JP
#DOC
a = ggplot(filter(full.nooch,LakeID=='JP'),
           aes(x=Date,y=DOC_mgpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(1,6),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ", y=" ",title=bquote("DOC (mg "*L^-1*")")) +
  theme_classic(); a

#TP
b = ggplot(filter(full.nooch,LakeID=='JP'),
           aes(x=Date,y=TP_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,20),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ",title=bquote("TP ("*mu*"g "*L^-1*")")) +
  theme_classic(); b

#TN
c = ggplot(filter(full.nooch,LakeID=='JP'),
           aes(x=Date,y=TN_mgpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ",title=bquote("TN ("*mu*"g "*L^-1*")")) +
  theme_classic(); c

#NO3
d = ggplot(filter(full.nooch,LakeID=='JP'),
           aes(x=Date,y=NO3_ugpl,color=Site)) +
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,60),n.breaks=7) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ",title=expression(NO[3]^{"-"}~(mu*g~L^{-1}))) + 
  theme_classic(); d

#NH4
e = ggplot(filter(full.nooch,LakeID=='JP'),
           aes(x=Date,y=NH4_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,40),n.breaks=7) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ",title=expression(NH[4]^{"+"}~(mu*g~L^{-1}))) +
  theme_classic(); e

#DIN:TP
f = ggplot(filter(full.nooch,LakeID=='JP'),
           aes(x=Date,y=DIN_TP,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  geom_rect(aes(xmin=as.Date('2020-02-15'),xmax=as.Date('2020-11-15'),ymin=1.5,ymax=3.4),
            color=NA,fill='turquoise1',alpha=0.002,show.legend=F) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,16),n.breaks=5) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ",title="DIN:TP") +
  theme_classic(); f

#chl a
g = ggplot(filter(full.nooch,LakeID=='JP'),
           aes(x=Date,y=chla_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,10),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ", title=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  theme_classic(); g

#combine
jp = ggarrange(b,d,e,f,ncol=4,nrow=1,legend='none'); jp

#format
jp.f = jp +
  theme(title=element_text(size=4,hjust=0),
        axis.title=element_text(size=2),
        axis.text=element_text(size=2)); jp.f
#save
ggsave(filename="jpnooch.png", plot=jp.f,device="png",
       height=2, width=18, units="in", dpi=400)



#BB
#DOC
a = ggplot(filter(full.nooch,LakeID=='BB'),
           aes(x=Date,y=DOC_mgpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(1,6),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); a
#TP
b = ggplot(filter(full.nooch,LakeID=='BB'),
           aes(x=Date,y=TP_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,20),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); b
#TN
c = ggplot(filter(full.nooch,LakeID=='BB'),
           aes(x=Date,y=TN_mgpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); c
#NO3
d = ggplot(filter(full.nooch,LakeID=='BB'),
           aes(x=Date,y=NO3_ugpl,color=Site)) +
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,60),n.breaks=7) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); d
#NH4
e = ggplot(filter(full.nooch,LakeID=='BB'),
           aes(x=Date,y=NH4_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,40),n.breaks=7) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); e
#DIN:TP
f = ggplot(filter(full.nooch,LakeID=='BB'),
           aes(x=Date,y=DIN_TP,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  geom_rect(aes(xmin=as.Date('2020-02-15'),xmax=as.Date('2020-11-15'),ymin=1.5,ymax=3.4),
            color=NA,fill='turquoise1',alpha=0.002,show.legend=F) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,16),n.breaks=5) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ",title=" ") +
  theme_classic(); f
#chl a
g = ggplot(filter(full.nooch,LakeID=='BB'),
           aes(x=Date,y=chla_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,10),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ", title=" ") +
  theme_classic(); g

#combine
bb = ggarrange(b,d,e,f,ncol=4,nrow=1,legend='none'); bb




#SC
#DOC
a = ggplot(filter(full.nooch,LakeID=='SC'),
           aes(x=Date,y=DOC_mgpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(1,6),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); a
#TP
b = ggplot(filter(full.nooch,LakeID=='SC'),
           aes(x=Date,y=TP_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,20),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); b
#TN
c = ggplot(filter(full.nooch,LakeID=='SC'),
           aes(x=Date,y=TN_mgpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); c
#NO3
d = ggplot(filter(full.nooch,LakeID=='SC'),
           aes(x=Date,y=NO3_ugpl,color=Site)) +
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,60),n.breaks=7) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); d
#NH4
e = ggplot(filter(full.nooch,LakeID=='SC'),
           aes(x=Date,y=NH4_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,40),n.breaks=7) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); e
#DIN:TP
f = ggplot(filter(full.nooch,LakeID=='SC'),
           aes(x=Date,y=DIN_TP,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  geom_rect(aes(xmin=as.Date('2020-02-15'),xmax=as.Date('2020-11-15'),ymin=1.5,ymax=3.4),
            color=NA,fill='turquoise1',alpha=0.002,show.legend=F) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,16),n.breaks=5) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ",title=" ") +
  theme_classic(); f
#chl a
g = ggplot(filter(full.nooch,LakeID=='SC'),
           aes(x=Date,y=chla_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,10),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ", title=" ") +
  theme_classic(); g

#combine
sc = ggarrange(b,d,e,f,ncol=4,nrow=1,legend='none'); sc


#WH
#DOC
a = ggplot(filter(full.nooch,LakeID=='WH'),
           aes(x=Date,y=DOC_mgpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(1,6),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); a
#TP
b = ggplot(filter(full.nooch,LakeID=='WH'),
           aes(x=Date,y=TP_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,20),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); b
#TN
c = ggplot(filter(full.nooch,LakeID=='WH'),
           aes(x=Date,y=TN_mgpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); c
#NO3
d = ggplot(filter(full.nooch,LakeID=='WH'),
           aes(x=Date,y=NO3_ugpl,color=Site)) +
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,60),n.breaks=7) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); d
#NH4
e = ggplot(filter(full.nooch,LakeID=='WH'),
           aes(x=Date,y=NH4_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,40),n.breaks=7) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  theme_classic() + 
  labs(x=" ", y=" ", title = " "); e
#DIN:TP
f = ggplot(filter(full.nooch,LakeID=='WH'),
           aes(x=Date,y=DIN_TP,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  geom_rect(aes(xmin=as.Date('2020-02-15'),xmax=as.Date('2020-11-15'),ymin=1.5,ymax=3.4),
            color=NA,fill='turquoise1',alpha=0.002,show.legend=F) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,16),n.breaks=5) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ",title=" ") +
  theme_classic(); f
#chl a
g = ggplot(filter(full.nooch,LakeID=='WH'),
           aes(x=Date,y=chla_ugpl,color=Site)) + 
  geom_jitter(size=2,shape=19,position=position_jitter(width=2,height=0)) +
  scale_colour_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,10),n.breaks=6) +
  scale_x_date(limits=as.Date(c('2020-02-15','2020-11-15')),
               date_breaks="2 months",date_labels="%d-%b") +
  labs(x=" ",y=" ", title=" ") +
  theme_classic(); g

#combine
wh = ggarrange(b,d,e,f,ncol=4,nrow=1,legend='none'); wh

#combine
ql = ggarrange(jp,bb,sc,wh,nrow=4,ncol=1,common.legend=TRUE,legend='none'); ql


#format
ql.f = ql +
  theme(title=element_text(size=4,hjust=0),
        axis.title=element_text(size=2),
        axis.text=element_text(size=2)); ql.f
#save
ggsave(path="plots",filename="total_nooch.png", plot=ql.f,device="png",
       height=8, width=14, units="in", dpi=400)

