#New DO/PAR figures
#run buoy.load.R first
#updated 2020-12-06


#libraries
library(dplyr)
library(ggplot2)
library(ggpubr)


#load data

jp.do = read.csv('library/buoy.jp.do.csv',header=T)
sc.do = read.csv('library/buoy.sc.do.csv',header=T)
wh.do = read.csv('library/buoy.wh.do.csv',header=T)




#DO #############

#Jordan DO
p.jp.do = ggplot(jp.do,aes(x=as.Date(DateTime),y=DOsat,color=ID)) +
  geom_point(shape=1,alpha=0.5,size=0.5) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-10-11')),date_breaks="1 month",date_labels="%b") +
  scale_color_manual(labels = c("Bottom","Top"),values=c('darkblue','cyan'),
                     guide=guide_legend(" ",override.aes=list(shape=19,size=3),reverse=TRUE)) +
  scale_y_continuous(limits=c(0,125),n.breaks=5) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-12")),linetype=4,color="black") + #ice-out date
  #annotate("text",x=as.Date('2020-03-25'),y=72,label="Ice-out",size=4) + #annotate
  geom_vline(xintercept=as.numeric(as.Date("2020-06-25")),linetype=3,color="black") + #demonic intrusion
  #annotate("text",x=as.Date('2020-07-24'),y=72,label="Demonic Intrusion",size=4) + #annotate
  labs(title='Jordan Pond',
       x=NULL,
       y='Dissolved Oxygen Saturation %') + 
  theme_classic() +
  theme(plot.title=element_text(size=10),
        axis.title.y=element_text(size=10)); p.jp.do

#Seal Cove DO
p.sc.do = ggplot(sc.do,aes(x=as.Date(DateTime),y=DOsat,color=ID)) +
  geom_point(alpha=0.5,size=0.5) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-10-11')),date_breaks="1 month",date_labels="%b") +
  scale_y_continuous(limits=c(0,125),n.breaks=5) +
  scale_color_manual(labels = c("Bottom","Top"),values=c('darkblue','cyan'),
                     guide=guide_legend(" ",override.aes=list(shape=19,size=3),reverse=TRUE)) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-12")),linetype=4,color="black") +
  #annotate("text",x=as.Date('2020-03-25'),y=2,label="Ice-out",size=4) + 
  labs(title='Seal Cove Pond',
       x=NULL,
       y=NULL) + 
  theme_classic() +
  theme(plot.title=element_text(size=10)); p.sc.do 

#Witch Hole DO
p.wh.do = ggplot(wh.do,aes(x=as.Date(DateTime),y=DOsat,color=ID)) +
  geom_point(alpha=0.5,size=0.5) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-10-11')),date_breaks="1 month",date_labels="%b") +
  scale_y_continuous(limits=c(0,125),n.breaks=5) +
  scale_color_manual(labels = c("Bottom","Top"),values=c('darkblue','cyan'),
                     guide=guide_legend(" ",override.aes=list(shape=19,size=3),reverse=TRUE)) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-13")),linetype=4,color="black") +
  #annotate("text",x=as.Date('2020-03-27'),y=2,label="Ice-out",size=4) + 
  labs(title='Witch Hole Pond',
       x=NULL,
       y=NULL) + 
  theme_classic() +
  theme(plot.title=element_text(size=10)); p.wh.do 


#combine
comb.do = ggarrange(p.jp.do,p.sc.do,p.wh.do,nrow=1,ncol=3,common.legend=T,legend='right')
comb.do


#save
ggsave(filename="plots/formatted/buoy.do.jpg", 
       plot=comb.do, 
       device="jpg",
       height=4, 
       width=12, 
       units="in", 
       dpi=500)




#PAR ###################
jp.par = read.csv('library/buoy.jp.par.csv',header=T)
sc.par = read.csv('library/buoy.sc.par.csv',header=T)
wh.par = read.csv('library/buoy.wh.par.csv',header=T)



#JORDAN PAR
jp.par_ = jp.par %>% mutate(PAR=replace(PAR,Time < "10:00:00" | Time > "14:00:00",NA))

p.jp.par = 
  ggplot(jp.par_,aes(as.Date(DateTime),y=PAR,color=ID,fill=BDI))+
  geom_point(alpha=0.3,size=0.5, color='gray20') +
  geom_smooth(method="gam",formula=y~s(x,bs="cs",k=16),
              span=0.8,color='black',size=1,se=F) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-10-11')),
               date_breaks="1 month",date_labels="%b") +
  scale_y_continuous(limits=c(0,1000),n.breaks=6) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-12")),
             linetype=4,color="black") + #ice-out date
  geom_vline(xintercept=as.numeric(as.Date("2020-06-25")),
             linetype=3,color="black") + #demonic intrusion
  ylab(bquote('PAR ('*mu~ 'mol' ~m^-2~s^-1*')')) +
  xlab('Date') + 
  ggtitle(NULL) +
  theme_classic() +
  theme(axis.title.y=element_text(size=10),
        legend.position = "none"); p.jp.par 

#SEAL COVE PAR
#filter 10am-2pm
sc.par_ = sc.par %>% mutate(PAR=replace(PAR,Time < "10:00:00" | Time > "14:00:00",NA))

p.sc.par = 
  ggplot(sc.par_,aes(x=as.Date(DateTime),y=PAR, color=ID)) +
  geom_point(alpha=0.3, size=0.5, color='gray20') +
  geom_smooth(method="gam",formula=y~s(x,bs="cs",k=16),
              span=0.8,color='black',size=1,se=F) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-10-11')),
               date_breaks="1 month",date_labels="%b") +
  scale_y_continuous(limits=c(0,1000),n.breaks=5) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-12")),
             linetype=4,color="black") + #ice-out date
  ylab(NULL) + 
  xlab('Date') + 
  ggtitle(NULL) +
  theme_classic() +
  theme(legend.position = "none"); p.sc.par 


#Witch Hole PAR
#filter 10am-2pm
wh.par_ = wh.par %>% mutate(PAR=replace(PAR,Time < "10:00:00" | Time > "14:00:00",NA))

p.wh.par = ggplot(wh.par_,aes(x=as.Date(DateTime),y=PAR, color=ID)) +
  geom_point(alpha=0.3, size=0.5, color='gray20') +
  geom_smooth(method="gam",formula=y~s(x,bs="cs",k=16),
              span=0.8,color='black',size=1,se=F) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-10-11')),
               date_breaks="1 month",date_labels="%b") +
  scale_y_continuous(limits=c(0,1000),n.breaks=5) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-13")),
             linetype=4,color="black") + #ice-out date
  ylab(bquote(NULL)) + 
  xlab('Date') +
  ggtitle(NULL) +
  theme_classic() +
  theme(legend.position = "none"); p.wh.par 



#combine
comb.par = ggarrange(p.jp.par,p.sc.par,p.wh.par,nrow=1,ncol=3); comb.par



#save
ggsave(filename="plots/formatted/buoy.par.jpg",
       plot=comb.par, 
       device="jpg",
       height=4, 
       width=12, 
       units="in", 
       dpi=500)



#combne all
library(patchwork)
p = wrap_plots(p.jp.do, p.sc.do, p.wh.do,
               p.jp.par, p.sc.par, p.wh.par,
               ncol=3,
               nrow=2,
               guides='collect',
               guide); p


ggsave(filename="plots/formatted/buoy.do&par.jpg",
       plot=p, 
       device="jpg",
       height=8, 
       width=18, 
       units="in", 
       dpi=500)





