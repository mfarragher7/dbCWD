#Formatted buoy fig
#Temperature, DO, PAR
# 2021-08-05

#libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)




#TEMP #####

#load data. use temp from DO loggers

jp.do = read.csv('library/buoy.jp.do.csv',header=T)
sc.do = read.csv('library/buoy.sc.do.csv',header=T)
wh.do = read.csv('library/buoy.wh.do.csv',header=T)
bb.temp = read.csv('library/buoy.bb.temp.csv',header=T)
bb.t = bb.temp %>%
  filter(Date > as.Date('2020-02-20')) %>% 
  mutate(ID = ifelse(Depth==2,'Top',NA)) %>% 
  mutate(ID = replace(ID,Depth==11,'Bottom')) %>% 
  filter(!is.na(ID))
  


#Jordan Temp 
p.jp.t = ggplot(jp.do,
                aes(x=as.Date(DateTime),y=Temp,color=ID)) +
  geom_point(shape=1,alpha=0.5,size=0.5) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-10-11')),
               date_breaks="1 month",
               date_labels="%b") +
  scale_color_manual(labels = c("Bottom","Top"),
                     values=c('darkblue','cyan'),
                     guide=guide_legend(" ",
                                        
                                        override.aes=list(shape=19,size=3),
                                        reverse=TRUE)) +
  scale_y_continuous(limits=c(0,30),
                     n.breaks=5) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-12")),
             linetype=4,
             color="black") + #ice-out date
  geom_vline(xintercept=as.numeric(as.Date("2020-06-25")),
             linetype=3,
             color="black") + #demonic intrusion
  labs(y=NULL,
       x=NULL,
       title="Temperature (°C)") + 
  theme_classic() +
  theme(plot.title=element_text(size=10),
        axis.title.y=element_text(size=10)); p.jp.t



#Bubble Temp 
p.bb.t = ggplot(bb.t,
                aes(x=as.Date(DateTime),y=Temp,color=ID)) +
  geom_point(shape=1,alpha=0.5,size=0.5) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-10-11')),
               date_breaks="1 month",
               date_labels="%b") +
  scale_color_manual(labels = c("Bottom","Top"),
                     values=c('darkblue','cyan'),
                     guide=guide_legend(title=NULL,
                                        override.aes=list(shape=19,size=3),
                                        reverse=TRUE)) +
  scale_y_continuous(limits=c(0,30),
                     n.breaks=5) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-12")),
             linetype=4,
             color="black") + #ice-out date
  labs(title=NULL,
       x=NULL,
       y=NULL) + 
  theme_classic() +
  theme(plot.title=element_text(size=10),
        axis.title.y=element_text(size=10)); p.bb.t




#SEAL COVE TEMP
p.sc.t = ggplot(sc.do,
                aes(x=as.Date(DateTime),y=Temp,color=ID)) +
  geom_point(shape=1,alpha=0.5,size=0.5) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-10-11')),
               date_breaks="1 month",
               date_labels="%b") +
  scale_color_manual(labels = c("Bottom","Top"),
                     values=c('darkblue','cyan'),
                     guide=guide_legend(" ",
                                        override.aes=list(shape=19,size=3),
                                        reverse=TRUE)) +
  scale_y_continuous(limits=c(0,30),
                     n.breaks=5) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-12")),
             linetype=4,
             color="black") + #ice-out date
  labs(title=NULL,
       x=NULL,
       y=NULL) + 
  theme_classic() +
  theme(plot.title=element_text(size=10),
        axis.title.y=element_text(size=10)); p.sc.t




#WITCH HOLE
p.wh.t = ggplot(wh.do,
                aes(x=as.Date(DateTime),y=Temp,color=ID)) +
  geom_point(shape=1,alpha=0.5,size=0.5) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-10-11')),
               date_breaks="1 month",
               date_labels="%b") +
  scale_color_manual(labels = c("Bottom","Top"),
                     values=c('darkblue','cyan'),
                     guide=guide_legend(" ",
                                        override.aes=list(shape=19,size=3),
                                        reverse=TRUE)) +
  scale_y_continuous(limits=c(0,30),
                     n.breaks=5) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-12")),
             linetype=4,
             color="black") + #ice-out date
  labs(title=NULL,
       x='Date',
       y=NULL) + 
  theme_classic() +
  theme(plot.title=element_text(size=10),
        axis.title.y=element_text(size=10)); p.wh.t











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
  labs(y=NULL,
       x=NULL,
       title='Dissolved Oxygen Saturation %') + 
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
  labs(title=NULL,
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
  labs(title=NULL,
       x='Date',
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
  labs(title=(bquote('PAR ('*mu~ 'mol' ~m^-2~s^-1*')')),
       x=NULL,
       y=NULL) +
  theme_classic() +
  theme(plot.title=element_text(size=10),
        axis.title.y=element_text(size=10),
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
  labs(title=NULL,
       x=NULL,
       y=NULL) +
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
  labs(title=NULL,
       x='Date',
       y=NULL) +
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









#ANNOTATE #######
jp.text = ggplot(jp.do,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Jordan Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); jp.text

bb.text = ggplot(bb.t,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Bubble Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); bb.text

sc.text = ggplot(sc.do,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Seal Cove Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); sc.text

wh.text = ggplot(wh.do,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Witch Hole Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); wh.text


blank = ggplot(bb.t,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label=NA,size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); blank



#COMBINE ########


p = wrap_plots(
  jp.text, p.jp.t, p.jp.do, p.jp.par,
  bb.text, p.bb.t, blank, blank,
  sc.text, p.sc.t, p.sc.do, p.sc.par,
  wh.text, p.wh.t, p.wh.do, p.wh.par,
  ncol=4,
  nrow=4,
  widths=c(1,2,2,2)
  guides='collect'); p




ggsave(filename="plots/formatted/buoy.full.jpg",
       plot=p, 
       device="jpg",
       height=10, 
       width=14, 
       units="in", 
       dpi=500)





