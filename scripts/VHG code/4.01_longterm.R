#Longterm trends 
#Load data, figures, and stats
#updated 2021-04-05

#libraries
library(dplyr)
library(tidyr)
library(plyr)
library(Kendall)
library(ggplot2)
library(ggpubr)
library(patchwork)



#Load data ####



#load nutrient data 
full.nooch = read.csv("library/db.nooch.csv",header=T)
full.nooch = full.nooch %>% 
  mutate(lakename=ifelse(grepl('JP',LakeID),'Jordan Pond',NA)) %>% #add lakename
  mutate(lakename=replace(lakename,grepl('SC',LakeID), 'Seal Cove Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('WH',LakeID), 'Witch Hole Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('BB',LakeID), 'Bubble Pond')) 

#DOC
lt.doc = read.csv("dbLongterm/DOC_1988-2019.csv")
#rename cols
colnames(lt.doc)[1] = "lakename"
colnames(lt.doc)[7] = 'DOC_mgpl'
#coerce date
lt.doc$Date = as.Date(lt.doc$Date, format = "%m/%d/%Y")
str(lt.doc)
#select cols, add lakedate ID column
lt.doc = lt.doc %>% 
  dplyr::select(LakeID,Date,DOC_mgpl,lakename) %>%
  mutate(lakedate=paste(LakeID,Date,sep="_"))

#get 2020 doc avg for each lakedate
new.doc =  ddply(full.nooch, 
                 .(lakedate,LakeID,Date,lakename), 
                 summarize, 
                 DOC_mgpl=mean(doc.mgpl))

#merge lt and 2020 data
lt.doc = rbind(lt.doc,new.doc)
#filter dates
lt.doc = lt.doc %>% filter(Date >= "1995-01-01") # omitting 80s datapoints
lt.doc
#write csv
write.csv(lt.doc, "library/lt.doc.csv", row.names = F)



#Secchi
lt.secchi = read.csv("dbLongterm/Secchi_1970-2020.csv")
#rename cols
colnames(lt.secchi)[1] = "lakename"
#fix witch hole name
lt.secchi[grepl("Witch Hole",lt.secchi$lakename),1]= "Witch Hole Pond"
#coerce date
lt.secchi$Date = as.Date(lt.secchi$Date, format = "%m/%d/%Y")
str(lt.secchi)
#add lakedate ID column
lt.secchi = lt.secchi %>%  mutate(lakedate=paste(LakeID,Date,sep="_"))
#filter dates
lt.secchi = lt.secchi %>% filter(Date >= "1995-01-01") 
#write csv
write.csv(lt.secchi, "library/lt.secchi.csv", row.names = F)


#2020 summary
s = lt.secchi %>% filter(Date>'2020-01-01')
s %>%
  group_by(LakeID) %>%
  dplyr::summarise(across(Secchi_m, list(min = min,
                                     max = max,
                                     mean = mean,
                                     sd = sd), na.rm=T))





#SO4
lt.so4 = read.csv("dbLongterm/SO4_1982-2019.csv")
str(lt.so4)
#rename cols
colnames(lt.so4)[1] = "lakename"
#coerce date
lt.so4$Date = as.Date(lt.so4$Date, format = "%m/%d/%Y")
#add lakedate ID column
lt.so4 = lt.so4 %>%  mutate(lakedate=paste(LakeID,Date,sep="_"))
str(lt.so4)
#filter dates
lt.so4 = lt.so4 %>% filter(Date >= "1995-01-01") 
#write csv
write.csv(lt.so4, "library/lt.so4.csv", row.names = F)









#Figures ####

#reorder lakes
lt.doc$lakename = factor(lt.doc$lakename,levels=c("Jordan Pond","Bubble Pond","Seal Cove Pond","Witch Hole Pond"))
lt.secchi$lakename = factor(lt.secchi$lakename,levels=c("Jordan Pond","Bubble Pond","Seal Cove Pond","Witch Hole Pond"))
#lt.secchi$lakename = factor(lt.secchi$lakename,levels=c("Witch Hole Pond","Seal Cove Pond","Bubble Pond","Jordan Pond"))
lt.so4$lakename = factor(lt.so4$lakename,levels=c("Jordan Pond","Bubble Pond","Seal Cove Pond","Witch Hole Pond"))
#4 colors pairs
color.code = c('#35978F','#01665E','#BF812D','#543005')
# reversed
#color.code = c('#543005','#BF812D','#01665E','#80CDC1')

col2 = c('#03C66C', #teal
         '#C0A603', #gold
         '#470528', #purple
         '#F1948A') #red


#DOC
plot.lt.doc =
  ggplot(lt.doc, aes(x=as.Date(Date),y=DOC_mgpl,color=lakename)) +
  geom_point(size=2,shape=19,alpha=0.8) +  
  stat_smooth(aes(group=lakename),method="loess",
              linetype=2,size=1,se=F,show.legend=F) + 
  scale_y_continuous(limits=c(1,8),n.breaks=4) + 
  scale_x_date(date_labels="%Y",limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),by='5 years'))) +  #adjust date origin
  labs(title="DOC concentration",color="Lake",x=" ") +
  ylab(bquote("DOC (mg "*L^-1*")")) +
  #scale_color_manual(values=color.code) +
  scale_color_manual(values=col2) +
  guides(color=guide_legend(override.aes=list(shape=19,size=2))) +
  theme_classic(); plot.lt.doc  

#Secchi
plot.lt.secchi = 
  ggplot(lt.secchi,aes(x=as.Date(Date),y=Secchi_m,color=lakename)) +
  geom_point(size=2,shape=19,alpha=0.8) +  
  stat_smooth(aes(group=lakename),method="loess",
              linetype=2,size=1,se=F,show.legend=F) + 
  scale_y_reverse(limits=c(22,0)) + 
  scale_x_date(date_labels="%Y",limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),by='5 years'))) + 
  labs(title="Secchi depth",color="Lake",x=" ",y="Secchi depth (m)") +
  #scale_color_manual(values=color.code) +
  scale_color_manual(values=col2) +  
  guides(color=guide_legend(override.aes=list(shape=19,size=2))) +
  theme_classic(); plot.lt.secchi 


#combine 
plot.lt.doc.secchi = ggarrange(plot.lt.doc,plot.lt.secchi,
                               nrow=1,ncol=2,common.legend=TRUE,legend='right'); plot.lt.doc.secchi



#format
plot.lt.doc.secchi.formatted = plot.lt.doc.secchi +
  theme(title=element_text(size=4,hjust=0.5),
        axis.title=element_text(size=4),
        axis.text=element_text(size=4))

#save
ggsave(filename="plots/longterm/lt.doc.secchi.v2.png",
       plot=plot.lt.doc.secchi.formatted,
       device="png",
       height=6,
       width=16,
       units="in",
       dpi=500)



#just secchi
plot.secchi.formatted = plot.lt.secchi +
  theme(title=element_text(size=10,hjust=0.5),
        axis.title=element_text(size=10),
        axis.text=element_text(size=10))
#save
ggsave(filename="lt.secchi.png", plot=plot.secchi.formatted2,device="png",
       height=6, width=10, units="in", dpi=800)



#HABS Secchi

#Secchi
h = 
  ggplot(filter(lt.secchi,!LakeID=='BB'),aes(x=as.Date(Date),y=Secchi_m,color=lakename)) +
  geom_point(size=2,shape=19,alpha=0.8) +  
  stat_smooth(aes(group=lakename),method="loess",
              linetype=2,size=1,se=F,show.legend=F) + 
  scale_y_reverse(limits=c(22,0)) + 
  scale_x_date(date_labels="%Y",limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),by='5 years'))) + 
  labs(title="Secchi depth",color="Lake",x=NULL,y="Secchi depth (m)") +
  #scale_color_manual(values=color.code) +
  scale_color_manual(values=col2) +  
  guides(color=guide_legend(override.aes=list(shape=19,size=2))) +
  theme_classic(); h 
#save
ggsave(filename="HABs/figures/lt.secchi.jpg", plot=h,device="jpg",
       height=6, width=10, units="in", dpi=800)



#SO4
#label help:  xlab(bquote('Assimilation ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')'))

plot.lt.so4 =
  ggplot(lt.so4, aes(x=Date,y=SO4_ueqpl,color=lakename)) +
  geom_point(size=1,shape=19,alpha=0.8) +  
  stat_smooth(aes(group=lakename),method="loess",
              linetype=2,size=0.6,se=F,show.legend=F) + 
  #scale_y_continuous(limits=c(1,8),n.breaks=4) + 
  scale_x_date(date_labels="%Y",limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),by='5 years'))) +  #adjust date origin
  labs(title="Sulfate concentration",color="Lake",x="Date") +
  ylab(bquote(~SO[4]~" ("*mu"eq "*L^-1*")")) +
  #scale_color_manual(values=color.code) +
  scale_color_manual(values=col2) +  
  guides(color=guide_legend(override.aes=list(size=2))) +
  theme_classic(); plot.lt.so4  

#format
plot.lt.so4.formatted = plot.lt.so4 +
  theme(title=element_text(size=12,hjust=0.5),
        axis.title=element_text(size=12),
        axis.text=element_text(size=12))
#save
ggsave(filename="lt.so4.png", plot=plot.lt.so4.formatted,device="png",
       height=6, width=9, units="in", dpi=800)


#combine all
plot.lt = ggarrange(plot.lt.so4, plot.lt.doc,plot.lt.secchi,
                    nrow=1,ncol=3,common.legend=TRUE,legend='bottom'); plot.lt

#format
plot.lt.formatted = plot.lt +
  theme(title=element_text(size=0.1,hjust=0.5),
        axis.title=element_text(size=1),
        axis.text=element_text(size=1))
#save
ggsave(filename="lt.png", plot=plot.lt.formatted,device="png",
       height=5, width=12, units="in", dpi=500)
























#summaries ######
psecchi = lt.secchi %>%
  filter(Date > as.Date('2020-01-01')) %>% 
  group_by(LakeID) %>%
  summarise(across(Secchi_m, list(min = min,
                                  max = max,
                                  mean = mean,
                                  sd = sd)))







#DOC ####
JP = subset(lt.doc, LakeID %in% "JP")
SC = subset(lt.doc, LakeID %in% "SC")
BB = subset(lt.doc, LakeID %in% "BB")
WH = subset(lt.doc, LakeID %in% "WH")

# complete averages 
avg = ddply(lt.doc, c("LakeID"), summarize,  
            N = sum(!is.na(DOC_mgpl)),   #output table of summary values
            mean = mean(DOC_mgpl), 
            sd = sd(DOC_mgpl), 
            se = sd/sqrt(N))

# are trends significant? Mann-Kendall trend test 
mk.jp = MannKendall(JP$DOC_mgpl)  # run mann-kendall test 
mk.jp$LakeID = "JP"     # add lakeID column
mk.jp = ldply(mk.jp, data.frame)    #make list a df
mk.jp = data.frame(t(mk.jp[-1]))    #transpose df (switch rows and columns)
colnames(mk.jp) = c("tau","sl","S","D","varS","LakeID")  #colnames
cnames = colnames(mk.jp)
mk.sc = MannKendall(SC$DOC_mgpl)
mk.sc$LakeID = "SC"
mk.sc = ldply(mk.sc, data.frame)
mk.sc = data.frame(t(mk.sc[-1]))
colnames(mk.sc) = cnames
mk.bb = MannKendall(BB$DOC_mgpl)
mk.bb$LakeID = "BB"
mk.bb = ldply(mk.bb, data.frame)
mk.bb = data.frame(t(mk.bb[-1]))
colnames(mk.bb) = cnames
mk.wh = MannKendall(WH$DOC_mgpl)
mk.wh$LakeID = "WH"
mk.wh = ldply(mk.wh, data.frame)
mk.wh = data.frame(t(mk.wh[-1]))
colnames(mk.wh) = cnames

mk = rbind(mk.jp,mk.sc,mk.bb,mk.wh)   # bind mks together
rownames(mk) = seq(length=nrow(mk))   # renumber rows
summary.doc = merge(avg,mk,by="LakeID")   # bind summary table together :)




#Secchi  ####
# subset
library(dplyr)
JP = subset(lt.secchi, LakeID %in% "JP"); str(JP)
SC = subset(lt.secchi, LakeID %in% "SC")
BB = subset(lt.secchi, LakeID %in% "BB")
WH = subset(lt.secchi, LakeID %in% "WH")

# complete averages 
avg = ddply(lt.secchi, c("LakeID"), summarise,  
            N = sum(!is.na(Secchi_m)),   #output table of summary values
            mean = mean(Secchi_m), 
            sd = sd(Secchi_m), 
            se = sd/sqrt(N))


# are trends significant?
mk.jp = MannKendall(JP$Secchi_m)  # run mann-kendall test 
mk.jp$LakeID = "JP"      # add lakeID column
mk.jp = ldply(mk.jp, data.frame)    #make list a df
mk.jp = data.frame(t(mk.jp[-1]))    #transpose df (switch rows and columns)
colnames(mk.jp) = c("tau","sl","S","D","varS","LakeID")  #colnames
cnames = colnames(mk.jp)
mk.sc = MannKendall(SC$Secchi_m)
mk.sc$LakeID = "SC"
mk.sc = ldply(mk.sc, data.frame)
mk.sc = data.frame(t(mk.sc[-1]))
colnames(mk.sc) = cnames
mk.bb = MannKendall(BB$Secchi_m)
mk.bb$LakeID = "BB"
mk.bb = ldply(mk.bb, data.frame)
mk.bb = data.frame(t(mk.bb[-1]))
colnames(mk.bb) = cnames
mk.wh = MannKendall(WH$Secchi_m)
mk.wh$LakeID = "WH"
mk.wh = ldply(mk.wh, data.frame)
mk.wh = data.frame(t(mk.wh[-1]))
colnames(mk.wh) = cnames

mk = rbind(mk.jp,mk.sc,mk.bb,mk.wh) 
rownames(mk) = seq(length=nrow(mk))   
summary.secchi = merge(avg,mk,by="LakeID")  





str(lt.secchi)

#Figs V2 ######
#new figure version
jpsecchi = 
  ggplot(filter(lt.secchi,LakeID=='JP'),
         aes(x=as.Date(Date),y=Secchi_m)) +
  geom_point(size=2,
             shape=1,
             alpha=0.8) +  
  stat_smooth(method="loess",
              linetype=2,
              color='#35978F',
              size=1,
              se=F,
              show.legend=F,) + 
  annotate("text", x=as.Date('1995-01-01'), y=0, label="tau == -0.13", parse=T, size=4, hjust=0) +
  annotate("text", x=as.Date('1995-01-01'), y=2.1, label="italic(p) == 0.015", parse=T, size=4, hjust=0) +
  scale_y_reverse(limits=c(22,0)) + 
  scale_x_date(date_labels="%Y",
               limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),
                            by='5 years'))) + 
  labs(title="Secchi depth (m)",
       x=NULL,
       y=NULL) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        axis.text=element_text(size=rel(1))); jpsecchi 


bbsecchi = 
  ggplot(filter(lt.secchi,LakeID=='BB'),
         aes(x=as.Date(Date),y=Secchi_m)) +
  geom_point(size=2,
             shape=1,
             alpha=0.8) +  
  stat_smooth(method="loess",
              linetype=1,
              size=1,
              color='#35978F',
              se=F,
              show.legend=F) + 
  scale_y_reverse(limits=c(13,0),n.breaks=7) + 
  scale_x_date(date_labels="%Y",
               limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),
                            by='5 years'))) + 
  annotate("text", x=as.Date('1995-01-01'), y=0, label="tau == -0.19", parse=T, size=4, hjust=0) +
  annotate("text", x=as.Date('1995-01-01'), y=1.4, label="italic(p) < 0.001", parse=T, size=4, hjust=0) +
  labs(title=NULL,
       x=NULL,
       y=NULL) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        axis.text=element_text(size=rel(1))); bbsecchi 


scsecchi = 
  ggplot(filter(lt.secchi,LakeID=='SC'),
         aes(x=as.Date(Date),y=Secchi_m)) +
  geom_point(size=2,
             shape=1,
             alpha=0.8) +  
  stat_smooth(method="loess",
              linetype=1,
              color='#35978F',
              size=1,
              se=F,
              show.legend=F) + 
  scale_y_reverse(limits=c(13,0),n.breaks=7) + 
  scale_x_date(date_labels="%Y",
               limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),
                            by='5 years'))) + 
  annotate("text", x=as.Date('1995-01-01'), y=0, label="tau == -0.17", parse=T, size=4, hjust=0) +
  annotate("text", x=as.Date('1995-01-01'), y=1.4, label="italic(p) < 0.01 ", parse=T, size=4, hjust=0) +
  labs(title=NULL,
       x=NULL,
       y=NULL) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        axis.text=element_text(size=rel(1))); scsecchi 

whsecchi = 
  ggplot(filter(lt.secchi,LakeID=='WH'),
         aes(x=as.Date(Date),y=Secchi_m)) +
  geom_point(size=2,
             shape=1,
             alpha=0.8) +  
  stat_smooth(method="loess",
              linetype=1,
              color='#35978F',
              size=1,
              se=F,
              show.legend=F) + 
  scale_y_reverse(limits=c(10,0),n.breaks=7) + 
  scale_x_date(date_labels="%Y",
               limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),
                            by='5 years'))) + 
  annotate("text", x=as.Date('1995-01-01'), y=0, label="tau == 0.21 ", parse=T, size=4, hjust=0) +
  annotate("text", x=as.Date('1995-01-01'), y=1.1, label="italic(p) < 0.001", parse=T, size=4, hjust=0) +
  labs(title=NULL,
       x='Date',
       y=NULL) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        axis.text=element_text(size=rel(1))); whsecchi 







#DOC
jpdoc = 
  ggplot(filter(lt.doc,LakeID=='JP'),
         aes(x=as.Date(Date),y=DOC_mgpl)) +
  geom_point(size=2,shape=1,alpha=0.8) +  
  stat_smooth(method="loess",
              linetype=2,
              color='#35978F',
              size=1,
              se=F,
              show.legend=F) + 
  scale_y_continuous(limits=c(1,8),n.breaks=4) + 
  scale_x_date(date_labels="%Y",
               limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),
                            by='5 years'))) + 
  annotate("text", x=as.Date('1995-01-01'), y=8, label="tau == 0.04", parse=T, size=4, hjust=0) +
  annotate("text", x=as.Date('1995-01-01'), y=7.2, label="italic(p) == 0.56", parse=T, size=4, hjust=0) +
  labs(title=bquote("DOC (mg "*L^-1*")"),
       x=NULL,
       y=NULL) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        axis.text=element_text(size=rel(1))); jpdoc 

bbdoc = 
  ggplot(filter(lt.doc,LakeID=='BB'),
         aes(x=as.Date(Date),y=DOC_mgpl)) +
  geom_point(size=2,shape=1,alpha=0.8) +  
  stat_smooth(method="loess",
              linetype=2,
              size=1,
              color='#35978F',
              se=F,
              show.legend=F) + 
  scale_y_continuous(limits=c(1,8),n.breaks=4) + 
  scale_x_date(date_labels="%Y",
               limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),
                            by='5 years'))) + 
  annotate("text", x=as.Date('1995-01-01'), y=8, label="tau == -0.11", parse=T, size=4, hjust=0) +
  annotate("text", x=as.Date('1995-01-01'), y=7.2, label="italic(p) == 0.19 ", parse=T, size=4, hjust=0) +
  labs(title=NULL,
       x=NULL,
       y=NULL) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        axis.text=element_text(size=rel(1))); bbdoc 


scdoc = 
  ggplot(filter(lt.doc,LakeID=='SC'),
         aes(x=as.Date(Date),y=DOC_mgpl)) +
  geom_point(size=2,shape=1,alpha=0.8) +  
  stat_smooth(method="loess",
              linetype=2,
              size=1,
              color='#35978F',
              se=F,
              show.legend=F) + 
  scale_y_continuous(limits=c(1,8),n.breaks=4) + 
  scale_x_date(date_labels="%Y",
               limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),
                            by='5 years'))) + 
  annotate("text", x=as.Date('1995-01-01'), y=8, label="tau == -0.21", parse=T, size=4, hjust=0) +
  annotate("text", x=as.Date('1995-01-01'), y=7.2, label="italic(p) == 0.011", parse=T, size=4, hjust=0) +
  labs(title=NULL,
       x=NULL,
       y=NULL) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        axis.text=element_text(size=rel(1))); scdoc 

whdoc = 
  ggplot(filter(lt.doc,LakeID=='WH'),
         aes(x=as.Date(Date),y=DOC_mgpl)) +
  geom_point(size=2,shape=1,alpha=0.8) +  
  stat_smooth(method="loess",
              linetype=1,
              color='#35978F',
              size=1,
              se=F,
              show.legend=F) + 
  scale_y_continuous(limits=c(1,8),n.breaks=4) + 
  scale_x_date(date_labels="%Y",
               limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),
                            by='5 years'))) + 
  annotate("text", x=as.Date('1995-01-01'), y=8, label="tau == -0.35", parse=T, size=4, hjust=0) +
  annotate("text", x=as.Date('1995-01-01'), y=7.2, label="italic(p) < 0.001", parse=T, size=4, hjust=0) +
  labs(title=NULL,
       x='Date',
       y=NULL) +
  theme_classic() +
  theme(plot.title=element_text(size=rel(1)),
        axis.text=element_text(size=rel(1))); whdoc 



#annotation ####
jp.text = ggplot(lt.doc,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Jordan Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); jp.text

bb.text = ggplot(lt.doc,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Bubble Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); bb.text

sc.text = ggplot(lt.doc,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Seal Cove Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); sc.text

wh.text = ggplot(lt.doc,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Witch Hole Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); wh.text




#combine and save
lt.plots = wrap_plots(jp.text,jpdoc,jpsecchi,
                      bb.text,bbdoc,bbsecchi,
                      sc.text,scdoc,scsecchi,
                      wh.text,whdoc,whsecchi,
                      ncol=3,
                      nrow=4,
                      widths=c(1,3,3)); lt.plots



#save
ggsave(filename="plots/formatted/lt.jpg", 
       plot=lt.plots,
       device="jpg",
       height=10,
       width=12, 
       units="in", 
       dpi=400)








#FIG V3 ########


#DOC
lt.doc = read.csv("dbLongterm/DOC_1988-2019.csv")
#rename cols
colnames(lt.doc)[1] = "lakename"
colnames(lt.doc)[7] = 'DOC_mgpl'
#coerce date
lt.doc$Date = as.Date(lt.doc$Date, format = "%m/%d/%Y")
str(lt.doc)
#select cols, add lakedate ID column
lt.doc = lt.doc %>% 
  dplyr::select(LakeID,Date,DOC_mgpl,lakename) %>%
  mutate(lakedate=paste(LakeID,Date,sep="_"))
lt.doc = lt.doc %>% filter(Date >= "1995-01-01") # omitting 80s datapoints


#Secchi
lt.secchi = read.csv("dbLongterm/Secchi_1970-2020.csv")
#rename cols
colnames(lt.secchi)[1] = "lakename"
#fix witch hole name
lt.secchi[grepl("Witch Hole",lt.secchi$lakename),1]= "Witch Hole Pond"
#coerce date
lt.secchi$Date = as.Date(lt.secchi$Date, format = "%m/%d/%Y")
str(lt.secchi)
#add lakedate ID column
lt.secchi = lt.secchi %>%  mutate(lakedate=paste(LakeID,Date,sep="_"))
#filter dates
lt.secchi = lt.secchi %>% filter(Date >= "1995-01-01") 
lt.secchi = lt.secchi %>% filter(Date <= "2020-01-01")
lt.secchi = lt.secchi %>% dplyr::select(-LakeID,-lakename,-Date)

#join
lt = join(lt.doc,lt.secchi,by='lakedate')

#reorder lakes
lt$lakename = factor(lt$lakename,levels=c("Jordan Pond","Bubble Pond","Seal Cove Pond","Witch Hole Pond"))
#4 colors pairs
col = c('#03C66C', #teal
        '#C0A603', #gold
        '#470528', #purple
        '#AF3A3D') #red


#save formula
f = y ~ x + I(x^2)
m1 = lm(Secchi_m ~ DOC_mgpl + I(DOC_mgpl^2), data=lt); m1
summary(m1)
   
# R2 = 0.7587 
# p < 0.001


#plot
lt.cor = ggplot(lt,
                aes(y=Secchi_m,
                    x=DOC_mgpl,
                    color=lakename)) +
  geom_point(size=2) +
  stat_smooth(aes(group=1),
              method="lm",
              formula = f,
              size=1,
              color='black',
              se=F) +
  annotate("text", x=6, y=19, label="italic(R) ^ 2 == 0.76", parse=T, size=4, hjust=0) + 
  annotate("text", x=6, y=18.3, label="italic(p) < 0.001", parse=T, size=4, hjust=0) +
  scale_color_manual(values=col) +
  labs(title=NULL,
       color="Lake",
       x=bquote("DOC (mg "*L^-1*")"),
       y="Secchi depth (m)") +
  theme_classic() +
  theme(axis.title.x = element_text(size=rel(1)), 
        axis.title.y = element_text(size=rel(1))) ; lt.cor
    
#save
ggsave(filename="plots/formatted/lt.cor.jpg", 
       plot=lt.cor,
       device="jpg",
       height=6,
       width=8, 
       units="in", 
       dpi=400)



#all lakes
cor.test(lt$DOC_mgpl, lt$Secchi_m, method="pearson")
#t = -23.049, df = 230, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.8705215 -0.7917830
#sample estimates:
#  cor 
#-0.8353882 


#Jordan
jplt = lt %>% filter(LakeID=='JP')
cor.test(jplt$DOC_mgpl, jplt$Secchi_m, method="pearson")

#Bubb
bblt = lt %>% filter(LakeID=='BB')
cor.test(bblt$DOC_mgpl, bblt$Secchi_m, method="pearson")

#Seal 
sclt = lt %>% filter(LakeID=='SC')
cor.test(sclt$DOC_mgpl, sclt$Secchi_m, method="pearson")

#Witch 
whlt = lt %>% filter(LakeID=='WH')
cor.test(whlt$DOC_mgpl, whlt$Secchi_m, method="pearson")

















