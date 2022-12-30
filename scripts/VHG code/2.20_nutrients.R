#better nooch figure
#2021-02-22


#load nooch
nooch = read.csv('library/db.nooch.csv',header=T)

#libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)


#modify dataframes


#drop spring 1 and 2 dates
nooch = nooch %>% filter(!seasonID %in% 'spring1' & !seasonID %in% 'spring2')

#make new season columns to help with grouping
nooch = nooch %>% mutate(szn = ifelse(grepl('winter',seasonID),'Winter',NA)) %>% 
  mutate(szn = replace(szn,grepl('spring',seasonID),'Spring')) %>% 
  mutate(szn = replace(szn,grepl('summer',seasonID),'Summer')) %>% 
  mutate(szn = replace(szn,grepl('fall',seasonID),'Autumn'))
  
#reorder seasons chronologically
nooch$szn = factor(nooch$szn, levels = c("Winter", "Spring", "Summer", "Autumn"))
#reorder layers
nooch$Site = factor(nooch$site, levels = c("Epi", "Meta", "Hypo"))
str(nooch)



#summary stats
names(nooch)
vars = names(nooch[,5:12])

pn = nooch %>%
  group_by(LakeID) %>%
  dplyr::summarise(across(vars, list(min = min,
                                     max = max,
                                     mean = mean,
                                     sd = sd), na.rm=T))
names(pn)
write.csv(pn, 'library/nooch.summary.csv',row.names=F)



ggplot(nooch,aes(x=Date,y=doc.mgpl)) +
  geom_point() +
  facet_wrap(vars(LakeID))

ggplot(nooch,aes(x=Date,y=tp.ugpl)) +
  geom_point() +
  facet_wrap(vars(LakeID))

ggplot(nooch,aes(x=Date,y=din.ugpl)) +
  geom_point() +
  facet_wrap(vars(LakeID))

ggplot(nooch,aes(x=Date,y=din.tp)) +
  geom_point() +
  facet_wrap(vars(LakeID))

ggplot(nooch,aes(x=Date,y=chla.ugpl)) +
  geom_point() +
  facet_wrap(vars(LakeID))









#make seperate df for winter in order to use geom_point for that
wn = nooch %>% filter(szn %in% 'Winter')
str(wn)

#change winter to NA for nooch
nooch = nooch %>% 
  #change one to BB so plots all match (no winter data for bb)
  mutate(LakeID = replace(LakeID,lakedepth=='JP_2020-02-21_Epi_1','BB')) %>% 
  mutate(chla.ugpl = replace(chla.ugpl,szn=='Winter',NA)) %>% 
  mutate(doc.mgpl = replace(doc.mgpl,szn=='Winter',NA)) %>% 
  mutate(tp.ugpl = replace(tp.ugpl,szn=='Winter',NA)) %>% 
  mutate(no3.ugpl = replace(no3.ugpl,szn=='Winter',NA)) %>% 
  mutate(nh4.ugpl = replace(nh4.ugpl,szn=='Winter',NA)) %>% 
  mutate(din.ugpl = replace(din.ugpl,szn=='Winter',NA)) %>% 
  mutate(din.tp = replace(din.tp,szn=='Winter',NA)) 












#plots ####
#set colors
cols = c('#03C66C','#C0A603','#470528')

#Jordan ####

#Chl a
jpchl = 
  ggplot(filter(nooch,LakeID=='JP'),aes(x=szn,y=chla.ugpl,fill=Site)) + 
  
  geom_boxplot(fatten=1) + #add boxplots for seasons with multiple samples
  
  geom_jitter(filter(wn,LakeID=='JP'),
              mapping=aes(x=szn,y=chla.ugpl,color=Site), #add points for single winter samples
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) + #set colors
  
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  
  scale_y_continuous(limits=c(0,10),n.breaks=6) + #same scales across lakes
  
  labs(x=NULL,y=NULL,title=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")")) +
  
  theme_classic() + 
  
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=10), #smaller title. 
        plot.margin=unit(c(0,0,-0.3,0),"cm")); jpchl


#DOC
jpdoc = 
  ggplot(filter(nooch,LakeID=='JP'),aes(x=szn,y=doc.mgpl,fill=Site)) + 
  geom_boxplot(fatten=1) + #add boxplots for seasons with multiple samples
  geom_jitter(filter(wn,LakeID=='JP'),
              mapping=aes(x=szn,y=doc.mgpl,color=Site), #add points for single winter samples
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) + #set colors
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(1,6),n.breaks=6) + #same scales across lakes
  labs(x=NULL,y=NULL,title=bquote("DOC (mg "*L^-1*")")) +
  theme_classic() +
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=10), #smaller title. 
        plot.margin=unit(c(0,0,-0.3,0),"cm")); jpdoc

#TP
jptp = 
  ggplot(filter(nooch,LakeID=='JP'),aes(x=szn,y=tp.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) + #add boxplots for seasons with multiple samples
  geom_jitter(filter(wn,LakeID=='JP'),
             mapping=aes(x=szn,y=tp.ugpl,color=Site), #add points for single winter samples
             size=2,
             position=position_jitter(width=0.1,height=0),
             show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) + #set colors
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,20),n.breaks=6) + #same scales across lakes
  labs(x=NULL,y=NULL,title=bquote("TP ("*mu*"g "*L^-1*")")) +
  theme_classic() +
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=10), #smaller title. 
        plot.margin=unit(c(0,0,-0.3,0),"cm")); jptp #resize plots so they all match

#NO3
jpno3 = 
  ggplot(filter(nooch,LakeID=='JP'),aes(x=szn,y=no3.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='JP'),
              mapping=aes(x=szn,y=no3.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,60),n.breaks=7) +
  labs(x=NULL,y=NULL,title=expression(NO[3]^{"-"}~(mu*g~L^{-1}))) + 
  theme_classic() + 
  theme(axis.text.x=element_blank(),
        plot.title=element_text(size=10),
        plot.margin=unit(c(0,0,-0.3,0),"cm")); jpno3

#NH4
jpnh4 = 
  ggplot(filter(nooch,LakeID=='JP'),aes(x=szn,y=nh4.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='JP'),
              mapping=aes(x=szn,y=nh4.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,40),n.breaks=7) +
  labs(x=NULL,y=NULL,title=expression(NH[4]^{"+"}~(mu*g~L^{-1}))) +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.title=element_text(size=10),
        plot.margin=unit(c(0,0,-0.3,0),"cm")); jpnh4

#DIN
jpdin = 
  ggplot(filter(nooch,LakeID=='JP'),aes(x=szn,y=din.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='JP'),
              mapping=aes(x=szn,y=din.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,100),n.breaks=7) +
  labs(x=NULL,y=NULL,title=bquote("DIN ("*mu*"g "*L^-1*")")) +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.title=element_text(size=10),
        plot.margin=unit(c(0,0,-0.3,0),"cm")); jpdin

#DIN:TP
jpdintp = ggplot(filter(nooch,LakeID=='JP'),aes(x=szn,y=din.tp,fill=Site)) + 
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=1.5,ymax=3.4), #geom-rect first to set layers. need to use Inf
            color=NA,
            fill='turquoise1',
            alpha=0.002,
            show.legend=F) +
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='JP'),
              mapping=aes(x=szn,y=din.tp,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,16),n.breaks=5) +
  labs(x=NULL,y=NULL,title="DIN:TP") +
  theme_classic() + 
  theme(axis.text.x=element_blank(),
        plot.title=element_text(size=10),
        plot.margin=unit(c(0,0,-0.3,0),"cm")); jpdintp







#Bubble ####

#Chl a
bbchl = 
  ggplot(filter(nooch,LakeID=='BB'),aes(x=szn,y=chla.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) + #add boxplots for seasons with multiple samples
  geom_jitter(filter(wn,LakeID=='BB'),
              mapping=aes(x=szn,y=chla.ugpl,color=Site), #add points for single winter samples
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) + #set colors
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,10),n.breaks=6) + #same scales across lakes
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=10), #smaller title. 
        plot.margin=unit(c(-0.3,0,0,0),"cm")); bbchl

#DOC
bbdoc = 
  ggplot(filter(nooch,LakeID=='BB'),aes(x=szn,y=doc.mgpl,fill=Site)) + 
  geom_boxplot(fatten=1) + #add boxplots for seasons with multiple samples
  geom_jitter(filter(wn,LakeID=='BB'),
              mapping=aes(x=szn,y=doc.mgpl,color=Site), #add points for single winter samples
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) + #set colors
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(1,6),n.breaks=6) + #same scales across lakes
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=10), #smaller title. 
        plot.margin=unit(c(-0.3,0,0,0),"cm")); bbdoc


#TP
bbtp = ggplot(filter(nooch,LakeID=='BB'),aes(x=szn,y=tp.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='BB'),
              mapping=aes(x=szn,y=tp.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,20),n.breaks=6) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() + 
  theme(axis.text.x=element_blank(),
        plot.margin=unit(c(-0.3,0,0,0),"cm")); bbtp

#NO3
bbno3 = ggplot(filter(nooch,LakeID=='BB'),aes(x=szn,y=no3.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='BB'),
              mapping=aes(x=szn,y=no3.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,60),n.breaks=7) +
  labs(x=NULL,y=NULL,title=NULL) + 
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.margin=unit(c(-0.3,0,0,0),"cm")); bbno3

#NH4
bbnh4 = ggplot(filter(nooch,LakeID=='BB'),aes(x=szn,y=nh4.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='BB'),
              mapping=aes(x=szn,y=nh4.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,40),n.breaks=7) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.margin=unit(c(-0.3,0,0,0),"cm")); bbnh4


#DIN
bbdin = 
  ggplot(filter(nooch,LakeID=='BB'),aes(x=szn,y=din.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='BB'),
              mapping=aes(x=szn,y=din.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,100),n.breaks=7) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.title=element_text(size=10),
        plot.margin=unit(c(-0.3,0,0,0),"cm")); bbdin



#DIN:TP
bbdintp = ggplot(filter(nooch,LakeID=='BB'),aes(x=szn,y=din.tp,fill=Site)) + 
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=1.5,ymax=3.4),
            color=NA,
            fill='turquoise1',
            alpha=0.002,
            show.legend=F) +
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='BB'),
              mapping=aes(x=szn,y=din.tp,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,16),n.breaks=5) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.margin=unit(c(-0.3,0,0,0),"cm")); bbdintp






#Seal ####

#Chl a
scchl = 
  ggplot(filter(nooch,LakeID=='SC'),aes(x=szn,y=chla.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) + #add boxplots for seasons with multiple samples
  geom_jitter(filter(wn,LakeID=='SC'),
              mapping=aes(x=szn,y=chla.ugpl,color=Site), #add points for single winter samples
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) + #set colors
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,10),n.breaks=6) + #same scales across lakes
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=10), #smaller title. 
        plot.margin=unit(c(-0.3,0,0,0),"cm")); scchl

#DOC
scdoc = 
  ggplot(filter(nooch,LakeID=='SC'),aes(x=szn,y=doc.mgpl,fill=Site)) + 
  geom_boxplot(fatten=1) + #add boxplots for seasons with multiple samples
  geom_jitter(filter(wn,LakeID=='SC'),
              mapping=aes(x=szn,y=doc.mgpl,color=Site), #add points for single winter samples
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) + #set colors
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(1,6),n.breaks=6) + #same scales across lakes
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=10), #smaller title. 
        plot.margin=unit(c(-0.3,0,0,0),"cm")); scdoc


#TP
sctp = ggplot(filter(nooch,LakeID=='SC'),aes(x=szn,y=tp.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='SC'),
              mapping=aes(x=szn,y=tp.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,20),n.breaks=6) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.margin=unit(c(-0.3,0,0,0),"cm")); sctp

#NO3
scno3 = ggplot(filter(nooch,LakeID=='SC'),aes(x=szn,y=no3.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='SC'),
              mapping=aes(x=szn,y=no3.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,60),n.breaks=7) +
  labs(x=NULL,y=NULL,title=NULL) + 
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.margin=unit(c(-0.3,0,0,0),"cm")); scno3

#NH4
scnh4 = ggplot(filter(nooch,LakeID=='SC'),aes(x=szn,y=nh4.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='SC'),
              mapping=aes(x=szn,y=nh4.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,40),n.breaks=7) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.margin=unit(c(-0.3,0,0,0),"cm")); scnh4

#DIN
scdin = 
  ggplot(filter(nooch,LakeID=='SC'),aes(x=szn,y=din.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='SC'),
              mapping=aes(x=szn,y=din.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,100),n.breaks=7) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.title=element_text(size=10),
        plot.margin=unit(c(-0.3,0,0,0),"cm")); scdin


#DIN:TP
scdintp = ggplot(filter(nooch,LakeID=='SC'),aes(x=szn,y=din.tp,fill=Site)) + 
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=1.5,ymax=3.4),
            color=NA,
            fill='turquoise1',
            alpha=0.002,
            show.legend=F) +
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='SC'),
              mapping=aes(x=szn,y=din.tp,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,16),n.breaks=5) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.margin=unit(c(-0.3,0,0,0),"cm")); scdintp






#Witch ####
#Chl a
whchl = 
  ggplot(filter(nooch,LakeID=='WH'),aes(x=szn,y=chla.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) + #add boxplots for seasons with multiple samples
  geom_jitter(filter(wn,LakeID=='WH'),
              mapping=aes(x=szn,y=chla.ugpl,color=Site), #add points for single winter samples
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) + #set colors
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,10),n.breaks=6) + #same scales across lakes
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(plot.margin=unit(c(-0.3,0,0,0),"cm")); whchl

#DOC
whdoc = 
  ggplot(filter(nooch,LakeID=='WH'),aes(x=szn,y=doc.mgpl,fill=Site)) + 
  geom_boxplot(fatten=1) + #add boxplots for seasons with multiple samples
  geom_jitter(filter(wn,LakeID=='WH'),
              mapping=aes(x=szn,y=doc.mgpl,color=Site), #add points for single winter samples
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) + #set colors
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(1,6),n.breaks=6) + #same scales across lakes
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(plot.margin=unit(c(-0.3,0,0,0),"cm")); whdoc


#DIN
whdin = 
  ggplot(filter(nooch,LakeID=='WH'),aes(x=szn,y=din.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='WH'),
              mapping=aes(x=szn,y=din.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,100),n.breaks=7) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(plot.margin=unit(c(-0.3,0,0,0),"cm")); whdin


#TP
whtp = ggplot(filter(nooch,LakeID=='WH'),aes(x=szn,y=tp.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='WH'),
              mapping=aes(x=szn,y=tp.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,20),n.breaks=6) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(plot.margin=unit(c(-0.3,0,0,0),"cm")); whtp

#NO3
whno3 = ggplot(filter(nooch,LakeID=='WH'),aes(x=szn,y=no3.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='WH'),
              mapping=aes(x=szn,y=no3.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,60),n.breaks=7) +
  labs(x=NULL,y=NULL,title=NULL) + 
  theme_classic() +
  theme(plot.margin=unit(c(-0.3,0,0,0),"cm")); whno3

#NH4
whnh4 = ggplot(filter(nooch,LakeID=='WH'),aes(x=szn,y=nh4.ugpl,fill=Site)) + 
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='WH'),
              mapping=aes(x=szn,y=nh4.ugpl,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,40),n.breaks=7) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(plot.margin=unit(c(-0.3,0,0,0),"cm")); whnh4

#DIN:TP
whdintp = ggplot(filter(nooch,LakeID=='WH'),aes(x=szn,y=din.tp,fill=Site)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=1.5,ymax=3.4),
            color=NA,
            fill='turquoise1',
            alpha=0.002,
            show.legend=F) +
  geom_boxplot(fatten=1) +
  geom_jitter(filter(wn,LakeID=='WH'),
              mapping=aes(x=szn,y=din.tp,color=Site),
              size=2,
              position=position_jitter(width=0.1,height=0),
              show.legend=F) +
  scale_fill_manual(values=cols,guide=guide_legend("Depth")) +
  scale_color_manual(values=cols,guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,16),n.breaks=5) +
  labs(x=NULL,y=NULL,title=NULL) +
  theme_classic() +
  theme(plot.margin=unit(c(-0.3,0,0,0),"cm")); whdintp




#annotation ####
jp.text = ggplot(nooch,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Jordan Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); jp.text

bb.text = ggplot(nooch,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Bubble Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); bb.text

sc.text = ggplot(nooch,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Seal Cove Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); sc.text

wh.text = ggplot(nooch,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0,label='Witch Hole Pond',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); wh.text





#Combine ####
full = wrap_plots(
  jp.text,jpchl,jpdoc,jptp,jpno3,jpnh4,jpdin,jpdintp,
  bb.text,bbchl,bbdoc,bbtp,bbno3,bbnh4,bbdin,bbdintp,
  sc.text,scchl,scdoc,sctp,scno3,scnh4,scdin,scdintp,
  wh.text,whchl,whdoc,whtp,whno3,whnh4,whdin,whdintp,
  ncol=8,
  nrow=4,
  guides='collect'); full

#save
ggsave(
  filename="plots/formatted/nooch.full.jpg",
  plot=full, 
  device="jpg",
  height=8, 
  width=20, 
  units="in", 
  dpi=400)


#selected vars
p = wrap_plots(
  jp.text,jpchl,jpdoc,jptp,jpdin,jpdintp,
  bb.text,bbchl,bbdoc,bbtp,bbdin,bbdintp,
  sc.text,scchl,scdoc,sctp,scdin,scdintp,
  wh.text,whchl,whdoc,whtp,whdin,whdintp,
  ncol=6,
  nrow=4,
  guides='collect'); p


#save
ggsave(
  filename="plots/formatted/nooch.jpg",
  plot=p, 
  device="jpg",
  height=8, 
  width=16, 
  units="in", 
  dpi=400)








#HABS version ####

habs = wrap_plots(
  jp.text,jptp,jpno3,jpnh4,jpdintp,
  sc.text,sctp,scno3,scnh4,scdintp,
  wh.text,whtp,whno3,whnh4,whdintp,
  ncol=5,
  nrow=3,
  guides='collect'); habs

#save
ggsave(
  filename="HABs/figures/nooch.jpg",
  plot=habs, 
  device="jpg",
  height=8, 
  width=14, 
  units="in", 
  dpi=400)








#end