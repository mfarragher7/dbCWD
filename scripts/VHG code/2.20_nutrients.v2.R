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
  
  labs(x=NULL,
       y=bquote("Chl"~italic("a")~"("*mu*"g "*L^-1*")"),
       title='Jordan Pond') +
  
  theme_classic() + 
  
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=11)); jpchl


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
  labs(x=NULL,
       y=bquote("DOC (mg "*L^-1*")"),
       title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=11)); jpdoc

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
  labs(x=NULL,
       y=bquote("TP ("*mu*"g "*L^-1*")"),
       title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=11)); jptp #resize plots so they all match

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
  labs(x=NULL,
       y=bquote("DIN ("*mu*"g "*L^-1*")"),
       title=NULL) +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        plot.title=element_text(size=11)); jpdin

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
  labs(x=NULL,
       y="DIN:TP",
       title=NULL) +
  theme_classic(); jpdintp







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
  labs(x=NULL,
       y=NULL,
       title='Bubble Pond') +
  theme_classic() +
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=11)); bbchl

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
        plot.title=element_text(size=11)); bbdoc


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
  theme(axis.text.x=element_blank()); bbtp


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
        plot.title=element_text(size=11)); bbdin



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
  theme_classic(); bbdintp






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
  labs(x=NULL,y=NULL,title='Seal Cove Pond') +
  theme_classic() +
  theme(axis.text.x=element_blank(), #remove x axis from all but bottom plots
        plot.title=element_text(size=11), #smaller title. 
        plot.margin=unit(c(0,0,0,0),"cm")); scchl

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
        plot.title=element_text(size=11)); scdoc


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
  theme(axis.text.x=element_blank()); sctp


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
        plot.title=element_text(size=11)); scdin


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
  theme_classic(); scdintp






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
  labs(x=NULL,y=NULL,title='Witch Hole Pond') +
  theme_classic() +
  theme(plot.title=element_text(size=11),
        axis.text.x=element_blank()); whchl

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
  theme(axis.text.x=element_blank()); whdoc


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
  theme(axis.text.x=element_blank()); whdin


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
  theme(axis.text.x=element_blank()); whtp


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
  theme(plot.margin=unit(c(0,0,0,0),"cm")); whdintp







#Combine ####
full = wrap_plots(
  jpchl,bbchl,scchl,whchl,
  jpdoc,bbdoc,scdoc,whdoc,
  jptp,bbtp,sctp,whtp,
  jpdin,bbdin,scdin,whdin,
  jpdintp,bbdintp,scdintp,whdintp,
  ncol=4,
  nrow=5,
  guides='collect'); full

#save
ggsave(
  filename="plots/formatted/nooch.v2.jpg",
  plot=full, 
  device="jpg",
  height=10, 
  width=11, 
  units="in", 
  dpi=400)






#end