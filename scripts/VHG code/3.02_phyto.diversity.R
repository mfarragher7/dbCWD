#Phyto summary stats part 2 - diversity
# *** test hypotheses 2a and 2b ***
#created 2021-06-09


#load libraries
library(plyr)
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(ggpubr)
library(patchwork)

#source for nmds: https://chrischizinski.github.io/rstats/vegan-ggplot2/



#load data ####
phyto = read.csv("library/db.phyto.csv",header=T)
str(phyto)
#remove non-algae
phyto = phyto %>% 
  filter(!taxa=='blue blob'& !taxa=='brown blob'& !taxa=='green blob')
#remove chytrids
phyto = phyto %>% 
  filter(!group=='chytrids')
#check
unique(phyto$taxa)
names(phyto)
str(phyto)
#collapse cosmarium 1,2,3, collapse kateblepharids. everything on 'genus' level mostly
phyto.condensed = phyto %>% 
  group_by(sampleID,LakeID,Date,lakedate,seasonID,Site,Depth,vol.settled.ml,trophic.mode,group,taxa) %>% 
  mutate(taxa = replace(taxa, grepl('cosmarium',taxa),'cosmarium')) %>% 
  mutate(taxa = replace(taxa, grepl('katablepharid',taxa),'ketablepharid')) %>% 
  summarize(n=sum(n))
#check 
str(phyto.condensed)




#Overview ####
#get taxa and groups for each layer
phy.layers = ddply(phyto.condensed,
                   .(sampleID,
                     LakeID,
                     Date,
                     lakedate,
                     Site), 
                   summarize,
                   n.group = length(unique(group)),
                   n.taxa = length(unique(taxa)))
#overview plot
#set colors
cols = c('#03C66C','#C0A603','#470528')
#reorder sites
phy.layers$Site = factor(phy.layers$Site,levels=c("Epi","Meta","Hypo"))

ggplot(filter(phy.layers,LakeID=='JP'),
       aes(x=Date,y=n.taxa,color=Site)) +
  geom_jitter(size=5,position=position_jitter(width=0.1,height=0)) + 
  scale_y_continuous(limits=c(10,40),n.breaks=6) +
  scale_color_manual(values=cols) +
  labs(x=NULL,y='OTU Richness',color="Depth",title='Jordan Pond') +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        plot.title=element_text(size=rel(1)))

ggplot(filter(phy.layers,LakeID=='SC'),
       aes(x=Date,y=n.taxa,color=Site)) +
  geom_jitter(size=5,position=position_jitter(width=0.1,height=0)) + 
  scale_y_continuous(limits=c(10,40),n.breaks=6) +
  scale_color_manual(values=cols) +
  labs(x='Date',y='OTU Richness',color="Depth",title="Seal Cove Pond") +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        plot.title=element_text(size=rel(1)))






# Lakes ################

#B-diversity
#Hypothesis 2b - OTU turnover (entire lake, between dates)

#get densities for each layer
phy.layers = ddply(phyto.condensed,
                   .(sampleID,
                     LakeID,
                     Date,
                     lakedate,
                     Site,
                     seasonID,
                     vol.settled.ml,
                     trophic.mode,
                     group,
                     taxa), 
                   summarize,
                   cell.count.raw = sum(n),
                   cell.count.conv = cell.count.raw*28,
                   cells.pml = cell.count.conv / vol.settled.ml,
                   phyID = paste(group,taxa,sep='-')) #taxa ID

#condense layers to get total counts for entire lake
names(phy.layers)
phy.lakes = ddply(phy.layers,
                  .(lakedate,
                    LakeID,
                    seasonID,
                    Date,
                    group,
                    taxa,
                    trophic.mode,
                    phyID), 
                  summarize,
                  cells.pml = sum(cells.pml))

#check if number of groups and taxa for each lakedate matches plot
phy.lakes %>% group_by(lakedate) %>% summarise(count = n_distinct(group))
phy.lakes %>% group_by(lakedate) %>% summarise(count = n_distinct(taxa))

#format wide df for vegan
phy.lv = phy.lakes %>% 
  pivot_wider(id_cols = c(lakedate,LakeID,seasonID),
              names_from = taxa,
              values_from = cells.pml,
              values_fill = 0)

names(phy.lv)
str(phy.lv)

#save ID cols as factors
lakedateID = factor(phy.lv$lakedate) #get group names
lakeID = phy.lv$LakeID
seasonID = c("Winter","Spring",'Summer',"Autumn")


#**horn ##########
#Morisita-Horn distance 
horn.nmds = metaMDS(phy.lv[,4:53], distance="horn", k=2, trymax=1000)
horn.nmds
plot(horn.nmds, type="t")

#get scores
data.scores = as.data.frame(scores(horn.nmds))  
data.scores$site = rownames(data.scores)  
data.scores$lakedateID = lakedateID 
data.scores$Lake = lakeID
data.scores=data.scores %>% 
  mutate(Lake=replace(Lake,Lake=="JP","Jordan Pond")) %>% 
  mutate(Lake=replace(Lake,Lake=="SC","Seal Cove Pond"))
data.scores$Season = rep(seasonID,times=2)
data.scores$Season = factor(data.scores$Season,levels=c("Winter","Spring",'Summer',"Autumn"))
data.scores  

#test
stressplot(horn.nmds)
#get distances
horn = vegdist(phy.lv[,4:53], method="horn")
horn
mod = betadisper(horn, data.scores$Lake, type="median")
mod
permutest(mod, pairwise=T) #if the dispersion is different between groups, then examine
plot(mod)
boxplot(mod)

#PERMANOVA - between lakes
p = adonis2(phy.lv[,4:53] ~ LakeID, data=phy.lv); p
#          Df SumOfSqs R2     F       Pr(>F)  
#LakeID    1   0.2937 0.22458 1.7377  0.039 * 


#PERMANOVA - between dates
p = adonis2(phy.lv[,4:53] ~ seasonID, data=phy.lv); p
#          Df SumOfSqs R2     F       Pr(>F)  
#seasonID  3  0.61930 0.47354 1.1993  0.249



#Post-hoc test
## Tukey's Honest Significant Differences
mod.HSD = TukeyHSD(mod); mod.HSD
plot(mod.HSD)

#sp scores
species.scores = as.data.frame(scores(horn.nmds, "species"))  
species.scores$species = rownames(species.scores) 
species.scores

# hulls
a=data.scores[data.scores$Lake=="Jordan Pond",][chull(data.scores[data.scores$Lake=="Jordan Pond",c("NMDS1","NMDS2")]), ]
b=data.scores[data.scores$Lake=="Seal Cove Pond",][chull(data.scores[data.scores$Lake=="Seal Cove Pond",c("NMDS1","NMDS2")]), ]
hull.data=rbind(a,b)  #combine grp.a and grp.b
hull.data

plot.nmds.horn = ggplot() + 
  geom_polygon(data=hull.data,
               aes(x=NMDS1, y=NMDS2,fill=Lake,group=Lake),alpha=0.30,show.legend=F) + 
  geom_text(data=species.scores,
            aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5,size=3) +  
  geom_point(data=data.scores,
             aes(x=NMDS1,y=NMDS2,shape=Season, color=Lake),size=2) + 
  annotate("text", x=0.55, y=0.4, label="italic(R) ^ 2 == 0.22", parse=T, size=4) + #from phy.jp PERMANOVA
  annotate("text", x=0.55, y=0.35, label="italic(p) == 0.039", parse=T, size=4) +
  scale_shape_manual(values=c(19,15,17,18)) + 
  scale_color_manual(values=c('#45B39D','#EC7063')) +
  scale_fill_manual(values=c('#45B39D','#EC7063')) +
  ggtitle(NULL) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.title.x = element_text(size=rel(1)), 
        axis.title.y = element_text(size=rel(1)), 
        plot.title = element_text(size=rel(1)),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank()) ; plot.nmds.horn



ggsave(filename="plots/phyto/div.beta.horn.jpg",
       plot=plot.nmds.horn,
       device="jpg",
       height=8,
       width=16,
       units="in",
       dpi=400)







# Layers #############################################################

# HYPOTHESIS 2:
# We also predicted 2a) greater vertical heterogeneity in the distribution of phytoplankton taxa, 
# and 2b) greater seasonal turnover of taxa in higher-DOC lakes from under-ice through the open water season. 

# Q: How does Phyto structure change between lakes from under ice to open water?
# Q: how similar are layers? VHGs


#get densities for each layer
phy.layers = ddply(phyto.condensed,
                   .(sampleID,
                     LakeID,
                     Date,
                     lakedate,
                     Site,
                     seasonID,
                     vol.settled.ml,
                     trophic.mode,
                     group,
                     taxa), 
                   summarize,
                   cell.count.raw = sum(n),
                   cell.count.conv = cell.count.raw*28,
                   cells.pml = cell.count.conv / vol.settled.ml,
                   phyID = paste(group,taxa,sep='-')) #taxa ID

#wide df for vegan
phy.wide = phy.layers %>% 
  pivot_wider(id_cols = c(sampleID,lakedate,LakeID,Date,Site,seasonID),
              names_from = taxa,
              values_from = cells.pml,
              values_fill = 0)

#check columns
names(phy.wide)
str(phy.wide)

#drop ID cols for vegan
phy.vegan = phy.wide %>% dplyr::select(-sampleID,-lakedate,-LakeID,-Site,-Date,-seasonID)

#save diversity indexes 
phy.wide$H = diversity(phy.vegan, index="shannon") # Shannon H
phy.wide$S = specnumber(phy.vegan) # species richness
phy.wide$P = phy.wide$H /log(phy.wide$S) # Pielou evenness

#summary table
phy.layers.summary = phy.wide %>% 
  dplyr::select(sampleID,lakedate,LakeID,Date,seasonID,Site,H,S,P)



#* overview plots ###########
#diversity metrics, H S and P

#reorder sites
phy.layers.summary$Site = factor(phy.layers.summary$Site,levels=c("Epi","Meta","Hypo"))

#rename seasons
phy.layers.summary = phy.layers.summary %>% 
  mutate(seasonID=replace(seasonID,seasonID=='fall4','Autumn')) %>% 
  mutate(seasonID=replace(seasonID,seasonID=='spring3','Spring')) %>% 
  mutate(seasonID=replace(seasonID,seasonID=='summer3','Summer')) %>% 
  mutate(seasonID=replace(seasonID,seasonID=='winter1','Winter'))
#reorder seasons
phy.layers.summary$seasonID = factor(phy.layers.summary$seasonID,
                                     levels=c("Winter","Spring",'Summer',"Autumn"))
#add lake names
phy.layers.summary = phy.layers.summary %>% 
  mutate(lakename=ifelse(grepl('JP',LakeID),'Jordan Pond',NA)) %>% 
  mutate(lakename=replace(lakename,grepl('SC',LakeID), 'Seal Cove Pond')) 

#set colors
cols = c('#03C66C','#C0A603','#470528')

#sp richness
div1 = ggplot(phy.layers.summary,
              aes(x=Site,y=S,color=Site)) +
  geom_point(size=3) +
  #geom_bar(position=position_dodge(), stat='identity') +
  #geom_jitter(shape=19,size=5,position=position_jitter(width=0.15,height=0)) + 
  facet_grid(rows=vars(lakename),
             cols=vars(seasonID)) +
  scale_color_manual(values=cols,
                     guide=guide_legend('Depth')) +
  labs(x=NULL,
       title='OTU Richness',
       color="Depth") +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        plot.title=element_text(size=rel(1))); div1

#shannon div
div2 = ggplot(phy.layers.summary,
              aes(x=Site,y=H,color=Site)) +
  geom_point(size=3) +
  facet_grid(rows=vars(lakename),
             cols=vars(seasonID)) +
  scale_color_manual(values=cols,
                     guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,3)) +
  labs(x=NULL,
       title='Shannon Diversity',
       color="Depth") +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        plot.title=element_text(size=rel(1))); div2

#pielou evenness
div3 = ggplot(phy.layers.summary,
              aes(x=Site,y=P,color=Site)) +
  geom_point(size=3) +
  facet_grid(rows=vars(lakename),
             cols=vars(seasonID)) +
  scale_color_manual(values=cols,
                     guide=guide_legend("Depth")) +
  scale_y_continuous(limits=c(0,1)) +
  labs(x=NULL,
       title='Evenness',
       color="Depth") +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        plot.title=element_text(size=rel(1))); div3

#save
ggsave(filename="plots/phyto/div.layers.s.jpg",plot=div1,device="jpg",height=8,width=12,units="in",dpi=400)
ggsave(filename="plots/phyto/div.layers.h.jpg",plot=div2,device="jpg",height=8,width=12,units="in",dpi=400)
ggsave(filename="plots/phyto/div.layers.p.jpg",plot=div3,device="jpg",height=8,width=12,units="in",dpi=400)






#* nMDS ############

#* JP single date ######
phy.jp.ice = phy.wide %>% 
  filter(LakeID=='JP') %>% 
  filter(seasonID=='winter1') %>% 
  dplyr::select(-H,-S,-P)

#Morisita-Horn distance 
horn.nmds = metaMDS(phy.jp.ice[,7:56], distance="horn", k=2, trymax=1000)
horn.nmds
plot(horn.nmds, type="t")
#get scores
data.scores = as.data.frame(scores(horn.nmds)) 
#save depths
Depth = c('Epi','Meta','Hypo')
#add layers
data.scores$Depth = Depth
data.scores$Depth = factor(data.scores$Depth,levels=c('Epi','Meta','Hypo'))
data.scores  

#test
stressplot(horn.nmds) #doesn't really work with only 3 points.........
#get distances
jp.horn = vegdist(phy.jp.ice[,7:56], method="horn")
jp.horn
mod = betadisper(jp.horn, data.scores$Depth, type="median")
mod
permutest(mod, pairwise=T) #if the dispersion is different between groups, then examine
plot(mod)
boxplot(mod)
#PERMANOVA
p = adonis2(phy.jp.ice[,7:56] ~ Depth, data=phy.jp.ice); p
#PERMANOVA with horn distances
d = adonis2(jp.horn ~ Depth, data=phy.jp.ice); d

#save sp scores
species.scores = as.data.frame(scores(horn.nmds, "species"))  
species.scores$species = rownames(species.scores) 
species.scores

ggplot() + 
  geom_text(data=species.scores,
            aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5,size=2.5) +  
  geom_point(data=data.scores,
             aes(x=NMDS1,y=NMDS2,shape=Depth),size=4) + 
  scale_shape_manual(values=c(19,15,17,18)) + 
  scale_color_manual(values=c('#45B39D','#EC7063')) +
  scale_fill_manual(values=c('#45B39D','#EC7063')) +
  ggtitle("Morisita-Horn distance") +
  coord_equal() +
  theme_bw() 





#* JP all dates ######
phy.jp = phy.wide %>% 
  filter(LakeID=='JP') %>% dplyr::select(-H,-S,-P)

#run nmds just on sp data
horn.nmds = metaMDS(phy.jp[,7:56], distance="horn", k=2, trymax=1000)
horn.nmds
plot(horn.nmds, type="t")
#get scores
data.scores = as.data.frame(scores(horn.nmds)) 
#add depth and season cols to scores
data.scores$Depth = phy.jp$Site
data.scores$Depth = factor(data.scores$Depth,levels=c('Epi','Meta','Hypo'))
seasonID = c("Winter","Spring",'Summer',"Autumn")
data.scores$Season = rep(seasonID,each=3)
data.scores$Season = factor(data.scores$Season,levels=c("Winter","Spring",'Summer',"Autumn"))
data.scores

#test
stressplot(horn.nmds)
#get distances
jp.horn = vegdist(phy.jp[,7:56], method="horn")
jp.horn
mod = betadisper(jp.horn, data.scores$Season, type="median")
mod
permutest(mod, pairwise=T) #if the dispersion is different between groups, then examine
#Response: Distances
#           Df Sum Sq  Mean Sq  F        N.Perm  Pr(>F)
#Groups     3 0.089446 0.029816 1.5021   999     0.282
#Residuals  8 0.158795 0.019849    

#Pairwise comparisons:
#  (Observed p-value below diagonal, permuted p-value above diagonal)
#Winter (Under-ice)  Spring  Summer  Fall
#Winter (Under-ice)                    0.84200 0.22300 0.288
#Spring                        0.83758         0.19300 0.229
#Summer                        0.21217 0.19547         0.670
#Fall                          0.26935 0.23893 0.64156  
plot(mod)
boxplot(mod)

#PERMANOVA
p = adonis2(phy.jp[,7:56] ~ seasonID, data=phy.jp); p
#Results
#             Df SumOfSqs R2     F      Pr(>F)    
#  SeasonID  3   1.56869 0.66494 5.292  0.001 ***
#  Residual  8   0.79047 0.33506                 
#  Total    11   2.35916 1.00000 


#Post-hoc test
## Tukey's Honest Significant Differences
mod.HSD = TukeyHSD(mod); mod.HSD
plot(mod.HSD)

#get sp scores
species.scores = as.data.frame(scores(horn.nmds, "species"))  
species.scores$species = rownames(species.scores) 
species.scores

#PLOT 
# save hulls
s1 = data.scores[data.scores$Season=="Winter", ][chull(data.scores[data.scores$Season=="Winter", c("NMDS1","NMDS2")]), ]
s2 = data.scores[data.scores$Season=="Spring", ][chull(data.scores[data.scores$Season=="Spring", c("NMDS1","NMDS2")]), ]  
s3 = data.scores[data.scores$Season=="Summer", ][chull(data.scores[data.scores$Season=="Summer", c("NMDS1","NMDS2")]), ]  
s4 = data.scores[data.scores$Season=="Autumn", ][chull(data.scores[data.scores$Season=="Autumn", c("NMDS1","NMDS2")]), ] 
hull.data = rbind(s1,s2,s3,s4)
hull.data

#set colors
cols = c('turquoise','coral1','darkolivegreen3','darkgoldenrod2')

plot.nmds.horn.jp = 
  ggplot() + 
  geom_polygon(data=hull.data,
               aes(x=NMDS1, y=NMDS2,fill=Season,group=Season),alpha=0.30,show.legend=F) + 
  geom_text(data=species.scores,
            aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5,size=3) +  
  geom_point(data=data.scores,
             aes(x=NMDS1,y=NMDS2,shape=Depth, color=Season),size=2) + 
  annotate("text", x=-0.9, y=0.8, label="italic(R) ^ 2 == 0.66", parse=T, size=4) + #from phy.jp PERMANOVA
  annotate("text", x=-0.9, y=0.7, label="italic(p) == 0.001", parse=T, size=4) +
  scale_shape_manual(values=c(19,15,17,18)) + 
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  xlim(-1.1, 1.25) +
  ylim(-1.3, 0.8) +
  ggtitle("Jordan Pond") +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.title.x = element_text(size=rel(1)), 
        axis.title.y = element_text(size=rel(1)), 
        plot.title = element_text(size=rel(1)),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank()) ; plot.nmds.horn.jp





#* SC all dates ######
phy.sc = phy.wide %>% 
  filter(LakeID=='SC') %>% dplyr::select(-H,-S,-P)

#run nmds just on sp data
horn.nmds = metaMDS(phy.sc[,7:56], distance="horn", k=2, trymax=1000)
horn.nmds
plot(horn.nmds, type="t")
#get scores
data.scores = as.data.frame(scores(horn.nmds)) 
#add depth and season cols to scores
data.scores$Depth = phy.jp$Site
data.scores$Depth = factor(data.scores$Depth,levels=c('Epi','Meta','Hypo'))
seasonID = c("Winter","Spring",'Summer',"Autumn")
data.scores$Season = rep(seasonID,each=3)
data.scores$Season = factor(data.scores$Season,levels=c("Winter","Spring",'Summer',"Autumn"))
data.scores

#test
stressplot(horn.nmds)
#get distances
sc.horn = vegdist(phy.sc[,7:56], method="horn")
sc.horn
mod = betadisper(sc.horn, data.scores$Season, type="median")
mod
permutest(mod, pairwise=T) #if the dispersion is different between groups, then examine
plot(mod)
boxplot(mod)
#PERMANOVA
p = adonis2(phy.sc[,7:56] ~ seasonID, data=phy.sc); p
#Results
#            Df SumOfSqs R2     F       Pr(>F)   
#  seasonID  3  1.09805 0.60643 4.1089  0.002 **
#  Residual  8  0.71264 0.39357                 
#  Total    11  1.81068 1.00000  

## Tukey's Honest Significant Differences
mod.HSD = TukeyHSD(mod); mod.HSD
plot(mod.HSD)

#sp scores
species.scores = as.data.frame(scores(horn.nmds, "species"))  
species.scores$species = rownames(species.scores) 
species.scores

#PLOT
# hulls
s1 = data.scores[data.scores$Season=="Winter", ][chull(data.scores[data.scores$Season=="Winter", c("NMDS1","NMDS2")]), ]
s2 = data.scores[data.scores$Season=="Spring", ][chull(data.scores[data.scores$Season=="Spring", c("NMDS1","NMDS2")]), ]  
s3 = data.scores[data.scores$Season=="Summer", ][chull(data.scores[data.scores$Season=="Summer", c("NMDS1","NMDS2")]), ]  
s4 = data.scores[data.scores$Season=="Autumn", ][chull(data.scores[data.scores$Season=="Autumn", c("NMDS1","NMDS2")]), ] 
hull.data = rbind(s1,s2,s3,s4)
hull.data

plot.nmds.horn.sc = 
  ggplot() + 
  geom_polygon(data=hull.data,
               aes(x=NMDS1, y=NMDS2,fill=Season,group=Season),alpha=0.30,show.legend=F) + 
  geom_text(data=species.scores,
            aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5,size=3) +  
  geom_point(data=data.scores,
             aes(x=NMDS1,y=NMDS2,shape=Depth, color=Season),size=2) + 
  annotate("text", x=-0.9, y=1.0, label="italic(R) ^ 2 == 0.61", parse=T, size=4) +
  annotate("text", x=-0.9, y=0.9, label="italic(p) == 0.002", parse=T, size=4) +
  scale_shape_manual(values=c(19,15,17,18)) + 
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  xlim(-1.1, 1.25) +
  ylim(-1.1, 1.0) +
  ggtitle("Seal Cove Pond") +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.title.x = element_text(size=rel(1)), 
        axis.title.y = element_text(size=rel(1)), 
        plot.title = element_text(size=rel(1)),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank()) ; plot.nmds.horn.sc


#combine and save
horn = ggarrange(plot.nmds.horn.jp,
                 plot.nmds.horn.sc,
                 ncol=2,
                 common.legend=T,
                 legend='right'); horn

ggsave(filename="plots/phyto/div.horn.layers.jpg",
       plot=horn,
       device="jpg",
       height=8,
       width=12,
       units="in",
       dpi=400)



#combine all 3
comb = wrap_plots(plot.nmds.horn.jp,
                  plot.nmds.horn.sc,
                  ncol=2,
                  guides='collect')

all = wrap_plots(plot.nmds.horn,
                 comb,
                 ncol=1,
                 heights=c(3,5)); all


ggsave(filename="plots/phyto/div.nmds.jpg",
       plot=all,
       device="jpg",
       height=12,
       width=12,
       units="in",
       dpi=400)






#end part 2








#OLD stuff #####


#**bray #####
#Bray-Curtis distance
bray.nmds = metaMDS(phy.lv[,4:53], distance="bray", k=2, trymax=1000)
bray.nmds
plot(bray.nmds, type="t")

#get scores
data.scores = as.data.frame(scores(bray.nmds))  
data.scores$site = rownames(data.scores)  
data.scores$lakedateID = lakedateID 
data.scores$Lake = lakeID
data.scores=data.scores %>% 
  mutate(Lake=replace(Lake,Lake=="JP","Jordan Pond")) %>% 
  mutate(Lake=replace(Lake,Lake=="SC","Seal Cove Pond"))
data.scores$Season = rep(seasonID,times=2)
data.scores$Season = factor(data.scores$Season,levels=c("Winter","Spring",'Summer',"Autumn"))
data.scores  

#test
stressplot(bray.nmds)
#get distances
bray = vegdist(phy.lv[,4:53], method="bray")
bray
mod = betadisper(bray, data.scores$Lake, type="median")
mod
permutest(mod, pairwise=T) #if the dispersion is different between groups, then examine
plot(mod)
boxplot(mod)

#PERMANOVA
p = adonis2(phy.lv[,4:53] ~ LakeID, data=phy.lv); p
#          Df SumOfSqs R2     F       Pr(>F)  
#LakeID    1   0.2937 0.22458 1.7377  0.059 .
#Residual  6   1.0141 0.77542                
#Total     7   1.3078 1.00000  

#Post-hoc test
## Tukey's Honest Significant Differences
mod.HSD = TukeyHSD(mod); mod.HSD
plot(mod.HSD)

#sp scores
species.scores = as.data.frame(scores(bray.nmds, "species"))  
species.scores$species = rownames(species.scores) 
species.scores

# hulls
a = data.scores[data.scores$Lake=="Jordan Pond",][chull(data.scores[data.scores$Lake=="Jordan Pond",c("NMDS1","NMDS2")]), ]  
b = data.scores[data.scores$Lake=="Seal Cove Pond",][chull(data.scores[data.scores$Lake=="Seal Cove Pond",c("NMDS1","NMDS2")]), ]  
hull.data = rbind(a, b)  #combine grp.a and grp.b
hull.data

plot.nmds.bray = ggplot() + 
  geom_polygon(data=hull.data,
               aes(x=NMDS1, y=NMDS2,fill=Lake,group=Lake),alpha=0.30) + 
  geom_text(data=species.scores,
            aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5,size=2.5,check_overlap=F) +  
  geom_point(data=data.scores,
             aes(x=NMDS1,y=NMDS2,shape=Season, color=Lake),size=4) + 
  scale_shape_manual(values=c(19,15,17,18)) + 
  #scale_color_manual(values=c("A" = "red", "B" = "blue")) +
  ggtitle("Bray-Curtis distance") +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.title.x = element_text(size=rel(1)), 
        axis.title.y = element_text(size=rel(1)), 
        plot.title = element_text(size=rel(1)),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank()); plot.nmds.bray















# Jaccard index #####
j = metaMDS(phy.lv[,4:53], distance="jaccard", k=3, trymax=100)
j2 = metaMDS(phy.lv[,4:53], distance="jaccard", k=3, previous.best = j)
j
stressplot(j)
ordiplot(j, type="none")
points(j, "sites")
ordihull(j, phy.lv$LakeID, draw="polygon", conf.int=0.95, alpha=20, border=NULL, col="purple", label=T)
j

j = vegdist(phy.lv[,4:53], method="jaccard", binary=T, upper=T)
j = as.matrix(j)
j

mod = betadisper(j, phy.lv$LakeID, type="median")
mod
permutest(mod, pairwise=T) #if the dispersion is different between groups, then examine
plot(mod)
boxplot(mod)

#Post-hoc test
## Tukey's Honest Significant Differences
mod.HSD = TukeyHSD(mod); mod.HSD
plot(mod.HSD)











#BONUS HYP ###########

#*load profiles #####
profiles = read.csv("library/profiles.all.csv",header=T)
names(profiles)
sort(unique(profiles$lakedate))

#just SC and JP
profiles = profiles %>% 
  filter(lakedate=='JP_2020-02-21' |
           lakedate=='JP_2020-05-28'| 
           lakedate=='JP_2020-08-04'|
           lakedate=='JP_2020-10-31'|
           lakedate=='SC_2020-02-21'|
           lakedate=='SC_2020-05-28'|
           lakedate=='SC_2020-08-03'|
           lakedate=='SC_2020-11-06')

#subset relevant parameters:
pro = profiles %>%
  dplyr::select(
    #keep lakedate, ID
    lakedate,
    LakeID,
    seasonID,
    Date,
    #Temp: mean, mixing depth, ssi, max buoyancy freq
    temp.mean.c3,
    thermocline.depth.c3,
    schmidt.index,
    buoyancy.freq,
    #DO mean, max depth
    do.mean,
    do.max.depth,
    #Light: kd, depth 1% par, secchi?
    kd,
    z1,
    z1.maxdepth,
    #Chlorophyll RFU: mean, depth max (from Lofton2020 method), 
    dcm.depth.curvefit,
    stand.peak.width,
    chl.avg,
    dcm.maxdepth,
    #nutrient mean and sd
    chla.ugpl.avg, chla.ugpl.sd,
    doc.mgpl.avg, doc.mgpl.sd,
    tp.ugpl.avg, tp.ugpl.sd,
    tn.ugpl.avg, tn.ugpl.sd,
    nh4.ugpl.avg, nh4.ugpl.sd,
    no3.ugpl.avg, no3.ugpl.sd,
    din.ugpl.avg, din.ugpl.sd,
    din.tp.avg, din.tp.sd) 

#drop ID cols for vegan
pro.vegan = pro %>% dplyr::select(-lakedate,-LakeID,-seasonID,-Date)
str(pro.vegan)


#explain phyto com structure using profile vars.... maybe not enough data though


#CCA plot ####
names(pro.vegan)
cca = rda(phy.vegan ~ 
            schmidt_index + 
            z1 +
            tp.ugpl.avg + 
            doc.mgpl.avg + 
            chla.ugpl.avg,
          pro, 
          scale=T)
cca
summary(cca)

plot(cca, scaling=1)
ordihull(cca, pro$LakeID, col="green", label=T)
ordihull(cca, pro$Date, col="gold", label=T)

#significance of within Time and within space variability - PERMANOVA
ado = adonis(phy.vegan ~ LakeID/seasonID, data=pro, method="horn", permutations=999)
ado



mod = cca(phy.vegan ~ 
            schmidt_index + 
            z1 + 
            tp.ugpl.avg + 
            doc.mgpl.avg, 
          pro.vegan)

mod
summary(mod)

data=plot(mod, type="n")
text(mod, dis="cn")
points(mod, pch=21, col="red", bg="yellow", cex=1.2)
text(mod, "species", col="blue", cex=0.8)
head(summary(mod), tail=2)
## Scaling can be numeric or more user-friendly names
## e.g. Hill's scaling for (C)CA
scrs = scores(mod, scaling = "sites", hill = TRUE)
scrs
## or correlation-based scores in PCA/RDA
scrs = scores(rda(phy.lakes.vegan ~ 
                    schmidt_index + 
                    z1 + 
                    tp.ugpl.avg + 
                    doc.mgpl.avg, 
                  pro.vegan),
              scaling="sites", 
              correlation=T)
scrs


#envir fitting #####
names(pro)

pro.sub = pro %>% 
  dplyr::select(temp_mean_c3,
                schmidt_index,
                z1,
                do_mean,
                chl.avg,
                dcm.depth.curvefit,
                stand.peak.width,
                tp.ugpl.avg,
                doc.mgpl.avg,
                chla.ugpl.epi,
                chla.ugpl.meta,
                chla.ugpl.hypo,
                din.tp.avg,
                nh4.ugpl.avg,
                no3.ugpl.avg)

#just vars from subset ^^^
ef = envfit(horn.nmds, pro.sub, permu = 999); ef
plot(horn.nmds, display = "sites")
plot(ef, p.max=0.1)

#everything except thermocline depth because of NAs
pro.all = pro %>% dplyr::select(-thermocline_depth_c3)
ef = envfit(horn.nmds, pro.all, permu = 999); ef
plot(horn.nmds, display = "sites")
plot(ef, p.max=0.1)


#separate lakes
phy.jp = phy.lakes.wide %>% filter(LakeID=='JP') %>% dplyr::select(-lakedate,-LakeID,-seasonID)
phy.sc = phy.lakes.wide %>% filter(LakeID=='SC') %>% dplyr::select(-lakedate,-LakeID,-seasonID)
pro.jp = pro %>% filter(LakeID=='JP')  %>% dplyr::select(-lakedate,-LakeID,-seasonID,-Date)
pro.sc = pro %>% filter(LakeID=='SC')  %>% dplyr::select(-lakedate,-LakeID,-seasonID,-Date)


#Jordan
jp.nmds = metaMDS(phy.jp, distance="horn", k=2, trymax=1000); jp.nmds
pro.jp.sub = pro.jp %>% 
  dplyr::select(temp_mean_c3,
                schmidt_index,
                z1,
                do_mean,
                chl.avg,
                dcm.depth.curvefit,
                stand.peak.width,
                tp.ugpl.avg,
                doc.mgpl.avg,
                chla.ugpl.epi,
                chla.ugpl.meta,
                chla.ugpl.hypo,
                din.tp.avg,
                nh4.ugpl.avg,
                no3.ugpl.avg)

jp.ef = envfit(jp.nmds, pro.jp.sub, permu = 999); jp.ef
#plot
plot(jp.nmds, display="sites")
plot(jp.ef, p.max=0.1)



#Seal Cove
sc.nmds = metaMDS(phy.sc, distance="horn", k=2, trymax=1000); sc.nmds
pro.sc.sub = pro.sc %>% 
  dplyr::select(temp_mean_c3,
                schmidt_index,
                z1,
                do_mean,
                chl.avg,
                dcm.depth.curvefit,
                stand.peak.width,
                tp.ugpl.avg,
                doc.mgpl.avg,
                chla.ugpl.epi,
                chla.ugpl.meta,
                chla.ugpl.hypo,
                din.tp.avg,
                nh4.ugpl.avg,
                no3.ugpl.avg)

sc.ef = envfit(sc.nmds, pro.sc.sub, permu = 999); sc.ef
#plot
plot(sc.nmds, display="sites")
plot(sc.ef, p.max=0.1); plot.sc












stressplot(horn.nmds)
ordiplot(horn.nmds, type = "none")
ordisurf(horn.nmds, pro$schmidt_index, main="",col="forestgreen")
ordihull(horn.nmds, pro$seasonID)
pro$LakeID = factor(pro$LakeID)
points(horn.nmds, "sites", pch = 19, col = pro$LakeID)
points(horn.nmds, "species", pch = 2, col = pro$LakeID)
plot(fit,  col = "gold")




#from vegan 
mod <- cca(dune ~ A1 + Moisture + Management, dune.env)
plot(mod, type="n")
text(mod, dis="cn")
points(mod, pch=21, col="red", bg="yellow", cex=1.2)
text(mod, "species", col="blue", cex=0.8)
## Limited output of 'summary'
head(summary(mod), tail=2)
## Scaling can be numeric or more user-friendly names
## e.g. Hill's scaling for (C)CA
scrs <- scores(mod, scaling = "sites", hill = TRUE)
## or correlation-based scores in PCA/RDA
scrs <- scores(rda(dune ~ A1 + Moisture + Management, dune.env),
               scaling = "sites", correlation = TRUE)



#PCA ####
library(ggfortify)







#*test homogeneity ####
#from vegan tutorial
dis = vegdist(phy.vegan)
groups = factor(c(rep(1,4), rep(2,4)), labels=c("JP","SC")); groups

## Calculate multivariate dispersions

mod = betadisper(dis, groups); mod
## Perform test
anova(mod)

## Permutation test for F
permutest(mod, pairwise=T, permutations=99)
#*****************  reject Null ************************************

#test difference
#adonis and adonis 2 permanova



## Tukey's Honest Significant Differences
mod.HSD = TukeyHSD(mod)
plot(mod.HSD)

#First two PCoA axes
plot(mod)

## with data ellipses instead of hulls
plot(mod, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse
plot(mod, ellipse = TRUE, hull = FALSE, conf = 0.90) # 90% data ellipse

## can also specify which axes to plot, ordering respected
plot(mod, axes = c(3,1), seg.col = "forestgreen", seg.lty = "dashed")

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

## `scores` and `eigenvals` also work
scrs <- scores(mod)
str(scrs)
head(scores(mod, 1:4, display = "sites"))
# group centroids/medians 
scores(mod, 1:4, display = "centroids")
# eigenvalues from the underlying principal coordinates analysis
eigenvals(mod) 

## try out bias correction; compare with mod3
(mod3B <- betadisper(dis, groups, type = "median", bias.adjust=TRUE))
anova(mod3B)
permutest(mod3B, permutations = 99)

## should always work for a single group
group <- factor(rep("JP", NROW(phy.lakes.vegan)))
(tmp <- betadisper(dis, group, type = "median"))
(tmp <- betadisper(dis, group, type = "centroid"))


## Using group centroids
mod3 <- betadisper(dis, groups, type = "centroid")
mod3
permutest(mod3, permutations = 99)
anova(mod3)
plot(mod3)
boxplot(mod3)
plot(TukeyHSD(mod3))


#just one lake
#changes in beta-diversity over time
jp.horn.beta = vegdist(phy.jp, method="horn", upper=T)
jp.horn.beta = as.matrix(jp.horn.beta)

jp.jaccard = vegdist(phy.jp, method="jaccard", binary=T, upper=T)
jp.jaccard = as.matrix(jp.jaccard)

jac.nmds = metaMDS(jp.jaccard, k=2, trymax=1000)
ordiplot(jac.nmds, type = "none")
ordihull(jac.nmds, pro.jp$seasonID)
points(jac.nmds, "sites", pch = 19)























#MORE OLD STUFF ###########################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~



#NMDS ####
#Messing with Vegan  
data(varespec)
vare.dis <- vegdist(varespec)
vare.mds0 <- isoMDS(vare.dis)
stressplot(vare.mds0, vare.dis)
ordiplot(vare.mds0, type = "t")
vare.mds <- metaMDS(varespec, trace = FALSE)
vare.mds
metaMDS(comm = varespec, trace = FALSE)
plot(vare.mds, type = "t")


#phyto 
p.dist = vegdist(phy.lakes.vegan)
p.mds0 = isoMDS(p.dist)
stressplot(p.mds0, p.dist)

ordiplot(p.mds0, type="t")
p.mds = metaMDS(phy., trace=F); p.mds
metaMDS(comm = phy., trace=F)
plot(p.mds, type="t")


#vegan environmental var
data(varechem)
rankindex(scale(varechem), varespec, c("euc", "man", "bray", "jac", "kul"))




#phyto
rankindex(scale(pro.vegan), phy.lakes.vegan, c("euc", "man", "bray", "jac", "kul"))


tmp = wisconsin(sqrt(phy.lakes.vegan))
dis = vegdist(tmp)
p.mds0 = isoMDS(dis, trace = 0)
procrustes(p.mds, p.mds0)
plot(procrustes(p.mds, p.mds0), kind = 2)




#drop most nutrients
pro2 = profiles %>% 
  dplyr::select(
    #keep lakedate, ID
    lakedate,
    LakeID,
    #Temp: mean, mixing depth, ssi, max buoyancy freq
    temp_mean_c3,
    thermocline_depth_c3,
    schmidt_index,
    buoyancy_freq,
    #DO mean, max depth
    do_mean,
    do_max_depth,
    #Light: kd, depth 1% par, secchi?
    kd,
    z1,
    #chlorophyll: mean, depth max (Lofton), 
    dcm.depth.curvefit,
    stand.peak.width,
    chl.avg,
    #Nutrients: for each depth - also averages ???
    chla.ugpl.avg,
    doc.mgpl.avg,
    tp.ugpl.avg,
    nh4.ugpl.avg,
    no3.ugpl.avg,
    din.ugpl.avg,
    din.tp.avg)

pro2. = pro2 %>% dplyr::select(-lakedate,-LakeID)


#VECTOR FITTING ####
ef = envfit(p.mds, pro2., permu=999, na.rm=T);  ef
plot(p.mds, display="sites")
plot(ef, p.max=0.1)










#PCA ####
p.pca = rda(phy., scale=T); p.pca
plot(p.pca)

sum(apply(phy., 2, var))

biplot(p.pca, scaling = 3)
dim(phy.)


#RAREFACTION ####
pr = round(phy., digits = 0)
raremax = min(rowSums(pr))
Srare = rarefy(pr, raremax)
pro$rare = Srare


boxplot(rare~lakedate, data=pro)

rarecurve(pr, step=1, sample=raremax, xlab="Sample Size", ylab="OTU", label=T)





#Diversity ####
pro$shannon = diversity(phy., index="shannon") # Shannon H
pro$richness = specnumber(phy.) # number of species
pro$evenness = pro$shannon/log(pro$richness) # Pielou evenness


boxplot(richness ~ lakedate, pro)
boxplot(shannon ~ lakedate, pro)

plot(z1 ~ evenness, pro)


boxplot(ind ~ Period*) #compare # of individuals among periods and lakes
boxplot(env$MXV ~ Period*Lake, env)
boxplot(chl ~ Period*Lake, env)



