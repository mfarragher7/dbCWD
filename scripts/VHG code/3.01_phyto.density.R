#Phyto summary stats
#2021-03-15 03:10 lol


#libraries
library(plyr)
library(dplyr)
library(vegan)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(pals)



#load data
phyto = read.csv("library/db.phyto.csv",header=T)
str(phyto)


#remove non-algae
phyto = phyto %>% filter(!taxa=='blue blob'& !taxa=='brown blob'& !taxa=='green blob')
#remove chytrids
phyto = phyto %>% filter(!group=='chytrids')
sort(unique(phyto$taxa))


#DENSITY ####

#Conversion factor notes:
#490.63	mm^2 total slide area
#25	mm diameter of slide
#0.35	mm width transect
#8.75	mm^2 area one transect
#17.5	mm^2 area 2 transects
#28.036	conv factor. Multiply phyto count * conv factor for estimated total slide count 
#if there's 1680 cells in the 2 counted transects…	
#1680	cells in 17.5 mm^2
#47100.48	cells in 490 mm^2
#and if I settled 25 ml…	
#25	ml
#1884.0192	cells/ml


density.groups = ddply(phyto,
                       .(sampleID,
                         LakeID,
                         Date,
                         lakedate,
                         Site,
                         seasonID,
                         vol.settled.ml,
                         group), 
                       summarize,
                       cell.count.raw = sum(n),
                       cell.count.conv = cell.count.raw*28) #for 2 transects each

#cells per ml (depending on vol settled. 25ml except for one)
density.groups$cells.pml = density.groups$cell.count.conv / density.groups$vol.settled.ml

#check
sort(unique(density.groups$group))


#reorder groups
density.groups$group = factor(density.groups$group,
                              levels=c("chlorophyta",
                                       "chrysophyta",
                                       "cryptophyta",
                                       "cyanophyta",  
                                       "diatoms",
                                       "dinophyta",
                                       "euglena",
                                       "haptophyta",
                                       "synurophyta",
                                       "unid. cysts",
                                       "unid. non-flagellated"))

#reorder sites
density.groups$Site = factor(density.groups$Site,levels=c("Epi","Meta","Hypo"))

#rename seasons
density.groups = density.groups %>% 
  mutate(seasonID=replace(seasonID,seasonID=='fall4','Autumn')) %>% 
  mutate(seasonID=replace(seasonID,seasonID=='spring3','Spring')) %>% 
  mutate(seasonID=replace(seasonID,seasonID=='summer3','Summer')) %>% 
  mutate(seasonID=replace(seasonID,seasonID=='winter1','Winter'))
#reorder seasons
density.groups$seasonID = factor(density.groups$seasonID,
                                 levels=c("Winter","Spring",'Summer',"Autumn"))

#add lake names
density.groups = density.groups %>% 
  mutate(lakename=ifelse(grepl('JP',LakeID),'Jordan Pond',NA)) %>% 
  mutate(lakename=replace(lakename,grepl('SC',LakeID), 'Seal Cove Pond')) 
#mutate(lakename=replace(lakename,grepl('WH',LakeID), 'Witch Hole Pond')) %>% 
#mutate(lakename=replace(lakename,grepl('BB',LakeID), 'Bubble Pond')) 




#* figures ####

#* density plot #####
#all dates all phyto 

all.groups = ggplot(density.groups,
                    aes(x=Site,y=cells.pml,fill=group)) +
  geom_bar(position='stack',stat='identity') +
  facet_grid(rows=vars(lakename),
             cols=vars(seasonID)) +
  scale_y_continuous(limits=c(0,2500),n.breaks=6) +
  scale_fill_manual(values=as.vector(cubehelix(n=12,gamma=0.85))) +
  labs(x=NULL,y=bquote("Cell density (cells "*mL^-1*")"),fill="Taxa") +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text.x=element_text(hjust=0),
        strip.text.y=element_blank(),
        plot.title=element_text(size=rel(1))); all.groups

#annotation
labels = ggplot(density.groups,aes(x=NULL,y=NULL)) +
  xlab(NULL) +
  ylab(NULL) +
  annotate('text',x=0,y=0.2,label=' ',size=rel(4)) +
  annotate('text',x=0,y=0.12,label='Jordan Pond',size=rel(4)) +
  annotate('text',x=0,y=-0.1,label='Seal Cove Pond',size=rel(4)) +
  annotate('text',x=0,y=-0.2,label=' ',size=rel(4)) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank()); labels




p = wrap_plots(labels,
               all.groups,
               ncol=2,
               widths=c(2,7)); p


#save
ggsave(
  filename="plots/phyto/all.phyto.jpg",
  plot=p, 
  device="jpg",
  height=8, 
  width=14, 
  units="in", 
  dpi=400)


#save
ggsave(
  filename="plots/phyto/all.phyto2.jpg",
  plot=p, 
  device="jpg",
  height=5, 
  width=10, 
  units="in", 
  dpi=400)







#Taxa density ####

#get taxa
density.taxa = ddply(phyto,
                     .(sampleID,
                       LakeID,
                       Date,
                       lakedate,
                       Site,
                       seasonID,
                       vol.settled.ml,
                       group,
                       taxa), 
                     summarize,
                     cell.count.raw = sum(n),
                     cell.count.conv = cell.count.raw*28) #for 2 transects
#cells per ml
density.taxa$cells.pml = density.taxa$cell.count.conv / density.taxa$vol.settled.ml

#reorder taxa?
#density.taxa$taxa.clean = factor(density.taxa$taxa.clean, levels=c(""))

#reorder sites
density.taxa$Site = factor(density.taxa$Site,levels=c("Epi","Meta","Hypo"))

#rename seasons
density.taxa = density.taxa %>% 
  mutate(seasonID=replace(seasonID,seasonID=='fall4','Autumn')) %>% 
  mutate(seasonID=replace(seasonID,seasonID=='spring3','Spring')) %>% 
  mutate(seasonID=replace(seasonID,seasonID=='summer3','Summer')) %>% 
  mutate(seasonID=replace(seasonID,seasonID=='winter1','Winter'))

#reorder seasons
density.taxa$seasonID = factor(density.taxa$seasonID, 
                               levels=c("Winter","Spring",'Summer',"Autumn"))

#add lake names
density.taxa = density.taxa %>% 
  mutate(lakename=ifelse(grepl('JP',LakeID),'Jordan Pond',NA)) %>% 
  mutate(lakename=replace(lakename,grepl('SC',LakeID), 'Seal Cove Pond')) 
#mutate(lakename=replace(lakename,grepl('WH',LakeID), 'Witch Hole Pond')) %>% 
#mutate(lakename=replace(lakename,grepl('BB',LakeID), 'Bubble Pond')) 




#* figures #####

#* diatoms ####
d1 = ggplot(filter(density.taxa,group=='diatoms'),
           aes(x=Site,y=cells.pml,fill=taxa)) +
  geom_bar(position='stack',stat='identity') +
  facet_grid(rows=vars(lakename),
             cols=vars(seasonID)) +
  scale_y_continuous(limits=c(0,500),n.breaks=6) +
  scale_fill_manual(values=as.vector(cubehelix(n=11,gamma=0.85))) +
  labs(title='Bacillariophyta',
       x=NULL,
       y=bquote("Cell density (cells "*mL^-1*")"),
       fill="Taxa") +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        plot.title=element_text(size=rel(1))); d1

#save
ggsave(
  filename="plots/phyto/diatoms.jpg",
  plot=d1, 
  device="jpg",
  height=8, 
  width=14, 
  units="in", 
  dpi=400)




#* greens ####
chl1 = ggplot(filter(density.taxa,group=='chlorophyta'),
            aes(x=Site,y=cells.pml,fill=taxa)) +
  geom_bar(position='stack',stat='identity') +
  facet_grid(rows=vars(lakename),
             cols=vars(seasonID)) +
  scale_y_continuous(limits=c(0,300),n.breaks=6) +
  scale_fill_manual(values=as.vector(cubehelix(n=15,gamma=0.85))) +
  labs(title='Chlorophyta',
       x=NULL,
       y=bquote("Cell density (cells "*mL^-1*")"),
       fill="Taxa") +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        plot.title=element_text(size=rel(1))); chl1

#save
ggsave(
  filename="plots/phyto/chlorophyta.jpg",
  plot=chl1, 
  device="jpg",
  height=8, 
  width=14, 
  units="in", 
  dpi=400)



#* chryso / hapto ####
d2 = density.taxa
d2$taxa = factor(d2$taxa,
                 levels=c("bitrichia",
                          "chromulina",
                          "chrysochromulina",
                          "cf chrysochromulina",
                          "chrysolykos",
                          "dinobryon",
                          "kephyrion"))

ch1 = ggplot(filter(d2,
                    group=='chrysophyta'|
                    group=='haptophyta'),
               aes(x=Site,y=cells.pml,fill=taxa)) +
  geom_bar(position='stack',stat='identity') +
  facet_grid(rows=vars(lakename),
             cols=vars(seasonID)) +
  scale_y_continuous(limits=c(0,2000),n.breaks=6) +
  scale_fill_manual(values=as.vector(cubehelix(n=8,gamma=0.85))) +
  labs(title='Chrysophyta and haptophyta',
       x=NULL,
       y=bquote("Cell density (cells "*mL^-1*")"),
       fill="Taxa") +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        plot.title=element_text(size=rel(1))); ch1

#save
ggsave(
  filename="plots/phyto/chryso.hapto.jpg",
  plot=ch1, 
  device="jpg",
  height=8, 
  width=14, 
  units="in", 
  dpi=400)



#* dino / synura / euglena ####
des = ggplot(filter(density.taxa,
                    group=='dinophyta'|
                      group=='euglena'|
                      group=='synurophyta'),
             aes(x=Site,y=cells.pml,fill=taxa)) +
  geom_bar(position='stack',stat='identity') +
  facet_grid(rows=vars(lakename),
             cols=vars(seasonID)) +
  scale_y_continuous(limits=c(0,400),n.breaks=6) +
  scale_fill_manual(values=as.vector(cubehelix(n=8,gamma=0.85))) +
  labs(title='Dinophyta, Euglena, Synurophyta ',
       x=NULL,
       y=bquote("Cell density (cells "*mL^-1*")"),
       fill="Taxa") +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        plot.title=element_text(size=rel(1))); des

#save
ggsave(
  filename="plots/phyto/dino.eu.syn.jpg",
  plot=des, 
  device="jpg",
  height=8, 
  width=14, 
  units="in", 
  dpi=400)



#* cyano ####
cc = ggplot(filter(density.taxa,group=='cyanophyta'),
             aes(x=Site,y=cells.pml,fill=taxa)) +
  geom_bar(position='stack',stat='identity') +
  facet_grid(rows=vars(lakename),
             cols=vars(seasonID)) +
  scale_y_continuous(limits=c(0,150),n.breaks=6) +
  scale_fill_manual(values=as.vector(cubehelix(n=9,gamma=0.85))) +
  labs(title='Cyanophyta',
       x=NULL,
       y=bquote("Cell density (cells "*mL^-1*")"),
       fill="Taxa") +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        plot.title=element_text(size=rel(1))); cc

#save
ggsave(
  filename="plots/phyto/cyano.jpg",
  plot=cc, 
  device="jpg",
  height=8, 
  width=14, 
  units="in", 
  dpi=400)



#* crypto ####
d2 = density.taxa
d2$taxa = factor(d2$taxa,
                 levels=c("cryptomonas",
                          "plagioselmis",
                          "rhodomonas",
                          "unid. cryptophyte",
                          "katablepharid 1",
                          "katablepharid 2",
                          "katablepharid 3"))

c1 = ggplot(filter(d2,group=='cryptophyta'),
            aes(x=Site,y=cells.pml,fill=taxa)) +
  geom_bar(position='stack',stat='identity') +
  facet_grid(rows=vars(lakename),
             cols=vars(seasonID)) +
  scale_y_continuous(limits=c(0,500),n.breaks=6) +
  scale_fill_manual(values=as.vector(cubehelix(n=8,gamma=0.85))) +
  labs(title='Cryptophyta',
       x=NULL,
       y=bquote("Cell density (cells "*mL^-1*")"),
       fill="Taxa") +
  theme_bw() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        plot.title=element_text(size=rel(1))); c1

#save
ggsave(
  filename="plots/phyto/crypto.jpg",
  plot=c1, 
  device="jpg",
  height=8, 
  width=14, 
  units="in", 
  dpi=400)





# other shit? fungi, unknowns, etc




