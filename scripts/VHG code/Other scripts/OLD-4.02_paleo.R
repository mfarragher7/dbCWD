#SC pigment core analysis and plots
#2021-02-19



#sources
#https://fishandwhistle.net/post/2018/stratigraphic-diagrams-with-tidypaleo-ggplot2/
#https://paleolimbot.github.io/r4paleolim/strat-diagrams.html



#TO-DO

#add error bars? for averaged years

#fix units
#run PCA, get 2 axes, leave out DOC for PCA
#add PCA 
#add dendro graph
#add total biomass?

#bonus:
#what other trends can I add like DOC?





#libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(tidypaleo)
library(vegan)
library(patchwork)

#load pigments
data = read.csv('dbPhytoplankton/paleo/SC_pigments.csv')
str(data)

#aggregate yearly averages
data = data %>% group_by(Date) %>% summarise_each(list(mean))

#melt
paleo = melt(data, id = c("Date","Code"))
#remove sample number and depth
paleo = paleo %>% filter(!variable %in% 'Sample_number' & !variable %in% 'Depth_cm')
str(paleo)

paleo$variable = as.character(paleo$variable)

#rename
paleo = paleo %>% 
  mutate(variable=replace(variable,variable=='B.carotene','B carotene')) %>% 
  mutate(variable=replace(variable,variable=='Chl_a','Chlorophyll a')) %>% 
  mutate(variable=replace(variable,variable=='Chl_b','Chlorophyll b')) %>% 
  mutate(variable=replace(variable,variable=='Chl_c','Chlorophyll c')) %>% 
  mutate(variable=replace(variable,variable=='Chl_c2','Chlorophyll c2')) %>% 
  mutate(variable=replace(variable,variable=='Lutein_zeaxanthin','Lutein zeaxanthin')) %>% 
  mutate(variable=replace(variable,variable=='Pheophorbide_a','Pheophorbide a')) %>% 
  mutate(variable=replace(variable,variable=='Pheophytin_a','Pheophytin a')) 
  

#reorder alphabetically
paleo$variable = factor(paleo$variable,levels=c('Alloxanthin',
                                                'B carotene',
                                                'Canthaxanthin',
                                                'Chlorophyll a',
                                                'Chlorophyll b',
                                                'Chlorophyll c',
                                                'Chlorophyll c2',
                                                'Diadinoxanthin',
                                                'Diatoxanthin',
                                                'Echinenone',
                                                'Fucoxanthin',
                                                'Lutein zeaxanthin',
                                                'Pheophorbide a',
                                                'Pheophytin a',
                                                'Ratio_Chl_a_pheophytin_a',
                                                'Total_algal_biomass',
                                                'PCA_Axis_1',
                                                'LOI'))


#save just pigment data
pigs = paleo %>%
  filter(!variable %in% 'Total_algal_biomass' &
           !variable %in% 'Ratio_Chl_a_pheophytin_a' &
           !variable %in% 'LOI' &
           !variable %in% 'PCA_Axis_1')

#save other info
other = paleo %>% filter(variable %in% c('Total_algal_biomass','Ratio_Chl_a_pheophytin_a','LOI','PCA_Axis_1'))

#remove Chl a
pigs = pigs %>% filter(!variable %in% 'Chlorophyll a')
#remove pre-1980
pigs = pigs %>% filter(Date>=1995)




#PCA ####
#save wide format df
pigs_wide = pigs %>% 
  pivot_wider(id_cols=Date,names_from=variable,values_from=value) %>% 
  select(!Date)

#save pca df
pca = vegan::rda(pigs_wide,scale=F)
#plot
barplot(as.vector(pca$CA$eig)/sum(pca$CA$eig)) 
#calculate the percent of variance explained by first two axes
sum((as.vector(pca$CA$eig)/sum(pca$CA$eig))[1:2]) #82.8%
#biplot
plot(pca)


#using tidypaleo
pca2 = pigs %>% 
  nested_data(qualifiers=Date, key=variable, value=value) %>%
  nested_prcomp() %>%
  unnest(c(qualifiers, scores)) %>%
  gather(key=component, value=value, starts_with("PC")) %>%
  filter(component %in% c("PC1", "PC2"))


#dendrogram ####
dendro = pigs %>%
  nested_data(qualifiers=Date, key=variable, value=value) %>%
  nested_chclust_coniss()



#DOC ####
lt.doc = read.csv("library/lt.doc.csv",header=T)
doc = lt.doc %>% filter(LakeID %in% "SC") 
doc$Date = as.Date(doc$Date)
doc = doc %>% filter(Date >= "1995-01-01")
doc = doc %>% mutate(LakeID=replace(LakeID,LakeID=="SC","DOC")) 
doc$year = format(as.Date(doc$Date, format="%Y-%m-%d"),"%Y")
#aggregate yearly averages
doc2 = doc %>% group_by(year,LakeID) %>% summarise_each(list(mean))
doc2$year = as.numeric(doc2$year)
str(doc2)



#Diversity
#don't really need this because it's pigments not species. stupid!

#div = pigs %>%
#  group_by(Date) %>%
#  summarise(Richness = sum(length(which(value > 0))))
#simpsons = 1 - sum((relative_abundance_percent / 100) ^ 2)
#hill_n2 = 1 / sum((relative_abundance_percent / 100) ^ 2
#  gather(key = param, value = value, -depth_cm)



#PLOTS ####

# 1995-2016

#pigments
plot.pigs = ggplot(pigs, aes(x=value, y=Date)) +
  #geom_path(size=0.5) +
  geom_col_segsh() +
  facet_abundanceh(vars(variable)) +
  scale_y_continuous(n.breaks=6) + 
  scale_x_continuous(breaks=c(1,5,10,20,30,40,50)) +
  labs(y="Year", x=bquote(mu*"mol "*g^-1*"OM")) +
  theme(panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x=element_text(angle=60)); plot.pigs

#pca
plot.pca = ggplot(pca2, aes(x=value, y=Date)) +
  geom_lineh(size=0.5) +
  facet_geochem_gridh(vars(component)) +
  scale_y_continuous(n.breaks=6) + 
  labs(x=NULL) +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.pca


#dendro
plot.dendro = ggplot() +
  layer_dendrogram(dendro, aes(y=Date)) +
  scale_y_continuous(n.breaks=6) +
  facet_geochem_gridh(vars("CONISS")) +
  labs(x="Dissimilarity") +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.dendro


#doc loess avg
plot.doc = ggplot(doc2, aes(x=year, y=DOC_mgpl)) +
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  facet_wrap(vars(LakeID)) +
  scale_y_continuous(limits=c(3,6),expand=c(0,0)) +
  scale_x_continuous(limits=c(1995,2016),n.breaks=6) + 
  #scale_y_date(breaks=c(seq(from=as.Date("1980-01-01"),
  #                            to=as.Date("2016-10-31"),by='5 years')),
  #            limits=as.Date(c("1980-01-01","2016-10-31")),
  #           date_labels="%Y") +
  labs(y=NULL, x=bquote("DOC (mg "*L^-1*")")) +
  coord_flip() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.doc


#doc avg
plot.doc2 = ggplot(doc2, aes(x=DOC_mgpl, y=year)) +
  geom_lineh(size=0.5) +
  facet_wrap(vars(LakeID)) +
  scale_x_continuous(limits=c(3,6),expand=c(0,0)) +
  scale_y_continuous(limits=c(1995,2016),n.breaks=6) + 
  #scale_y_date(breaks=c(seq(from=as.Date("1980-01-01"),
  #                            to=as.Date("2016-10-31"),by='5 years')),
  #            limits=as.Date(c("1980-01-01","2016-10-31")),
  #           date_labels="%Y") +
  labs(y=NULL, x=bquote("DOC (mg "*L^-1*")")) +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.doc2






#combine
full = 
  
  wrap_plots(
  
  plot.pigs + 
    theme(strip.background=element_blank(), strip.text.y=element_blank()),

  plot.pca +
    theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()) +
    labs(y=NULL),
  
  plot.dendro +
    theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()) +
    labs(y=NULL),
  
  plot.doc2 +
    theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()) + 
    labs(y=NULL),
  
  ncol=4,
  nrow=1,
  widths=c(6,1,1,1)
  
  ); full



#save
ggsave(filename="plots/paleo/sc.paleo4.png",
       plot=full,
       device="png",
       height=8,
       width=12,
       units="in",
       dpi=400)








#version 2 #####

#load data, don't average anything

#load pigments
data = read.csv('dbPhytoplankton/paleo/SC_pigments.csv')
str(data)


#melt
paleo = melt(data, id = c("Date","Code","Depth_cm"))
#remove sample number and depth
paleo = paleo %>% filter(!variable %in% 'Sample_number')
str(paleo)

paleo$variable = as.character(paleo$variable)

#rename
paleo = paleo %>% 
  mutate(variable=replace(variable,variable=='B.carotene','B carotene')) %>% 
  mutate(variable=replace(variable,variable=='Chl_a','Chlorophyll a')) %>% 
  mutate(variable=replace(variable,variable=='Chl_b','Chlorophyll b')) %>% 
  mutate(variable=replace(variable,variable=='Chl_c','Chlorophyll c')) %>% 
  mutate(variable=replace(variable,variable=='Chl_c2','Chlorophyll c2')) %>% 
  mutate(variable=replace(variable,variable=='Lutein_zeaxanthin','Lutein zeaxanthin')) %>% 
  mutate(variable=replace(variable,variable=='Pheophorbide_a','Pheophorbide a')) %>% 
  mutate(variable=replace(variable,variable=='Pheophytin_a','Pheophytin a')) 


#reorder alphabetically
paleo$variable = factor(paleo$variable,levels=c('Alloxanthin',
                                                'B carotene',
                                                'Canthaxanthin',
                                                'Chlorophyll a',
                                                'Chlorophyll b',
                                                'Chlorophyll c',
                                                'Chlorophyll c2',
                                                'Diadinoxanthin',
                                                'Diatoxanthin',
                                                'Echinenone',
                                                'Fucoxanthin',
                                                'Lutein zeaxanthin',
                                                'Pheophorbide a',
                                                'Pheophytin a',
                                                'Ratio_Chl_a_pheophytin_a',
                                                'Total_algal_biomass',
                                                'PCA_Axis_1',
                                                'LOI'))


#save just pigment data
pigs = paleo %>%
  filter(!variable %in% 'Total_algal_biomass' &
           !variable %in% 'Ratio_Chl_a_pheophytin_a' &
           !variable %in% 'LOI' &
           !variable %in% 'PCA_Axis_1')

#save other info
other = paleo %>% filter(variable %in% c('Total_algal_biomass','Ratio_Chl_a_pheophytin_a','LOI','PCA_Axis_1'))

#remove Chl a
pigs = pigs %>% filter(!variable %in% 'Chlorophyll a')
#remove pre-1980
pigs = pigs %>% filter(Date>=1995)




#PCA ####
#save wide format df
pigs_wide = pigs %>% 
  pivot_wider(id_cols=Date,names_from=variable,values_from=value) %>% 
  select(!Date)

#save pca df
pca = vegan::rda(pigs_wide,scale=F)
#plot
barplot(as.vector(pca$CA$eig)/sum(pca$CA$eig)) 
#calculate the percent of variance explained by first two axes
sum((as.vector(pca$CA$eig)/sum(pca$CA$eig))[1:2]) #82.8%
#biplot
plot(pca)


#using tidypaleo
pca2 = pigs %>% 
  nested_data(qualifiers=Date, key=variable, value=value) %>%
  nested_prcomp() %>%
  unnest(c(qualifiers, scores)) %>%
  gather(key=component, value=value, starts_with("PC")) %>%
  filter(component %in% c("PC1", "PC2"))


#dendrogram ####
dendro = pigs %>%
  nested_data(qualifiers=Date, key=variable, value=value) %>%
  nested_chclust_coniss()



#DOC ####
lt.doc = read.csv("library/lt.doc.csv",header=T)
doc = lt.doc %>% filter(LakeID %in% "SC") 
doc$Date = as.Date(doc$Date)
doc = doc %>% filter(Date >= "1995-01-01")
doc = doc %>% mutate(LakeID=replace(LakeID,LakeID=="SC","DOC")) 
doc$year = format(as.Date(doc$Date, format="%Y-%m-%d"),"%Y")
#aggregate yearly averages
doc2 = doc %>% group_by(year,LakeID) %>% summarise_each(list(mean))
doc2$year = as.numeric(doc2$year)
str(doc2)



# 1995-2016

#pigments
plot.pigs = ggplot(pigs, aes(x=value, y=Year)) +
  geom_col_segsh() +
  facet_abundanceh(vars(variable)) +
  scale_y_reverse(n.breaks=8) + 
  scale_x_continuous(breaks=c(1,5,10,20,30,40,50)) +
  labs(y="Depth (cm)", x=bquote(mu*"mol "*g^-1*"OM")) +
  theme(panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x=element_text(angle=60)); plot.pigs

#pca
plot.pca = ggplot(pca2, aes(x=value, y=Date)) +
  geom_lineh(size=0.5) +
  facet_geochem_gridh(vars(component)) +
  scale_y_continuous(n.breaks=8) + 
  labs(x=NULL) +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.pca


#dendro
plot.dendro = ggplot() +
  layer_dendrogram(dendro, aes(y=Date)) +
  scale_y_continuous(n.breaks=8) +
  facet_geochem_gridh(vars("CONISS")) +
  labs(x="Dissimilarity") +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.dendro


#doc avg
plot.doc = ggplot(doc, aes(x=DOC_mgpl, y=year)) +
  geom_lineh(size=0.5) +
  facet_wrap(vars(LakeID)) +
  scale_x_continuous(limits=c(3,6),expand=c(0,0)) +
  scale_y_continuous(limits=c(1995,2016),n.breaks=8) + 
  #scale_y_date(breaks=c(seq(from=as.Date("1980-01-01"),
  #                            to=as.Date("2016-10-31"),by='5 years')),
  #            limits=as.Date(c("1980-01-01","2016-10-31")),
  #           date_labels="%Y") +
  labs(y=NULL, x=bquote("DOC (mg "*L^-1*")")) +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.doc


