#SC pigment core analysis and plots
#2021-04-16



#sources
#https://fishandwhistle.net/post/2018/stratigraphic-diagrams-with-tidypaleo-ggplot2/
#https://paleolimbot.github.io/r4paleolim/strat-diagrams.html



#TO-DO

#add error bars for averaged years
#run PCA, get 2 axes, leave out DOC for PCA
#add PCA 
#add dendro graph
#add DOC


#libraries
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(vegan)
library(tidypaleo)
library(patchwork)



# pigments ####
data = read.csv('dbPaleo/SC_pigments.csv')
str(data)


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
paleo$variable = factor(paleo$variable,
                        levels=c('Alloxanthin',
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


#save just pigment data, remove Chl a, remove pre-1995
pigs = paleo %>%
  filter(!variable %in% 'Total_algal_biomass' &
         !variable %in% 'Ratio_Chl_a_pheophytin_a' & 
         !variable %in% 'LOI' & 
         !variable %in% 'PCA_Axis_1' &
         !variable %in% 'Chlorophyll a') %>% 
  filter(Date>=1995) 


#aggregate yearly averages in new df
pigs_avg = ddply(pigs, .(Date,variable), summarize,
                  value_avg = mean(value),
                  value_sd = sd(value))





# diatoms ####
diatoms = read.csv("dbPaleo/SC_diatoms.csv",header=T)
str(diatoms)

#aggregate yearly averages
d.avg = ddply(diatoms,.(Date),summarize,
              MDI = mean(MDI))
# use 'P:B' if you really wanna average together pb ratio
#longer
d.avg2 = melt(d.avg, id = c("Date"))
mdi1=d.avg2


#DOC ####
lt.doc = read.csv("library/lt.doc.csv",header=T)
doc = lt.doc %>% filter(LakeID %in% "SC") 
doc$Date = as.Date(doc$Date)
doc = doc %>% filter(Date >= "1995-01-01")
doc = doc %>% mutate(LakeID=replace(LakeID,LakeID=="SC","DOC")) 
doc$year = format(as.Date(doc$Date, format="%Y-%m-%d"),"%Y")
doc$year = as.numeric(doc$year)
#aggregate yearly averages. new df
doc2 = doc %>% group_by(year,LakeID) %>% summarise_each(list(mean))
doc2$year = as.numeric(doc2$year)
str(doc2)

#this didn't work
#doc_avg = ddply(pigs, .(Date,variable,LakeID), summarize,
#      value_avg = mean(value),
#      value_sd = sd(value))



#correlate MDI and DOC
m = d.avg2 %>% filter(Date>1994) %>% dplyr::rename(year=Date) %>% dplyr::rename(mdi=value)
df = join(doc2,m,by="year")
names(df)
ggplot(df,aes(x=DOC_mgpl,y=mdi)) + geom_point()
cor.test(df$DOC_mgpl, df$mdi, method="pearson")
#data:  df$DOC_mgpl and df$mdi
#t = 0.38143, df = 15, p-value = 0.7082
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.4015504  0.5526224
#sample estimates:
#  cor 
#0.09801073 


#mess with MDI
#add up aula without A. ambigua
d = diatoms
d = d %>% dplyr::select(-Aulacoseira.ambigua)
names(d)
str(d)

d$aula = apply(d[,c(7:15)], 1, sum)

d$aula
d$Aulacoseira.spp.

d$mdi2 = d$aula / d$Discostella.stelligera
d$mdi2

ggplot() + 
  geom_line(filter(d,Date>'1995-01-01'),mapping=aes(x=Date,y=mdi2,color='red')) +
  geom_line(filter(d,Date>'1995-01-01'),mapping=aes(x=Date,y=MDI,color='blue')) 
  

#now try correlation again
#aggregate yearly averages
d2 = ddply(d,.(Date),summarize, MDI = mean(mdi2))
d3 = melt(d2, id = c("Date"))
d3$value
m = d3 %>% filter(Date>1994) %>% dplyr::rename(year=Date) %>% dplyr::rename(mdi2=value)
df = join(doc2,m,by="year")
names(df)
ggplot(df,aes(x=DOC_mgpl,y=mdi2)) + geom_point()
cor.test(df$DOC_mgpl, df$mdi2, method="pearson")
#data:  df$DOC_mgpl and df$mdi2
#t = 0.10379, df = 15, p-value = 0.9187
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.4597752  0.5009837
#sample estimates:
#  cor 
#0.02678977



#Use M1+1 / M2+1 equation
d$mdi2 = (d$aula + 1) / (d$Discostella.stelligera + 1)
d$mdi2
d2 = ddply(d,.(Date),summarize, MDI = mean(mdi2))
d3 = melt(d2, id = c("Date"))
d3$value
m = d3 %>% filter(Date>1994) %>% dplyr::rename(year=Date) %>% dplyr::rename(mdi2=value)
df = join(doc2,m,by="year")
names(df)
ggplot(df,aes(x=DOC_mgpl,y=mdi2)) + geom_point()
cor.test(df$DOC_mgpl, df$mdi2, method="pearson")
#t = 0.13533, df = 15, p-value = 0.8941
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.4533322  0.5070556
#sample estimates:
#  cor 
#0.03492174 




#try just A subarctica and d steligera
#d$mdi3 = d$Aulacoseira.subarctica / d$Discostella.stelligera
#d$mdi3
#ggplot() + 
 # geom_line(filter(d,Date>'1995-01-01'),mapping=aes(x=Date,y=mdi2,color='red')) +
  #geom_line(filter(d,Date>'1995-01-01'),mapping=aes(x=Date,y=MDI,color='blue')) +
  #geom_line(filter(d,Date>'1995-01-01'),mapping=aes(x=Date,y=mdi3,color='gold')) 



#d2 = ddply(d,.(Date),summarize,MDI=mean(mdi3))
#d3 = melt(d2,id=c("Date"))
#d3$value

#m = d3 %>% filter(Date>1994) %>% dplyr::rename(year=Date) %>% dplyr::rename(mdi3=value)
#df = join(doc2,m,by="year")
##names(df)
#ggplot(df,aes(x=DOC_mgpl,y=mdi3)) + geom_point()

#cor.test(df$DOC_mgpl, df$mdi3, method="pearson")
#t = 1.416, df = 15, p-value = 0.1772
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.1643923  0.7072935
#sample estimates:
#  cor 
#0.3433847 

#best one


#other formula



#try just A subarctica and d steligera
#d$mdi4 = (d$Aulacoseira.subarctica+1) / (d$Discostella.stelligera+1)
#d$mdi4

#d2 = ddply(d,.(Date),summarize,MDI=mean(mdi4))
#d3 = melt(d2,id=c("Date"))
#d3$value
#m = d3 %>% filter(Date>1994) %>% dplyr::rename(year=Date) %>% dplyr::rename(mdi3=value)
#df = join(doc2,m,by="year")
#names(df)
#ggplot(df,aes(x=DOC_mgpl,y=mdi3)) + geom_point()
#cor.test(df$DOC_mgpl, df$mdi3, method="pearson")

#t = 1.6843, df = 15, p-value = 0.1128
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.1012428  0.7379914
#sample estimates:
#  cor 
#0.3988087 





#PCA ####
#save wide format df
pigs_wide = pigs_avg %>% 
  pivot_wider(id_cols=Date,names_from=variable,values_from=value_avg)

#using vegan
#save pca df
pca_vegan = vegan::rda(pigs_wide,scale=F)
#plot
barplot(as.vector(pca_vegan$CA$eig)/sum(pca_vegan$CA$eig)) 
#calculate the percent of variance explained by first two axes
sum((as.vector(pca_vegan$CA$eig)/sum(pca_vegan$CA$eig))[1:2]) #88.8%
#biplot
plot(pca_vegan)

#without date
pigs_wide_no_date = pigs_avg %>% 
  pivot_wider(id_cols=Date,names_from=variable,values_from=value_avg) %>% select(-Date)
#save df
pca_vegan_no_date = vegan::rda(pigs_wide_no_date,scale=F)
#plot
barplot(as.vector(pca_vegan_no_date$CA$eig)/sum(pca_vegan_no_date$CA$eig)) 
#calculate the percent of variance explained by first two axes
sum((as.vector(pca_vegan_no_date$CA$eig)/sum(pca_vegan_no_date$CA$eig))[1:2]) #88.8%
#biplot
plot(pca_vegan_no_date)

sum((as.vector(pca_vegan_no_date$CA$eig)/sum(pca_vegan_no_date$CA$eig))[1]) # 51.47 %
sum((as.vector(pca_vegan_no_date$CA$eig)/sum(pca_vegan_no_date$CA$eig))[2]) # 28.56 %



#test correlation between PC Axis 1 and DOC
summary(pca_vegan_no_date)
pc1.scores = scores(pca_vegan_no_date, choices=c(1))
pc1 = pc1.scores$sites

df = df %>% filter(year <= 2016)
df$pc1 = pc1

ggplot(df,aes(x=DOC_mgpl,y=pc1)) + geom_point()
ggplot(df,aes(x=mdi2,y=pc1)) + geom_point()

cor.test(df$DOC_mgpl, df$pc1, method="pearson")
cor.test(df$mdi2, df$pc1, method="pearson")



pc2.scores = scores(pca_vegan_no_date, choices=c(2))
pc2 = pc2.scores$sites

df$pc2 = pc2

cor.test(df$DOC_mgpl, df$pc2, method="pearson")





#things to report 
#Axis 1 = 51.47 % explained
#Cumulative proportion first 2 axes = 88.03 % explained
#DOC MDI cor = 0.0349
#DOC PC1 cor = 0.1612
#MDI PC1 cor = -0.035






#using tidypaleo
pca_tidypaleo = pigs_avg %>% 
  nested_data(qualifiers=Date, key=variable, value=value_avg) %>%
  nested_prcomp() %>%
  unnest(c(qualifiers, scores)) %>%
  gather(key=component, value=value_avg, starts_with("PC")) %>%
  filter(component %in% c("PC1", "PC2"))

#dendrogram ####
dendro = pigs_avg %>%
  nested_data(qualifiers=Date, key=variable, value=value_avg) %>%
  nested_chclust_coniss()



#Figs ####

#pigments averaged
plot.pigs = 
  ggplot(pigs_avg, aes(x=value_avg, y=Date)) +
  geom_path(size=0.5) +
  geom_col_segsh() +
  #geom_errorbar(aes(xmin=-value_sd,xmax=value_sd),width=5) +
  facet_abundanceh(vars(variable)) +
  scale_y_continuous(n.breaks=6) + 
  scale_x_continuous(breaks=c(1,5,10,20,30,40,50)) +
  labs(y="Year", x=bquote(mu*"mol "*g^-1*"OM")) +
  theme(panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x=element_text(angle=60)); plot.pigs


#pigs not averaged
pigs.duplicates = pigs %>% 
  mutate(pig.year=paste(Date,variable,sep="_")) %>% 
  filter(duplicated(pig.year) | duplicated(pig.year, fromLast = TRUE))

plot.pigs1 = 
  ggplot(pigs.duplicates, aes(x=value, y=Date)) +
  geom_point(color='gray60',size=1,shape=1) +
  facet_abundanceh(vars(variable)) +
  geom_path(data=pigs_avg,mapping=aes(x=value_avg,y=Date)) +
  scale_y_continuous(n.breaks=6) + 
  scale_x_continuous(breaks=c(1,5,10,20,30,40,50)) +
  labs(y="Year", x=bquote("Pigment concentration ("*mu*"mol "*g^-1*"OM)")) +
  theme(panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x=element_text(angle=60)); plot.pigs1


#pigments with error bars
plot.pigs2 = 
  ggplot(pigs_avg, aes(y=value_avg, x=Date)) +
  geom_path(size=0.5) +
  #geom_col_segsh() +
  geom_errorbar(aes(ymin=value_avg-value_sd,
                    ymax=value_avg+value_sd),
                    width=0.5) +
  facet_abundanceh(vars(variable)) +
  scale_x_continuous(n.breaks=6) + 
  scale_y_continuous(breaks=c(1,5,10,20,30,40,50)) +
  labs(x="Year", y=bquote(mu*"mol "*g^-1*"OM")) +
  coord_flip() +
  theme(panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.y=element_text(angle=60)); plot.pigs2


#pca
plot.pca = 
  ggplot(pca_tidypaleo, aes(x=value_avg, y=Date)) +
  geom_lineh(size=0.5) +
  facet_geochem_gridh(vars(component),rotate_axis_labels=0) +
  scale_y_continuous(n.breaks=6) + 
  labs(x=NULL) +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.pca


#dendro
plot.dendro = 
  ggplot() +
  layer_dendrogram(dendro, aes(y=Date)) +
  scale_y_continuous(n.breaks=6) +
  facet_geochem_gridh(vars("CONISS"),rotate_axis_labels=0) +
  labs(x="Dispersion") +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.dendro


#doc loess
plot.doc = 
  ggplot(doc, aes(x=year, y=DOC_mgpl)) +
  geom_point(color='gray60',size=1,shape=1) +
  stat_smooth(method="loess",size=0.5,color='black',se=F) + 
  facet_wrap(vars(LakeID)) +
  scale_y_continuous(limits=c(3,6),expand=c(0,0)) +
  scale_x_continuous(limits=c(1995,2016),n.breaks=6) + 
  labs(x=NULL, y=bquote("DOC (mg "*L^-1*")")) +
  coord_flip() +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.doc

#doc both
plot.doc1 = 
  ggplot(doc, aes(y=year, x=DOC_mgpl)) +
  geom_point(color='gray60',size=1,shape=1) +
  geom_lineh(doc2,mapping=aes(y=year,x=DOC_mgpl)) +
  facet_wrap(vars(LakeID)) +
  scale_x_continuous(limits=c(3,6),expand=c(0,0)) +
  scale_y_continuous(limits=c(1995,2016),n.breaks=6) + 
  labs(y=NULL, x=bquote("DOC (mg "*L^-1*")")) +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.doc1

#doc avg
plot.doc2 = 
  ggplot(doc2, aes(x=DOC_mgpl, y=year)) +
  geom_lineh(size=0.5) +
  facet_wrap(vars(LakeID)) +
  scale_x_continuous(limits=c(3,6),expand=c(0,0)) +
  scale_y_continuous(limits=c(1995,2016),n.breaks=6) + 
  labs(y=NULL, x=bquote("DOC (mg "*L^-1*")")) +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.doc2





d.duplicates = d %>% 
 # mutate(pig.year=paste(Date,variable,sep="_")) %>% 
  filter(duplicated(Date)  | duplicated(Date, fromLast = TRUE))

#diatom figs
plot.d = 
  ggplot(d3, aes(x=value,y=Date)) +
  geom_lineh(size=0.5) +
  geom_point(data=d.duplicates,mapping=aes(x=mdi2,y=Date),color='gray60',size=1,shape=1) +
  facet_wrap(vars(variable)) +
  scale_x_continuous(limits=c(0,1.25),breaks=c(0,0.5,1),expand=c(0,0)) +
  scale_y_continuous(limits=c(1995,2016),n.breaks=6) + 
  labs(y=NULL, x=NULL) +
  theme(strip.background=element_rect(color=NA,fill=NA),
        strip.text=element_text(hjust=0),
        panel.background=element_rect(fill='white',color='gray60'),
        panel.grid.major=element_line(color='gray80'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()); plot.d


#Save ####


#combine
comb = 
  
  wrap_plots(
    
    plot.pigs1 + 
      theme(strip.background=element_blank(), strip.text.y=element_blank()),
    
    plot.pca +
      theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()) +
      labs(y=NULL),
    
    plot.dendro +
      theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()) +
      labs(y=NULL),
    
    plot.doc +
      theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()), 
    
    ncol=4,
    nrow=1,
    widths=c(6,2,1,2)
    
  ); comb




ggsave(plot=comb,
       filename="plots/paleo/sc.paleo9.png",
       device="png",
       height=8,
       width=12,
       units="in",
       dpi=400)




docs = wrap_plots(plot.doc,
                  plot.doc1,
                  plot.doc2,
                  ncol=3); docs
ggsave(plot=docs,
       filename="plots/paleo/sc.paleo.docs.png",
       device="png",
       height=8,
       width=12,
       units="in",
       dpi=400)

piggies = wrap_plots(plot.pigs,
                     plot.pigs1,
                     plot.pigs2,
                     ncol=3); piggies

ggsave(plot=piggies,
       filename="plots/paleo/sc.paleo.piggies.png",
       device="png",
       height=8,
       width=12,
       units="in",
       dpi=400)




#Everything
#combine
comb = 
  
  wrap_plots(
    
    plot.pigs1 + 
      theme(strip.background=element_blank(), strip.text.y=element_blank()),
    
    plot.pca +
      theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()) +
      labs(y=NULL),
    
    plot.dendro +
      theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()) +
      labs(y=NULL),
    
    plot.d +
      theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()) +
      labs(y=NULL),
    
    plot.doc1 +
      theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()), 
    
    ncol=5,
    nrow=1,
    widths=c(6,2,1,1,2)
    
  ); comb





ggsave(plot=comb,
       filename="plots/paleo/sc.paleo.mdi.jpg",
       device="jpg",
       height=8,
       width=12,
       units="in",
       dpi=400)




comb = 
  
  wrap_plots(
    
    plot.pigs1 + 
      theme(strip.background=element_blank(), strip.text.y=element_blank()),
    
    plot.pca +
      theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()) +
      labs(y=NULL),
    
    plot.dendro +
      theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()) +
      labs(y=NULL),
    
    plot.d +
      theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()) +
      labs(y=NULL),
    
    plot.doc +
      theme(axis.text.y.left=element_blank(), axis.ticks.y.left=element_blank()), 
    
    ncol=5,
    nrow=1,
    widths=c(6,2,1,1,2)
    
  ); comb





ggsave(plot=comb,
       filename="plots/paleo/sc.paleo.mdi2.jpg",
       device="jpg",
       height=8,
       width=12,
       units="in",
       dpi=400)

ggsave(plot=comb,
       filename="plots/formatted/sc.paleo.jpg",
       device="jpg",
       height=8,
       width=12,
       units="in",
       dpi=400)

