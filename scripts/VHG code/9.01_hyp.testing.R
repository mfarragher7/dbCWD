#vertical habitat gradients: analysis
#compare community structure, habitat structure, and vertical gradients across lakes
#created 2021-05-30

#libraries
library(plyr)
library(dplyr)
library(vegan)
library(car)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(pals)


#Hyp 1 ####
# We predicted:
# 1a) greater seasonal heterogeneity of vertical habitat gradients, and 
# 1b) higher algal biomass across the higher-DOC lakes than in low-DOC lakes. 

#Key questions to answer: 
# 1. What variables change or stay the same from under ice to open water? how do changes differ between lakes?
#Compare variability of vhg parameters in each lake over time


#load profiles
profiles = read.csv("library/profiles.all.csv",header=T)
names(profiles)
sort(unique(profiles$lakedate))


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
    mixing.maxdepth,
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

    #Nutrients: for each depth
    #chla.ugpl.epi, chla.ugpl.meta, chla.ugpl.hypo,
    #doc.mgpl.epi, doc.mgpl.meta, doc.mgpl.hypo,
    #tp.ugpl.epi, tp.ugpl.meta, tp.ugpl.hypo,
    #nh4.ugpl.epi, nh4.ugpl.meta, nh4.ugpl.hypo,
    #no3.ugpl.epi, no3.ugpl.meta, no3.ugpl.hypo,
    #tn.ugpl.epi, tn.ugpl.meta, tn.ugpl.hypo,
    #din.ugpl.epi, din.ugpl.meta, din.ugpl.hypo,
    #din.tp.epi, din.tp.meta, din.tp.hypo
   
#drop spring1 and spring2
pro = pro %>% 
  filter(!lakedate=='SC_2020-03-25') %>% 
  filter(!lakedate=='SC_2020-05-13') %>% 
  filter(!lakedate=='JP_2020-05-13')

#reorder lakes 
pro$LakeID = factor(pro$LakeID, levels=c("JP","SC","BB","WH"))




#summarize #####
names(pro)
vars = names(pro[,5:18])

psum = pro %>%
  group_by(LakeID) %>%
  dplyr::summarise(across(all_of(vars), list(min = min,
                                             max = max,
                                             mean = mean,
                                             sd = sd), na.rm=T))
names(psum)

#save
write.csv(psum,"library/profile.sum.stats.csv",row.names=F)


#just sd
pvariance = pro %>%
  group_by(LakeID) %>%
  dplyr::summarise(across(all_of(vars), list(sd = sd), na.rm=T))
#save
write.csv(pvariance,"tables/profiles.variance.csv",row.names=F)





#plot ssi
ggplot(pro,aes(x=Date,y=schmidt.index)) + 
  geom_point() +
  facet_wrap(vars(LakeID),scales="free")

# light
ggplot(pro,aes(x=Date,y=z1.maxdepth)) + 
  geom_point() +
  facet_wrap(vars(LakeID),scales="free")





# 1a) greater seasonal heterogeneity of vertical habitat gradients in higher-DOC lakes 

#source: 
#https://towardsdatascience.com/how-to-do-a-t-test-or-anova-for-many-variables-at-once-in-r-and-communicate-the-results-in-a-6defaa712e5

# profile data
# group by Lake
x <- which(names(pro) == "LakeID")
# names of vars to test
y <- which(names(pro) == "temp.mean.c3" 
           | names(pro) == "schmidt.index"
           | names(pro) == "thermocline.depth.c3"
           | names(pro) == "mixing.maxdepth"
           | names(pro) == "buoyancy.freq"
           | names(pro) == "z1"
           | names(pro) == "z1.maxdepth"
           | names(pro) == "do.mean"
           | names(pro) == "do.max.depth"
           | names(pro) == "chla.ugpl.avg"
           | names(pro) == "chla.ugpl.sd"
           | names(pro) == "doc.mgpl.avg"
           | names(pro) == "doc.mgpl.sd"
           | names(pro) == "tp.ugpl.avg"
           | names(pro) == "tp.ugpl.sd"
           | names(pro) == "din.ugpl.avg"
           | names(pro) == "din.ugpl.sd"
           | names(pro) == "din.tp.avg"
           | names(pro) == "din.tp.sd"
           | names(pro) == "dcm.depth.curvefit"
           | names(pro) == "dcm.maxdepth"
           | names(pro) == "stand.peak.width"
           | names(pro) == "chl.avg")

method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
groups <- list(c("JP", "SC"), 
               c("JP", "BB"),
               c("JP", "WH"),
               c("SC", "BB"),
               c("SC", "WH"),
               c("BB", "WH")) # comparisons for post-hoc tests. all lake pairs

#empty list to save plots
plot_list = list()

#generate plots with anova values
for (i in y) {
  for (j in x) {
    p <- ggboxplot(pro,
                   x = colnames(pro[j]), 
                   y = colnames(pro[i]),
                   color = colnames(pro[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter")
    
    p = print(p + stat_compare_means(aes(label=paste0(..method.., ", p-value = ", ..p.format..)),
                                     method=method1, label.y=max(pro[, i], na.rm=T))
              
              # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
              + stat_compare_means(comparisons=groups, method=method2, label="p.format"))
    
    #save plot in list
    plot_list[[i]] = p
  }
}

#save pdf of all combined plots
pdf("plots/vhg/profile.vars.anova.all.pairs.pdf")
for (i in 1:length(plot_list)) {
  print(plot_list[[i]])}
dev.off()





#Tables ########

#save output table of values
# something like this v v v

#Lake Pair       ssi     z1:depth    do.mean     doc     tp     chla    dcm:depth
#SC-JP
#BB-JP
#WH-JP
#BB-SC
#WH-SC
#WH-BB




#T-test ########
#test the process
m1 <- lm(dcm.maxdepth ~ LakeID, data=pro); m1
a <- aov(m1); a
summary(a)
t <- TukeyHSD(a); t
t$LakeID
result <- data.frame(t$LakeID)
result["p.adj"]

#test the process
test = pro %>% filter(LakeID=='JP' | LakeID=='SC')
m1 <- lm(dcm.maxdepth ~ LakeID, data=test); m1
a <- aov(m1); a
summary(a)
t <- TukeyHSD(a); t
t$LakeID
result <- data.frame(t$LakeID)
result["p.adj"]


#make output table
lake.pairs = c('SC-JP','BB-JP','WH-JP','BB-SC','WH-SC','WH-BB')
hyp1 = as.data.frame(lake.pairs)

#mean temp 
mod = lm(temp.mean.c3 ~ LakeID, data=pro)
a = aov(mod); a
t = TukeyHSD(a); t
#run Tukey post-hoc test
output = data.frame(t$LakeID) #save output as df
hyp1$mean.temp = output$p.adj #save p values

#ssi 
mod = lm(schmidt.index ~ LakeID, data=pro)
a = aov(mod) #save anova
t = TukeyHSD(a);t #run Tukey post-hoc test
output = data.frame(t$LakeID) #save output as df
hyp1$ssi = output$p.adj #save p values

#mixing depth : max depth
mod = lm(mixing.maxdepth ~ LakeID, data=pro)
a = aov(mod) #save anova
t = TukeyHSD(a) #run Tukey post-hoc test
t
output = data.frame(t$LakeID) #save output as df
hyp1$mixing.maxdepth = output$p.adj #save p values


#z1:depth
mod = lm(z1.maxdepth ~ LakeID, data=pro)
a = aov(mod)
t = TukeyHSD(a) 
t
output = data.frame(t$LakeID) 
hyp1$z1.maxdepth = output$p.adj 

#DO
mod = lm(do.mean ~ LakeID, data=pro)
a = aov(mod)
t = TukeyHSD(a) 
t
output = data.frame(t$LakeID) 
hyp1$mean.do = output$p.adj 
              
#chla
mod = lm(chla.ugpl.avg ~ LakeID, data=pro)
a = aov(mod)
t = TukeyHSD(a) 
t
output = data.frame(t$LakeID)
hyp1$mean.chla = output$p.adj

#chla vhg
mod = lm(chla.ugpl.sd ~ LakeID, data=pro)
a = aov(mod) 
t = TukeyHSD(a)
t
output = data.frame(t$LakeID) 
hyp1$chla.sd = output$p.adj

#doc
mod = lm(doc.mgpl.avg ~ LakeID, data=pro)
a = aov(mod) 
t = TukeyHSD(a) 
t
output = data.frame(t$LakeID)
hyp1$doc = output$p.adj

#TP
mod = lm(tp.ugpl.avg ~ LakeID, data=pro)
a = aov(mod) 
t = TukeyHSD(a) 
t
output = data.frame(t$LakeID)
hyp1$tp = output$p.adj

#DIN
mod = lm(din.ugpl.avg ~ LakeID, data=pro)
a = aov(mod) 
t = TukeyHSD(a) 
output = data.frame(t$LakeID)
hyp1$din = output$p.adj

#DCM:depth
mod = lm(dcm.maxdepth ~ LakeID, data=pro)
a = aov(mod) 
t = TukeyHSD(a) 
t
output = data.frame(t$LakeID) 
hyp1$dcm.maxdepth = output$p.adj

colnames(hyp1) = c("Lake.pairs", "Mean.Temp", "SSI", "MixingDepth:MaxDepth",
                   "Z1:MaxDepth", "DO", "Chl", "Chl.sd", "DOC", "TP", "DIN", "DCM:MaxDepth")

#format table a little
hyp1 = format(hyp1, scientific=F)
hyp1



#save table, format in excel
write.csv(hyp1, 'tables/hyp1.tukey.csv',row.names=F)









#Levene's test #########

#make output table
lake.pairs = c('JP:SC','BB:WH','JP:BB','JP:WH','SC:BB','SC:WH')
hyp1 = as.data.frame(lake.pairs)
hyp1$DOC = NA
hyp1$TP = NA
hyp1$Chla = NA
hyp1$DO = NA
hyp1$Ss = NA
hyp1$ZmixZmax = NA
hyp1$Z1Zmax = NA
hyp1$DCMZmax = NA


#JP-SC
jpsc = pro %>% filter(LakeID=="JP" | LakeID=="SC")


jp = pro %>% filter(LakeID=='JP')
#test for normality 
shapiro.test(jp$mixing.maxdepth)





l = leveneTest(doc.mgpl.avg ~ LakeID, jpsc); l
hyp1[1,2] = l[1,3]
l = leveneTest(tp.ugpl.avg ~ LakeID, jpsc); l
hyp1[1,3] = l[1,3]
l = leveneTest(chla.ugpl.avg ~ LakeID, jpsc); l
hyp1[1,4] = l[1,3]
l = leveneTest(do.mean ~ LakeID, jpsc); l
hyp1[1,5] = l[1,3]
l = leveneTest(schmidt.index ~ LakeID, jpsc); l
hyp1[1,6] = l[1,3]
l = leveneTest(mixing.maxdepth ~ LakeID, jpsc); l
hyp1[1,7] = l[1,3]
l = leveneTest(z1.maxdepth ~ LakeID, jpsc); l
hyp1[1,8] = l[1,3]
l = leveneTest(dcm.maxdepth ~ LakeID, jpsc); l
hyp1[1,9] = l[1,3]
hyp1

#others
l = leveneTest(temp.mean.c3 ~ LakeID, jpsc); l
l = leveneTest(thermocline.depth.c3 ~ LakeID, jpsc); l
l = leveneTest(z1 ~ LakeID, jpsc); l
l = leveneTest(dcm.depth.curvefit ~ LakeID, jpsc); l
l = leveneTest(stand.peak.width ~ LakeID, jpsc); l




#BB-WH
bbwh = pro %>% filter(LakeID=="BB" | LakeID=="WH")
l = leveneTest(doc.mgpl.avg ~ LakeID, bbwh); l
hyp1[2,2] = l[1,3]
l = leveneTest(tp.ugpl.avg ~ LakeID, bbwh); l
hyp1[2,3] = l[1,3]
l = leveneTest(chla.ugpl.avg ~ LakeID, bbwh); l
hyp1[2,4] = l[1,3]
l = leveneTest(do.mean ~ LakeID, bbwh); l
hyp1[2,5] = l[1,3]
l = leveneTest(schmidt.index ~ LakeID, bbwh); l
hyp1[2,6] = l[1,3]
l = leveneTest(mixing.maxdepth ~ LakeID, bbwh); l
hyp1[2,7] = l[1,3]
l = leveneTest(z1.maxdepth ~ LakeID, bbwh); l
hyp1[2,8] = l[1,3]
l = leveneTest(dcm.maxdepth ~ LakeID, bbwh); l
hyp1[2,9] = l[1,3]
hyp1


#JP-BB
jpbb = pro %>% filter(LakeID=="JP" | LakeID=="BB")
l = leveneTest(doc.mgpl.avg ~ LakeID, jpbb); l
hyp1[3,2] = l[1,3]
l = leveneTest(tp.ugpl.avg ~ LakeID, jpbb); l
hyp1[3,3] = l[1,3]
l = leveneTest(chla.ugpl.avg ~ LakeID, jpbb); l
hyp1[3,4] = l[1,3]
l = leveneTest(do.mean ~ LakeID, jpbb); l
hyp1[3,5] = l[1,3]
l = leveneTest(schmidt.index ~ LakeID, jpbb); l
hyp1[3,6] = l[1,3]
l = leveneTest(mixing.maxdepth ~ LakeID, jpbb); l
hyp1[3,7] = l[1,3]
l = leveneTest(z1.maxdepth ~ LakeID, jpbb); l
hyp1[3,8] = l[1,3]
l = leveneTest(dcm.maxdepth ~ LakeID, jpbb); l
hyp1[3,9] = l[1,3]
hyp1


#JP-WH
jpwh = pro %>% filter(LakeID=="JP" | LakeID=="WH")
l = leveneTest(doc.mgpl.avg ~ LakeID, jpwh); l
hyp1[4,2] = l[1,3]
l = leveneTest(tp.ugpl.avg ~ LakeID, jpwh); l
hyp1[4,3] = l[1,3]
l = leveneTest(chla.ugpl.avg ~ LakeID, jpwh); l
hyp1[4,4] = l[1,3]
l = leveneTest(do.mean ~ LakeID, jpwh); l
hyp1[4,5] = l[1,3]
l = leveneTest(schmidt.index ~ LakeID, jpwh); l
hyp1[4,6] = l[1,3]
l = leveneTest(mixing.maxdepth ~ LakeID, jpwh); l
hyp1[4,7] = l[1,3]
l = leveneTest(z1.maxdepth ~ LakeID, jpwh); l
hyp1[4,8] = l[1,3]
l = leveneTest(dcm.maxdepth ~ LakeID, jpwh); l
hyp1[4,9] = l[1,3]
hyp1


#SC-BB
scbb = pro %>% filter(LakeID=="SC" | LakeID=="BB")
l = leveneTest(doc.mgpl.avg ~ LakeID, scbb); l
hyp1[5,2] = l[1,3]
l = leveneTest(tp.ugpl.avg ~ LakeID, scbb); l
hyp1[5,3] = l[1,3]
l = leveneTest(chla.ugpl.avg ~ LakeID, scbb); l
hyp1[5,4] = l[1,3]
l = leveneTest(do.mean ~ LakeID, scbb); l
hyp1[5,5] = l[1,3]
l = leveneTest(schmidt.index ~ LakeID, scbb); l
hyp1[5,6] = l[1,3]
l = leveneTest(mixing.maxdepth ~ LakeID, scbb); l
hyp1[5,7] = l[1,3]
l = leveneTest(z1.maxdepth ~ LakeID, scbb); l
hyp1[5,8] = l[1,3]
l = leveneTest(dcm.maxdepth ~ LakeID, scbb); l
hyp1[5,9] = l[1,3]
hyp1


#SC-WH
scwh = pro %>% filter(LakeID=="SC" | LakeID=="WH")
l = leveneTest(doc.mgpl.avg ~ LakeID, scwh); l
hyp1[6,2] = l[1,3]
l = leveneTest(tp.ugpl.avg ~ LakeID, scwh); l
hyp1[6,3] = l[1,3]
l = leveneTest(chla.ugpl.avg ~ LakeID, scwh); l
hyp1[6,4] = l[1,3]
l = leveneTest(do.mean ~ LakeID, scwh); l
hyp1[6,5] = l[1,3]
l = leveneTest(schmidt.index ~ LakeID, scwh); l
hyp1[6,6] = l[1,3]
l = leveneTest(mixing.maxdepth ~ LakeID, scwh); l
hyp1[6,7] = l[1,3]
l = leveneTest(z1.maxdepth ~ LakeID, scwh); l
hyp1[6,8] = l[1,3]
l = leveneTest(dcm.maxdepth ~ LakeID, scwh); l
hyp1[6,9] = l[1,3]
hyp1

#format table a little
hyp1 = format(hyp1, scientific=F)
hyp1


#save table, format in excel
write.csv(hyp1, 'tables/hyp1.levenes.csv',row.names=F)






anova(lm(tn_ugpl ~ ponduse, data = sponds, na.action = na.omit))


#anova instead

#NAH

anova(lm(doc.mgpl.avg ~ LakeID, data=jpsc, na.action = na.omit))
hyp1[1,2] = l[1,3]
anova(lm(tp.ugpl.avg ~ LakeID, jpsc, na.action = na.omit))
hyp1[1,3] = l[1,3]
anova(lm(chla.ugpl.avg ~ LakeID, jpsc, na.action = na.omit))
hyp1[1,4] = l[1,3]
anova(lm(do.mean ~ LakeID, jpsc, na.action = na.omit))
hyp1[1,5] = l[1,3]
anova(lm(schmidt.index ~ LakeID, jpsc, na.action = na.omit))
hyp1[1,6] = l[1,3]
anova(lm(mixing.maxdepth ~ LakeID, jpsc, na.action = na.omit))
hyp1[1,7] = l[1,3]
anova(lm(z1.maxdepth ~ LakeID, jpsc, na.action = na.omit))
hyp1[1,8] = l[1,3]
anova(lm(dcm.maxdepth ~ LakeID, jpsc, na.action = na.omit))
hyp1[1,9] = l[1,3]
hyp1



#BB-WH
bbwh = pro %>% filter(LakeID=="BB" | LakeID=="WH")
l = anova(lm(doc.mgpl.avg ~ LakeID, bbwh); l
hyp1[2,2] = l[1,3]
l = anova(lm(tp.ugpl.avg ~ LakeID, bbwh); l
hyp1[2,3] = l[1,3]
l = anova(lm(chla.ugpl.avg ~ LakeID, bbwh); l
hyp1[2,4] = l[1,3]
l = anova(lm(do.mean ~ LakeID, bbwh); l
hyp1[2,5] = l[1,3]
l = anova(lm(schmidt.index ~ LakeID, bbwh); l
hyp1[2,6] = l[1,3]
l = anova(lm(mixing.maxdepth ~ LakeID, bbwh); l
hyp1[2,7] = l[1,3]
l = anova(lm(z1.maxdepth ~ LakeID, bbwh); l
hyp1[2,8] = l[1,3]
l = anova(lm(dcm.maxdepth ~ LakeID, bbwh); l
hyp1[2,9] = l[1,3]
hyp1


#JP-BB
jpbb = pro %>% filter(LakeID=="JP" | LakeID=="BB")
l = anova(lm(doc.mgpl.avg ~ LakeID, jpbb); l
hyp1[3,2] = l[1,3]
l = anova(lm(tp.ugpl.avg ~ LakeID, jpbb); l
hyp1[3,3] = l[1,3]
l = anova(lm(chla.ugpl.avg ~ LakeID, jpbb); l
hyp1[3,4] = l[1,3]
l = anova(lm(do.mean ~ LakeID, jpbb); l
hyp1[3,5] = l[1,3]
l = anova(lm(schmidt.index ~ LakeID, jpbb); l
hyp1[3,6] = l[1,3]
l = anova(lm(mixing.maxdepth ~ LakeID, jpbb); l
hyp1[3,7] = l[1,3]
l = anova(lm(z1.maxdepth ~ LakeID, jpbb); l
hyp1[3,8] = l[1,3]
l = anova(lm(dcm.maxdepth ~ LakeID, jpbb); l
hyp1[3,9] = l[1,3]
hyp1


#JP-WH
jpwh = pro %>% filter(LakeID=="JP" | LakeID=="WH")
l = anova(lm(doc.mgpl.avg ~ LakeID, jpwh); l
hyp1[4,2] = l[1,3]
l = anova(lm(tp.ugpl.avg ~ LakeID, jpwh); l
hyp1[4,3] = l[1,3]
l = anova(lm(chla.ugpl.avg ~ LakeID, jpwh); l
hyp1[4,4] = l[1,3]
l = anova(lm(do.mean ~ LakeID, jpwh); l
hyp1[4,5] = l[1,3]
l = anova(lm(schmidt.index ~ LakeID, jpwh); l
hyp1[4,6] = l[1,3]
l = anova(lm(mixing.maxdepth ~ LakeID, jpwh); l
hyp1[4,7] = l[1,3]
l = anova(lm(z1.maxdepth ~ LakeID, jpwh); l
hyp1[4,8] = l[1,3]
l = anova(lm(dcm.maxdepth ~ LakeID, jpwh); l
hyp1[4,9] = l[1,3]
hyp1


#SC-BB
scbb = pro %>% filter(LakeID=="SC" | LakeID=="BB")
l = anova(lm(doc.mgpl.avg ~ LakeID, scbb); l
hyp1[5,2] = l[1,3]
l = anova(lm(tp.ugpl.avg ~ LakeID, scbb); l
hyp1[5,3] = l[1,3]
l = anova(lm(chla.ugpl.avg ~ LakeID, scbb); l
hyp1[5,4] = l[1,3]
l = anova(lm(do.mean ~ LakeID, scbb); l
hyp1[5,5] = l[1,3]
l = anova(lm(schmidt.index ~ LakeID, scbb); l
hyp1[5,6] = l[1,3]
l = anova(lm(mixing.maxdepth ~ LakeID, scbb); l
hyp1[5,7] = l[1,3]
l = anova(lm(z1.maxdepth ~ LakeID, scbb); l
hyp1[5,8] = l[1,3]
l = anova(lm(dcm.maxdepth ~ LakeID, scbb); l
hyp1[5,9] = l[1,3]
hyp1


#SC-WH
scwh = pro %>% filter(LakeID=="SC" | LakeID=="WH")
l = anova(lm(doc.mgpl.avg ~ LakeID, scwh); l
hyp1[6,2] = l[1,3]
l = anova(lm(tp.ugpl.avg ~ LakeID, scwh); l
hyp1[6,3] = l[1,3]
l = anova(lm(chla.ugpl.avg ~ LakeID, scwh); l
hyp1[6,4] = l[1,3]
l = anova(lm(do.mean ~ LakeID, scwh); l
hyp1[6,5] = l[1,3]
l = anova(lm(schmidt.index ~ LakeID, scwh); l
hyp1[6,6] = l[1,3]
l = anova(lm(mixing.maxdepth ~ LakeID, scwh); l
hyp1[6,7] = l[1,3]
l = anova(lm(z1.maxdepth ~ LakeID, scwh); l
hyp1[6,8] = l[1,3]
l = anova(lm(dcm.maxdepth ~ LakeID, scwh); l
hyp1[6,9] = l[1,3]
hyp1

#format table a little
hyp1 = format(hyp1, scientific=F)
hyp1


#save table, format in excel
write.csv(hyp1, 'tables/hyp1.levenes.csv',row.names=F)




























