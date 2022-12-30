#Test Hyp1: vertical habitat gradients
#created 2021-07-18

#libraries
library(dplyr)
library(car)


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

#drop spring1 and spring2
pro = pro %>% 
  filter(!lakedate=='SC_2020-03-25') %>% 
  filter(!lakedate=='SC_2020-05-13') %>% 
  filter(!lakedate=='JP_2020-05-13')

#reorder lakes 
pro$LakeID = factor(pro$LakeID, levels=c("JP","SC","BB","WH"))





#Levene's ########

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










##colnames(hyp1) = c("Lake.pairs", "Mean.Temp", "SSI", "MixingDepth:MaxDepth",
#                   "Z1:MaxDepth", "DO", "Chl", "Chl.sd", "DOC", "TP", "DIN", "DCM:MaxDepth")




#vars = c("doc.mgpl.avg",
 #        "tp.ugpl.avg",
  #       "chla.ugpl.avg",
   #      "do.mean",
    #     "schmidt.index",
     #    "mixing.maxdepth",
      #   "z1.maxdepth",
       #  "dcm.maxdepth")


#temp_list = list()
#
#
#for (i in 1:length(vars)){ #for every var,
  
 # temp = jpsc %>% drop_na(vars[[i]])
  
  #l = leveneTest([[i]][1] ~ LakeID, temp) #run levene's test
  
  
  #temp_list[[i]] = l #save list
