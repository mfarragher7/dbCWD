#summary stats for longterm DOC, secchi, SO4
#2020-12-01

#load survey data 
source('Rscripts/4.01_load.longterm.R')

#load libraries
library(plyr)
library(Kendall)


# need to fix this shit 





#DOC ####
#clunky code.... probably not gona fix it
# subset
JP = subset(data, LakeID %in% "JP")
SC = subset(data, LakeID %in% "SC")
BB = subset(data, LakeID %in% "BB")
WH = subset(data, LakeID %in% "WH")

# complete averages 
avg = ddply(data, c("LakeID"), summarize,  N = sum(!is.na(DOC)),   #output table of summary values
            mean = mean(DOC), sd = sd(DOC), se = sd/sqrt(N))

# are trends significant? Mann-Kendall trend test 
mk.jp = MannKendall(JP$DOC)  # run mann-kendall test 
mk.jp$LakeID = "JP"     # add lakeID column
mk.jp = ldply(mk.jp, data.frame)    #make list a df
mk.jp = data.frame(t(mk.jp[-1]))    #transpose df (switch rows and columns)
colnames(mk.jp) = c("tau","sl","S","D","varS","LakeID")  #colnames
cnames = colnames(mk.jp)
mk.sc = MannKendall(SC$DOC)
mk.sc$LakeID = "SC"
mk.sc = ldply(mk.sc, data.frame)
mk.sc = data.frame(t(mk.sc[-1]))
colnames(mk.sc) = cnames
mk.bb = MannKendall(BB$DOC)
mk.bb$LakeID = "BB"
mk.bb = ldply(mk.bb, data.frame)
mk.bb = data.frame(t(mk.bb[-1]))
colnames(mk.bb) = cnames
mk.wh = MannKendall(WH$DOC)
mk.wh$LakeID = "WH"
mk.wh = ldply(mk.wh, data.frame)
mk.wh = data.frame(t(mk.wh[-1]))
colnames(mk.wh) = cnames

mk = rbind(mk.jp,mk.sc,mk.bb,mk.wh)   # bind mks together
rownames(mk) = seq(length=nrow(mk))   # renumber rows
summary = merge(avg,mk,by="LakeID")   # bind summary table together :)




#Secchi  ####
# subset
library(dplyr)
JP = subset(data, LakeID %in% "JP")
SC = subset(data, LakeID %in% "SC")
BB = subset(data, LakeID %in% "BB")
WH = subset(data, LakeID %in% "WH")

# complete averages 
library(plyr)
avg = ddply(data, c("LakeID"), summarise,  N = sum(!is.na(Secchi)),   #output table of summary values
            mean = mean(Secchi), sd = sd(Secchi), se = sd/sqrt(N))


# are trends significant?
library(Kendall)
mk.jp = MannKendall(JP$Secchi)  # run mann-kendall test 
mk.jp$LakeID = "JP"      # add lakeID column
mk.jp = ldply(mk.jp, data.frame)    #make list a df
mk.jp = data.frame(t(mk.jp[-1]))    #transpose df (switch rows and columns)
colnames(mk.jp) = c("tau","sl","S","D","varS","LakeID")  #colnames
cnames = colnames(mk.jp)
mk.sc = MannKendall(SC$Secchi)
mk.sc$LakeID = "SC"
mk.sc = ldply(mk.sc, data.frame)
mk.sc = data.frame(t(mk.sc[-1]))
colnames(mk.sc) = cnames
mk.bb = MannKendall(BB$Secchi)
mk.bb$LakeID = "BB"
mk.bb = ldply(mk.bb, data.frame)
mk.bb = data.frame(t(mk.bb[-1]))
colnames(mk.bb) = cnames
mk.wh = MannKendall(WH$Secchi)
mk.wh$LakeID = "WH"
mk.wh = ldply(mk.wh, data.frame)
mk.wh = data.frame(t(mk.wh[-1]))
colnames(mk.wh) = cnames

mk = rbind(mk.jp,mk.sc,mk.bb,mk.wh) 
rownames(mk) = seq(length=nrow(mk))   
summary = merge(avg,mk,by="LakeID")  

















