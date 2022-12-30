#SO4 data



# load datasets
data = read.csv("LongTerm/4L_SO4.csv")
data = data[-c(1,5)] #remove first col - site name and empty last col
data$Date = as.Date(data$Date, format = "%m/%d/%Y") #format date

# subset
library(dplyr)
data = data %>% filter(Date >= "1995-01-01") # omitting 38 datapoints
JP = subset(data, LakeID %in% "JP")
SC = subset(data, LakeID %in% "SC")
BB = subset(data, LakeID %in% "BB")
WH = subset(data, LakeID %in% "WH")

# complete averages 
library(plyr)
avg = ddply(data, c("LakeID"), summarise,  N = sum(!is.na(SO4)),   #output table of summary values
            mean = mean(SO4), sd = sd(SO4), se = sd/sqrt(N))

# are trends significant?
library(Kendall)
mk.jp = MannKendall(JP$SO4)  # run mann-kendall test 
mk.jp$LakeID = "JP"      # add lakeID column
mk.jp = ldply(mk.jp, data.frame)    #make list a df
mk.jp = data.frame(t(mk.jp[-1]))    #transpose df (switch rows and columns)
colnames(mk.jp) = c("tau","sl","S","D","varS","LakeID")  #colnames
cnames = colnames(mk.jp)
mk.sc = MannKendall(SC$SO4)
mk.sc$LakeID = "SC"
mk.sc = ldply(mk.sc, data.frame)
mk.sc = data.frame(t(mk.sc[-1]))
colnames(mk.sc) = cnames
mk.bb = MannKendall(BB$SO4)
mk.bb$LakeID = "BB"
mk.bb = ldply(mk.bb, data.frame)
mk.bb = data.frame(t(mk.bb[-1]))
colnames(mk.bb) = cnames
mk.wh = MannKendall(WH$SO4)
mk.wh$LakeID = "WH"
mk.wh = ldply(mk.wh, data.frame)
mk.wh = data.frame(t(mk.wh[-1]))
colnames(mk.wh) = cnames

mk = rbind(mk.jp,mk.sc,mk.bb,mk.wh)   # bind mks together
rownames(mk) = seq(length=nrow(mk))   # renumber rows
summary = merge(avg,mk,by="LakeID")   # bind summary table together :)

#figures
library(ggplot2)
library(gridExtra)
library(tidyverse)

# tried annotating figures in here instead of manually, didnt work
# loess smoothing trends
#L1y= range(JP$SO4)
#L1x = range(JP$Date)
#L1caption = paste(strwrap("p < 0.001 *tau = -0.66", 11), collapse = "\n")
#L1caption 

# +annotate("text", x = L1x[1], y = L1y[1], label = , hjust = 0.1, vjust = 0, size = 6)


L1 = ggplot(JP, aes(Date, SO4)) + geom_point(shape = 1) +  
  scale_shape_identity() + theme_classic() + 
  ggtitle("Jordan Pond") + labs(y = expression("SO"[4]^-{2}*~"(ueq/L)")) +
  scale_x_date(limits = as.Date(c("1995-01-01","2020-01-01"))) + 
  stat_smooth(method = "loess", color = "red",  size = 0.5, se = FALSE)
L1

#SC plot
L2 = ggplot(SC, aes(Date, SO4)) + geom_point(shape = 1) +  
  scale_shape_identity() + theme_classic() + 
  ggtitle("Seal Cove Pond") + labs(y = expression("SO"[4]^-{2}*~"(ueq/L)")) +
  scale_x_date(limits = as.Date(c("1995-01-01","2020-01-01"))) + 
  stat_smooth(method = "loess", color = "red",  size = 0.5, se = FALSE)
L2

#BB plot
L3 = ggplot(BB, aes(Date, SO4)) + geom_point(shape = 1) + 
  scale_shape_identity() + theme_classic() + 
  ggtitle("Bubble Pond") + labs(y = expression("SO"[4]^-{2}*~"(ueq/L)")) +
  scale_x_date(limits = as.Date(c("1995-01-01","2020-01-01"))) + 
  stat_smooth(method = "loess", color = "red",  size = 0.5, se = FALSE)
L3

#WH plot
L4 = ggplot(WH, aes(Date, SO4)) + geom_point(shape = 1) + 
  scale_shape_identity() + theme_classic() +
  ggtitle("Witch Hole Pond") + labs(y = expression("SO"[4]^-{2}*~"(ueq/L)")) +
  scale_x_date(limits = as.Date(c("1995-01-01","2020-01-01"))) + 
  stat_smooth(method = "loess", color = "red",  size = 0.5, se = FALSE)
L4

# grid
grid.arrange(L1,L2,L3,L4)