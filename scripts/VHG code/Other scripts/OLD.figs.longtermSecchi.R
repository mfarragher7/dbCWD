#complete Secchi 1995-2019
#new and old figures, and stats
#updated 2020-08-27

#libraries
library(plyr)
library(Kendall)
library(ggplot2)
library(gridExtra)


#Load data ####
full.longterm.secchi = read.csv("LongTerm/4L_completeSecchi.csv")
full.longterm.secchi = full.longterm.secchi[-c(1)]
full.longterm.secchi$Date = as.Date(full.longterm.secchi$Date, format = "%m/%d/%Y")
#add lakename
full.longterm.secchi = full.longterm.secchi %>% 
  mutate(lakename = ifelse(grepl('JP',LakeID),'Jordan Pond',NA)) %>%
  mutate(lakename=replace(lakename,grepl('SC',LakeID), 'Seal Cove Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('WH',LakeID), 'Witch Hole Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('BB',LakeID), 'Bubble Pond'))


#Plots ####
#figure for aug 2020 lab meeting
#4 colors pairs
color.code = c('#01665E','#80CDC1','#BF812D','#543005')

#plot doc
plot.longterm.doc = ggplot(full.longterm.secchi, aes(x=Date, y=Secchi, color=lakename)) +
  geom_point(size=2) +  
  stat_smooth(aes(group=lakename), method="loess", linetype=2, size=0.8, se=F, show.legend=FALSE) +
  scale_x_date(date_labels="%Y", 
               breaks=c(seq(from=as.Date("1995-01-01"),to=as.Date("2020-03-31"), by='5 years')), 
               limits=as.Date(c("1995-01-01", "2020-02-25"))) + 
  labs(title="Long-term Secchi trends", color="Lake", x="Date", y="Secchi depth (m))") +
  scale_color_manual(values=color.code) +
  guides(color=guide_legend(override.aes = list(shape = 19))) +
  ylim(1,7) + 
  theme_classic()
plot.longterm.doc  
























# subset
library(dplyr)
data = data %>% filter(Date >= "1995-01-01") # omitting 80s datapoints
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

mk = rbind(mk.jp,mk.sc,mk.bb,mk.wh)   # bind mks together
rownames(mk) = seq(length=nrow(mk))   # renumber rows
summary = merge(avg,mk,by="LakeID")   # bind summary table together :)

#figgys
library(ggplot2)
library(gridExtra)

# loess smoothing trends
L1 = ggplot(JP, aes(Date, Secchi)) + geom_point(shape = 1) + 
  scale_shape_identity() + ylim(5, 22) +  theme_classic() + 
  ggtitle("Jordan Pond") + ylab("Secchi depth (m)") +
  scale_x_date(limits = as.Date(c("1995-07-01","2020-02-25"))) + 
  stat_smooth(method = "loess", color = "red",  size = 0.5, se = FALSE)   # + geom_label(label="p = 0.122", x=as.Date(1990-01-01), y=6,label.size = 5, color = "black")
L1

#SC plot
L2 = ggplot(SC, aes(Date, Secchi)) + geom_point(shape = 1) +  
  scale_shape_identity() + ylim(2, 15) + theme_classic() + 
  ggtitle("Seal Cove Pond") + ylab("Secchi depth (m)") +
  scale_x_date(limits = as.Date(c("1995-07-01","2020-02-25"))) + 
  stat_smooth(method = "loess", color = "red",  size = 0.5, se = FALSE)
L2

#BB plot
L3 = ggplot(BB, aes(Date, Secchi)) + geom_point(shape = 1) + 
  scale_shape_identity() + ylim(2, 15) + theme_classic() + 
  ggtitle("Bubble Pond") + ylab("Secchi depth (m)") +
  scale_x_date(limits = as.Date(c("1995-07-01","2020-02-25"))) + 
  stat_smooth(method = "loess", color = "red",  size = 0.5, se = FALSE)
L3

#WH plot
L4 = ggplot(WH, aes(Date, Secchi)) + geom_point(shape = 1) + 
  scale_shape_identity() + ylim(2, 8) + theme_classic() +
  ggtitle("Witch Hole Pond") + ylab("Secchi depth (m)") +
  scale_x_date(limits = as.Date(c("1995-07-01","2020-02-25"))) + 
  stat_smooth(method = "loess", color = "red",  size = 0.5, se = FALSE)
L4

# grid
grid.arrange(L1,L2,L3,L4)