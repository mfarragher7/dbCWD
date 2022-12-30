#complete DOC datasets for 4L
#new and old figures, and stats
#updated 2020-08-27

#figure for aug 2020 lab meeting

#libraries
library(plyr)
library(Kendall)
library(ggplot2)
library(ggpubr)

#Load DOC data ####
longterm.doc = read.csv("LongTerm/4L_completeDOC.csv")
colnames(longterm.doc)[1] = "lakename"
longterm.doc$Date = as.Date(longterm.doc$Date, format = "%m/%d/%Y")

#Plot DOC
#4 colors pairs
color.code = c('#01665E','#80CDC1','#BF812D','#543005')
#plot doc
plot.longterm.doc = ggplot(longterm.doc, aes(x=Date, y=DOC, color=lakename)) +
    geom_point(size=2) +  
    stat_smooth(aes(group=lakename), method="loess", linetype=2, 
        size=0.8, se=F, show.legend=FALSE) + 
    scale_x_date(date_labels="%Y", limits=as.Date(c("1995-01-01", "2020-02-25")),
        breaks=c(seq(from=as.Date("1995-01-01"), to=as.Date("2020-03-31"), by='5 years'))) +  #adjust date origin
    ylim(1,7) + 
    labs(title="Long-term DOC trends", color="Lake", x="Date", y="DOC (mg/L)") +
    scale_color_manual(values=color.code) +
    theme_classic()
plot.longterm.doc  
    

#Load Secchi data ####
longterm.secchi = read.csv("LongTerm/4L_completeSecchi.csv")
colnames(longterm.secchi)[1] = "lakename"
longterm.secchi$Date = as.Date(longterm.secchi$Date, format = "%m/%d/%Y")

#Plot Secchi
plot.longterm.secchi = ggplot(longterm.secchi, aes(x=Date, y=Secchi, color=lakename)) +
    geom_point(size=2) +  
    stat_smooth(aes(group=lakename), method="loess", linetype=2, 
        size=0.8, se=F, show.legend=FALSE) + 
    scale_x_date(date_labels="%Y", limits=as.Date(c("1995-01-01", "2020-02-25")),
        breaks=c(seq(from=as.Date("1995-01-01"), to=as.Date("2020-03-31"), by='5 years'))) +  #adjust date origin
    ylim(2,22) + 
    labs(title="Long-term Secchi trends", color="Lake", x="Date", y="Secchi depth (m)") +
    scale_color_manual(values=color.code) +
    theme_classic()
plot.longterm.secchi 


#combine figs
plot.longterm.doc.and.secchi = ggarrange(plot.longterm.doc, plot.longterm.secchi, 
                                         nrow=1, ncol=2, common.legend=TRUE, legend='bottom')
plot.longterm.doc.and.secchi



#DOC stats ####
#clunky code.... oh well!
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


#Secchi stats ####
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












# Old Secchi plots ####
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


#Old DOC plots ####
#figgys
# JP PLOT
p1 = ggplot(JP, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Jordan Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    geom_smooth(method = "lm", color = "red", size = 0.5)
p1

#SC plot
p2 = ggplot(SC, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Seal Cove Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    geom_smooth(method = "lm", color = "red", size = 0.5)
p2

#BB plot
p3 = ggplot(BB, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Bubble Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    geom_smooth(method = "lm", color = "red", size = 0.5)
p3

#WH plot
p4 = ggplot(WH, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Witch Hole Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    geom_smooth(method = "lm", color = "red", size = 0.5)
p4
 
# make another plot with all lakes on one graph
grid.arrange(p1,p2,p3,p4)




# try quadratic trend line
# JP PLOT
pp1 = ggplot(JP, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Jordan Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    stat_smooth(method = "lm", color = "red", formula = y ~ x + I(x^2), size = 0.5)
pp1

#SC plot
pp2 = ggplot(SC, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Seal Cove Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    stat_smooth(method = "lm", color = "red",formula = y ~ x + I(x^2), size = 0.5)
pp2

#BB plot
pp3 = ggplot(BB, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Bubble Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    stat_smooth(method = "lm", color = "red", formula = y ~ x + I(x^2), size = 0.5)
pp3

#WH plot
pp4 = ggplot(WH, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Witch Hole Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    stat_smooth(method = "lm",color = "red", formula = y ~ x + I(x^2), size = 0.5)
pp4

# grid
grid.arrange(pp1,pp2,pp3,pp4)




# loess smoothing trends



L1r = c("R^2 = 0.01") # nah

L1 = ggplot(JP, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Jordan Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    stat_smooth(method = "loess", color = "red",  size = 0.5) 

# + geom_text(aes(label = L1r))
L1

# :(




#SC plot
L2 = ggplot(SC, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Seal Cove Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    stat_smooth(method = "loess", color = "red",  size = 0.5)
L2

#BB plot
L3 = ggplot(BB, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Bubble Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    stat_smooth(method = "loess", color = "red",  size = 0.5)
L3

#WH plot
L4 = ggplot(WH, aes(Date, DOC)) + geom_point() +  scale_shape_identity() +
    ylim(0, 7) + theme_classic() + ggtitle("Witch Hole Pond") + ylab("DOC (mg/L)") +
    scale_x_date(limits = as.Date(c("1988-08-15","2020-02-25"))) + 
    stat_smooth(method = "loess", color = "red",  size = 0.5)
L4

# grid
grid.arrange(L1,L2,L3,L4)





