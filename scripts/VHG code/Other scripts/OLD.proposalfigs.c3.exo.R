#thesis proposal figs
#OLD CODE ####
# load jp C3 data
jp = read.csv("2020 sampling/4L_JP/JP_C3.csv")
jp$dateID = NA
jp[grepl("02/21/20", jp$DateTime),8] = '1'
jp[grepl("05/28/20", jp$DateTime),8] = '2'
#subset 
jp = subset(jp, Depth >= 0) # remove neg values first
jp_feb = subset(jp, dateID %in% "1")
jp_may = subset(jp, dateID %in% "2")
jp_may = subset(jp_may, Depth >= 2)

# load seal cove C3 
sc = read.csv("2020 sampling/4L_SC/SC_C3.csv")
sc$dateID = NA
sc[grepl("02/21/20", sc$DateTime),8] = '1'
sc[grepl("05/28/20", sc$DateTime),8] = '2'
#subset 
sc = subset(sc, Depth >= 0) #remove neg depth values
sc_feb = subset(sc, dateID %in% "1")
sc_may = subset(sc, dateID %in% "2")
sc_may = subset(sc_may, Depth >= 2)

# load jp Exo
jp_exo = read.csv("2020 sampling/4L_JP/JP_exo.csv")
jp_exo = subset(jp_exo, Date %in% "2020-05-28")

#load sc Exo 
sc_exo = read.csv("2020 sampling/4L_SC/SC_exo.csv")
sc_exo = subset(sc_exo, Date %in% "2020-05-28")


#figures
library(ggplot2)
library(gridExtra)

#chlorophyll profiles
a1 = ggplot(jp_feb, aes(Chlorophyll_a, Depth)) + geom_point() +  scale_shape_identity() +
  theme_classic() + ggtitle("Jordan Pond - 21 Feb 2020") + xlab("Chlorophyll a (RFU)") +
  ylab("Depth (m)") + scale_y_continuous(trans = "reverse") + xlim(0,300)

a2 = ggplot(jp_may, aes(Chlorophyll_a, Depth)) + geom_point() +  scale_shape_identity() +
  theme_classic() + ggtitle("Jordan Pond - 28 May 2020") + xlab("Chlorophyll a (RFU)") +
  ylab("Depth (m)") + scale_y_continuous(trans = "reverse") + xlim(0,300)

b1 = ggplot(sc_feb, aes(Chlorophyll_a, Depth)) + geom_point() +  scale_shape_identity() +
  theme_classic() + ggtitle("Seal Cove Pond - 21 Feb 2020") + xlab("Chlorophyll a (RFU)") +
  ylab("Depth (m)") + scale_y_continuous(trans = "reverse")

b2 = ggplot(sc_may, aes(Chlorophyll_a, Depth)) + geom_point() +  scale_shape_identity() +
  theme_classic() + ggtitle("Seal Cove Pond - 28 May 2020") + xlab("Chlorophyll a (RFU)") +
  ylab("Depth (m)") + scale_y_continuous(trans = "reverse")


#oxygen profiles
a3 = ggplot(jp_exo, aes(ODO, Depth)) + geom_point(shape=1) +  scale_shape_identity() +
  theme_classic() + ggtitle("Jordan Pond - 28 May 2020") + xlab("Dissolved Oxygen (mg/L)") +
  ylab("Depth (m)") + scale_y_continuous(trans = "reverse") + xlim(8,12)

b3 = ggplot(sc_exo, aes(ODO, Depth)) + geom_point(shape=1) +  scale_shape_identity() +
  theme_classic() + ggtitle("Seal Cove - 28 May 2020") + xlab("Dissolved Oxygen (mg/L)") +
  ylab("Depth (m)") + scale_y_continuous(trans = "reverse") + xlim(7,10)


#Hyp1 figure
grid.arrange(a3,a2,b3,b2)

#Hyp2 figure
grid.arrange(a1,b1,a2,b2)


