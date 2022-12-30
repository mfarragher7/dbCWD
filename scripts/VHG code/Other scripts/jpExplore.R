# Jordan Pond buoy exploratory script

# clear environment
rm(list=ls()) 

# set wd, load libraries
setwd("~/Maine/Acadia/jpBuoy")
library(dplyr)
library(RSQLite)



# loading data from jpBuoy2013-2018.csv in jpBuoy folder
data = read.csv("jpBuoy20132018.csv")

# subset data. selecting variables i want first. updated to include temp profiles so i can learn shit
data2 = subset(data, select = c(1, 3, 7, 9, 10,11, 12, 15, 17:32))
# rename columns
names(data2)[1] <- "dateTime"
names(data2)[2] <- "temp"
names(data2)[3] <- "pH"
names(data2)[4] <- "DOsat"
names(data2)[5] <- "DOmgpL"
names(data2)[6] <- "chlRFU"
names(data2)[7] <- "chlugpL"
names(data2)[8] <- "fDOMRFU"

data2$Year=NA       # Add 'Year' column
data2[grepl("2013", data2$dateTime),9] = '2013'
data2[grepl("2014", data2$dateTime),9] = '2014'
data2[grepl("2015", data2$dateTime),9] = '2015'
data2[grepl("2016", data2$dateTime),9] = '2016'
data2[grepl("2017", data2$dateTime),9] = '2017'
data2[grepl("2018", data2$dateTime),9] = '2018'
# split dateTime
data2$Date = sapply(strsplit(as.character(data2$dateTime), " "), "[", 1)
data2$Time = sapply(strsplit(as.character(data2$dateTime), " "), "[", 2)
# subset 2018
jp18 = subset(data2, Year %in% '2018')


# make output df to calculate avg in each date for 2018
dates18 = unique(jp18$Date)
daily18 = as.data.frame(matrix(NA, ncol = 6, nrow = length(dates18)))
daily18$Date = unique(jp18$Date)
# aggrerate avgs 
daily18 = aggregate(cbind(pH, temp, DOsat, DOmgpL, chlRFU, chlugpL, fDOMRFU) ~ Date, jp18, mean)
colnames(daily18) = c("Date","pHavg","tempavg","DOsatavg","DOmgpLavg","chlRFUavg","chlugpLavg","fDOMavg")


# ggplot figures
# load ggplot
library(ggplot2)
library(scales)

# make dates act like dates
daily18$Date <- as.Date(daily18$Date)

a1 = ggplot(daily18, aes(x=Date, y=pHavg)) + geom_point() + theme_classic() + ggtitle("average pH 2018")+
     scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month")+
     theme(axis.text.x=element_text(angle=45, hjust=1)) 
a1

a2 = ggplot(daily18, aes(x=Date, y=fDOMavg)) +geom_point() + theme_classic() + ggtitle("average fDOM 2018")+
     scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month")+
     theme(axis.text.x=element_text(angle=45, hjust=1)) 
a2

a3 = ggplot(daily18, aes(x=Date, y=tempavg)) + geom_point() + theme_classic() + ggtitle("average temp 2018")+
     scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month")+
     theme(axis.text.x=element_text(angle=45, hjust=1)) 
a3


# 2013-2018 daily averages
dates = unique(data2$Date)
daily = as.data.frame(matrix(NA, ncol = 6, nrow = length(dates)))
daily$Date = unique(data2$Date)
# aggrerate avgs 
daily = aggregate(cbind(pH, temp, DOsat, DOmgpL, chlRFU, chlugpL, fDOMRFU) ~ Date, data2, mean)
colnames(daily) = c("Date","pHavg","tempavg","DOsatavg","DOmgpLavg","chlRFUavg","chlugpLavg","fDOMavg")
# make dates act like dates
daily$Date <- as.Date(daily$Date)


# figures
# load ggplot
library(ggplot2)
library(scales)

# plot all ~900 fDOM points lol. and then pH, then temp
b1 = ggplot(daily, aes(x=Date, y=fDOMavg)) + ylab("fDOM (RFU)") +
     geom_point() + theme_classic() +
     ggtitle("Daily fDOM 2013-2018") +
     scale_x_date(date_labels = "%m-%Y", date_breaks = "4 months")+
     theme(axis.text.x=element_text(angle=45, hjust=1)) 
b1

b2 = ggplot(daily, aes(x=Date, y=pHavg)) + ylab("pH") +
     geom_point() + theme_classic() +
     ggtitle("Daily pH 2013-2018") +
     scale_x_date(date_labels = "%m-%Y", date_breaks = "4 months")+
     theme(axis.text.x=element_text(angle=45, hjust=1)) 
b2

b3 = ggplot(daily, aes(x=Date, y=tempavg)) + ylab("temp C") +
     geom_point() + theme_classic() +
     ggtitle("Daily temp 2013-2018") +
     scale_x_date(date_labels = "%m-%Y", date_breaks = "4 months")+
     theme(axis.text.x=element_text(angle=45, hjust=1)) 
b3

# SMASH THINGS TOGETHER
# pH vs fDOM
c1 = ggplot(daily, aes(x=fDOMavg, y=pHavg)) + xlab("fDOM") + ylab("Ph") +
     geom_point() +  ggtitle("pH vs fDOM") 
c1

# chl vs DO
c2 = ggplot(daily, aes(x=chlRFUavg, y=DOmgpLavg)) + xlab("clh") + ylab("DO") +
     geom_point() +  ggtitle("chl vs DO") +
     theme_classic()
c2

# temp vs DO
c3 = ggplot(daily, aes(x=tempavg, y=DOmgpLavg)) + xlab("temp") + ylab("DO") +
     geom_point() +  ggtitle("temp vs DO") +
     theme_classic()
c3

# fDOM vs DO
c4 = ggplot(daily, aes(x=fDOMavg, y=DOmgpLavg)) + xlab("fDOM") + ylab("DO") +
     geom_point() +  ggtitle("fDOM vs DO") +
     theme_classic()
c4


# load historical data
df1 = read.csv("~/Maine/Acadia/jpBuoy/Historical Data/jp_pH_06-12.csv")
df2 = read.csv("~/Maine/Acadia/jpBuoy/Historical Data/jp_chem_pre06.csv")

# subset pH
df1_ = subset(df1, select =c(Date, CCpH))
colnames(df1_) = c("Date","pHavg")
df1_$Date <- as.Date(df1_$Date)

# daily avg from historical df
df2_ = aggregate(cbind(Lab.pH) ~ Date, df2, mean)
colnames(df2_) = c("Date","pHavg")
df2_$Date <- as.Date(df2_$Date)

# subset pHavg from 2013-2018 df
df3 = subset(daily, select = c(Date, pHavg))

# combine dataframes, sort by date
pH = rbind(df1_, df2_, df3)
pH <- pH[nrow(pH):1,]

# figures
d1 = ggplot(pH, aes(x=Date, y=pHavg)) + xlab("Date") + ylab("pH") + 
    geom_point() + theme_classic() + ggtitle("Historic pH") +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "5 years")+
    theme(axis.text.x=element_text(angle=45, hjust=1)) 
d1

# pH 1980-2018. Any way to do this without making another subset df?
ss <- subset(pH, Date > as.Date("1980-01-01"))
d2 = ggplot(ss, aes(x=Date, y=pHavg)) + xlab("Date") + ylab("pH") + 
    geom_point() + theme_classic() + ggtitle("Historic pH 1980-2018")
d2



# PROFILES
library(rLakeAnalyzer)

# temp data
data3 = subset(data, select = c(1,3,7,9,10,11,12,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32))
# rename columns
names(data3) = c('dateTime','temp','pH','DOsat','DOmgpL','chlRFU','chlugpL','fDOMRFU',"Temp00","Temp01","Temp02","Temp03",  
                 "Temp04","Temp05","Temp06","Temp07","Temp08","Temp09","Temp10","Temp11","Temp12","Temp13","Temp14","Temp15")



#where is DO profile data



# TO DO LIST
#
# - make a summary table
# - learn how to do temp and DO profile analysis. lakeAnalyzer?
# - ISIC rain data. Find rain pH data

