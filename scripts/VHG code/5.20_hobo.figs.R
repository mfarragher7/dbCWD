#Hobo code
#Created 1 July 2020

#libraries
library(lubridate)
library(dplyr)
library(ggplot2)

#load  data 
jp.hobo = read.csv('library/buoy.jp.hobo.csv',header=T)
jp.hobo$DateTime = as_datetime(jp.hobo$DateTime)

sc.hobo = read.csv('library/buoy.sc.hobo.csv',header=T)
sc.hobo$DateTime = as_datetime(sc.hobo$DateTime)

wh.hobo = read.csv('library/buoy.wh.hobo.csv',header=T)
wh.hobo$DateTime = as_datetime(wh.hobo$DateTime)

bb.hobo





#rainbow, from cold to hot
cols = c('#2E0769','#07766F','#59A64D','#E9CE28','#F94343')






#Jordan Pond
plot.jp.hobo = 
  ggplot(jp.hobo,aes(x=DateTime,
                     y=Depth,
                     color=TempC)) +
  geom_tile() +
  scale_y_reverse(n.breaks=20,
                  limits=c(36,0)) +
  scale_x_datetime(limits=ymd_hms(c('2020-02-22 00:00:00',
                                    '2020-10-15 00:00:00')),
                   breaks="1 month",
                   date_labels="%d-%b") +
  scale_color_gradientn(colours=cols,
                        limits=c(0,30),
                        na.value="white") +
  labs(color="Temp C",
       x="Date",
       y="Depth (m)") +
  theme_classic(); plot.jp.hobo








#Seal Cove
plot.sc.hobo = 
  ggplot(sc.hobo,aes(x=DateTime,
                     y=Depth,
                     color=TempC)) +
  geom_tile() +
  scale_y_reverse(n.breaks=7,
                  limits=c(12,0)) +
  scale_x_datetime(limits=ymd_hms(c('2020-02-22 00:00:00',
                                    '2020-10-15 00:00:00')),
                   breaks="1 month",
                   date_labels="%d-%b") +
  scale_color_gradientn(colours=cols,
                        limits=c(0,30),
                        na.value="white") +
  labs(color="Temp C",
       x="Date",
       y="Depth (m)") +
  theme_classic(); plot.sc.hobo








#Witch Hole
plot.wh.hobo = 
  ggplot(wh.hobo,aes(x=DateTime,
                     y=Depth,
                     color=TempC)) +
  geom_tile() +
  scale_y_reverse(n.breaks=7,
                  limits=c(9,0)) +
  scale_x_datetime(limits=ymd_hms(c('2020-02-22 00:00:00',
                                    '2020-10-15 00:00:00')),
                   breaks="1 month",date_labels="%d-%b") +
  scale_color_gradientn(colours=cols,
                        limits=c(0,30),
                        na.value="white") +
  labs(color="Temp C",
       x="Date"
       ,y="Depth (m)") +
  theme_classic(); plot.wh.hobo






#Bubble 
plot.bb.hobo = 
  ggplot(bb.hobo,aes(x=DateTime,
                     y=Depth,
                     color=TempC)) +
  geom_tile() +
  scale_y_reverse(n.breaks=7,
                  limits=c(12,0)) +
  scale_x_datetime(limits=ymd_hms(c('2020-02-22 00:00:00',
                                    '2020-10-15 00:00:00')),
                   breaks="1 month",date_labels="%d-%b") +
  scale_color_gradientn(colours=cols,
                        limits=c(0,30),
                        na.value="white") +
  labs(color="Temp C",
       x="Date",
       y="Depth (m)") +
  theme_classic(); plot.bb.hobo








#other plots
#check when buoy sunk
plot.jp.hobo3 = ggplot(jp.hobo, aes(x=DateTime, y=Depth, color=TempC)) +
  geom_tile(size=2) +
  scale_y_reverse(n.breaks=20) +
  scale_x_datetime(limits=ymd_hms(c("2020-06-24 14:00:00","2020-06-28 01:00:00")),
                   breaks='1 hour',date_labels="%d-%H:%M") +
  labs(color="Temp C", x="Date-time", y="Depth (m)") +
  scale_color_gradient2(midpoint=12,high='yellow',mid='red',low='navy') +
  theme_classic()
plot.jp.hobo3

#another check
plot.jp.hobo3 = ggplot(jp.hobo, aes(x=DateTime, y=Depth, color=TempC)) +
  geom_tile(size=2) + 
  scale_y_reverse(n.breaks=20) +
  scale_x_datetime(limits=ymd_hms(c("2020-06-25 14:00:00","2020-06-26 01:00:00")),
                   breaks='1 hour',date_labels="%d-%H:%M") +
  labs(color="Temp C", x="Date-time", y="Depth (m)") +
  scale_color_gradient2(limits=c(7,22),midpoint=17,high='yellow',mid='red',low='navy') +
  theme_classic()
plot.jp.hobo3


#Complete bubble
plot.bb.hobo = 
  ggplot(bb.hobo,aes(x=DateTime,y=Depth,color=TempC)) +
  geom_tile() +
  scale_y_reverse(n.breaks=7,limits=c(12,0)) +
  scale_x_datetime(breaks="3 months",date_labels="%m-%Y") +
  scale_color_gradientn(colours=cols,limits=c(0,30),na.value="white") +
  labs(color="Temp C",x="Date",y="Depth (m)") +
  theme_classic(); plot.bb.hobo


