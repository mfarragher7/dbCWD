#sensor data, created 30 June 2020
#explore buoy data, DO & PAR 

#load libraries
library(tidyverse)
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(ggpubr)

#DO ####
#JP 
#deployed Feb21 @ 15:00
#download Jun13 @ 11:30
#DO top
jp.top = read.delim('sampling/buoyJP/DOtop/2020-06-13/Cat.txt',sep=',')
#delete and dplyr::rename columns
jp.top = jp.top %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  mutate(ID='JPtop')#add ID column
#DO bottom
jp.bot = read.delim('sampling/buoyJP/DObottom/2020-06-13/Cat.txt',sep=',')
#delete and dplyr::rename columns
jp.bot = jp.bot %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  mutate(ID='JPbottom') %>% 
  slice(-1,) #delete 'units' row
#SC
#deployed Feb21 @ 11:00
#download Jun14 @ 14:00
#DO top
sc.top = read.delim('sampling/buoySC/DOtop/2020-06-14/Cat.txt',sep=',')
#delete and dplyr::rename columns
sc.top = sc.top %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  mutate(ID='SCtop') %>% 
  slice(-1,) 
#DO bottom
sc.bot = read.delim('sampling/buoySC/DObottom/2020-06-14/Cat.txt',sep=',')
#delete and dplyr::rename columns
sc.bot = sc.bot %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  mutate(ID='SCbottom') %>% 
  slice(-1,) 
#WH
#deployed Feb25 @ 15:00
#download Jun13 @ 16:00
#DO top
wh.top = read.delim('sampling/buoyWH/DOtop/2020-06-13/Cat.txt',sep=',')
#delete and dplyr::rename columns
wh.top = wh.top %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  mutate(ID='WHtop') %>% 
  slice(-1,) 
#DO bottom
wh.bot = read.delim('sampling/buoyWH/DObottom/2020-06-13/Cat.txt',sep=',')
#delete and dplyr::rename columns
wh.bot = wh.bot %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Q) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('DOmgpl'='Dissolved.Oxygen') %>% 
  dplyr::rename('DOsat'='Dissolved.Oxygen.Saturation') %>% 
  mutate(ID='WHbottom') %>% 
  slice(-1,) 
#BB
bb.do = read.csv('sampling/buoyBB/DEP datasets/Bubb_DO_full.csv')
bb.do = bb.do %>% 
  select(-Lake,-MIDAS,-ID)
#separate top
bb.do.top = bb.do %>% 
  select(-2,-5,-6,-7,-8,-9,-10,-11,-12,-13) %>% 
  dplyr::rename('DOmgpl'='DO_1m') %>% 
  dplyr::rename('Temp'='Temp_1m') %>% 
  mutate(ID ='BBtop')
#separate middle
bb.do.mid = bb.do %>% 
  select(-2,-3,-4,-5,-6,-9,-10,-11,-12,-13) %>% 
  dplyr::rename('DOmgpl'='DO_7m') %>% 
  dplyr::rename('Temp'='Temp_7m') %>% 
  mutate(ID ='BBmid')
#separate top
bb.do.bot = bb.do %>% 
  select(-2,-3,-4,-5,-6,-7,-8,-9,-10,-13) %>% 
  dplyr::rename('DOmgpl'='DO_11m') %>% 
  dplyr::rename('Temp'='Temp_11m') %>% 
  mutate(ID ='BBbot')
#merge
bb.do2 = rbind(bb.do.top,bb.do.mid,bb.do.bot)


#*PLOTS ####
#BB plot first since it doesn't matter as much
plot.bb.do = ggplot(bb.do2, aes(x=as.Date(DateTime), y=DOmgpl, color=ID)) +
  geom_point(alpha = 0.5, size = 0.8) +
  labs(x="Date", y=expression("Dissolved Oxygen (mg L"^-{1}*~')')) + 
  scale_x_date(limits=as.Date(c('2019-06-09','2019-11-20')), date_breaks = "1 month")  +
  ylim(-0.02,11) +
  scale_color_manual(labels = c("Bottom DO","Middle DO", "Top DO"), values = c('lightcyan4','black','lightcyan3')) +
  guides(color=guide_legend("DO Depth",override.aes=list(size=5),reverse=TRUE)) + 
  theme_classic() + 
  ggtitle("Bubble Pond")
plot.bb.do 

#JP DO
jp.do = rbind(jp.top,jp.bot)
#im a idiot. coerce back into numeric
jp.do$DOmgpl = as.numeric(jp.do$DOmgpl)
#str(jp.do)
#plot
plot.jp.do = ggplot(jp.do, aes(x=as.Date(DateTime), y=DOmgpl, color=ID)) +
  geom_point(alpha = 0.5, size = 0.8) +
   xlab(" ") + 
  scale_x_date(limits=as.Date(c('2020-02-26','2020-06-12')), date_breaks = "1 month")  +
  ylab(" ") + 
  ylim(9,14) +
  scale_color_manual(labels = c("Bottom DO","Top DO"), values = c('lightcyan4','lightcyan3')) +
  guides(color=guide_legend("DO Depth",override.aes=list(size=5),reverse=TRUE)) + # change legend title, and point size
  geom_vline(xintercept = as.numeric(as.Date("2020-03-15")), linetype=4, color = "black") +
  theme_classic() + 
  ggtitle("Jordan Pond")
plot.jp.do

#SC DO
sc.do = rbind(sc.top,sc.bot)
sc.do$DOmgpl = as.numeric(sc.do$DOmgpl)
#str(sc.do)
#plot
plot.sc.do = ggplot(sc.do, aes(x=as.Date(DateTime), y=DOmgpl, color=ID)) +
  geom_point(alpha = 0.5, size = 0.8) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-06-12')), date_breaks = "1 month")  +
  labs(x=" ", y=expression("Dissolved Oxygen (mg L"^-{1}*~')')) + 
  ylim(5,13) +
  scale_color_manual(labels = c("Bottom DO","Top DO"), values = c('lightcyan4','lightcyan3')) +
  guides(color=guide_legend("DO Depth",override.aes=list(size=5),reverse=TRUE)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-15")), linetype=4, color = "black") +
  theme_classic() + 
  ggtitle("Seal Cove Pond")
plot.sc.do 

#WH DO
wh.do = rbind(wh.top,wh.bot)
wh.do$DOmgpl = as.numeric(wh.do$DOmgpl)
#str(wh.do)
#plot
plot.wh.do = ggplot(wh.do, aes(x=as.Date(DateTime), y=DOmgpl, color=ID)) +
  geom_point(alpha = 0.5, size = 0.8) +
  xlab('Date') + 
  scale_x_date(limits=as.Date(c('2020-02-26','2020-06-12')), date_breaks = "1 month")  +
  ylab(' ') + ylim(4,12) +
  scale_color_manual(labels = c("Bottom DO","Top DO"), values = c('lightcyan4','lightcyan3')) +
  guides(color=guide_legend("DO Depth",override.aes=list(size=5),reverse=TRUE)) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-15")), linetype=4, color = "black") +
  theme_classic() + 
  ggtitle("Witch Hole Pond")
plot.wh.do 


#saturation ####
#instead
#Jordan
jp.do$DOsat = as.numeric(jp.do$DOsat)
#str(jp.do)
#plot
plot.jp.dosat = ggplot(jp.do, aes(x=as.Date(DateTime), y=DOsat, color=ID)) +
  geom_point(alpha = 0.5, size = 0.8) +
  xlab(" ") + 
  scale_x_date(limits=as.Date(c('2020-02-26','2020-06-12')), date_breaks = "1 month")  +
  ylab("Dissolved Oxygen (% saturation)") + 
  scale_y_continuous(n.breaks=8) +
  scale_color_manual(labels = c("Bottom DO","Top DO"), values = c('black','lightcyan4')) +
  guides(color=guide_legend("DO Depth",override.aes=list(size=5),reverse=TRUE)) + # change legend title, and point size
  geom_vline(xintercept = as.numeric(as.Date("2020-03-15")), linetype=4, color = "black") +
  theme_classic() + 
  ggtitle("Jordan Pond")
plot.jp.dosat

#SC DO
sc.do$DOsat = as.numeric(sc.do$DOsat)
#str(sc.do)
#plot
plot.sc.dosat = ggplot(sc.do, aes(x=as.Date(DateTime), y=DOsat, color=ID)) +
  geom_point(alpha = 0.5, size = 0.8) +
  scale_x_date(limits=as.Date(c('2020-02-26','2020-06-12')), date_breaks = "1 month")  +
  labs(x=" ", y="Dissolved Oxygen (% Saturation)") + 
  scale_color_manual(labels = c("Bottom DO","Top DO"), values = c('lightcyan4','lightcyan3')) +
  guides(color=guide_legend("DO Depth",override.aes=list(size=5),reverse=TRUE)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-15")), linetype=4, color = "black") +
  theme_classic() + 
  ggtitle("Seal Cove Pond")
plot.sc.dosat

#WH DO
wh.do$DOsat = as.numeric(wh.do$DOsat)
#str(wh.do)
#plot
plot.wh.dosat = ggplot(wh.do, aes(x=as.Date(DateTime), y=DOsat, color=ID)) +
  geom_point(alpha = 0.5, size = 0.8) +
  xlab('Date') + 
  ylab(" ") + 
  scale_x_date(limits=as.Date(c('2020-02-26','2020-06-12')), date_breaks = "1 month")  +
  scale_color_manual(labels = c("Bottom DO","Top DO"), values = c('lightcyan4','lightcyan3')) +
  guides(color=guide_legend("DO Depth",override.aes=list(size=5),reverse=TRUE)) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-15")), linetype=4, color = "black") +
  theme_classic() + 
  ggtitle("Witch Hole Pond")
plot.wh.dosat



#PAR ####
#JP
jp.par = read.delim('sampling/buoyJP/PAR/2020-06-13/Cat.txt',sep=',')
#delete and dplyr::rename columns
#WARNING - used miniDOTconcatenate, not miniPAR, so need to dplyr::rename columns as PAR. double checked txt files
jp.par = jp.par %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Dissolved.Oxygen.Saturation) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('PAR'='Dissolved.Oxygen') %>% 
  dplyr::rename('Ax'='Q') %>% 
  dplyr::rename('Temerature_par'='Temperature') %>% 
  mutate(ID='JPpar') %>% 
  slice(-1,) %>% 
  filter(as.Date(DateTime) > as.Date('2020-02-21 00:00:00')) #delete old date
#SC
sc.par = read.delim('sampling/buoySC/PAR/2020-06-14/Cat.txt',sep=',')
#delete and dplyr::rename columns
#WARNING - used miniDOTconcatenate, not miniPAR, so need to dplyr::rename columns as PAR. double checked txt files
sc.par = sc.par %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Dissolved.Oxygen.Saturation) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('PAR'='Dissolved.Oxygen') %>% 
  dplyr::rename('Ax'='Q') %>% 
  dplyr::rename('Temerature_par'='Temperature') %>% 
  mutate(ID='SCpar') %>% 
  slice(-1,)
#WH
wh.par = read.delim('sampling/buoyWH/PAR/2020-06-13/Cat.txt',sep=',')
#delete and dplyr::rename columns
#WARNING - used miniDOTconcatenate, not miniPAR, so need to dplyr::rename columns as PAR. double checked txt files
wh.par = wh.par %>% 
  select(-Unix.Timestamp,-UTC_Date_._Time,-Battery,-Dissolved.Oxygen.Saturation) %>%
  dplyr::rename('DateTime'='Eastern.Standard.Time') %>% 
  dplyr::rename('PAR'='Dissolved.Oxygen') %>% 
  dplyr::rename('Ax'='Q') %>% 
  dplyr::rename('Temerature_par'='Temperature') %>% 
  mutate(ID='WHpar') %>% 
  slice(-1,)


#*PLOTS ####
#JP PAR
plot.jp.par = ggplot(jp.par, aes(x=as.Date(DateTime), y=log(PAR), color=ID)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_smooth(se = TRUE, method = "gam", color = "black", size = 1, 
    formula = y ~ s(x, bs = "cs", k = 16), span = 0.8) +
  xlab(" ") + 
  scale_x_date(limits=as.Date(c('2020-02-26','2020-06-12')), date_breaks = "1 month")  +
  ylab(" ") + 
  ylim(0,10) +
  scale_color_manual(labels='log(PAR)', values='black') +
  guides(color=guide_legend(' ',override.aes=list(size=5))) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-15")), linetype=4, color = "black") +
  theme_classic() + 
  ggtitle(" ")
plot.jp.par 

#SC PAR
#coerce
sc.par$PAR = as.numeric(sc.par$PAR)
#plot
plot.sc.par = ggplot(sc.par, aes(x=as.Date(DateTime), y=log(PAR), color=ID)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_smooth(se = TRUE, method = "gam", color = "black", size = 1,
    formula = y ~ s(x, bs = "cs", k = 16), span = 0.8) +
  xlab(" ") + 
  scale_x_date(limits=as.Date(c('2020-02-26','2020-06-12')), date_breaks = "1 month")  +
  ylab(bquote('PAR ('*mu~ 'mol' ~m^-2~s^-1*')')) + 
  ylim(0,10) +
  scale_color_manual(labels='log(PAR)', values='black') +
  guides(color=guide_legend(' ',override.aes=list(size=5))) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-15")), linetype=4, color = "black") +
  theme_classic() + 
  ggtitle(" ")
plot.sc.par 

#WH PAR
#coerce
wh.par$PAR = as.numeric(wh.par$PAR)
#plot
plot.wh.par = ggplot(wh.par, aes(x=as.Date(DateTime), y=log(PAR), color=ID)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_smooth(se = TRUE, method = "gam", color = "black", size = 1,
    formula = y ~ s(x, bs = "cs", k = 16), span = 0.8) +
  xlab("Date") + 
  scale_x_date(limits=as.Date(c('2020-02-26','2020-06-12')), date_breaks = "1 month")  +
  ylab(" ") + 
  ylim(0,10) +
  scale_color_manual(labels='log(PAR)', values='black') +
  guides(color=guide_legend(' ',override.aes=list(size=5))) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-15")), linetype=4, color = "black") +
  theme_classic() + 
  ggtitle(" ")
plot.wh.par


#Combine ####
#combined DO plots
#grid.arrange(plot.jp.do,plot.sc.do,plot.wh.do)
plot.do.3L = ggarrange(plot.jp.do,plot.sc.do,plot.wh.do, ncol=1, nrow=3, common.legend=TRUE, legend=c("bottom"))
plot.do.3L 
#sat
#grid.arrange(plot.jp.do,plot.sc.do,plot.wh.do)
plot.dosat.3L = ggarrange(plot.jp.dosat,plot.sc.dosat,plot.wh.dosat, ncol=1, nrow=3, common.legend=TRUE, legend=c("bottom"))
plot.dosat.3L 


#combine PAR plots
plot.par.3L = ggarrange(plot.jp.par,plot.sc.par,plot.wh.par, ncol=1, nrow=3, common.legend=TRUE, legend="bottom")
plot.par.3L

#full sat
plot.dosat.par.3L = ggarrange(plot.dosat.3L, plot.par.3L, ncol=2,nrow=1)
plot.dosat.par.3L
#three buoy lakes
plot.do.par.3L = ggarrange(plot.do.3L, plot.par.3L, ncol=2,nrow=1)
plot.do.par.3L




#SHIT TO LEARN
  #annotate labels better. one x and y axis
  #annotate change date on x axis to "May 20











