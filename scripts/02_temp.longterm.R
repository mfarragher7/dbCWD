#Longterm temperature analysis
#created 2023-01-17


#libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rLakeAnalyzer)
library(ggpubr)

#load profile summary
pro = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/profiles.1975-2021.summary.csv", header=T)


#format dates
pro = pro %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(dm = format(as.Date(date), "%m-%d"))
str(pro)

pro = pro %>% 
  mutate(mm = ifelse(month==1, 'Jan', NA)) %>%
  mutate(mm = replace(mm, month==2, 'Feb')) %>%
  mutate(mm = replace(mm, month==3, 'Mar')) %>%
  mutate(mm = replace(mm, month==4, 'Apr')) %>%
  mutate(mm = replace(mm, month==5, 'May')) %>%
  mutate(mm = replace(mm, month==6, 'Jun')) %>%
  mutate(mm = replace(mm, month==7, 'Jul')) %>%
  mutate(mm = replace(mm, month==8, 'Aug')) %>%
  mutate(mm = replace(mm, month==9, 'Sep')) %>% 
  mutate(mm = replace(mm, month==10, 'Oct')) %>%
  mutate(mm = replace(mm, month==11, 'Nov')) %>%
  mutate(mm = replace(mm, month==12, 'Dec'))

pro$mm = factor(pro$mm, levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
str(pro)

#check for dups
temp = plyr::count(pro$sampID)



#test temperature values. one lake, all temp variables
pppro = filter(pro, lake=='pleasant' & (dm >= '06-01' & dm <= '09-14') & station==1)

#long df epi, hypo, top5m, mean, min, max
pptempvars = pppro %>% 
  tidyr::pivot_longer(cols = c(temp.min, temp.max, temp.mean, temp.epi, temp.hypo, temp.top5m),
               names_to = 'temp.param',
               values_to = 'temp.value') 


pptempvars$temp.param = factor(pptempvars$temp.param, levels=c('temp.min', 'temp.mean', 'temp.max', 'temp.epi', 'temp.hypo', 'temp.top5m'))
pptempvars$date = as.Date(pptempvars$date)
str(pptempvars)


#plot
ggplot(pptempvars, aes(x=date, y=temp.value, color=temp.param)) +
  geom_point() +
  #geom_line(linetype=3, show.legend=F) +
  stat_smooth(method="loess", linewidth=0.75, se=F, show.legend=F) + 
  facet_wrap(~mm, ncol=2, nrow=2, scale='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_color_manual(values=c('red3','blue','gray15','green','pink','orange'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Pleasant Pond',
       x="Date",
       y="Temp C") + 
  theme(title=element_text(size=10))


#just mean temp, epi, and top5
ggplot(filter(pptempvars, 
              temp.param=='temp.mean' | temp.param=='temp.epi' | temp.param=='temp.top5m'),
       aes(x=date, y=temp.value, color=temp.param)) +
  geom_point() +
  #geom_line(linetype=3, show.legend=F) +
  stat_smooth(method="loess", linewidth=0.75, se=F, show.legend=F) + 
  facet_wrap(~mm, ncol=2, nrow=2, scale='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_color_manual(values=c('red','blue','green'),
                     guide=guide_legend(NULL,override.aes=list(shape=16,size=2))) +
  labs(title='Pleasant Pond',
       x="Date",
       y="Temp C") + 
  theme_bw() +
  theme(title=element_text(size=10))



#workflow
#use top 5m temp for now. split stations, maybe use just deep site eventually
##one lake at a time too
#june through sep 14
#all sample dates first, then think about monthly averages
#Also: consider deep vs shallow lakes, separate stations, other envir vars
#time series stats: mann-kendall, what else


#Anna#####
#annabessacook first
#stations as factors
pro$station = factor(pro$station, levels=c(1,2,3,4,5,6,7))

#* all profiles #####
ggplot(filter(pro, lake=='annabessacook' & 
                (dm >= '06-01' & dm <= '09-15') &
                (station==1 | station==2)), 
       aes(x=date, y=temp.top5m, 
           color=station)) +
  geom_point(shape=19,
             alpha=0.75) +
  geom_line(stat="smooth",
            method='loess',
            linewidth = 0.75,
            linetype ="solid",
            alpha = 0.25,
            show.legend = F)  +
  #stat_smooth(method="loess", linewidth=0.75, se=F, show.legend=F, mapping=aes(alpha=0.1)) + 
  facet_wrap(~mm, 
             ncol=2, nrow=2, scale='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(limits=c(12,28)) +
  scale_color_manual(values=c('blue','green')) +
  labs(title='Annabessacook Temperature Top 5m',
       x="Date",
       y="Temp C",
       color='Station') +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))






#* monthly ######
#summarize profile stats to one point per month
#combines dep and cwd dbs, projects, etc, but not stations
#average min, average max, average 'mean'.... st.dev of means from profiles
#add fake date (yyyy-mm-01) so it can still be read in like a date
names(pro)

#monthly summary
pro$ym = paste(pro$year, pro$mm, sep='-')
#just summer
mmprosummer = pro %>% filter(dm >= '06-01' & dm <= '09-15')
  

mmpro = ddply(mmprosummer, 
              .(midas, lake, station, ym),
              summarize, 
              #n profiles
              n.profiles = length(unique(sampID)),
              #temp
              mm.temp.min = mean(temp.min), #avg monthly min
              mm.temp.max = mean(temp.max), #avg monthly max
              mm.temp.mean = mean(temp.mean), #avg monthly means
              mm.sd.temp.mean = sd(temp.mean), #sd of monthly means
              mm.thermo.depth = mean(thermo.depth),
              mm.temp.epi = mean(temp.epi),
              mm.temp.hypo = mean(temp.hypo),
              mm.temp.top5m = mean(temp.top5m),
              mm.meta.top = mean(meta.top),
              mm.meta.bottom = mean(meta.bottom),
              #do
              mm.do.min = mean(do.min), 
              mm.do.max = mean(do.max),
              mm.do.mean = mean(do.mean),
              mm.do.maxdepth = mean(do.max.depth))

#see if that worked....
plyr::count(mmpro$n.profiles)
#most cases = 1 profile each month

#test
pro.test1 = filter(mmpro, lake=='cochnewagon', ym=='2019-Jul' )
pro.test2 = filter(pro, lake=='cochnewagon', ym=='2019-Jul' )
#check if these match. they do
head(pro.test1)
mean(pro.test2$temp.epi)
mean(pro.test2$temp.hypo)
mean(pro.test2$thermo.depth)
#NA for stdev columns when n=1 which is correct

#split year and month
mmpro = mmpro %>% separate(ym, c("year","mm"), sep="-", remove=F)
mmpro$mm = factor(mmpro$mm, levels=c('Jun','Jul','Aug','Sep'))
#months as numbers again
mmpro = mmpro %>% 
  mutate(month = ifelse(mm=='Jun', 6, NA)) %>% 
  mutate(month = replace(month, mm=='Jul', 7)) %>% 
  mutate(month = replace(month, mm=='Aug', 8)) %>% 
  mutate(month = replace(month, mm=='Sep', 9))
mmpro$year = as.numeric(mmpro$year)
str(mmpro)


#stations as factors

#plot monthly avg all years
ggplot(filter(mmpro, lake=='annabessacook' &
                (station==1 | station==2)), 
       aes(x=year, y=mm.temp.top5m, 
           color=station)) +
  geom_point(shape=19,
             alpha=0.75) +
  geom_line(stat="smooth",
            method='loess',
            size = 0.75,
            linetype ="solid",
            alpha = 0.25,
            show.legend = F)  +
  #stat_smooth(method="loess", linewidth=0.75, se=F, show.legend=F, mapping=aes(alpha=0.1)) + 
  facet_wrap(~mm, 
             ncol=2, nrow=2, scale='fixed') +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(12,28)) +
  scale_color_manual(values=c('blue','green')) +
  labs(title='Annabessacook temp top 5m monthly mean',
       x="Date",
       y="Temp C",
       color='Station') +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))






#* yearly ######
#one point per year (june july and august, thru sep 14)
summerpro = pro %>% 
  filter(dm >= '06-01' & dm <= '09-15')
temp = plyr::count(summerpro$year)
sum(temp$freq)
length(summerpro$sampID)
#stations as factors

yrpro = ddply(summerpro,
              .(lake, station, year),
              summarize, 
              #n profiles
              n.profiles = length(unique(sampID)),
              #temp
              yr.temp.min = mean(temp.min), #avg monthly min
              yr.temp.max = mean(temp.max), #avg monthly max
              yr.temp.mean = mean(temp.mean), #avg monthly means
              yr.sd.temp.mean = sd(temp.mean), #sd of monthly means
              yr.thermo.depth = mean(thermo.depth),
              yr.temp.epi = mean(temp.epi),
              yr.temp.hypo = mean(temp.hypo),
              yr.temp.top5m = mean(temp.top5m),
              yr.meta.top = mean(meta.top),
              yr.meta.bottom = mean(meta.bottom),
              #do
              yr.do.min = mean(do.min), 
              yr.do.max = mean(do.max),
              yr.do.mean = mean(do.mean),
              yr.do.maxdepth = mean(do.max.depth))

unique(yrpro$station)
yrpro$station = factor(yrpro$station, levels=c(1,2,3,4,5,6,7))


#test
test = pro %>% 
  filter(lake=='annabessacook' &
           (dm >= '06-01' & dm <= '09-15'))
plyr::count(test$year)
test2 = summerpro %>% filter(lake=='annabessacook')
plyr::count(test2$year)
#difference in counts is from different # stations

#plot yearly annabessacook
#plot monthly avg all years
ggplot(filter(yrpro, 
              lake=='annabessacook' & 
                (station==1 | station==2)),
       aes(x=year, y=yr.temp.top5m, 
           color=station)) +
  geom_point(shape=19,
             alpha=0.75) +
  geom_line(stat="smooth",
            method='loess',
            size = 0.75,
            linetype ="solid",
            alpha = 0.25,
            show.legend = F)  +
  #scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(18,26)) +
  scale_color_manual(values=c('blue','green')) +
  labs(title='Annabessacook temp top 5m summer mean',
       x="Date",
       y="Temp C",
       color='Station') +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))







#COBB #####
cobbpro = pro %>% filter(lake=='cobbossee')
plyr::count(cobbpro$station) #2 stations
cprosep = pro %>% filter(lake=='cobbossee' & mm=='Sep')
cprosep = cprosep %>% 
  mutate(earlysep = ifelse((dm >= '09-01' & dm <= '09-14'),'1-14','15-30'))
plyr::count(cprosep$earlysep) 
#42 early september values, 74 late sep values

summary(filter(cobbpro, dm >= '06-01' & dm <= '09-14')$temp.top5m) #one NA point




#* all pro #####
ggplot(filter(pro, lake=='cobbossee' & 
                (dm >= '06-01' & dm <= '09-15') & 
         (station==1 | station==2)),
aes(x=date, y=temp.top5m, 
    color=station)) +
  geom_point(shape=19,
             alpha=0.75) +
  geom_line(stat="smooth",
            method='loess',
            linewidth = 0.75,
            linetype ="solid",
            alpha = 0.25,
            show.legend = F)  +
  facet_wrap(~mm, 
             ncol=2, nrow=2, 
             scale='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(limits=c(12,28)) +
  scale_color_manual(values=c('blue','green')) +
  labs(title='Cobbossee Temperature Top 5m all profiles',
       x="Date",
       y="Temp C",
       color='Station') +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))



#* month ###########

#plot monthly avg all years
ggplot(filter(mmpro, lake=='cobbossee' &
                (station==1 | station==2)), 
       aes(x=year, y=mm.temp.top5m, 
           color=station)) +
  geom_point(shape=19,
             alpha=0.75) +
  geom_line(stat="smooth",
            method='loess',
            size = 0.75,
            linetype ="solid",
            alpha = 0.25,
            show.legend = F)  +
  facet_wrap(~mm, 
             ncol=2, nrow=2, 
             scale='fixed') +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(12,28)) +
  scale_color_manual(values=c('blue','green')) +
  labs(title='Cobbossee temp top 5m monthly mean',
       x="Date",
       y="Temp C",
       color='Station') +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))


#* year ###########
ggplot(filter(yrpro, 
              lake=='cobbossee' & 
                (station==1 | station==2)),
       aes(x=year, y=yr.temp.top5m, 
           color=station)) +
  geom_point(shape=19,
             alpha=0.75) +
  geom_line(stat="smooth",
            method='loess',
            size = 0.75,
            linetype ="solid",
            alpha = 0.25,
            show.legend = F)  +
  #scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(18,26)) +
  scale_color_manual(values=c('blue','green')) +
  labs(title='Cobbossee temp top 5m summer mean',
       x="Date",
       y="Temp C",
       color='Station') +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))





#PLEASANT #########

#check
pppro = pro %>% filter(lake=='pleasant')
plyr::count(pppro$station) #3 stations, plot 1 and 3
summary(filter(pppro, dm >= '06-01' & dm <= '09-14')$temp.top5m)

#* all pro #####
ggplot(filter(pro, lake=='pleasant' & 
                (dm >= '06-01' & dm <= '09-15') & 
                (station==1 | station==3)),
       aes(x=date, y=temp.top5m, 
           color=station)) +
  geom_point(shape=19,
             alpha=0.75) +
  geom_line(stat="smooth",
            method='loess',
            linewidth = 0.75,
            linetype ="solid",
            alpha = 0.25,
            show.legend = F)  +
  facet_wrap(~mm, 
             ncol=2, nrow=2, 
             scale='fixed') +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(limits=c(12,28)) +
  scale_color_manual(values=c('blue','green')) +
  labs(title='Pleasant temperature top 5m all profiles',
       x="Date",
       y="Temp C",
       color='Station') +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))



#* month ###########

#plot monthly avg all years
ggplot(filter(mmpro, lake=='pleasant' &
                (station==1 | station==3)), 
       aes(x=year, y=mm.temp.top5m, 
           color=station)) +
  geom_point(shape=19,
             alpha=0.75) +
  geom_line(stat="smooth",
            method='loess',
            size = 0.75,
            linetype ="solid",
            alpha = 0.25,
            show.legend = F)  +
  facet_wrap(~mm, 
             ncol=2, nrow=2, 
             scale='fixed') +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(12,28)) +
  scale_color_manual(values=c('blue','green')) +
  labs(title='Pleasant temp top 5m monthly mean',
       x="Date",
       y="Temp C",
       color='Station') +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))


#* year ###########
ggplot(filter(yrpro, 
              lake=='pleasant' & 
                (station==1 | station==3)),
       aes(x=year, y=yr.temp.top5m, 
           color=station)) +
  geom_point(shape=19,
             alpha=0.75) +
  geom_line(stat="smooth",
            method='loess',
            size = 0.75,
            linetype ="solid",
            alpha = 0.25,
            show.legend = F)  +
  #scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(18,28)) +
  scale_color_manual(values=c('blue','green')) +
  labs(title='Pleasant temp top 5m summer mean',
       x="Date",
       y="Temp C",
       color='Station') +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))






#Trends ##############
#Trend analysis

#mann kendall
#sens slope

library(Kendall)
library(zyp)
library(trend)
#compare packages/methods

#test cobbossee august temps
cobbaug = mmpro %>% filter(lake=='cobbossee' & station==1, mm=='Aug')

#mean temp from profile. not vol weighted btw
ggplot(cobbaug, aes(x=year,y=mm.temp.mean)) + geom_point()
#same mk results
Kendall::MannKendall(cobbaug$mm.temp.mean)
trend::mk.test(cobbaug$mm.temp.mean)
#same sens slope
zyp::zyp.sen(mm.temp.mean~year, cobbaug)
trend::sens.slope(cobbaug$mm.temp.mean)


#top 5m temps
ggplot(cobbaug, aes(x=year,y=mm.temp.top5m)) + geom_point()
cobbaug = cobbaug %>% drop_na(mm.temp.top5m)
mk = Kendall::MannKendall(cobbaug$mm.temp.top5m)
ss = trend::sens.slope(cobbaug$mm.temp.top5m)

#sens slope = .055
# 0.55 deg C warming per decade
ss
ss[1] # sens slope *
ss[2] # z stat
ss[3] # p value *
ss[4] # null value...
ss[5] # alternative...
ss[6] # data...
ss[7] # method....
ss[8] # n
ss[9] # conf level

mk
mk[1] #tau
mk[2] #2 sided p value


#what about yearly values?

anna.yr = yrpro %>% filter(lake=='annabessacook', station==1) 
ggplot(anna.yr, aes(x=year,y=yr.temp.top5m)) + geom_point()
mk = Kendall::MannKendall(anna.yr$yr.temp.top5m)
ss = trend::sens.slope(anna.yr$yr.temp.top5m)
mk[1]
mk[2]
ss[1]
ss[3]




#* year #########
#mann kendall and sens slope for each lake in summary table. 

yrprosub = yrpro %>% 
  filter(station==1) %>% 
  drop_na(yr.temp.top5m)

ytrends = ddply(yrprosub, .(lake), summarize, 
                n.years = length(unique(year)),
                n.pro = sum(n.profiles),
                temp.5m.mean = mean(yr.temp.top5m),
                temp.5m.sd = sd(yr.temp.top5m),
                temp.5m.min = min(yr.temp.top5m),
                temp.5m.max = max(yr.temp.top5m),
                mk.tau = NA,
                mk.p = NA,
                sens.slope = NA,
                sens.p = NA)
                
lakes = unique(ytrends$lake)                
for (i in 1:length(lakes)){ #for every lake
  td = yrprosub[yrprosub$lake == lakes[i], ] 
  mk = Kendall::MannKendall(td$yr.temp.top5m) #run mk fxn
  ss = trend::sens.slope(td$yr.temp.top5m) #run sens slope fxn
  ytrends[i,8] = mk[1]  #mk tau
  ytrends[i,9] = mk[2]  #mk p
  ytrends[i,10] =  ss[1]  #sens slope
  ytrends[i,11] =  ss[3]  #sens p
}
  
mean(ytrends$sens.slope) * 10
# CWD lakes are warming by 0.3 degrees C per decade !!!


#* month #########
# to do:

#ID of lake-month-year
prosub = pro %>% filter(station==1)

#add new category for earlysep and late sep

prosub = prosub %>% 
  mutate(Sep = ifelse((dm >= '09-01' & dm <= '09-14'),'earlySep',
                      ifelse((dm >= '09-15' & dm <= '09-30'), 'lateSep',NA)))
  
prosub$lake.ym = paste(prosub$lake, prosub$ym, sep='_')

#averages together temp values by month
monthpro = ddply(prosub, 
                 .(midas, lake, lake.ym, year, month, mm),
                 summarize, 
                 #n profiles
                 n.profiles = length(unique(sampID)),
                 #temp
                 mm.temp.top5m = mean(temp.top5m, na.rm=T))

#4085 unique lake-year-month. ~30 * ~6 * ~6 = >5000 makes sense


#check
ggplot(cobbaug, aes(x=year,y=mm.temp.top5m)) + geom_point()
ggplot(filter(monthpro, lake=='cobbossee' & month==8),
       aes(x=year,y=mm.temp.top5m)) + geom_point()
#gained one point somehow.........

#check some more
unique(monthpro$lake.m)
length(unique(monthpro$lake.m))
temp = plyr::count(monthpro$lake.ym)


#summarize each month
monthpro$lake.m = paste(monthpro$lake, monthpro$mm, sep='_')

#long term averages for each month for each lake. 
monthtrends = ddply(monthpro, 
                    .(midas, lake, lake.m, month, mm),
                    summarize, 
                    n.pro = sum(n.profiles),
                    n.year = length(unique(year)),
                    temp.5m.mean = mean(mm.temp.top5m),
                    temp.5m.sd = sd(mm.temp.top5m),
                    temp.5m.min = min(mm.temp.top5m),
                    temp.5m.max = max(mm.temp.top5m),
                    mk.tau = NA,
                    mk.p = NA,
                    sens.slope = NA,
                    sens.p = NA)

#for each lake-month....
lake.m = unique(monthpro$lake.m)   



##########################################
# needs work !!!




for (i in 1:length(lake.m)){ #for every lake
  td = monthpro[monthpro$lake.m == lake.m[i], ] 
  # if n >2 run stats, # if n = 1 or 2, return NA
  ifelse(length(td$mm.temp.top5m) > 2, 
         { 
           mk = Kendall::MannKendall(td$mm.temp.top5m) #run mk fxn
           ss = trend::sens.slope(td$mm.temp.top5m) #run sens slope fxn
           monthtrends[i,12] = mk[1]  #mk tau
           monthtrends[i,13] = mk[2]  #mk p
           monthtrends[i,14] =  ss[1]  #sens slope
           monthtrends[i,15] =  ss[3] 
         }, 
         {
           monthtrends[i,12] = NA
           monthtrends[i,13] = NA
           monthtrends[i,14] = NA
           monthtrends[i,15] = NA}
  )
}







