#new script for just LSM work, 2023-04-19
#making figures and stats tables for LSM conference
#pulled from other temp



#libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rLakeAnalyzer)
library(nlme)
library(ggpubr)

#outline
#1. Surface temperature trends
  #Overall trends 
  #Monthly trends
  #Shallow vs deep lakes
    #Shallow monthly
    #Deep monthly

#2. Hypolimnion temperature
  #Shallow vs deep 

#3. Thermocline depth 
  #Shallow vs deep


#*reload data ######
pro = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/profiles.1975-2022.summary.csv",header=T)
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

#from yrpro df, one point per year ft. june, july, august, sep 1-14
summerpro = pro %>% filter(dm >= '06-01' & dm <= '09-15')
temp = plyr::count(summerpro$year) 
sum(temp$freq)
length(summerpro$sampID)

yrpro = ddply(summerpro,
              .(lake, midas, station, year),
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

#yrprosub, it's just station 1 from each lake and na.rm for temp.top5m
yrprosub = yrpro %>% 
  filter(station==1) %>% 
  drop_na(yr.temp.top5m)

unique(yrprosub$lake)
names(yrprosub)

yrprosub = yrprosub %>% 
  mutate(lake.yr = paste(lake, year, sep='_'))

#Surface temps #################

#top 5m
ggplot(yrprosub, aes(x=year, y=yr.temp.top5m, color=lake)) +
  geom_point(shape=1, alpha=0.25) +
  geom_line(stat="smooth", method='loess', linewidth = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(16,26),
                     n.breaks = 6) +
  #scale_color_manual(values=c('blue','green')) +
  labs(title='Summer mean surface temperature (June to 14-Sep)',
       x="Date",
       y="Temp C",
       color='Lake')

# decreasing temp in recent years in shed
ggplot(filter(yrprosub, lake=='shed'),aes(x=year,y=yr.temp.top5m,color=lake)) +
  geom_point(shape=1,alpha=0.50) +
  geom_line(stat="smooth",method='loess',linewidth = 0.75,
            linetype ="solid",alpha = 0.75,show.legend = F)

# weird dip is because of Hutchinson pond 
ggplot(filter(yrprosub, lake=='hutchinson'),aes(x=year,y=yr.temp.top5m,color=lake)) +
  geom_point(shape=1,alpha=0.50) +
  geom_line(stat="smooth",method='loess',linewidth = 0.75,
            linetype ="solid",alpha = 0.75,show.legend = F)

# drop lake.yr hutchinson_1989 and jimmy_1983, little purgatory_1983
# long gaps in between years
yrprosub.droppedgaps = yrprosub %>% 
  filter(!lake.yr == 'hutchinson_1989') %>% 
  filter(!lake.yr == 'jimmy_1983') %>% 
  filter(!lake.yr == 'little purgatory_1983')


#*FIG - all lakes #######
#all lakes, summer surface temps, one trendline

#function for getting labels
lm_eqn = function(yrprosub.droppedgaps){
  m = lm(yr.temp.top5m ~ year, yrprosub.droppedgaps);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}

#save label
eq = ddply(yrprosub.droppedgaps,.(), lm_eqn)

#change geom_text size
update_geom_defaults("text", list(size = 5))

ggplot(yrprosub.droppedgaps, 
       aes(x=year, y=yr.temp.top5m)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', linewidth = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(16,26), n.breaks = 6) +
  geom_text(data=eq,aes(x=1985, y=25.5,label=V1), 
            parse=T, inherit.aes=F) +
  labs(title='Mean Summer surface temperature trends',
       x="Date",
       y="Temp C",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))



#turn off sci notification 
options(scipen=999)

#** stats 1 ######
ytrends = ddply(yrprosub.droppedgaps, 
                .(lake), 
                summarize, 
                n.years = length(unique(year)),
                first.year = min(year),
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
  td = yrprosub.droppedgaps[yrprosub.droppedgaps$lake == lakes[i], ] 
  mk = Kendall::MannKendall(td$yr.temp.top5m) #run mk fxn
  ss = trend::sens.slope(td$yr.temp.top5m) #run sens slope fxn
  ytrends[i,9] = mk[1]  #mk tau
  ytrends[i,10] = mk[2]  #mk p
  ytrends[i,11] =  ss[1]  #sens slope
  ytrends[i,12] =  ss[3]  #sens p
}

#change per decade, multiply by 10
mean(ytrends$sens.slope) * 10


#useless, but.....
#now pool all temperatures together and run trend analysis
pooled.temps = ddply(yrprosub.droppedgaps, 
                     .(year), 
                     summarize, 
                     n.lakes = length(unique(lake)),
                     n.pro = sum(n.profiles),
                     temp.5m.mean = mean(yr.temp.top5m),
                     temp.5m.sd = sd(yr.temp.top5m),
                     temp.5m.min = min(yr.temp.top5m),
                     temp.5m.max = max(yr.temp.top5m))

mk = Kendall::MannKendall(pooled.temps$temp.5m.mean) #run mk fxn
ss = trend::sens.slope(pooled.temps$temp.5m.mean) #run sens slope fxn
mk[1]  #mk tau
mk[2]  #mk p
ss[1]  #sens slope
ss[3]  #sens p

ggplot(yrprosub.droppedgaps, 
       aes(x=year, y=yr.temp.top5m)) +
  geom_point(shape=1, alpha=0.5) +
  geom_line(stat="smooth", method='loess', linewidth = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(16,26), n.breaks = 6) +
  #labs(title='Mean Summer surface temperature trends',
  #    x="Date",
  #   y="Temp C",
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))




#GLS ############
#attempting to assess variance of lt trends
yrgls = yrprosub.droppedgaps
test = yrgls %>% filter(lake=='cobbossee')

#one lake
mod.gls = gls(yr.temp.top5m ~ year,
              data=test, 
              correlation=corARMA(p=2),
              method="ML")
summary(mod.gls)




#*FIG - sens slope ############
#show direction, magnitude, significance of change for each lake
#bold lakes that are significant

ytrends = ytrends %>% 
  mutate(dir = ifelse(sens.slope > 0, 'pos','neg')) %>% 
  mutate(lakes.sig = ifelse(sens.p < 0.05, paste('*',lake,sep=' '),lake)) %>% 
  mutate(change.per.decade = sens.slope * 10)
#another way
ytrends = ytrends %>% 
  mutate(sig = ifelse(sens.p < 0.05, 1, 0)) #if significant, 1


#directional change fig
ggplot(ytrends,
       aes(reorder(lake, change.per.decade),
           change.per.decade, 
           fill=dir,
           label=ifelse(sig==1, "*", NA))) +
  geom_bar(stat='identity') +
  geom_hline(yintercept=0, 
             linetype=5,
             linewidth=0.5,
             color='gray30') +
  geom_text(hjust = -0.5,
            vjust = 0.65,
            size = 6) + 
  scale_fill_manual(values = c('#37C1E6','#FF8778')) +
  coord_flip() +
  scale_y_continuous(limits=c(-0.5, 2)) +
  scale_x_discrete(labels=c("annabessacook"=expression(bold(annabessacook)),
                            "cobbossee"=expression(bold(cobbossee)),
                            "cochnewagon"=expression(bold(cochnewagon)),
                            "maranacook"=expression(bold(maranacook)),
                            "narrows lower"=expression(bold('narrows lower')),
                            "pleasant"=expression(bold(pleasant)),
                            "torsey"=expression(bold(torsey)),
                            "wilson"=expression(bold(wilson)))) +
  labs(y = 'Temperature change (\u00b0C \u00b7 decade\U207b\u00b9)',
       x = NULL,
       title = 'Change in mean summer surface water temperature') +
  theme_bw() + 
  theme(title=element_text(size=10),
        legend.position = "none")




#*MONTHLY #############
#reload data 

#ID of lake-month-year
prosub = pro %>% filter(station==1)
prosub$lake.ym = paste(prosub$lake, prosub$ym, sep='_')

#averages together temp values by month
monthpro = ddply(prosub, 
                 .(midas, lake, lake.ym, year, month, mm),
                 summarize, 
                 #n profiles
                 n.profiles = length(unique(sampID)),
                 #temp
                 mm.temp.top5m = mean(temp.top5m),
                 mm.td = mean(thermo.depth),
                 mm.hypo = mean(temp.hypo),
                 lake.m = paste(lake, mm, sep='_'))

#look at monthly avg
monthprosub = monthpro %>% 
  filter(mm=='May'|
           mm=='Jun'|
           mm=='Jul'|
           mm=='Aug'|
           mm=='Sep'|
           mm=='Oct') %>% 
  drop_na(mm.temp.top5m)

#top 5m
ggplot(monthprosub, aes(x=year, y=mm.temp.top5m, color=lake)) +
  geom_point(shape=1, alpha=0.25) +
  geom_line(stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  facet_wrap(~month, ncol=3, nrow=2) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(5,30), n.breaks = 6) 

#why sep no loess
ggplot(filter(monthprosub, month==9),
       aes(x=year, y=mm.temp.top5m, color=lake)) +
  geom_point(shape=1, alpha=0.25) +
  geom_line(stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F) 

#check #of months for each lake
mmcheck = plyr::count(monthprosub$lake.m)
#if < 10 values, drop from monthprosub
mmdrop = mmcheck %>% filter(freq < 10)
x = mmdrop$x
monthprosub.droppedsome = monthprosub %>% filter(!lake.m %in% x)

#top 5m
ggplot(monthprosub.droppedsome, aes(x=year, y=mm.temp.top5m, color=lake)) +
  geom_point(shape=1, alpha=0.25) +
  geom_line(stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  facet_wrap(~mm, ncol=3, nrow=2) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(5,30), n.breaks = 6) +
  #scale_color_manual(values=c('blue','green')) +
  labs(title='Mean surface (top 5m) temperature',
       x="Date",
       y="Temp C",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))


#*FIG - monthly ###############
#with one trendline
#add r2 values to each facet

#function for getting labels for each month (mm)
lm_eqn = function(monthprosub.droppedsome){
  m = lm(mm.temp.top5m ~ year, monthprosub.droppedsome);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
#save momth r2 labels
eq = ddply(monthprosub.droppedsome,.(mm),lm_eqn)
#change geom_text size lol
update_geom_defaults("text", list(size = 4))
                     
ggplot(monthprosub.droppedsome, aes(x=year, y=mm.temp.top5m)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  facet_wrap(~mm, ncol=3, nrow=2, scales='fixed') +
  geom_text(data=eq,aes(x=1998, y=29,label=V1), 
            parse=T, inherit.aes=F) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(5,30), n.breaks = 6) +
  labs(title='Mean surface (top 5m) temperature',
       x="Date",
       y="Temperature \u00b0C",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))





#*Deep/shallow ###########

#split deep and shallow lakes
md = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/lakemd.csv', header=T)
md = md %>% 
  filter(station==1) %>% 
  select(midas, depth_m)
#join to other modified profile df
yrprosub.droppedgaps = plyr::join(yrprosub.droppedgaps, md, by='midas')
yrprosub.droppedgaps = yrprosub.droppedgaps %>% 
  rename(max.depth = depth_m)



#*Shallow ############
yrpro.shallow = yrprosub.droppedgaps %>% filter(max.depth <= 10)
unique(yrpro.shallow$lake)

#*FIG - shallow summer temps ########
#overall surface temp trends

#function for getting label
lm_eqn = function(yrpro.shallow){
  m = lm(yr.temp.top5m ~ year, yrpro.shallow);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}

#save label
eq = ddply(yrpro.shallow,.(), lm_eqn)
#change geom_text size lol
update_geom_defaults("text", list(size = 5))

ggplot(yrpro.shallow, 
       aes(x=year, y=yr.temp.top5m)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 1,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1982, y=25, label=V1), 
            parse=T, inherit.aes=F) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(16,26), n.breaks = 6) +
  #scale_color_manual(values = palette.colors(palette = "Okabe-Ito")) +
  labs(title='Mean Summer surface temperature trends - Shallow lakes (\u2264 10m)',
       x="Date",
       y="Temp C",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))






#*FIG - shallow monthly ##########
#add depth to month df
monthprosub.droppedsome = plyr::join(monthprosub.droppedsome, md, by='midas')
monthprosub.droppedsome = monthprosub.droppedsome %>% 
  rename(max.depth = depth_m)
#get shallow df for monthly breakdown
mpro.shallow = monthprosub.droppedsome %>% 
  filter(max.depth <= 10)

#get labels
lm_eqn = function(mpro.shallow){
  m = lm(mm.temp.top5m ~ year, mpro.shallow);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
#save momth r2 labels
eq = ddply(mpro.shallow,.(mm),lm_eqn)
#change geom_text size lol
update_geom_defaults("text", list(size = 4))


#surface temp 
ggplot(mpro.shallow, aes(x=year, y=mm.temp.top5m)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1995, y=28, label=V1), 
            parse=T, inherit.aes=F) +
  facet_wrap(~mm, ncol=3, nrow=2) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(5,30), n.breaks = 6) +
  #scale_color_manual(values=c('blue','green')) +
  labs(title='Mean monthly temperature - Shallow lakes (\u2264 10m)',
       x="Date",
       y="Temp C",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))







#*Deep ###############
#station ones, deep lakes (>10m), dropped years with many yrs of no data in between, see above for deets
yrpro.deep = yrprosub.droppedgaps %>% filter(max.depth > 10)
unique(yrpro.deep$lake)


#*FIG - deep summer temps ########
#overall surface temp trends

#function for getting label
lm_eqn = function(yrpro.deep){
  m = lm(yr.temp.top5m ~ year, yrpro.deep);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
#save label
eq = ddply(yrpro.deep,.(), lm_eqn)
#change geom_text size lol
update_geom_defaults("text", list(size = 5))

#one trendline
ggplot(yrpro.deep, 
       aes(x=year, y=yr.temp.top5m)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq, aes(x=1983, y=25.5, label=V1), 
            parse=T, inherit.aes=F) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(16,26), n.breaks = 6) +
  #scale_color_manual(values = palette.colors(palette = "Okabe-Ito")) +
  labs(title='Mean Summer surface temperature trends - Deep lakes (>10m)',
       x="Date",
       y="Temp C",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))





#*FIG - deep monthly #######
mpro.deep = monthprosub.droppedsome %>% filter(max.depth > 10)
unique(mpro.deep$lake)

#with one trendline
#function for getting labels for each month (mm)
lm_eqn = function(mpro.deep){
  m = lm(mm.temp.top5m ~ year, mpro.deep);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(mpro.deep,.(mm),lm_eqn)
#change geom_text size
update_geom_defaults("text", list(size = 4))


ggplot(mpro.deep, aes(x=year, y=mm.temp.top5m)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1995, y=29,label=V1), 
            parse=T, inherit.aes=F) +
  facet_wrap(~mm, ncol=3, nrow=2) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(5,30), n.breaks = 6) +
  #scale_color_manual(values=c('blue','green')) +
  labs(title='Mean monthly surface temperature - Deep lakes (>10m)',
       x="Date",
       y="Temp C",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))









#Thermocline ######

#overall trend
lm_eqn = function(yrprosub.droppedgaps){
  m = lm(yr.thermo.depth ~ year, yrprosub.droppedgaps);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(yrprosub.droppedgaps,.(),lm_eqn)

ggplot(yrprosub.droppedgaps,
       aes(x=year, y=yr.thermo.depth)) +
  geom_point(aes(color=lake), shape=1, alpha=0.55) +
  geom_line(aes(color=lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.5,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_text(data=eq,aes(x=1985, y=1,label=V1), 
            parse=T, inherit.aes=F) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_reverse(limits=c(10,0), n.breaks=6) +
  labs(title='Mean thermocline depth Summer (June to 14-Sep)',
       x="Date",
       y="Thermocline depth (m)",
       color='Lake') +   
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))



#*Thermo stats ###### 
thermoclinetrends = ddply(yrprosub.droppedgaps, 
                          .(lake), 
                          summarize, 
                          n.years = length(unique(year)),
                          first.year = min(year),
                          n.pro = sum(n.profiles),
                          td.mean = mean(yr.thermo.depth, na.rm=T),
                          td.sd = sd(yr.thermo.depth, na.rm=T),
                          td.min = min(yr.thermo.depth, na.rm=T),
                          td.max = max(yr.thermo.depth, na.rm=T),
                          mk.tau = NA,
                          mk.p = NA,
                          sens.slope = NA,
                          sens.p = NA)

lakes = unique(thermoclinetrends$lake)                

for (i in 1:length(lakes)){ #for every lake
  td = yrprosub.droppedgaps[yrprosub.droppedgaps$lake == lakes[i], ] 
  td = td %>% drop_na(yr.thermo.depth)
  mk = Kendall::MannKendall(td$yr.thermo.depth) #run mk fxn
  ss = trend::sens.slope(td$yr.thermo.depth) #run sens slope fxn
  thermoclinetrends[i,9] = mk[1]  #mk tau
  thermoclinetrends[i,10] = mk[2]  #mk p
  thermoclinetrends[i,11] =  ss[1]  #sens slope
  thermoclinetrends[i,12] =  ss[3]  #sens p
}

mean(thermoclinetrends$sens.slope) * 10

#0.0962 meters shallower per decade. not much

thermoclinetrends = thermoclinetrends %>% 
  mutate(dir = ifelse(sens.slope > 0, 'pos','neg')) %>% 
  mutate(lakes.sig = ifelse(sens.p < 0.05, paste('*',lake,sep=' '),lake)) %>% 
  mutate(change.per.decade = sens.slope * 10)
#another way
thermoclinetrends = thermoclinetrends %>% 
  mutate(sig = ifelse(sens.p < 0.05, 1, 0)) #if significant, 1

#*FIG - thermo sens  ########
ggplot(thermoclinetrends,
       aes(reorder(lake, change.per.decade),
           change.per.decade, 
           fill=dir,
           label=ifelse(sig==1, "*", NA))) +
  geom_bar(stat='identity') +
  geom_hline(yintercept=0, 
             linetype=5,
             linewidth=0.5,
             color='gray30') +
  geom_text(hjust = 1.25,
            vjust = 0.65,
            size = 6) + 
  scale_fill_manual(values = c('lightblue','darkblue')) +
  coord_flip() +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_x_discrete(labels=c("little cobbossee"=expression(bold('little cobbossee')),
                            "wilson"=expression(bold(wilson)),
                            "carlton"=expression(bold(carlton)),
                            "jimmy"=expression(bold(jimmy)),
                            "narrows upper"=expression(bold('narrows upper')),
                            "narrows lower"=expression(bold('narrows lower')),
                            "wilson"=expression(bold(wilson)))) +
  labs(y = 'Thermocline depth change (m \u00b7 decade\U207b\u00b9)',
       x = NULL,
       title = 'Change in mean summer thermocline depth') +
  theme_bw() + 
  theme(title=element_text(size=10),
        legend.position = "none")





#Thermo monthly ########
#get labels 
#subset df for jun jul aug sep
mmtherm = monthprosub.droppedsome %>% 
  filter(mm=='Jun' |mm=='Jul' | mm=='Aug' | mm=='Sep')

lm_eqn = function(mmtherm){
  m = lm(mm.td ~ year, mmtherm);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(mmtherm,.(mm),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 4))

ggplot(mmtherm, aes(x=year, y=mm.td)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  facet_wrap(~mm, ncol=2, nrow=2, scales='fixed') +
  geom_text(data=eq,aes(x=1991, y=0.5,label=V1), 
            parse=T, inherit.aes=F) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_reverse(limits=c(10,0), n.breaks=6) +
  labs(title='Mean thermocline depth - Monthly',
       x="Date",
       y="Thermocline depth (m)",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))










#*Shallow thermo ########
lm_eqn = function(yrpro.shallow){
  m = lm(yr.thermo.depth ~ year, yrpro.shallow);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(yrpro.shallow,.(),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 5))

ggplot(yrpro.shallow,
       aes(x=year, y=yr.thermo.depth)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', linewidth = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1983, y=0.5,label=V1), 
            parse=T, inherit.aes=F) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_reverse(limits=c(8,0)) +
  labs(title='Mean thermocline depth - Shallow lakes (\u2264 10m)',
       x="Date",
       y="Thermocline depth (m)",
       color='Lake') +   
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))





#*Shallow thermo monthly #######
#get shallow lakes 
mmtherm.shallow = mmtherm %>% 
  filter(max.depth <= 10)

lm_eqn = function(mmtherm.shallow){
  m = lm(mm.td ~ year, mmtherm.shallow);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(mmtherm.shallow,.(mm),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 4))

ggplot(mmtherm.shallow, aes(x=year, y=mm.td)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black') +
  geom_text(data=eq,aes(x=1992, y=0.5,label=V1), 
            parse=T, inherit.aes=F) +
  facet_wrap(~mm, ncol=2, nrow=2) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_reverse(limits=c(10,0), n.breaks = 6) +
  #scale_color_manual(values=c('blue','green')) +
  labs(title='Mean thermocline depth - Monthly - Shallow lakes (\u2264 10m)',
       x="Date",
       y="Thermocline depth (m)",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))






#*Deep thermo ######
lm_eqn = function(yrpro.deep){
  m = lm(yr.thermo.depth ~ year, yrpro.deep);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(yrpro.deep,.(),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 5))

ggplot(yrpro.deep,
       aes(x=year, y=yr.thermo.depth, color=lake)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black') +
  geom_text(data=eq,aes(x=1983, y=0.5,label=V1), 
            parse=T, inherit.aes=F) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_reverse(limits=c(10,0), n.breaks = 6) +
  labs(title='Mean thermocline depth Summer (June to 14-Sep) - Deep lakes (>10m)',
       x="Date",
       y="Thermocline depth (m)",
       color='Lake') +   
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))



#Deep thermo monthly #######
#get shallow lakes 
mmtherm.deep = mmtherm %>% 
  filter(max.depth > 10)

lm_eqn = function(mmtherm.deep){
  m = lm(mm.td ~ year, mmtherm.deep);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(mmtherm.deep,.(mm),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 4))

ggplot(mmtherm.deep, aes(x=year, y=mm.td)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1990, y=0.5,label=V1), 
            parse=T, inherit.aes=F) +
  facet_wrap(~mm, ncol=2, nrow=2) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_reverse(limits=c(15,0), n.breaks = 6) +
  #scale_color_manual(values=c('blue','green')) +
  labs(title='Mean thermocline depth - Monthly - Deep lakes (>10m)',
       x="Date",
       y="Thermocline depth (m)",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))







#Hypolimnion ###########

lm_eqn = function(yrprosub.droppedgaps){
  m = lm(yr.temp.hypo ~ year, yrprosub.droppedgaps);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(yrprosub.droppedgaps,.(),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 5))


ggplot(yrprosub.droppedgaps,
       aes(x=year, y=yr.temp.hypo, color=lake)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1985, y=24,label=V1), 
            parse=T, inherit.aes=F) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(0,25)) +
  labs(title='Mean hypolimnion temperature Summer (June to 14-Sep)',
       x="Date",
       y="Temperature (\u00b0C)",
       color='Lake')  +   
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))



#hypo trends
hypo.trends = ddply(yrprosub.droppedgaps, 
                    .(lake), 
                    summarize, 
                    n.years = length(unique(year)),
                    first.year = min(year),
                    n.pro = sum(n.profiles),
                    hypo.mean = mean(yr.temp.hypo, na.rm=T),
                    hypo.sd = sd(yr.temp.hypo, na.rm=T),
                    hypo.min = min(yr.temp.hypo, na.rm=T),
                    hypo.max = max(yr.temp.hypo, na.rm=T),
                    mk.tau = NA,
                    mk.p = NA,
                    sens.slope = NA,
                    sens.p = NA)

lakes = unique(hypo.trends$lake)                

for (i in 1:length(lakes)){ #for every lake
  td = yrprosub.droppedgaps[yrprosub.droppedgaps$lake == lakes[i], ] 
  td = td %>% drop_na(yr.temp.hypo)
  mk = Kendall::MannKendall(td$yr.temp.hypo) #run mk fxn
  ss = trend::sens.slope(td$yr.temp.hypo) #run sens slope fxn
  hypo.trends[i,9] = mk[1]  #mk tau
  hypo.trends[i,10] = mk[2]  #mk p
  hypo.trends[i,11] =  ss[1]  #sens slope
  hypo.trends[i,12] =  ss[3]  #sens p
}

mean(hypo.trends$sens.slope) * 10


#*FIG - Hypo sens #########
hypo.trends = hypo.trends %>% 
  mutate(dir = ifelse(sens.slope > 0, 'pos','neg')) %>% 
  mutate(lakes.sig = ifelse(sens.p < 0.05, paste('*',lake,sep=' '),lake)) %>% 
  mutate(change.per.decade = sens.slope * 10)
#another way
hypo.trends = hypo.trends %>% 
  mutate(sig = ifelse(sens.p < 0.05, 1, 0)) #if significant, 1

#directional change fig
ggplot(hypo.trends,
       aes(reorder(lake, change.per.decade),
           change.per.decade, 
           fill=dir,
           label=ifelse(sig==1, "*", NA))) +
  geom_bar(stat='identity') +
  geom_hline(yintercept=0, 
             linetype=5,
             linewidth=0.5,
             color='gray30') +
  geom_text(hjust = 1.5,
            vjust = 0.65,
            size = 6) + 
  scale_fill_manual(values = c('#37C1E6','#FF8778')) +
  coord_flip() +
 scale_y_continuous(limits=c(-2, 2.5)) +
  scale_x_discrete(labels=c("dexter"=expression(bold('dexter')),
                            "berry"=expression(bold(berry)),
                            "pleasant"=expression(bold(pleasant)),
                            "hutchinson"=expression(bold(hutchinson)))) +
  labs(y = 'Temperature change (\u00b0C \u00b7 decade\U207b\u00b9)',
       x = NULL,
       title = 'Change in mean summer hypolimnion temperature') +
  theme_bw() + 
  theme(title=element_text(size=10),
        legend.position = "none")



#Hypo monthly ###########
#get labels 
#subset df for jun jul aug sep
mmhypo = monthprosub.droppedsome %>% 
  filter(mm=='Jun' |mm=='Jul' | mm=='Aug' | mm=='Sep')

lm_eqn = function(mmhypo){
  m = lm(mm.hypo ~ year, mmhypo);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(mmhypo,.(mm),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 4))

ggplot(mmhypo, aes(x=year, y=mm.hypo)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1993, y=24,label=V1), 
            parse=T, inherit.aes=F) +
  facet_wrap(~mm, ncol=2, nrow=2, scales='fixed') +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(0,25), n.breaks=6) +
  labs(title='Mean hypolimnion temperature - Monthly',
       x="Date",
       y="Temperature (\u00b0C)",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))








#Shallow Hypo ####### 
lm_eqn = function(yrpro.shallow){
  m = lm(yr.temp.hypo ~ year, yrpro.shallow);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(yrpro.shallow,.(),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 5))

ggplot(yrpro.shallow,
       aes(x=year, y=yr.temp.hypo)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1985, y=24,label=V1), 
            parse=T, inherit.aes=F) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(5,25)) +
  labs(title='Mean hypolimnion temperature - Shallow lakes (\u2264 10m)',
       x="Date",
       y="Temperature (\u00b0C)",
       color='Lake')  +   
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))




#*Shallow hypo Monthly ########
#get shallow lakes 
mmhypo.shallow = mmhypo %>% 
  filter(max.depth <= 10)

lm_eqn = function(mmhypo.shallow){
  m = lm(mm.hypo ~ year, mmhypo.shallow);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(mmhypo.shallow,.(mm),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 4))


ggplot(mmhypo.shallow, aes(x=year, y=mm.hypo)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1990, y=24,label=V1), 
            parse=T, inherit.aes=F) +
  facet_wrap(~mm, ncol=2, nrow=2) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(0,25), n.breaks = 6) +
  #scale_color_manual(values=c('blue','green')) +
  labs(title='Mean hypolimnion temperature - Monthly -  Shallow lakes (\u2264 10m)',
       x="Date",
       y="Temperature (\u00b0C)",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))







#Deep hypo ########
lm_eqn = function(yrpro.deep){
  m = lm(yr.temp.hypo ~ year, yrpro.deep);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(yrpro.deep,.(),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 5))


ggplot(yrpro.deep,
       aes(x=year, y=yr.temp.hypo)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1983, y=19,label=V1), 
            parse=T, inherit.aes=F) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(0,20)) +
  labs(title='Mean hypolimnion temperature - Deep lakes (>10m)',
       x="Date",
       y="Temperature (\u00b0C)",
       color='Lake')  +   
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))





#Deep Hypo monthly ##########
mmhypo.deep = mmhypo %>% 
  filter(max.depth > 10)

lm_eqn = function(mmhypo.deep){
  m = lm(mm.hypo ~ year, mmhypo.deep);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(mmhypo.deep,.(mm),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 4))

ggplot(mmhypo.deep, aes(x=year, y=mm.hypo)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1990, y=23,label=V1), 
            parse=T, inherit.aes=F) +
  facet_wrap(~mm, ncol=2, nrow=2) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(0,25), n.breaks = 6) +
  #scale_color_manual(values=c('blue','green')) +
  labs(title='Mean hypolimnion temperature - Monthly -  Deep lakes (>10m)',
       x="Date",
       y="Temperature (\u00b0C)",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))



#STATS - Deep/Shallow ##############

#*shallow #######
ytrends.shallow = ddply(yrpro.shallow, 
                        .(lake), 
                        summarize, 
                        n.years = length(unique(year)),
                        first.year = min(year),
                        n.pro = sum(n.profiles),
                        temp5m.mean = mean(yr.temp.top5m, na.rm=T),
                        temp5m.sd = sd(yr.temp.top5m, na.rm=T),
                        temp5m.min = min(yr.temp.top5m, na.rm=T),
                        temp5m.max = max(yr.temp.top5m, na.rm=T),
                        td.mean = mean(yr.thermo.depth, na.rm=T),
                        td.sd = sd(yr.thermo.depth, na.rm=T),
                        td.min = min(yr.thermo.depth, na.rm=T),
                        td.max = max(yr.thermo.depth, na.rm=T),
                        hypo.mean = mean(yr.thermo.depth, na.rm=T),
                        hypo.sd = sd(yr.thermo.depth, na.rm=T),
                        hypo.min = min(yr.thermo.depth, na.rm=T),
                        hypo.max = max(yr.thermo.depth, na.rm=T),
                        st.sens.slope = NA,
                        st.sens.p = NA,
                        td.sens.slope = NA,
                        td.sens.p = NA,
                        hypo.sens.slope = NA,
                        hypo.sens.p = NA)

lakes = unique(ytrends.shallow$lake)                

for (i in 1:length(lakes)){ #for every lake
  #surface temp
  td = yrpro.shallow[yrpro.shallow$lake == lakes[i], ] 
  td = td %>% drop_na(yr.temp.top5m)
  ss = trend::sens.slope(td$yr.temp.top5m) #run sens slope fxn
  ytrends.shallow[i,17] =  ss[1]  #sens slope
  ytrends.shallow[i,18] =  ss[3]  #sens p
  #thermocline
  td = yrpro.shallow[yrpro.shallow$lake == lakes[i], ] 
  td = td %>% drop_na(yr.thermo.depth)
  ss = trend::sens.slope(td$yr.thermo.depth) #run sens slope fxn
  ytrends.shallow[i,19] =  ss[1]  #sens slope
  ytrends.shallow[i,20] =  ss[3]  #sens p
  #hypo
  td = yrpro.shallow[yrpro.shallow$lake == lakes[i], ] 
  td = td %>% drop_na(yr.temp.hypo)
  ss = trend::sens.slope(td$yr.temp.hypo) #run sens slope fxn
  ytrends.shallow[i,21] =  ss[1]  #sens slope
  ytrends.shallow[i,22] =  ss[3]  #sens p
}

#change per decade
mean(ytrends.shallow$st.sens.slope) * 10
mean(ytrends.shallow$td.sens.slope) * 10
mean(ytrends.shallow$hypo.sens.slope) * 10




#*deep #######
ytrends.deep = ddply(yrpro.deep, 
                     .(lake), 
                     summarize, 
                     n.years = length(unique(year)),
                     first.year = min(year),
                     n.pro = sum(n.profiles),
                     temp5m.mean = mean(yr.temp.top5m, na.rm=T),
                     temp5m.sd = sd(yr.temp.top5m, na.rm=T),
                     temp5m.min = min(yr.temp.top5m, na.rm=T),
                     temp5m.max = max(yr.temp.top5m, na.rm=T),
                     td.mean = mean(yr.thermo.depth, na.rm=T),
                     td.sd = sd(yr.thermo.depth, na.rm=T),
                     td.min = min(yr.thermo.depth, na.rm=T),
                     td.max = max(yr.thermo.depth, na.rm=T),
                     hypo.mean = mean(yr.thermo.depth, na.rm=T),
                     hypo.sd = sd(yr.thermo.depth, na.rm=T),
                     hypo.min = min(yr.thermo.depth, na.rm=T),
                     hypo.max = max(yr.thermo.depth, na.rm=T),
                     st.sens.slope = NA,
                     st.sens.p = NA,
                     td.sens.slope = NA,
                     td.sens.p = NA,
                     hypo.sens.slope = NA,
                     hypo.sens.p = NA)

lakes = unique(ytrends.deep$lake)                

for (i in 1:length(lakes)){ #for every lake
  #surface temp
  td = yrpro.deep[yrpro.deep$lake == lakes[i], ] 
  td = td %>% drop_na(yr.temp.top5m)
  ss = trend::sens.slope(td$yr.temp.top5m) #run sens slope fxn
  ytrends.deep[i,17] =  ss[1]  #sens slope
  ytrends.deep[i,18] =  ss[3]  #sens p
  #thermocline
  td = yrpro.deep[yrpro.deep$lake == lakes[i], ] 
  td = td %>% drop_na(yr.thermo.depth)
  ss = trend::sens.slope(td$yr.thermo.depth) #run sens slope fxn
  ytrends.deep[i,19] =  ss[1]  #sens slope
  ytrends.deep[i,20] =  ss[3]  #sens p
  #hypo
  td = yrpro.deep[yrpro.deep$lake == lakes[i], ] 
  td = td %>% drop_na(yr.temp.hypo)
  ss = trend::sens.slope(td$yr.temp.hypo) #run sens slope fxn
  ytrends.deep[i,21] =  ss[1]  #sens slope
  ytrends.deep[i,22] =  ss[3]  #sens p
}

#change per decade
mean(ytrends.deep$st.sens.slope) * 10
mean(ytrends.deep$td.sens.slope) * 10
mean(ytrends.deep$hypo.sens.slope) * 10





#quick plots ########
ytrends.new = ddply(yrprosub.droppedgaps, 
                    .(lake), 
                    summarize, 
                    n.years = length(unique(year)),
                    first.year = min(year),
                    n.pro = sum(n.profiles),
                    temp5m.mean = mean(yr.temp.top5m, na.rm=T),
                    temp5m.sd = sd(yr.temp.top5m, na.rm=T),
                    temp5m.min = min(yr.temp.top5m, na.rm=T),
                    temp5m.max = max(yr.temp.top5m, na.rm=T),
                    td.mean = mean(yr.thermo.depth, na.rm=T),
                    td.sd = sd(yr.thermo.depth, na.rm=T),
                    td.min = min(yr.thermo.depth, na.rm=T),
                    td.max = max(yr.thermo.depth, na.rm=T),
                    hypo.mean = mean(yr.thermo.depth, na.rm=T),
                    hypo.sd = sd(yr.thermo.depth, na.rm=T),
                    hypo.min = min(yr.thermo.depth, na.rm=T),
                    hypo.max = max(yr.thermo.depth, na.rm=T),
                    st.sens.slope = NA,
                    st.sens.p = NA,
                    td.sens.slope = NA,
                    td.sens.p = NA,
                    hypo.sens.slope = NA,
                    hypo.sens.p = NA)

lakes = unique(ytrends.new$lake)                

for (i in 1:length(lakes)){ #for every lake
  #surface temp
  td = yrprosub.droppedgaps[yrprosub.droppedgaps$lake == lakes[i], ] 
  td = td %>% drop_na(yr.temp.top5m)
  ss = trend::sens.slope(td$yr.temp.top5m) #run sens slope fxn
  ytrends.new[i,17] =  ss[1]  #sens slope
  ytrends.new[i,18] =  ss[3]  #sens p
  #thermocline
  td = yrprosub.droppedgaps[yrprosub.droppedgaps$lake == lakes[i], ] 
  td = td %>% drop_na(yr.thermo.depth)
  ss = trend::sens.slope(td$yr.thermo.depth) #run sens slope fxn
  ytrends.new[i,19] =  ss[1]  #sens slope
  ytrends.new[i,20] =  ss[3]  #sens p
  #hypo
  td = yrprosub.droppedgaps[yrprosub.droppedgaps$lake == lakes[i], ] 
  td = td %>% drop_na(yr.temp.hypo)
  ss = trend::sens.slope(td$yr.temp.hypo) #run sens slope fxn
  ytrends.new[i,21] =  ss[1]  #sens slope
  ytrends.new[i,22] =  ss[3]  #sens p
}


#mid-ass
midass = yrprosub.droppedgaps %>% select(lake, midas)
midass = midass %>% distinct(.keep_all = F)
ytrends.new = join(ytrends.new, midass, by='lake')

md = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/db.raw/lakemd.csv', header=T)
md = md %>% 
  filter(station==1) %>% 
  select(midas, depth_m) %>% 
  set_names(~ str_to_lower(.)) %>% 
  mutate_all(~ str_to_lower(.)) 

ytrends.new = join(ytrends.new, md, by='midas')
str(ytrends.new)
unique(ytrends.new$depth_m)
ytrends.new$depth_m = as.numeric(ytrends.new$depth_m)


#surface temp
ggplot(ytrends.new, aes(x=depth_m, y=st.sens.slope)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

#thermocline
ggplot(ytrends.new, aes(x=depth_m, y=td.sens.slope)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

#hypo
ggplot(ytrends.new, aes(x=depth_m, y=hypo.sens.slope)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")






#nice figure ##########
longboi = ytrends.new %>% 
  pivot_longer(cols = c(st.sens.slope,
                      td.sens.slope,
                      hypo.sens.slope),
               names_to = 'param',
               values_to = 'sens')

longboi = longboi %>% 
  mutate(param = replace(param, param=='st.sens.slope','Surface Temp')) %>% 
  mutate(param = replace(param, param=='hypo.sens.slope','Hypolimnion Temp')) %>% 
  mutate(param = replace(param, param=='td.sens.slope','Thermocline Depth'))
  
lm_eqn = function(longboi){
  m = lm(sens ~ depth_m, longboi);
  eq <- substitute(italic(b)~"="~bvalue*","~italic(r)^2~"="~r2, 
                   list(bvalue = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));}
#save momth r2 labels
eq = ddply(longboi,.(param),lm_eqn)
#change geom_text size 
update_geom_defaults("text", list(size = 4))


ggplot(longboi, aes(x=depth_m, y=sens)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") + 
  geom_text(data=eq,aes(x=16, y=0.18,label=V1), parse=T, inherit.aes=F) +
  facet_grid(~param) +
  labs(title=NULL,
       x="Lake depth (m)",
       y="Sen's Slope",
       color='Lake') +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))






ggplot(mmhypo.deep, aes(x=year, y=mm.hypo)) +
  geom_point(aes(color = lake), shape=1, alpha=0.5) +
  geom_line(aes(color = lake), stat="smooth", method='loess', size = 0.75,
            linetype ="solid", alpha = 0.75, show.legend = F)  +
  geom_line(stat="smooth", method='lm', linewidth = 1.25,
            linetype ="solid", alpha = 0.75, show.legend = F, color='black')  +
  geom_text(data=eq,aes(x=1990, y=23,label=V1), 
            parse=T, inherit.aes=F) +
  facet_wrap(~mm, ncol=2, nrow=2) +
  scale_x_continuous(limits=c(1975,2022)) +
  scale_y_continuous(limits=c(0,25), n.breaks = 6) +
  #scale_color_manual(values=c('blue','green')) +
  labs(title='Mean hypolimnion temperature - Monthly -  Deep lakes (>10m)',
       x="Date",
       y="Temperature (\u00b0C)",
       color='Lake') +
  guides(color = guide_legend(override.aes = list(shape=19, alpha=1))) +
  theme_bw() + 
  theme(title=element_text(size=10),
        strip.background=element_rect(fill='gray90'))




