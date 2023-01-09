#Precipitation figures and analysis
#created 2023-01-04


#load packages
library(tidyverse)
library(ggplot2)



#load data
precip = read.csv('~/GitHub/dbCWD/db.raw/db.cwd/winthrop.precip_1976-2022.csv')
str(precip)

p.summary = precip %>% 
  gather(Month, precip.in,
         Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, 
         na.rm = F, convert = F) %>% 
  group_by(Year) %>% 
  summarize(annual.total = sum(precip.in),
            monthly.mean = mean(precip.in),
            monthly.sd = sd(precip.in)) 


p.jan.jun = precip %>% 
  gather(Month, precip.in,
         Jan, Feb, Mar, Apr, May, Jun,
         na.rm = F, convert = F) %>% 
  group_by(Year) %>% 
  summarize(jan.to.jun.total = sum(precip.in))

#merge summary dfs
p.summary = merge(p.summary, p.jan.jun, by='Year')

#percentage of jan-jul total to whole yr
p.summary$jan.jun.portion = (p.summary$jan.to.jun.total / p.summary$annual.total)
  




#plots ################
            
#pivot long for ggplot
names(precip)
preciplong = precip %>% 
  pivot_longer(cols = c(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
               names_to = 'month',
               values_to = 'precip.in')

#add 'date' column
preciplong$mm = paste0(preciplong$month,"/01/",preciplong$Year)
preciplong$date = as.Date(preciplong$mm, format = "%b/%d/%Y")



#loess trend
ggplot(preciplong, aes(x=date, y=precip.in)) +
  geom_point(shape=1) +
  stat_smooth(method="loess", linewidth=0.75, se=T, show.legend=F)



#25/50/75 quantiles
q25 = seq(0.25, 0.75, by=0.25)

ggplot(preciplong, aes(x=date, y=precip.in)) +
  geom_point() +
  geom_quantile(quantiles = q25)  



#color by month
ggplot(preciplong, aes(x=date, y=precip.in, color=month)) +
  geom_line() +
  labs(title='Variability in Monthly Precipitation 1976-2022, measured at CWD',
       x='Date',
       y='Precipitation (in)',
       color='Month') +
  theme_bw()


#jan through june
jj = c('Jan','Feb','Mar','Apr','May','Jun')
prejj = preciplong %>% filter(month %in% jj)

ggplot(prejj, aes(x=date, y=precip.in)) + 
  geom_point() +
  stat_smooth(method="loess", linewidth=0.75, se=T, show.legend=F)




#plot annual totals
ggplot(p.summary, aes(x=Year, y=annual.total)) +
  geom_point(shape=19) +
  stat_smooth(method="loess", linewidth=0.75, se=F, show.legend=F)

#jan-june totals each year
ggplot(p.summary, aes(x=Year, y=jan.to.jun.total)) +
  geom_point(shape=19) +
  stat_smooth(method="loess", linewidth=0.75, se=F, show.legend=F)


#combine annual total and jan-july portion together
precip.annual.jj = p.summary
precip.annual.jj$jul.to.dec.total = precip.annual.jj$annual.total - precip.annual.jj$jan.to.jun.total


#longer for gggg
precip.annual.jj = precip.annual.jj %>% 
  pivot_longer(cols=c(jul.to.dec.total, jan.to.jun.total),
               names_to = 'timespan',
               values_to = 'precip.in')

#rename vars
precip.annual.jj = precip.annual.jj %>% 
  mutate(timespan=replace(timespan,grepl('jan',timespan),'Jan-Jun')) %>% 
  mutate(timespan=replace(timespan,grepl('jul',timespan),'Jul-Dec'))



ggplot(precip.annual.jj,
       aes(x=Year, y=precip.in, color=timespan)) +
  geom_point(shape=19) +
  geom_line(linetype=3) +
  stat_smooth(method="loess", size=0.75, se=F, show.legend=F) +
  labs(title='Annual Total Precipitation 1976-2022, measured at CWD',
       color=NULL,
       y='Precipitation (in)',
       x='Year') +
  theme_bw()







9#as columns instead of points. bleh
ggplot(p.summary, aes(x=Year, y=annual.total)) +
  geom_bar(stat='identity', color='white', fill='lightblue', width=1, position =position_dodge(0.1)) +
  labs(title='Annual Total Precipitation 1976-2022, measured at CWD',
       y='Precipitation (in)',
       x='Year') +
  theme_bw()
  



ggplot(precip.annual.jj,
       aes(x=Year, y=precip.in, color=timespan, fill=timespan)) +
  geom_bar(position="stack", stat="identity", width=0.5) +
  labs(title='Annual Total Precipitation 1976-2022, measured at CWD',
       color=NULL,
       fill=NULL,
       y='Precipitation (in)',
       x='Year') +
  theme_bw()










#2022 cumulative monthly precip

p22 = preciplong %>% filter(Year==2022)
p22$cumulative.precip = cumsum(p22$precip.in)
str(p22)
p22$month

p22$month = factor(p22$month, levels=c('Jan',
                                       'Feb',
                                       'Mar',
                                       'Apr',
                                       'May',
                                       'Jun',
                                       'Jul',
                                       'Aug',
                                       'Sep',
                                       'Oct',
                                       'Nov',
                                       'Dec'))


ggplot(p22, aes(x=month, y=cumulative.precip)) +
  geom_col(color='blue', fill='lightblue') +
  #scale_x_date(date_labels="%b", date_breaks='1 month') +
  scale_y_continuous(limits=(c(0,50))) +
  labs(title='2022 Cumulative Precipitation, measured at CWD',
       y='Precipitation (in)',
       x='Month')+
  theme_bw()








