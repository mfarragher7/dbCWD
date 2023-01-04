#Precipitation figures and analysis
#created 2023-01-04


#load packages
library(tidyverse)
library(ggplot2)



#load data
precip = read.csv("C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/db.raw/db.cwd/winthrop.precip_1976-2022.csv")
str(precip)

p.summary = precip %>% 
  gather(Month, precip.in,
         Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, 
         na.rm = F, convert = F) %>% 
  group_by(Year) %>% 
  summarize(annual.total = sum(precip.in),
            monthly.mean = mean(precip.in),
            monthly.sd = sd(precip.in)) 


p.jan.july = precip %>% 
  gather(Month, precip.in,
         Jan, Feb, Mar, Apr, May, Jun, Jul,
         na.rm = F, convert = F) %>% 
  group_by(Year) %>% 
  summarize(jan.to.jul.total = sum(precip.in))

#merge summary dfs
p.summary = merge(p.summary, p.jan.july, by='Year')

#percentage of jan-jul total to whole yr
p.summary$jan.jul.portion = (p.summary$jan.to.jul.total / p.summary$annual.total)
  

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
  geom_line()


#jan through july
jj = c('Jan','Feb','Mar','Apr','May','Jun','Jul')
prejj = preciplong %>% filter(month %in% jj)

ggplot(prejj, aes(x=date, y=precip.in)) + 
  geom_point() +
  stat_smooth(method="loess", linewidth=0.75, se=T, show.legend=F)




#plot annual totals
ggplot(p.summary, aes(x=Year, y=annual.total)) +
  geom_point(shape=19) +
  stat_smooth(method="loess", linewidth=0.75, se=F, show.legend=F)

#jan-july totals each year
ggplot(p.summary, aes(x=Year, y=jan.to.jul.total)) +
  geom_point(shape=19) +
  stat_smooth(method="loess", linewidth=0.75, se=F, show.legend=F)


#combine annual total and jan-july portion together
precip.annual.jj = p.summary %>% 
  pivot_longer(cols=c(annual.total, jan.to.jul.total),
               names_to = 'timespan',
               values_to = 'precip.in')


ggplot(precip.annual.jj,
       aes(x=Year, y=precip.in, color=timespan)) +
  geom_point(shape=19) +
  geom_line(linetype=3) +
  stat_smooth(method="loess", linewidth=0.75, se=F, show.legend=F)




#2022 cumulative monthly precip

p22 = preciplong %>% filter(Year==2022)
p22$cumulative.precip = cumsum(p22$precip.in)



ggplot(p22, aes(x=date, y=cumulative.precip)) +
  geom_col() +
  scale_x_date(date_labels="%b", date_breaks='1 month')
  







