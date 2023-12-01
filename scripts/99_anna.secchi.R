#Annabessacook secchi data from last 10 years for 2023 annual meeting
#created 2023-08-09


#libs 
library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)
library(rLakeAnalyzer)

secchi = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/survey.1974-2022.csv',h=T)

secchi$year = lubridate::year(secchi$date)
secchi$month = lubridate::month(secchi$date)

#average reps, get one value per basin per day
secchi.mean = plyr::ddply(secchi, .(lake, station, date), summarize, 
                          secchi.m = mean(secchi),
                          n = length(unique(secchi)))


#get anna 2013 to 2022
anna = secchi.mean %>% 
  filter(lake=='annabessacook') %>% 
  filter(date > '2013-01-01')

#everything below 3.00m
anna.sub3 = anna %>% 
  filter(secchi.m < 3)

#add year 
anna.sub3$year = lubridate::year(anna.sub3$date)
unique(anna.sub3$year) #every year 



anna.sub3.dates = plyr::ddply(anna.sub3, .(year, station), summarize,
                              min.secchi = min(secchi.m),
                                earliest.date = min(date),
                                latest.date = max(date))


write.csv(anna.sub3.dates, file='C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/annabessacook.secchi.summary.2013-2022.csv')


