# Pleasant Pond phosphorus 
#created 2023-01-24

#load libraries
library(tidyverse)
library(lubridate)
library(ggplot2)


#load tp df


tp = read.csv('https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/tp.cwd.1998-2021.csv',header=T)
pptp = tp %>% filter(lake=='pleasant')

#2000 through 2021... where's 1998, 1999? plus need all the old time data too....


str(pptp)
pptp$date = as.Date(pptp$date)





