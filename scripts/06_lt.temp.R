#Longterm temperature analysis
#created 2023-01-17


#libraries
library(tidyverse)
library(ggplot2)
library(rLakeAnalyzer)
library(ggpubr)

#load profile summary
pro = read.csv("https://raw.githubusercontent.com/mfarragher7/dbCWD/main/library/profiles.1975-2021.summary.csv", header=T)


#format dates
pro = pro %>% mutate(dm = format(as.Date(date), "%m-%d"))
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
temp = plyr::count(pro$sampleid)
