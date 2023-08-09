# making secchi figure for july 11 2023 board meeting


library(tidyverse)

secchi = read.csv('C:/Users/CWD2-Matt/OneDrive/Database/dbCWD/library/secchijuly2023botmeeting.csv',header=T)

ggplot(secchi, aes(x=Lake, y=Secchi, fill=Month)) +
  geom_col(position = 'jitter',
           width=0.5)
