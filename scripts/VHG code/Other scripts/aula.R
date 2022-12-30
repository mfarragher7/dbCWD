#AULACOSEIRA#

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Maine")
library(ggplot2); library(reshape2)

ad <- read.csv("all-aula.csv")
all.melt <- melt(ad, id = c("Year", "Lake"))

all <- ggplot(data = all.melt, aes(x = value, y = Year)) +
  geom_point(aes(), size = 1.5, alpha = 0.9) +
  geom_path(aes(), size = 1.5, alpha = 0.9) +
  facet_wrap(Lake ~ variable) +
  theme(text = element_text(face='plain',size=12,vjust=1))
all

all <- ggplot(data = all.melt, aes(x = Year, y = value, group = Lake)) +
  geom_point(aes(x = Year, y = value, color = Lake), size = 0.5, alpha = 0.9) +
  stat_smooth(aes(x = Year, y = value, color = Lake), geom = "line", method = "loess", 
              size = 1, alpha = 0.4, se = F, span = 0.2) +
  facet_wrap(~variable, ncol = 1, scales = "free") 
  geom_path(aes(x = Year, y = value, color = Lake), size = 1, alpha = 0.6)
  
all
ggsave("aula.pdf", all, height = 35, width = 10, units = "cm")

devtools::install_github("paleolimbot/tidypaleo")
library(tidyverse)
library(tidypaleo)

all.aula <- ggplot(all.melt, aes(x = value, y = Year)) +
  geom_col_segsh() +
  facet_abundanceh(vars(Lake), grouping = vars(variable)) +
  scale_x_continuous(breaks = c(1,5,10,15)) + 
  labs(x = "Relative abundance (%)", y = "Year")

all.aula
ggsave("aula4.pdf", all.aula, height = 20, width = 20, units = "cm")

    
#add smoother?
all.aula <- ggplot(all.melt, aes(x = value, y = Year)) +
  geom_col_segsh() +
  facet_abundanceh(vars(Lake), grouping = vars(variable)) +
  scale_x_continuous(breaks = c(1,5,10,15)) + 
  labs(x = "Relative abundance (%)", y = "Year")

all.aula
ggsave("aula4.pdf", all.aula, height = 20, width = 20, units = "cm")

##### Sebago

seb <- read.csv("sebago.csv")
seb$month <- factor(seb$month, levels = c("May", "June", "August", "September"))


sebago <- ggplot(data = seb, aes(x = Abundance, y = Depth)) +
  geom_point(aes(x = Abundance, y = Depth), size = 0.5, alpha = 0.9) +
  stat_smooth(aes(x = Abundance, y = Depth), geom = "line", method = "loess", 
              size = 1, alpha = 0.4, se = F, span = 0.2) +
  scale_y_reverse() + 
  geom_path(aes(x = Abundance, y = Depth), size = 1, alpha = 0.6) +
  facet_wrap(month~species, ncol = 3) 
sebago
ggsave("sebago.pdf", sebago, height = 20, width = 20, units = "cm")

sebago <- ggplot(data = seb, aes(x = Abundance, y = Depth, group = month)) +
  geom_point(aes(x = Abundance, y = Depth, color = month), size = 0.5, alpha = 0.6) +
  scale_y_reverse() + 
  geom_path(aes(x = Abundance, y = Depth, color = month), size = 1, alpha = 0.8) +
  facet_wrap(~species, ncol = 3) 
sebago
ggsave("sebago2.pdf", sebago, height = 5, width = 15, units = "cm")
