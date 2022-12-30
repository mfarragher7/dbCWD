
library(RCurl)
library(tidyverse)



### Lat Longs for Daymet

points<-read.csv("C:/R/HOBO/RMNsites.csv")
head(points)



# Get list of unique locations 

names<-unique(points$Lake)  # change the field to be the one for your data
num<-length(names)

# Loop for Daymet data extraction by lat/long points

w=1
for(w in 1:num)
{
  
  s<-subset(points, points$Lake == names[w])
  lat<-round(s$Lat,3)
  long<-round(s$Long,3)
  
  file<-paste('https://daymet.ornl.gov/single-pixel/api/data?lat=',lat,'&lon=',long, sep = "")
  
  myfile <- getURL(file, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  assign(names[w], read.csv(textConnection(myfile), skip = 6)) 

}




# For LSB:

### Daily means for all points:
LSB_daym<-LSB %>% 
  filter(year>=2018) %>% 
  filter(year<=2022) %>% 
  rename(daylight = 3, prcp = 4, srad = 5,swet = 6,
         tmax = 7,tmin = 8,vp = 9) %>%
  mutate(tavg = (tmax+tmin)/2) %>% 
  select(1,2,10,3:8) %>% 
  group_by(year,yday) %>%
  summarize_at(c("daylight","prcp","srad","swet","tmax","tmin","tavg"), mean) %>%
  ungroup()