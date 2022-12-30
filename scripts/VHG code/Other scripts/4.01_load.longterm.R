#load longterm data for DOC, Secchi, SO4, and merge with 2020 datasets
#Updated 2020-12-01

#load nutrient data 
full.nooch = read.csv("library/full.nooch.csv",header=T)

#libraries
library(dplyr)
library(tidyr)
library(plyr)


#DOC ####
lt.doc = read.csv("dbLongterm/DOC_1988-2019.csv")
#rename cols
colnames(lt.doc)[1] = "lakename"
colnames(lt.doc)[7] = 'DOC_mgpl'
#coerce date
lt.doc$Date = as.Date(lt.doc$Date, format = "%m/%d/%Y")
str(lt.doc)
#select cols, add lakedate ID column
lt.doc = lt.doc %>% 
  select(LakeID,Date,DOC_mgpl,lakename) %>%
  mutate(lakedate=paste(LakeID,Date,sep="_"))
#get 2020 doc avg for each lakedate
new.doc =  ddply(full.nooch, .(lakedate,LakeID,Date,lakename), summarize, DOC_mgpl=mean(DOC_mgpl))
#merge lt and 2020 data
lt.doc = rbind(lt.doc,new.doc)
#filter dates
lt.doc = lt.doc %>% filter(Date >= "1995-01-01") # omitting 80s datapoints
#write csv
write.csv(lt.doc, "library/lt.doc.csv", row.names = F)


#Secchi ####
lt.secchi = read.csv("dbLongterm/Secchi_1970-2020.csv")
#rename cols
colnames(lt.secchi)[1] = "lakename"
#fix witch hole name
lt.secchi[grepl("Witch Hole",lt.secchi$lakename),1]= "Witch Hole Pond"
#coerce date
lt.secchi$Date = as.Date(lt.secchi$Date, format = "%m/%d/%Y")
str(lt.secchi)
#add lakedate ID column
lt.secchi = lt.secchi %>%  mutate(lakedate=paste(LakeID,Date,sep="_"))
#filter dates
lt.secchi = lt.secchi %>% filter(Date >= "1995-01-01") 
#write csv
write.csv(lt.secchi, "library/lt.secchi.csv", row.names = F)



#SO4 ####
lt.so4 = read.csv("dbLongterm/SO4_1982-2019.csv")
str(lt.so4)
#rename cols
colnames(lt.so4)[1] = "lakename"
#coerce date
lt.so4$Date = as.Date(lt.so4$Date, format = "%m/%d/%Y")
#add lakedate ID column
lt.so4 = lt.so4 %>%  mutate(lakedate=paste(LakeID,Date,sep="_"))
str(lt.so4)
#filter dates
lt.so4 = lt.so4 %>% filter(Date >= "1995-01-01") 
#write csv
write.csv(lt.so4, "library/lt.so4.csv", row.names = F)





