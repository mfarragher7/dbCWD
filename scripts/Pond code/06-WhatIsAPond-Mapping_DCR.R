singleponds = read.csv("Data_cleaned/pond_latlon.csv") #new EDI version
head(singleponds)
dim(singleponds)
#remove out large/deep lakes. n=1364
# singleponds <- singleponds %>% 
#   filter((max_depth_m<=9 | is.na(max_depth_m)==T) &
#            (mean_depth_m<=9 | is.na(mean_depth_m)==T) &
#            (max_surfacearea_m2<=200000 | is.na(max_surfacearea_m2)==T) &
#            (mean_surfacearea_m2<=200000 | is.na(mean_surfacearea_m2)==T))
head(singleponds)
#project the lat/lon 
# ponds<-singleponds %>% 
#   drop_na(latitude, longitude) %>%   #drop na #only 660 with lat and lon
#select(author, year, journal, location, latitude, longitude) #select only certain columns 
coords <- data.frame(x=singleponds$longitude,y=singleponds$latitude) 
lake.ll <- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=NAD83"))   
single_pond<-addAttrToGeom(lake.ll, singleponds) #add back attributes 


#Get all ponds
singleponds_full <- read.csv("Data_cleaned/Singleponds_Cleaned.csv")

library(countrycode)
library(stringr)

#get country info
countryname_dict = countrycode::codelist
#one string of all countries
all_countries2 = str_c(unique(countryname_dict$country.name.en), collapse = "|")
all_countries1 = str_c(unique(countryname_dict$country.name.en))
all_countries1 <- as.data.frame(all_countries1)
head(all_countries1)
names(all_countries1)[1] <- "country"
head(all_countries1)
#all_countries = str_c(unique(countryname_dict$country.name.en))
all_countries =   tolower(all_countries2)
head(all_countries)
singleponds_full$country = sapply(str_extract_all(singleponds_full$location, all_countries), toString)

#pattern = str_c(all_countries, collapse = '|')  
#singleponds$country = NA
#singleponds = singleponds %>% mutate(country=replace(country,location %in% all_countries, NA))

#fill in the rest
singleponds_full = singleponds_full %>% mutate(country=replace(country,grepl('usa',location),"united states"))
singleponds_full = singleponds_full %>% mutate(country=replace(country,grepl('england',location),"united kingdom"))
singleponds_full = singleponds_full %>% mutate(country=replace(country,grepl('northumberland uk',location),"united kingdom"))
singleponds_full = singleponds_full %>% mutate(country=replace(country,grepl('stansfield et al',author),"united kingdom"))
singleponds_full = singleponds_full %>% mutate(country=replace(country,grepl('dorset; uk',location),"united kingdom"))
singleponds_full = singleponds_full %>% mutate(country=replace(country,grepl('ontario',location),"canada"))
singleponds_full = singleponds_full %>% mutate(country=replace(country,grepl('faroese islands',location),"faroe islands"))
singleponds_full = singleponds_full %>% mutate(country=replace(country,grepl('menorca',location),"spain"))
singleponds_full = singleponds_full %>% mutate(country=replace(country,grepl('alto guadalquivir',location),"spain"))
singleponds_full = singleponds_full %>% mutate(country=replace(country,grepl('Taiwan',location),"taiwan"))
singleponds_full = singleponds_full %>% mutate(country=replace(country,grepl('tarta mountains; western carpathians',location),"slovakia"))
<<<<<<< HEAD
#fill in US states
singleponds_full = singleponds_full %>% mutate(country=replace(country,NA,'united states'))
#didn't work! try making pattern again
states = datasets::state.name
states = tolower(states)
pattern2 = str_c(states, collapse = '|')  
head(singleponds)

head(all_countries)
all_countries <- as.data.frame(all_countries)
head(all_countries)
names(all_countries)[1] <- "countries"
head(singleponds)
str(all_countries)
all_countries$fill = sapply(str_extract_all(singleponds$country, all_countries), toString)

all_countries$country <- all_countries$countries
alle <- dplyr::inner_join(singleponds, all_countries, by = "country")
alle
head(singleponds)
pondsmap <- singleponds
dim(singleponds) #591, 8

#use a mutate if else statement

#write.csv(singleponds, file = "singleponds.csv")
=======
  #us states
  singleponds = singleponds %>% mutate(country=replace(country,grepl('colorado',location),"united states"))
singleponds = singleponds %>% mutate(country=replace(country,grepl('minnesota',location),"united states"))
singleponds = singleponds %>% mutate(country=replace(country,grepl('ohio',location),"united states"))
singleponds = singleponds %>% mutate(country=replace(country,grepl('wisconsin',location),"united states"))
singleponds = singleponds %>% mutate(country=replace(country,grepl('idaho',location),"united states"))
singleponds = singleponds %>% mutate(country=replace(country,grepl('michigan',location),"united states"))
singleponds = singleponds %>% mutate(country=replace(country,grepl('new hampshire',location),"united states"))
singleponds = singleponds %>% mutate(country=replace(country,grepl('south carolina',location),"united states"))

unique(singleponds$country)
#24 unique countries


mapView(single_pond, legend=TRUE)


#Graphing starts here
worlddata_withcountries<-left_join(worldtest,tibble(country=temp.country)%>%mutate(country=str_to_title(country),fill_var=1),by="country")
worlddata_withcountries<-worlddata_withcountries%>%mutate(fill_var=ifelse(is.na(fill_var),0,fill_var))

pond.map = ggplot(data=worldtest)+
  geom_sf(aes(geometry=geometry),fill="white")+
  geom_sf(data=worlddata_withcountries,aes(fill=factor(fill_var),geometry=geometry))+
  scale_fill_manual(values=c("white","#0072B2"))+
  geom_point(data=singleponds,aes(x=longitude,y=latitude),
             show.legend=F,size = 2, 
             shape = 21, fill = rgb(240,128,128,max=255)) + 
  labs(y="Latitude", x="Longitude") +
  theme(panel.border=element_rect(colour = "black", fill=NA, size=1),
        legend.position='none')+
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))


#save
ggsave(filename="Plots/Maps/PondMap-SupplementalFigureS1.png",
       plot=pond.map,
       device="jpg",
       height=5,
       width=6,
       units="in",
       dpi=500)