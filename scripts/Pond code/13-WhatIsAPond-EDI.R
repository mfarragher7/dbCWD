#Prep EDI csvs
#updated by MJF 2021-10-31


#load libraries
library(dplyr)



#load singleponds db
singleponds = read.csv("Data_cleaned/Singleponds_subsetted.csv")

#clean up col names
singleponds = singleponds %>% 
  dplyr::rename(pond_definition = definitionofpond,
                author_term = whatauthorscallit,
                hydroperiod = hydrology,
                initials_paperreviewer = initials_whoreviewedpaper) %>% 
  #drop cols not needed
  select(-check_notes_for,-depthDummy,-areaDummy,-paperID,-pondID)
names(singleponds)

#add empty title column between year and journal
singleponds$title = NA
singleponds = singleponds %>% relocate(title, .after = year)

#enter paper titles manually
write.csv(singleponds, "Data_cleaned/Singleponds_EDI.csv", row.names=F)


#load new db with paper titles
sp = read.csv("Data_cleaned/Singleponds_EDI_withtitles.csv")

#make lowercase
sp$title = tolower(sp$title)
str(sp)

#fix volume_issue issues
sp = sp %>% mutate(volume_issue=ifelse(author=="barica" & year==1974 & journal=="archiv fur hydrobiologie", "73/3", volume_issue))
unique(sp$volume_issue)

#fix mcenroe et al. pond name issue
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==1027, "7 3", pondname))
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==2030, "7 4", pondname))
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==1698, "8 3", pondname))
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==1688, "2 3", pondname))
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==4314, "9 9", pondname))
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==951, "16 8", pondname))
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==4620, "16 4", pondname))
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==7500, "17 3", pondname))
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==2098, "19 9", pondname))
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==1549, "19 8", pondname))
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==3120, "19 4", pondname))
sp = sp %>% mutate(pondname=ifelse(author=="mcenroe et al"&mean_surfacearea_m2==1129, "9 5", pondname))

#check for papers with NO DATA at all
sp_ = sp %>% filter(is.na(mean_surfacearea_m2==TRUE) & is.na(max_surfacearea_m2==TRUE))
#drop Hayes et al
temp = filter(sp, author == "hayes, nicole m.; deemer, bridget r.; corman, jessica r.; razavi, n. roxanna; strock, kristin e.")
sp = sp %>% filter(!grepl('hayes, nicole m.; deemer, bridget r.; corman, jessica r.; razavi, n. roxanna; strock, kristin e.', author)) 
#drop oertli et al
temp = filter(sp, author == "oertli, cereghino, hull, miracle")
sp = sp %>% filter(!grepl("oertli, cereghino, hull, miracle", author)) 
#drop beebee
temp = filter(sp, author == "beebee")
sp = sp %>% filter(!grepl("beebee", author)) 
#drop indermuehle et al
temp = filter(sp, author == "indermuehle, oertli, biggs, céréghino,grillas, hull,nicolet &scher")
sp = sp %>% filter(!grepl("indermuehle, oertli, biggs, céréghino,grillas, hull,nicolet &scher", author)) 
#drop
temp = filter(sp, author == "prates & bonomo")
sp = sp %>% filter(!grepl("prates & bonomo", author)) 
#drop
temp = filter(sp, author == "ricciardi & rasmussen")
sp = sp %>% filter(!grepl("ricciardi & rasmussen", author)) 
#drop
temp = filter(sp, author == "angeler")
sp = sp %>% filter(!grepl("angeler", author)) 
#drop
temp = filter(sp, author == "scheffer")
sp = sp %>% filter(!grepl("scheffer", author)) 
#drop
temp = filter(sp, author == "de meester")
sp = sp %>% filter(!grepl("de meester", author)) 
#drop
temp = filter(sp, author == "oertli")
sp = sp %>% filter(!grepl("oertli", author)) 
#drop
temp = filter(sp, author == "nicolet")
sp = sp %>% filter(!grepl("nicolet", author)) 
#drop
temp = filter(sp, author == "biggs, j.; von fumetti, s.; kelly-quinn, m.")
sp = sp %>% filter(!grepl("biggs, j.; von fumetti, s.; kelly-quinn, m.", author)) 
#drop
temp = filter(sp, author == "zedler")
sp = sp %>% filter(!grepl("zedler", author)) 
#drop
temp = filter(sp, author == "reverey, florian; grossart, hans-peter; premke, katrin; lischeid, gunnar")
sp = sp %>% filter(!grepl("reverey, florian; grossart, hans-peter; premke, katrin; lischeid, gunnar", author)) 
#drop
temp = filter(sp, author == "marrone, federico; alfonso, giuseppe; stoch, fabio; pieri, valentina; alonso, miguel; dretakis, michalis; naselli-flores, luigi")
sp = sp %>% filter(!grepl("marrone, federico; alfonso, giuseppe; stoch, fabio; pieri, valentina; alonso, miguel; dretakis, michalis; naselli-flores, luigi", author)) 


#drop lat/lon
sp = sp %>% select(-latitude,-longitude,-notes)

#new df
sp_csv = sp
#replace comma w semi-colon
sp_csv$author = gsub(",", ";", sp_csv$author)
sp_csv$title = gsub(",", ";", sp_csv$title)
sp_csv$journal = gsub(",", ";", sp_csv$journal)
sp_csv$volume_issue = gsub(",", ";", sp_csv$volume_issue)
sp_csv$pond_definition = gsub(",", ";", sp_csv$pond_definition)
sp_csv$pondname = gsub(",", ";", sp_csv$pondname)
sp_csv$author_term = gsub(",", ";", sp_csv$author_term)
sp_csv$location = gsub(",", ";", sp_csv$location)

#double check for commas
sp_csv %>% filter_all(any_vars(grepl(',', .)))

#save
write.csv(sp_csv, "EDI/pond_data.csv", row.names=F)


#fix pond defs
defs = read.csv("Data_cleaned/pond_definitions.csv")
colnames(defs)[1] = "Author"
defs$Author = gsub(",", ";", defs$Author)
defs$Citation = gsub(",", ";", defs$Citation)
defs$Pond_definition = gsub(",", ";", defs$Pond_definition)
defs %>% filter_all(any_vars(grepl(',', .)))
write.csv(defs, "EDI/pond_definitions.csv", row.names=F)



#fix state defs
sdefs = read.csv("Data_cleaned/state_definitions.csv")
sdefs$Pond_Definition = gsub(",", ";", sdefs$Pond_Definition)
sdefs$Lake_Definition = gsub(",", ";", sdefs$Lake_Definition)
sdefs$Wetland_Definition = gsub(",", ";", sdefs$Wetland_Definition)
sdefs %>% filter_all(any_vars(grepl(',', .)))
write.csv(sdefs, "EDI/state_definitions.csv", row.names=F)




