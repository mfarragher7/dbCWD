#Code to analyze Pond Definitions 
#Created 06-Jun-2020 by MJF
#updated 03-Sep-2020 by KK 
#Use tiered levels for navigation - end comment headers with four #, if it is a sub-level, add * at the beginning

#Install packages and download libraries  ####
if(!require(lubridate)){install.packages("lubridate")}  
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(tm)){install.packages("tm")}
if(!require(SnowballC)){install.packages("SnowballC")}
if(!require(wordcloud)){install.packages("wordcloud")}
if(!require(RColorBrewer)){install.packages("RColorBrewer")}
if(!require(plyr)){install.packages("plyr")}

library(tidyverse)
library(lubridate)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(dplyr)

#Load definitions ####
#*Load single defs  ####
single_defs = read_csv("RawData/CombinedDataframes/Combined_singleponds_coord_fix.csv") 

#subset relevant columns (Author, year, journal, def, location, what author's call it, notes, initials. 
  #Leaving out categorical vars for now though that would be interesting to compare)
single_defs <- select(single_defs,Author,Year,Journal,'Volume/Issue',DefinitionOfPond,'What do the authors call it?',Location,OtherNotesAndUniquePoints,`Initials-WhoReviewedPaper`) 

#rename columns
single_defs = single_defs %>% set_names(~ str_to_lower(.) %>% 
  str_replace_all(" ","") %>%
  str_replace_all("/","_") %>% 
  str_replace_all("-","_") %>% 
  str_replace_all("othernotesanduniquepoints","notes")) 

names(single_defs)[names(single_defs) == "whatdotheauthorscallit?"] = "whatauthorscallit"

#add source column to ID singlePonds defs
single_defs$source <- as.factor("single")
columns = colnames(single_defs) #save colnames

#*Load multi defs ####
multi_defs = read_csv("RawData/CombinedDataframes/Combined_MultiplePonds.csv") 
multi_defs <- select(multi_defs, Author,Year,Journal,'Volume/Issue',DefinitionOfPond,'What do the authors call it?',Location,OtherNotesAndUniquePoints,Initials_WhoReviewedPaper)
colnames(multi_defs) = columns # assign the same column names as single ponds 
multi_defs$source <- as.factor("multi")

#*Load other defs ####
other_defs = read.csv("RawData/OtherData/Definitions_WhatIsAPondOrganization.csv") 
other_defs = other_defs[c(1,3,5)]
colnames(other_defs) <- c("author","journal","definitionofpond") # calling source "Author" and sourcetype "Journal" to keep things simple
other_defs$source <- as.factor("other")
defs <- rbind.fill(single_defs,multi_defs,other_defs) #rbind and fill in missing columns with NA's

#Clean definitions ####
#*search for def info in notes before subsetting out 'NA' definition rows ####
notes_search = grep("def", defs$notes) # there are 34 rows like this
defs$notes_search = NA # made new col in defs.
defs[grepl("def", defs$notes), 11]="Yes" #df to identify notes with "def" in them
  #read through these manually, notes that talk about pond definitions also have something in the "definitionofpond" column. 
  #should still keep notes, and some notes reference citations the author used for pond definitions 

#*subset NA's ####
defs = defs[!is.na(defs$definitionofpond),] #2131 rows before, 881 rows after. 

#*remove duplicates ####
#don't want to filter based on "definitionofapond" column because some say "yes" and also if definitions are used across multiple publications this could be interesting 
def_subset<-defs[!duplicated(defs[c("author", "journal")]), ] %>% #remove duplicates based on author and journal
  filter(definitionofpond != "No definition" & definitionofpond != "The study do not mention ponds" & definitionofpond != "not focused on ponds") #remove definitions that say there is no definition
def_subset <- def_subset[-c(180, 177, 162, 181, 171, 185, 178, 186, 168, 182, 179, 169, 170, 184, 158, 160), ] #drop rows that are duplicates from the "other" category, note, this is based on rows, so if things change above it will mess this up 

# Add New Definitions ####
#note that the below code is based on specific rows 
#Zelnik 2018 KK got definition from the notes, definition was listed as "yes" 
def_subset[47, 5] <- "The author includes a few pond definitions too: Wetzel, 2001; Gopal, 2016;De Meester et al., 2005"
#Boda 2018 KK got definition from the notes, definition was listed as "yes"
def_subset[48, 5] <- "Brief definition of marsh and citing Wood et al, 2001"
#Attermeyer 2017 KK got definition from the paper, definition was listed as "yes"
def_subset[49, 5] <-"Kettle holes are depressions size ranges from 0.0001 to 0.03 km2 and they can be either permanently filled with water, completely dry or experience wet and dry cycles of different time periods."
#Biggs 2017 KK got definition from the Dave, definition was listed as "yes"
def_subset[50, 5] <- "Ponds are small standing waters varying in size from 1 m2 to about 2-5 ha in area and may be permanent or seasonal, man-made or naturally created (Pond Conservation Group, 1993; Collinson et al., 1995; Biggs et al., 2007; E.P.C.N., 2007; Cereghino et al., 2008).
Although there is a long history, dating back to the nineteenth century, of attempts to define the difference between a pond and a lake (Biggs et al., 2005), large ponds and small lakes share many characteristics in terms of structure and function, and the transition zone between the two types of habitat is very gradual (Søndergaard et al., 2005; De Meester et al., 2005). 
Indeed, ponds merge imperceptibly into virtually all other freshwater habitat types (Biggs et al., 1997). However, for practical purposes, such as estimating waterbody numbers or comparing waterbody types, most authors have adopted a size-based classification with a size boundary somewhere between 1 and 5 ha, which can be fairly easily measured in the field (e.g.  Williams et al., 2004; Kalettka & Rudat, 2006; Davies et al., 2008a, b; De Bie et al., 2008; Williams et al., 2010a). Occasionally, ‘pond’ studies are restricted to waters of no more than 0.5 ha (e.g. Lafitte et al., 2009) or extended to include those up to 8 m in depth or 10 ha in area (e.g. Oertli et al., 2000, 2005). The Ramsar Convention adopted a cut-off between ponds and lakes of 8 ha, although in practice this has not been widely applied by workers investigating these habitats (Ramsar Convention Secretariat, 2013)."
#Reverey 2016 KK got definition from the paper, definition was listed as "yes"
def_subset[51, 5] <- "Kettle holes are small (<1 ha), shallow (≤3 m depth) depression wetlands which emerged in young moraine landscapes shaped by the last glaciation period 12,000–10,000 years before present and filled by the melting of residual ice blocks (Tiner, 2003; Kalettka & Rudat, 2006)."
#Sondergaard 2018 KK got definition from the paper, definition was listed as "yes", but new definition is not of pond, just shallow lakes
def_subset[113, 5]<- "we divided the lakes into deep and shallow defined as lakes with maximum depths >10 m and from ≥1 to 3 m, respectively. Inclusion of shallow lakes with maximum depth of <1 m were excluded"
#Thornhill 2018  Water - KK got definition from the paper, definition was listed as "yes"
def_subset[114, 5]<- "temperate ponds (<2 ha)"
#Thornhill 2018 Urban Ecosystems -KK got definition from the notes, definition was listed as "yes"
def_subset[115, 5]<-"Pond deifinition referes to Boothby 1999"
#Thornhill 2018 Global Change Bio - KK got definition from the notes, definition was listed as "yes"
def_subset[116, 5]<-"Pond deifinition referes to Boothby 1999"

#add specific additional definitions
names(def_subset)
def_subset = def_subset %>% 
  #Larson et al 2020
  add_row(
    author='Larson',                
    year=2020,              
    journal='Hydrobiologia',
    volume_issue=NA,
    definitionofpond='Shallow lakes are defined by average depths of ~100 cm, maximum depths of <400 cm 
    (the approximate threshold beyond which light attenuation prevents plant growth), and minimal thermal 
    stratification throughout the water column (Scheffer, 2004; MNDNR, 2010). Shallow lakes exist in two 
    alternative ecosystem states: either a clearwater state that is characterized by high water clarity and
    lush macrophytes or a turbid state characterized by planktonic algae, low macrophyte biomass, and turbid 
    water (Moss et al., 1996; Scheffer, 2004).',
    whatauthorscallit='shallow lakes',
    location='Minnesota and Wisconsin',
    notes= NA,
    initials_whoreviewedpaper=NA,
    source='other', 
    notes_search = NA) %>% 
  #Wisconsin clean water act 2020 document 
  add_row(
    author='Wisconsin clean water act',                
    year=2020,              
    journal='government docs',
    volume_issue=NA,
    definitionofpond='Small lakes < 10 acres. For >10 acres, they use the Lathrop/Lillie Equation to distinguish between shallow and deep lakes:
    Shallow lakes: >10 acres and “unstratified or mixed” (Lathrop/Lillie <= 3.8)
    Deep lakes: >10 acres and “stratified” (Lathrop/Lillie > 3.8) (Lathrop & Lilly, 1980)',
    whatauthorscallit='small lake',
    location=' wisconsin',
    notes= NA,
    initials_whoreviewedpaper=NA,
    source='other', 
    notes_search = NA) %>% 
  #dd NYS Fed of Lake Assn / DEC document defs
  add_row(
    author='New York State Federation of Lake Associations',                
    year=2020,              
    journal='government docs',
    volume_issue=NA,
    definitionofpond='A lake is usually larger than ten acres in area and ten feet in maximum depth. A pond is usually described
    as a shallow body of water that is smaller than a lake. Typically, a pond
    has uniform water temperature from top to bottom, little wave action, and often an abundance of aquatic
    plants. Pond waters are generally supplied from a very small area. The term “pond” also refers to small
    but permanent waterbodies that are water-filled depressions in the earth, whether created by natural
    contours, by beaver dams, or by people looking for a steady supply of water for fire protection, livestock
    or attracting wildlife. Vernal ponds, also called vernal pools, are ephemeral, forming after spring thaw or
    large storm events, but dissipating before attaining any degree of permanence. In many ways, vernal pools
    are the transition between lakes and wetlands. Wetlands are unique habitats that form the transition between the lake and the surrounding land.
    Wetlands have several common characteristics: the dominance of plants that require a wet habitat in order to live;
    soils that have characteristics associated with flooded or saturated conditions, such as a gray color; and
    evidence of predictable annual flooding.',
    whatauthorscallit='lake; pond; vernal pool; wetland',
    location=' new york',
    notes= NA,
    initials_whoreviewedpaper=NA,
    source='other', 
    notes_search = NA)
  

#save csv
write.csv(def_subset, "Data_cleaned/Definitions.csv")

