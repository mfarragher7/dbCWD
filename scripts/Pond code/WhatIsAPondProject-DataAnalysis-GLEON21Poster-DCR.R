#Analyze pond data and create some figures for the G21 Poster

dataDir<-"C:/Users/richardd/Google Drive/DCRR_2019/GLEON_PONDING/DataAnalysis/PondPosterAnalysis"
setwd(dataDir)
getwd()

#Get tidyverse package installed
if(!require(lubridate)){install.packages("lubridate")}  
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(tm)){install.packages("tm")}
if(!require(SnowballC)){install.packages("SnowballC")}
if(!require(wordcloud)){install.packages("wordcloud")}
if(!require(RColorBrewer)){install.packages("RColorBrewer")}

library(tidyverse)
library(lubridate)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Read in definitions csv file
DefinitionsDF<-read_csv("WhatIsPondProjectOrganization-Definitions-Downloaded19Oct2019-DCR.csv")
text2<-DefinitionsDF$Definition

#Read in all text definitions
#text<-readLines("PONDINGDefinitions-Downloaded14Oct2019_DCR.txt")

# Load the data as a corpus 
docs <- Corpus(VectorSource(text2))
inspect(docs)
#Clean the data
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector: remove pond or ponds from definition
docs <- tm_map(docs, removeWords, c("pond", "ponds","five","ten")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#Document matrix is a table of the frequency of words
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
#Generate wordcloud and export
jpeg("PONDING-Figure-DefinitionWordCloud.jpg",width=7, height=7, units="in", res=300)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=160, random.order=FALSE, rot.per=0.1, 
          colors=c(rgb(108,171,221,max=255),brewer.pal(6, "Dark2"),"#666666"),scale=c(7.25,0.75))
dev.off()

#Summarize the depth maximum for ponds from the definitions file####
PercentilesSAfromDefinitions<-quantile(DefinitionsDF$SurfaceAreaLessThan_m2,na.rm=T)
PercentilesDepthfromDefinitions<-quantile(DefinitionsDF$DepthLessThan_m,na.rm=T)


#Read in all the data for size analysis
LAGOS<-read_csv("LAGOS_supporting_geophysical.csv")
#Standarize surface area and depth units and remove all other columns
LAGOS<-LAGOS%>%mutate(SurfaceArea_m2=lake_area_ha*10000,MaxDepth_m=maxdepth)%>%mutate(Source="LAGOS")%>%select(Source,SurfaceArea_m2,MaxDepth_m)

#Read in the NENA data and #PONDING metadata for surface area and depth
NENAandPONDING<-read_csv("PONDINGandNENA-DepthsAndSurfaceAreas.csv")
#Separate into two data frames for NENA and for PONDING
NENA<-NENAandPONDING%>%filter(Project=="NENA")%>%mutate(Source="NENA")%>%select(Source,SurfaceArea_m2,MaxDepth_m)
PONDING<-NENAandPONDING%>%filter(Project=="PONDING")%>%mutate(Source="PONDING")%>%select(Source,SurfaceArea_m2,MaxDepth_m)

#Loading in the multiple and single ponds from the literature survey
SURVEY.single.fullDF<-read_csv("WhatIsPondProjectOrganization-Database_SinglePonds-Downloaded14Oct2019-DCR.csv")
#Rename and then select the two columns
SURVEY.single<-SURVEY.single.fullDF%>%rename(SurfaceArea_m2=`Mean surface area_m2`)%>%rename(MaxDepth_m=`Max depth_m`)%>%mutate(Source="Survey.single")%>%select(Source,SurfaceArea_m2,MaxDepth_m)

#Loading in the multiple ponds
SURVEY.multiple.fullDF<-read_csv("WhatIsPondProjectOrganization-Database_MultiplePonds-Downloaded14Oct2019-DCR.csv")
#Rename and then select the two columns
SURVEY.multiple<-SURVEY.multiple.fullDF%>%rename(SurfaceArea_m2=`Mean surface area_m`)%>%rename(MaxDepth_m=`Mean depth_m`)%>%mutate(Source="Survey.multiple")%>%select(Source,SurfaceArea_m2,MaxDepth_m)
#Add in a source column to the full df
SURVEY.multiple.fullDF<-SURVEY.multiple.fullDF%>%mutate(Source="Survey.multiple")

#Rbind them all
All.df<-bind_rows(LAGOS,NENA,PONDING,SURVEY.single,SURVEY.multiple)

#Plot depth vs. surface area####
###Still left to do:####
# 
#Export as a 6 x 9 figure (or )
jpeg("PONDING-Figure-SizeVsDepth.jpg",width=9, height=6, units="in", res=300)
ggplot(All.df,aes(x=MaxDepth_m,y=SurfaceArea_m2,group=Source))+
  geom_point(aes(shape=Source,fill=Source,size=Source))+
  scale_color_manual(name="Legend",labels=c("Lake: LAGOS","Lake: NENA","Pond: PONDING","Pond: Lit, Multiple","Pond: Lit, Single"),values = c("black","black","black","black","black"))+
  scale_fill_manual(name="Legend",labels=c("Lake: LAGOS","Lake: NENA","Pond: PONDING","Pond: Lit, Multiple","Pond: Lit, Single"),values = c("grey", "grey", "sky blue","sky blue","sky blue"))+
  scale_shape_manual(name="Legend",labels=c("Lake: LAGOS","Lake: NENA","Pond: PONDING","Pond: Lit, Multiple","Pond: Lit, Single"),values = c(21,22,23,24,25))+
  scale_size_manual(name="Legend",labels=c("Lake: LAGOS","Lake: NENA","Pond: PONDING","Pond: Lit, Multiple","Pond: Lit, Single"),values=c(1,1,3,3,3))+
  scale_x_continuous(trans='log10',name="Maximum Depth (m)",breaks=c(0.1,1,10,100),limits=c(0.05,250)) +
  scale_y_continuous(trans='log10',name=expression(Surface~Area~"(m"^2*")"),breaks=c(1,100,10000,1000000,100000000),labels=c(expression(10^0),expression(10^2),expression(10^4),expression(10^6),expression(10^8)))+
  geom_hline(yintercept = PercentilesSAfromDefinitions[3])+
  geom_hline(yintercept = PercentilesSAfromDefinitions[2],col="grey")+
  geom_hline(yintercept = PercentilesSAfromDefinitions[4],col="grey")+
  theme_classic()+
  theme(text = element_text(size=18),axis.text = element_text(size = 18),panel.border = element_rect(colour = "black", fill=NA, size=1),legend.position = c(0.86, 0.15),
        legend.direction = "vertical",legend.background = element_rect(fill=alpha(0.01)))+
  geom_errorbar(data=SURVEY.multiple.fullDF,mapping=aes(x=`Mean depth_m`,y=`Mean surface area_m`,ymin=`Mean surface area_m`-`Standard dev Surface area_m`,ymax=`Mean surface area_m`+`Standard dev Surface area_m`),color="black",position=position_dodge(0.1))+
  geom_point(data=SURVEY.multiple.fullDF,mapping=aes(x=`Mean depth_m`,y=`Mean surface area_m`),color="black",fill="sky blue",size=3,pch=24)
dev.off()  


#Make nice looking names in both full data frames
names(SURVEY.multiple.fullDF)<-make.names(names(SURVEY.multiple.fullDF),unique = TRUE)
names(SURVEY.single.fullDF)<-make.names(names(SURVEY.single.fullDF),unique = TRUE)


#Function that will generate a vector of simulated values if n, mean, and sd are given
normv<-function(n,mean,sd){
  #Set up out variable
  out<-NULL
  #Loop through each row of means and sds, generate n of each
  for(i in 1:length(mean)){
    #If either the mean is NA or the sd is NA
    #Then output all the mean values
    if(is.na(mean[i])|is.na(sd[i])){
      out<-c(out,rep(mean[i],n[i]))
    }else{
      out<-c(out,rnorm( n[i] , mean = mean[i] , sd = sd[i] ))  
    }
    
  }
  #replace any negative values with 0
  out[out<0]<-NA
  #Return the vector
  return(out)
}

#Convert the multiple pond data frames into simulated with similar columns to the single 
SURVEY.multiple.fullDF.simulated<-tibble(Source="Multiple",Pond.use=rep(SURVEY.multiple.fullDF$Pond.use,SURVEY.multiple.fullDF$n))
SURVEY.multiple.fullDF.simulated<-SURVEY.multiple.fullDF.simulated%>%
  mutate(Mean.depth_m=normv(SURVEY.multiple.fullDF$n,SURVEY.multiple.fullDF$Mean.depth_m,SURVEY.multiple.fullDF$Standard.deviation.depth_m))%>%
  mutate(Mean.surface.area_m2=normv(SURVEY.multiple.fullDF$n,SURVEY.multiple.fullDF$Mean.surface.area_m,SURVEY.multiple.fullDF$Standard.dev.Surface.area_m))%>%
  mutate(DOC_mgpL=normv(SURVEY.multiple.fullDF$n,SURVEY.multiple.fullDF$Mean.DOC_mgpL,SURVEY.multiple.fullDF$Standard.dev.DOC_mgpL))%>%
  mutate(Chla_ugpL=normv(SURVEY.multiple.fullDF$n,SURVEY.multiple.fullDF$Mean.Chla_ugpL,SURVEY.multiple.fullDF$Standard.dev.Chla_ugpL))%>%
  mutate(Turbidity_Secchi_m=normv(SURVEY.multiple.fullDF$n,SURVEY.multiple.fullDF$Mean.Turbidity_Secchi_m,SURVEY.multiple.fullDF$Standard.dev.Turbidity_Secchi_m))

#Combined the two into one data frame
Survey.combined<-bind_rows(SURVEY.single.fullDF%>%mutate(Source="Single")%>%mutate(DOC_mgpL=as.numeric(DOC_mgpL))%>%mutate(Turbidity_Secchi_m=as.numeric(Turbidity_Secchi_m))%>%mutate(Mean.depth_m=Max.depth_m)%>%mutate(SecchiToMaxDepthRatio=Turbidity_Secchi_m/Mean.depth_m)%>%select(Source,Pond.use,Mean.depth_m,Mean.surface.area_m2,DOC_mgpL,Chla_ugpL,Turbidity_Secchi_m),SURVEY.multiple.fullDF.simulated)  
Survey.combined<-Survey.combined%>%
  mutate(Pond.use=replace(Pond.use,Pond.use=="natural","Natural"))%>%
  mutate(Pond.use=replace(Pond.use,Pond.use=="agricultural*","Farm"))%>%
  mutate(Pond.use=replace(Pond.use,Pond.use=="Natural/Fisheries/Multiple","Multiple"))%>%
  mutate(Pond.use=replace(Pond.use,Pond.use=="Water Storage"|Pond.use=="Fisheries"|Pond.use=="Water Storage/Fisheries/Other","Water Storage/Fisheries"))

#Here is the list of Pond.use
unique(as.factor(Survey.combined$Pond.use))
#Melt the data frame
Survey.combined.melt <- Survey.combined%>%gather(VariableType,Measurement,Mean.depth_m:Turbidity_Secchi_m)
#Log transform the surface area for display
Survey.combined.melt<-Survey.combined.melt%>%
          mutate(Measurement=replace(Measurement,VariableType=="Mean.surface.area_m2",log10(Measurement)))%>%
          mutate(Pond.use=replace(Pond.use,is.na(Pond.use),"Unknown"))%>%
          filter(Pond.use!="Beaver")%>%
          mutate(Pond.use = fct_relevel(Pond.use,levels=c("Natural","Farm","Stormwater","Water Storage/Fisheries","Multiple","Unknown")))%>%mutate(VariableType = fct_relevel(VariableType,levels=c("Mean.surface.area_m2","Mean.depth_m","Chla_ugpL","Turbidity_Secchi_m","DOC_mgpL")))
#Add in variable to include bquote labels for each variable
Survey.combined.melt$VariableType2 <- factor(Survey.combined.melt$VariableType, labels = c(as.character(expression(Surface~area~"("*log*"("*m^{2}*"))")),as.character(expression(Max.~depth~"("*m*")")),as.character(expression(Chl~italic(a)~"("*mu*g*"/"*"L"*")")), as.character(expression(Secchi~depth~"("*m*")")),as.character(expression(DOC~"("*mg*"/"*L*")"))))

#Create a number of boxplots full of means/medians of various variables####     
jpeg("PONDING-Figure-BoxplotByPondUse.jpg",width=9, height=6, units="in", res=300)
ggplot(Survey.combined.melt, aes(x = Pond.use, y = Measurement,  fill = Pond.use)) +
  geom_boxplot() +
  geom_jitter(shape=16,color="black",position=position_jitter(0.2))+
  facet_wrap(~ VariableType2, scales = "free_y",nrow=5,strip.position = "left",labeller = label_parsed)+
  theme_bw()+
  ylab(NULL) +
  xlab("Pond Use")+
  scale_fill_manual(values=brewer.pal(6, "RdBu"))+
  theme(legend.position = "none",strip.background = element_blank(),strip.placement = "outside",strip.text.x = element_blank(),    panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()