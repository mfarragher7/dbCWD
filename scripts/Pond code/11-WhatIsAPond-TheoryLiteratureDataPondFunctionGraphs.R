# Create some theoretical and empirical figures based on our pond definition ideas
# Created 07Apr2021 by Dave Richardson (hereafter: DCR)


#Packages and libraries ####
#*Checks for packages ####
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(scales)){install.packages("scales")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(gtools)){install.packages("gtools")}
if(!require(mgcv)){install.packages("mgcv")}
if(!require(gratia)){install.packages("gratia")}
if(!require(strucchange)){install.packages("strucchange")}
if(!require(segmented)){install.packages("segmented")}
if(!require(patchwork)){install.packages("patchwork")}
if(!require(paleoMAS)){install.packages("paleoMAS")}
if(!require(nlme)){install.packages("nlme")}
if(!require(pryr)){install.packages("pryr")}


#*Load libraries ####
library(tidyverse)
library(ggpubr)
library(scales)
library(dplyr)
library(ggplot2)
library(car) # Levene's test 
library(agricolae) #LSD test
library(gtools)
library(mgcv) ##GAM models
library(gratia) ##GAM models
library(strucchange) #segmented regressions
library(segmented)
library(patchwork) #laying out multipanel plots with the same size
library(paleoMAS) #for AIC for LOESS
library(nlme) #for losgitic self starting functions


#Theoretical figure data####
#Create data frame with three columns of data
  #include flatline, linear, and s shaped curve
theoretical.df<-tibble(Pond.Xaxis=seq(0,100,by=1),flat=rep(50,101))
#Add the linear 
theoretical.df<-theoretical.df%>%mutate(linear=Pond.Xaxis*-1+98)
#Add the non-linear curve
theoretical.df<-theoretical.df%>%mutate(nonlinear1=100/(1+exp(-0.5*(Pond.Xaxis-50))),nonlinear2=100-nonlinear1)
#Add the non-linear segmeneted curve
theoretical.df<-theoretical.df%>%mutate(segmented=ifelse(Pond.Xaxis<50,100-1.2*Pond.Xaxis,50-0.2*Pond.Xaxis))


#Plot the flat line####
gg.flat<-ggplot(data=theoretical.df,aes(x=Pond.Xaxis, y=flat))+
  geom_line(size=1.2)+
  scale_y_continuous(limits=c(0,108))+
  scale_x_continuous(limits=c(0,100))+
  labs(x="Definition scale",y="Ecosystem structure or function")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#Plot the linear line####
gg.linear<-ggplot(data=theoretical.df,aes(x=Pond.Xaxis, y=linear))+
  geom_line(size=1.2)+
  scale_y_continuous(limits=c(0,112))+
  scale_x_continuous(limits=c(0,100))+
  labs(x="Definition scale",y="Ecosystem structure or function")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#Plot the nonlinear line####
gg.nonlinear<-ggplot(data=theoretical.df,aes(x=Pond.Xaxis, y=nonlinear2))+
  geom_line(size=1.2)+
  scale_y_continuous(limits=c(0,112))+
  scale_x_continuous(limits=c(0,100))+
  geom_vline(xintercept=50,color="dark grey",linetype=2)+
  labs(x="Definition scale",y="Ecosystem structure or function")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#Plot the segmented line####
gg.segmented<-ggplot(data=theoretical.df,aes(x=Pond.Xaxis, y=segmented))+
  geom_line(size=1.2)+
  scale_y_continuous(limits=c(0,112))+
  scale_x_continuous(limits=c(0,100))+
  geom_vline(xintercept=50,color="dark grey",linetype=2)+
  labs(x="Definition scale",y="Ecosystem structure or function")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#Modify with panel letters###
panelLetter.normal <- data.frame(
  xpos = c(-Inf),
  ypos =  c(Inf),
  hjustvar = c(-0.5) ,
  vjustvar = c(1.5))

#Arrange all three in a horizontal 3 panel figure
gg.4panelhorizontal<-ggarrange(
                              gg.flat+
                                theme(axis.title.x=element_blank(),
                                      axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank(),
                                      plot.margin=unit(c(2,0,1,1), "lines"),
                                      axis.ticks.length.x = unit(0, "pt"))+
                                labs(y=NULL, title=NULL)+
                                geom_text(data=panelLetter.normal,
                                          aes(x=xpos,
                                              y=ypos,
                                              hjust=hjustvar,
                                              vjust=vjustvar,
                                              label="(a)",
                                              fontface="bold")),
                          gg.linear+
                            theme(axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank(),
                                  plot.margin=unit(c(2,0,1,0), "lines"),
                                  axis.ticks.length.x = unit(0, "pt"))+
                            labs(y=NULL, title=NULL)+
                            geom_text(data=panelLetter.normal,
                                      aes(x=xpos,
                                          y=ypos,
                                          hjust=hjustvar,
                                          vjust=vjustvar,
                                          label="(b)",
                                          fontface="bold")),
                          gg.segmented+
                            theme(axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank(),
                                  plot.margin=unit(c(2,0,1,0), "lines"),
                                  axis.ticks.length.x = unit(0, "pt"))+
                            labs(y=NULL, title=NULL)+
                            geom_text(data=panelLetter.normal,
                                      aes(x=xpos,
                                          y=ypos,
                                          hjust=hjustvar,
                                          vjust=vjustvar,
                                          label="(c)",
                                          fontface="bold")),
                          gg.nonlinear+
                            theme(plot.margin=unit(c(2,1,1,0), "lines"))+
                            labs(y=NULL, title=NULL,x=NULL)+
                            geom_text(data=panelLetter.normal,
                                      aes(x=xpos,y=ypos,hjust=hjustvar,
                                          vjust=vjustvar,label="(d)",
                                          fontface="bold")),
                          ncol=4,align="h")

gg.4panelhorizontal #print it out

#Add in common axes####
gg.4panelhorizontal.anno<-annotate_figure(gg.4panelhorizontal,bottom=text_grob("Definition scale",hjust=0.5,vjust=0,size=14),left=text_grob("Ecosystem structure/function",rot=90,size=14,hjust=0.5))
#FIGURE: theoeretical relationships between definition scale and ecosystem, structure function, print out as jpg####
ggsave("plots/WIAP-FigureX-TheoreticalRelationships.jpg", plot=gg.4panelhorizontal.anno, width=6, height=3,units="in", dpi=300)



#Diel temperature range figure####
#Data from Woolway et al. 2016, PlosOne
#*add in extra columns for m2 and ha surface area####
DielTempRange_Woolway<-read_csv('RawData/DataCollatedFromOtherStudies/Woolway_etal_2016_diel_range_depthArea.csv')%>%mutate(area_ha=area_km2*100,area_m2=area_km2*1000000)
  #Find the range
  summary(DielTempRange_Woolway$area_ha)
  
#*Data extracted from Martinsen et al. 2019, Hydrolobiologia, Table 1 for the surface area, Figure 3 #### 
DielTempRange_Martinsen<-tibble(area_m2=c(4336,483,704,1062,420,503,577,2206,682),max_depth_m=c(0.68,0.68,0.69,0.73,0.77,1.27,1.36,1.54,1.69),
                                  diel_rng=c(6+(9/22),
                                              8+(7/22),
                                             6-(10/22),
                                             6-(4/22),
                                             4+(12.5/22),
                                             4+(7.5/22),
                                             4+(10.5/22),
                                             4-(7/22),
                                             2+(10/22)
                                  ))%>%mutate(area_ha=area_m2*0.0001, area_km2=area_ha*0.01)

#*Data from PONDING mix project (DCR and MH)####
#see script from there 
DielTempRange_PONDINGmix<-read_csv('RawData/DataCollatedFromOtherStudies/DailyTemperatureRangeAndSurfaceAreaForWhatIsAPond.csv')%>%mutate(area_ha=area_m2*0.0001,area_km2=area_ha*0.01,diel_rng=DailyTemperatureRange_degC)
  #Summarize to find range
  summary(DielTempRange_PONDINGmix$area_ha)
  #Read in separate sheet for the depths
    #Reformat some things to get it to match
  Depths_PONDINGmix<-read_csv('RawData/DataCollatedFromOtherStudies/DailyTemperatureRangeAndSurfaceAreaForWhatIsAPond-DepthsFromPONDINGMix.csv')%>%rename(Pond_name=pond,max_depth_m=zmax_m)%>%mutate(Pond_name=str_replace_all(string=Pond_name,pattern=" ",repl=""))
  #Merge them together to get depths in
  DielTempRange_PONDINGmix<-DielTempRange_PONDINGmix%>%left_join(Depths_PONDINGmix,by="Pond_name")
  
#*Join all the data frames together####
  DielTempRange_all<-bind_rows(bind_rows(DielTempRange_PONDINGmix%>%mutate(source="PONDINGmix")%>%dplyr::select(-DailyTemperatureRange_degC,-Pond_name),DielTempRange_Martinsen%>%mutate(source="Martinsen")),DielTempRange_Woolway%>%mutate(source="Woolway"))
  #Create bins by log size class
  DielTempRange_all<-DielTempRange_all%>%mutate(bins_km2=factor(floor(log10(area_km2))))%>%
                      mutate(log_area_ha=log10(area_ha)) #create a log_area_ha variables
  
  
  #dtr: Flat model####
  dtr.flat<-lm(diel_rng~1,data=DielTempRange_all)
  summary(dtr.flat)
    #dtr: rsme####
  dtr.flat.rsme<-sqrt(c(crossprod(dtr.flat$residuals))/length(dtr.flat$residuals))
    #dtr: AICc###
    dtr.flat.aicc<-AIC(dtr.flat)+ (2 * 1^2 + 2 * 1)/(length(dtr.flat$residuals) - 1 - 1)
  #dtr: linear model####
  dtr.lm<-lm(diel_rng~log_area_ha,data=DielTempRange_all)
  summary(dtr.lm)
    #dtr: rsme####
  dtr.lm.rsme<-sqrt(c(crossprod(dtr.lm$residuals))/length(dtr.lm$residuals))
  #dtr: AICc###
    dtr.lm.aicc<-AIC(dtr.lm)+ (2 * 2^2 + 2 * 2)/(length(dtr.flat$residuals) - 2 - 1)
  
  #*Segmented regression####
  #Try the segmented package version: https://rpubs.com/MarkusLoew/12164####
  dtr.segmented<-segmented(lm(diel_rng ~ log_area_ha,data=DielTempRange_all),seg.z=log_area_ha,psi=c(0.6)) #initial guess makes a difference
  summary(dtr.segmented) 
  dtr.segmented$psi #get the breakpoints
  10^dtr.segmented$psi[,"Est."] #back calculate breakpoint - this one is at 0.35 and 11.2 ha
  #get the average of the middle segment
  10^mean(dtr.segmented$psi[,"Est."]) #This is a 2.00 ha mid point of the middle segment
  slope(dtr.segmented) #get the slopes  
  dtr.segmented.fitted<-fitted(dtr.segmented) #get the fitted data
  dtr.segmentedModel<-tibble(area_ha=10^DielTempRange_all$log_area_ha,log_area_ha=DielTempRange_all$log_area_ha,dtr.segmented.fitted=dtr.segmented.fitted) #calculate the fit with the back transformed x axis
  #dtr loess: rsme####
  dtr.segmented.rsme<-sqrt(c(crossprod(DielTempRange_all$diel_rng-dtr.segmentedModel$dtr.segmented.fitted)/length(DielTempRange_all$diel_rng)))
  #dtr: AICc###  
  dtr.segmented.aicc<-AIC(dtr.segmented)+ (2 * 4^2 + 2 * 4)/(length(dtr.flat$residuals) - 4 - 1)
  
  #*Create the loess model####
  dtr.loess<-loess(diel_rng~log_area_ha,data=DielTempRange_all,span=0.75)
  
  #*find the x range###  
  dtr.xrange <- range(DielTempRange_all$area_ha)
  #*create a sequence for the fitted values, make sure to log transform here####
  dtr.xseq <- seq(from=log10(dtr.xrange[1]), to=log10(dtr.xrange[2]), length=200)
  #*Create data frame with the loess predicted - this is what is generated in the geom_smooth output, note they are log transformed predictions####
  dtr.loess.fit<-tibble(loess_fit=predict(dtr.loess,newdata=data.frame(log_area_ha=dtr.xseq)),log_area_ha=dtr.xseq)
  #*Calculate the first derivative of the loess modeling using the differencing method, centered the areas, also log transformed####
  fd.dtr.loess<-tibble(d_diel_rng=as.numeric(diff(dtr.loess.fit$loess_fit)/diff(dtr.loess.fit$log_area_ha)),area_ha_centered=rowMeans(embed( dtr.loess.fit$log_area_ha,2)))
  #*Find the minimum slope (fastest rate of change), back transform to get into the same scale and print it####
  (dtr.cutoff<-10^(fd.dtr.loess%>%filter(d_diel_rng==min(d_diel_rng,na.rm=TRUE))%>%dplyr::select(area_ha_centered)%>%pull()))
  #*Just to check, plot the first derivative####
  plot(fd.dtr.loess$d_diel_rng~fd.dtr.loess$area_ha_centered)
  #dtr loess: rsme####
  dtr.loess.rsme<-sqrt(c(crossprod(DielTempRange_all$diel_rng-predict(dtr.loess))/length(DielTempRange_all$diel_rng)))
  #AIC for loess
  dtr.loess.aic<-akaike.l(y=DielTempRange_all$diel_rng,x=DielTempRange_all$log_area_ha,interval=c(0.75,0.75,0))
    print(dtr.loess.aic)
  dtr.loess.aic$aic.loess[1,3]+((2 * (dtr.loess$trace.hat + 1)) / (dtr.loess$n - dtr.loess$trace.hat - 2))
  
  #Function for AICc for loess from http://www.wellformedness.com/blog/loess-hyperparameters-without-tears/
  aicc.loess <- function(fit) {
    # compute AIC_C for a LOESS fit, from:
    # 
    # Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing 
    # parameter selection in nonparametric regression using an improved 
    # Akaike Information Criterion. Journal of the Royal Statistical 
    # Society B 60: 271–293.
    # 
    # @param fit        loess fit
    # @return           'aicc' value
    stopifnot(inherits(fit, 'loess'))
    # parameters
    n <- fit$n
    trace <- fit$trace.hat
    sigma2 <- sum(resid(fit) ^ 2) / (n - 1)
    return(log(sigma2) + 1 + (2 * (trace + 1)) / (n - trace - 2))
  }
  
  #Implementation of the aicc.loess which is much different than akaike.l loess    
  aicc.loess(dtr.loess)
  
  #Test case of nonlinear fit with logistic decay to theoretical data####
  #See here for more information: https://bscheng.com/2014/05/07/modeling-logistic-growth-data-in-r/
  wilson<-nls(nonlinear2~A+(B-A)/(1+exp((xmid-Pond.Xaxis)/scal)),
              start=list(A=0,B=100,xmid=50,scal=-2.2),data=theoretical.df,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
  
  #Fit a nonlinear logistic curve to the dtr data####
  dtr.logistic<-nls(diel_rng~A+(B-A)/(1+exp((xmid-log_area_ha)/scal)),
                    start=list(A=1,B=5,xmid=1,scal=-0.5),data=DielTempRange_all,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
  summary(dtr.logistic)
  #Get the cutoff from the logistic parameter as the xmid inflection point
  dtr.logistic.cutoff<-10^dtr.logistic$m$getAllPars()[3]
  #AICC for logistic model####
  dtr.logistic.aicc<-AIC(dtr.logistic)+ (2 * 4^2 + 2 * 4)/(length(dtr.flat$residuals) - 4 - 1)
  
  #Store rsme from each model as a column####
  #Keep adding to this with each analysis
  rsme.df<-tibble(type="dielTempRange",flat=dtr.flat.rsme,linear=dtr.lm.rsme,segmented=dtr.segmented.rsme,loess=dtr.loess.rsme)
  #Store AICc from each model as a column####
  #Keep adding to this with each analysis
  aicc.df<-tibble(type="dielTempRange",flat=dtr.flat.aicc,linear=dtr.lm.aicc,segmented=dtr.segmented.aicc,logistic=dtr.logistic.aicc)
  
  #How to compare the different models via AICC
  #From this one: https://www.sciencedirect.com/science/article/pii/S2468042719300508 - see Burnham and Anderson 2002 citation
    #Workflow
    #Find model with lowest AICc
    #calculate the delta compared to that min for all other models
    #If model delta is <2, then consider that model
    #If delta is 4 to 7, then that model has less support
    #If delta is >10 then model has no support and can be omitted
  
  
  #Plot each of the models overlaid
  ggplot(data=DielTempRange_all,aes(y=diel_rng, x=log_area_ha))+geom_point()+
    geom_hline(yintercept=mean(DielTempRange_all$diel_rng,na.rm=TRUE),color="orange")+
    geom_smooth(method='lm', se=FALSE,color="red")+
    geom_line(data=dtr.segmentedModel,aes(y=dtr.segmented.fitted,x=log_area_ha),color="blue")+ #This is the segmented regression
    geom_smooth(se=FALSE,color="green")+ #loess
    geom_line(data=tibble(logistic_predict=predict(dtr.logistic),log_area_ha=DielTempRange_all$log_area_ha), aes(y=logistic_predict,x=log_area_ha),color="purple")+
    geom_text(data=data.frame(rsme=round(dtr.flat.rsme,3)), aes(label = paste("flat=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 1)+
    geom_text(data=data.frame(rsme=round(dtr.lm.rsme,3)), aes(label = paste("linear=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 3)+
    geom_text(data=data.frame(rsme=round(dtr.segmented.rsme,3)), aes(label = paste("segmented=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 5)+
    geom_text(data=data.frame(rsme=round(dtr.loess.rsme,3)), aes(label = paste("loess=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 7)+
    theme_bw()
  

#Recreate woolway plot figure 
gg.dtr.scatter<-ggplot(data=DielTempRange_all,aes(x=area_ha,y=diel_rng))+
  geom_vline(xintercept=dtr.logistic.cutoff,color="black",linetype=1,size=1)+ #vertical line indicating the maximum first derivative from the GAM model
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(0.001,550000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  #geom_smooth(se=FALSE,color="black",size=1.2)+ #smooth gives slight differences than the GAM predicted, use GAM
  #geom_line(data=dtr.loess.fit,aes(y=loess_fit,x=10^log_area_ha),color="green")+
  geom_line(data=tibble(logistic_predict=predict(dtr.logistic),log_area_ha=DielTempRange_all$log_area_ha), aes(y=logistic_predict,x=10^log_area_ha),color="black",size=1.2)+
  labs(y=bquote(Diel~temp.~range~(degree*C)),x=bquote(Surface~area~(ha)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        )
#FIGURE: daily tempreature range scatter, print out as jpg####
ggsave("plots/WIAP-FigureX-dailyTemperatureTangeScatter.jpg", plot=gg.dtr.scatter, width=3, height=3,units="in", dpi=300)

#Daily temperature range in the boxplot style like Merediths figure  
gg.dtr.box<-ggplot(data=DielTempRange_all,aes(x=bins_km2,y=diel_rng))+
  geom_boxplot()+
  geom_jitter()+
  scale_x_discrete(labels=c(bquote("<"*10^-3),bquote(10^-3~-~10^-2),bquote(10^-2~-~10^-1),bquote(10^-1~-~10^0),bquote(10^0~-~10^1),bquote(10^1~-~10^2),bquote(">"*10^2)))+
  labs(y=bquote(Diel~temp.~range~(degree*C)),x=bquote(Surface~area~(km^2)))+
  #labels=bquote(.(parse(text=paste("10^",as.numeric(as.character(levels(DielTempRange_all$bins_km2),sep=""))))
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=-45,vjust=0.95,hjust=0))
#FIGURE: daily tempreature range scatter, print out as jpg####
ggsave("plots/WIAP-FigureX-dailyTemperatureTangeBoxplot.jpg", plot=gg.dtr.box, width=3, height=3,units="in", dpi=300)


DielTempRange_all$bins_km2 #look at the bins


#Methane fluxes figures####
#Read in methane fluxes csv
methaneFluxesDF<-read_csv('RawData/DataCollatedFromOtherStudies/MethaneFluxes_Dataset_for_Judith_20210114.csv')%>%
  mutate(area_km2=Surface_area_km2,area_ha=area_km2*100, area_m2=area_km2*1000000)%>% #create some new variables
  filter(Site_Type=="Lake")%>% #Select only lakes as per Meredith's suggestion
  mutate(bins_km2=floor(log10(area_km2)))%>% #create a factor for the boxplot
  mutate(bins_km2=factor(as.numeric(ifelse(bins_km2=="-5"|bins_km2=="-6","-4",ifelse(bins_km2=="3","2",bins_km2)))))%>%
  mutate(log_area_ha=log10(area_ha))%>% #add in a log transformed variable%>%
  filter(!is.na(area_ha))%>% #remove any rows missing data from our two columns of interest
  filter(!is.na(fch4_mgCm2d)) #remove any rows missing data from our two columns of interest
methaneFluxesDF$bins_km2 #check out the bins to make sure they are good
  summary(methaneFluxesDF$area_ha) #Check out the range of the area


  # #**Run the General Additive model (GAM)####
  # methaneFluxes.gam<-gam(fch4_mgCm2d~s(log_area_ha),
  #              data = methaneFluxesDF,
  #              #family=Gamma(link="log"),
  #              #correlation = corCAR1(form = ~ log_area_ha),
  #              method = "REML") #GAM model with x log transformed
  # summary(methaneFluxes.gam) #outputs p-values for the model
  # 
  # #**Add in the predicted values from the model####
  # methaneFluxesDF<-bind_cols(methaneFluxesDF,tibble(predict=predict(methaneFluxes.gam))) #generate the GAM fit predicted
  # 
  # #**find the first derivative####
  # fd.methaneFluxes<-fderiv(methaneFluxes.gam) #first derivative of the gam
  # #**Get the confidence intervals####
  # Term<-"log_area_ha"
  # fd.methaneFluxes.ci<-confint(fd.methaneFluxes,term=Term) #confidence intervals
  # fd.methaneFluxes.ci.bound<-bind_cols(fd.methaneFluxes$eval,fd.methaneFluxes.ci) #bind the area with the confidence intervals
  # #**find the first derviative with the biggest absoluate value####
  # methaneFluxes.cutoff<-10^(fd.methaneFluxes.ci.bound%>%filter(est==min(est))%>%dplyr::select(log_area_ha)%>%pull())
  
  #methand: Flat model####
  methaneFluxes.flat<-lm(log10(fch4_mgCm2d)~1,data=methaneFluxesDF)
  summary(methaneFluxes.flat)
  #methaneFluxes flat: rsme####
  methaneFluxes.flat.rsme<-sqrt(c(crossprod(methaneFluxes.flat$residuals))/length(methaneFluxes.flat$residuals))
  
  #methane fluxes: linear model####
  methaneFluxes.lm<-lm(log10(fch4_mgCm2d)~log_area_ha,data=methaneFluxesDF)
  summary(methaneFluxes.lm)
  #methaneFluxes linear: rsme####
  (methaneFluxes.lm.rsme<-sqrt(c(crossprod(methaneFluxes.lm$residuals))/length(methaneFluxes.lm$residuals)))
  
  #*Segmented regression####
  #Try the segmented package version: https://rpubs.com/MarkusLoew/12164####
  methaneFluxes.segmented<-segmented(lm(log10(fch4_mgCm2d) ~ log_area_ha,data=methaneFluxesDF),seg.z=log_area_ha,psi=c(-1)) #initial guess makes a difference
  summary(methaneFluxes.segmented) 
  methaneFluxes.segmented$psi #get the breakpoints
  10^methaneFluxes.segmented$psi[,"Est."] #back calculate breakpoint - this one is at 0.35 and 11.2 ha
  slope(methaneFluxes.segmented) #get the slopes  
  methaneFluxes.segmented.fitted<-fitted(methaneFluxes.segmented) #get the fitted data
  methaneFluxes.segmentedModel<-tibble(area_ha=10^methaneFluxesDF$log_area_ha,log_area_ha=methaneFluxesDF$log_area_ha,methaneFluxes.segmented.fitted=methaneFluxes.segmented.fitted) #calculate the fit with the back transformed x axis
  #dtr loess: rsme####
  (methaneFluxes.segmented.rsme<-sqrt(c(crossprod(log10(methaneFluxesDF$fch4_mgCm2d)-methaneFluxes.segmentedModel$methaneFluxes.segmented.fitted)/length(methaneFluxesDF$fch4_mgCm2d))))
  
  #*Create the loess model####
   methaneFluxes.loess<-loess(log10(fch4_mgCm2d)~log_area_ha,data=methaneFluxesDF)
   #*find the x range###  
   methaneFluxes.xrange <- range(methaneFluxesDF$area_ha)
   #*create a sequence for the fitted values, make sure to log transform here####
   methaneFluxes.xseq <- seq(from=log10(methaneFluxes.xrange[1]), to=log10(methaneFluxes.xrange[2]), length=200)
   #*Create data frame with the loess predicted - this is what is generated in the geom_smooth output, note they are log transformed predictions####
   methaneFluxes.loess.fit<-tibble(loess_fit=predict(methaneFluxes.loess,newdata=data.frame(log_area_ha=methaneFluxes.xseq)),area_ha=methaneFluxes.xseq)
   #*Calculate the first derivative of the loess modeling using the differencing method, centered the areas, also log transformed####
   fd.methaneFluxes.loess<-tibble(d_fch4_mgCm2d=as.numeric(diff( methaneFluxes.loess.fit$loess_fit)/diff( methaneFluxes.loess.fit$area_ha)),area_ha_centered=rowMeans(embed( methaneFluxes.loess.fit$area_ha,2)))
   #*Find the minimum slope (fastest rate of change), back transform to get into the same scale
   methaneFluxes.cutoff<-10^(fd.methaneFluxes.loess%>%filter(d_fch4_mgCm2d==min(d_fch4_mgCm2d,na.rm=TRUE))%>%dplyr::select(area_ha_centered)%>%pull())
    #*Just to check, plot the first derivative####
    plot(fd.methaneFluxes.loess$d_fch4_mgCm2d~fd.methaneFluxes.loess$area_ha_centered)
   #methaneFluxes loess: rsme####
   (methaneFluxes.loess.rsme<-sqrt(c(crossprod(log10(methaneFluxesDF$fch4_mgCm2d)-predict(methaneFluxes.loess))/length(log10(methaneFluxesDF$fch4_mgCm2d)))))
  
   #Fit a nonlinear logistic curve to the ch4 data####
   methaneFluxes.logistic<-nls(log10(fch4_mgCm2d)~A+(B-A)/(1+exp((xmid-log_area_ha)/scal)),
                     start=list(A=1,B=2,xmid=1,scal=-0.2),data=methaneFluxesDF,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
   summary(methaneFluxes.logistic)
    #Get the cutoff from the logistic parameter as the xmid inflection point
   methaneFluxes.logistic.cutoff<-10^methaneFluxes.logistic$m$getAllPars()[3]
   #rmse of the logistic model####
   sqrt(c(crossprod(log10(methaneFluxesDF$fch4_mgCm2d)-predict(methaneFluxes.logistic))/length(log10(methaneFluxesDF$fch4_mgCm2d))))
   
   #flat model: AICc###
   methaneFluxes.flat.aicc<-AIC(methaneFluxes.flat)+ (2 * 1^2 + 2 * 1)/(length(methaneFluxes.flat$residuals) - 1 - 1)
   
   #Linear model: AICc###
   methaneFluxes.lm.aicc<-AIC(methaneFluxes.lm)+ (2 * 2^2 + 2 * 2)/(length(methaneFluxes.flat$residuals) - 2 - 1)
   
   #Segmented model: AICc####  
   methaneFluxes.segmented.aicc<-AIC(methaneFluxes.segmented)+ (2 * 4^2 + 2 * 4)/(length(methaneFluxes.flat$residuals) - 4 - 1)
   
   #Logistic model: AICc#### 
   methaneFluxes.logistic.aicc<-methaneFluxes.logistic.aicc<-AIC(methaneFluxes.logistic)+ (2 * 4^2 + 2 * 4)/(length(methaneFluxes.flat$residuals) - 4 - 1)
   
   #Store aicc from each model as a row####
   #Keep adding to this with each analysis
   aicc.df<-bind_rows(aicc.df,tibble(type="methaneFluxes",flat=methaneFluxes.flat.aicc,linear=methaneFluxes.lm.aicc,segmented=methaneFluxes.segmented.aicc,logistic=methaneFluxes.logistic.aicc))
   
   
   #Store rsme from each model as a row####
   #Keep adding to this with each analysis
   rsme.df<-bind_rows(rsme.df,tibble(type="methaneFluxes",flat=methaneFluxes.flat.rsme,linear=methaneFluxes.lm.rsme,segmented=methaneFluxes.segmented.rsme,loess=methaneFluxes.loess.rsme))
   
   #Plot each of the models overlaid
   ggplot(data=methaneFluxesDF,aes(y=log10(fch4_mgCm2d), x=log_area_ha))+geom_point()+
     geom_hline(yintercept=mean(log10(methaneFluxesDF$fch4_mgCm2d),na.rm=TRUE),color="orange")+
     geom_smooth(method='lm', se=FALSE,color="red")+
     geom_line(data=methaneFluxes.segmentedModel,aes(y=methaneFluxes.segmented.fitted,x=log_area_ha),color="blue")+ #This is the segmented regression
     geom_smooth(se=FALSE,color="green")+ #loess
     geom_line(data=tibble(logistic_predict=predict(methaneFluxes.logistic),log_area_ha=methaneFluxesDF$log_area_ha), aes(y=logistic_predict,x=log_area_ha),color="purple")+
     geom_text(data=data.frame(rsme=round(methaneFluxes.flat.rsme,3)), aes(label = paste("flat=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 1)+
     geom_text(data=data.frame(rsme=round(methaneFluxes.lm.rsme,3)), aes(label = paste("linear=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 3)+
     geom_text(data=data.frame(rsme=round(methaneFluxes.segmented.rsme,3)), aes(label = paste("segmented=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 5)+
     geom_text(data=data.frame(rsme=round(methaneFluxes.loess.rsme,3)), aes(label = paste("loess=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 7)+
     theme_bw()
   
#Recreate woolway plot figure#### 
gg.ch4flux.scatter<-ggplot(data=methaneFluxesDF,aes(x=area_ha,y=fch4_mgCm2d))+
  geom_vline(xintercept=methaneFluxes.logistic.cutoff,color="black",linetype=1,size=1)+ #vertical line indicating the maximum first derivative from the loess model
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(0.001,550000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
     geom_line(data=tibble(logistic_predict=predict(methaneFluxes.logistic),log_area_ha=methaneFluxesDF$log_area_ha), aes(y=10^logistic_predict,x=10^log_area_ha),color="black",size=1.2)+
     #geom_line(data=methaneFluxes.segmentedModel,aes(y=10^methaneFluxes.segmented.fitted,x=10^log_area_ha),color="black",size=1.2)+ #This is the segmented regression
     #geom_smooth(method="loess",se=FALSE,size=1.2,color="black")+ #put in the loess smoothed line
  #geom_line(aes(y=predict),color="black",size=1.2)+
  #geom_line(data=methanefluxes.loess.fit,aes(x=10^log_area_ha,y=loess_fit),color="purple")+
  #geom_line(data=temp,aes(x=area_ha,y=loess_fit),color="green")+
  scale_y_log10()+
  labs(y=bquote(CH[4]~flux~(mg~C~m^-2~d^-1)),x=bquote(Surface~area~(ha)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
        )
#FIGURE: daily tempreature range scatter, print out as jpg####
ggsave("plots/WIAP-FigureX-methaneFluxesScatter.jpg", plot=gg.ch4flux.scatter, width=3, height=3,units="in", dpi=300)

#Daily temperature range in the boxplot style like Merediths figure  
gg.ch4flux.box<-ggplot(data=subset(methaneFluxesDF,!is.na(area_ha)),aes(x=bins_km2,y=fch4_mgCm2d))+
  geom_boxplot()+
  geom_jitter()+
  scale_x_discrete(labels=c(bquote("<"*10^-3),bquote(10^-3~-~10^-2),bquote(10^-2~-~10^-1),bquote(10^-1~-~10^0),bquote(10^0~-~10^1),bquote(10^1~-~10^2),bquote(">"*10^2)))+
  scale_y_log10()+
  labs(y=bquote(Methane~flux~(mg~C~m^-2~d^-1)),x=bquote(Surface~area~(km^2)))+
  #labels=bquote(.(parse(text=paste("10^",as.numeric(as.character(levels(DielTempRange_all$bins_km2),sep=""))))
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=-45,vjust=0.95,hjust=0))
#FIGURE: daily tempreature range scatter, print out as jpg####
ggsave("plots/WIAP-FigureX-methaneFluxesBoxplot.jpg", plot=gg.ch4flux.box, width=3, height=3,units="in", dpi=300)


#k600 figures####
#Read in Holgerson k600 lit review csv
k600DF<-read_csv('RawData/DataCollatedFromOtherStudies/Holgerson17_k600 studies.csv')%>%
  mutate(area_ha=area_km2*100)%>% #create some new variables
  mutate(bins_km2=floor(log10(area_km2)))%>% #create a factor for the boxplot
  mutate(bins_km2=factor(as.numeric(ifelse(bins_km2=="-5"|bins_km2=="-6","-4",ifelse(bins_km2=="3","2",bins_km2)))))%>%
  mutate(log_area_ha=log10(area_ha))
k600DF$bins_km2 #check out the bins to make sure they are good
summary(k600DF$area_ha) #Check out the range of the area

#k600: Flat model####
k600.flat<-lm(log10(k600_mday)~1,data=k600DF)
summary(k600.flat)
#k600 flat: rsme####
(k600.flat.rsme<-sqrt(c(crossprod(k600.flat$residuals))/length(k600.flat$residuals)))

#methane fluxes: linear model####
k600.lm<-lm(log10(k600_mday)~log_area_ha,data=k600DF)
summary(k600.lm)
#k600 linear: rsme####
(k600.lm.rsme<-sqrt(c(crossprod(k600.lm$residuals))/length(k600.lm$residuals)))

#*Segmented regression####
#Try the segmented package version: https://rpubs.com/MarkusLoew/12164####
k600.segmented<-segmented(lm(log10(k600_mday) ~ log_area_ha,data=k600DF),seg.z=log_area_ha,psi=c(0.6)) #initial guess makes a difference
summary(k600.segmented) 
k600.segmented$psi #get the breakpoints
10^k600.segmented$psi[,"Est."] #back calculate breakpoint - this one is at 0.35 and 11.2 ha
slope(k600.segmented) #get the slopes  
k600.segmented.fitted<-fitted(k600.segmented) #get the fitted data
k600.segmentedModel<-tibble(area_ha=10^k600DF$log_area_ha,log_area_ha=k600DF$log_area_ha,k600.segmented.fitted=k600.segmented.fitted) #calculate the fit with the back transformed x axis
#dtr loess: rsme####
(k600.segmented.rsme<-sqrt(c(crossprod(log10(k600DF$k600_mday)-k600.segmentedModel$k600.segmented.fitted)/length(k600DF$k600_mday))))

#*Create the loess model####
k600.loess<-loess(log10(k600_mday)~log_area_ha,data=k600DF)
#*find the x range###  
k600.xrange <- range(k600DF$area_ha)
#*create a sequence for the fitted values, make sure to log transform here####
k600.xseq <- seq(from=log10(k600.xrange[1]), to=log10(k600.xrange[2]), length=200)
#*Create data frame with the loess predicted - this is what is generated in the geom_smooth output, note they are log transformed predictions####
k600.loess.fit<-tibble(loess_fit=predict(k600.loess,newdata=data.frame(log_area_ha=k600.xseq)),area_ha=k600.xseq)
#*Calculate the first derivative of the loess modeling using the differencing method, centered the areas, also log transformed####
fd.k600.loess<-tibble(d_k600_mday=as.numeric(diff(k600.loess.fit$loess_fit)/diff(k600.loess.fit$area_ha)),area_ha_centered=rowMeans(embed(k600.loess.fit$area_ha,2)))
#*Find the minimum slope (fastest rate of change), back transform to get into the same scale
  #warning: make sure you pick max or min depending on the shape of the curve (max if increasing, min if decreasing)
(k600.cutoff<-10^(fd.k600.loess%>%filter(d_k600_mday==max(d_k600_mday,na.rm=TRUE))%>%dplyr::select(area_ha_centered)%>%pull()))
#*Just to check, plot the first derivative####
plot(fd.k600.loess$d_k600_mday~fd.k600.loess$area_ha_centered)
#k600 loess: rsme####
(k600.loess.rsme<-sqrt(c(crossprod(log10(k600DF$k600_mday)-predict(k600.loess))/length(log10(k600DF$k600_mday)))))

#Fit a nonlinear logistic curve to the dtr data####
k600.logistic<-nls(log10(k600_mday)~A+(B-A)/(1+exp((xmid-log_area_ha)/scal)),
                  start=list(A=1,B=5,xmid=1,scal=-0.5),data=k600DF,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
summary(k600.logistic)
#Get the cutoff from the logistic parameter as the xmid inflection point
k600.logistic.cutoff<-10^k600.logistic$m$getAllPars()[3]

#flat model: AICc###
k600.flat.aicc<-AIC(k600.flat)+ (2 * 1^2 + 2 * 1)/(length(k600.flat$residuals) - 1 - 1)

#Linear model: AICc###
k600.lm.aicc<-AIC(k600.lm)+ (2 * 2^2 + 2 * 2)/(length(k600.flat$residuals) - 2 - 1)

#Segmented model: AICc####  
k600.segmented.aicc<-AIC(k600.segmented)+ (2 * 4^2 + 2 * 4)/(length(k600.flat$residuals) - 4 - 1)

#Logistic model: AICc#### 
k600.logistic.aicc<-k600.logistic.aicc<-AIC(k600.logistic)+ (2 * 4^2 + 2 * 4)/(length(k600.flat$residuals) - 4 - 1)

#Store aicc from each model as a row####
#Keep adding to this with each analysis
aicc.df<-bind_rows(aicc.df,tibble(type="k600",flat=k600.flat.aicc,linear=k600.lm.aicc,segmented=k600.segmented.aicc,logistic=k600.logistic.aicc))

#Store rsme from each model as a row####
#Keep adding to this with each analysis
rsme.df<-bind_rows(rsme.df,tibble(type="k600",flat=k600.flat.rsme,linear=k600.lm.rsme,segmented=k600.segmented.rsme,loess=k600.loess.rsme))


#Recreate woolway plot figure 
gg.k600.scatter<-ggplot(data=k600DF,aes(x=area_ha,y=k600_mday))+
  geom_vline(xintercept=k600.logistic.cutoff,color="black",linetype=1,size=1)+ #vertical line indicating the maximum first derivative from the loess model
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(0.001,550000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  #geom_smooth(se=FALSE, color="black")+
  geom_line(data=tibble(logistic_predict=predict(k600.logistic),log_area_ha=k600DF$log_area_ha), aes(y=10^logistic_predict,x=10^log_area_ha),color="black",size=1.2)+
  scale_y_log10()+
  labs(y=bquote(k[600]~(m~d^-1)),x=bquote(Surface~area~(ha)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
  )
#FIGURE: daily tempreature range scatter, print out as jpg####
ggsave("plots/WIAP-FigureX-k600Scatter.jpg", plot=gg.k600.scatter, width=3, height=3,units="in", dpi=300)

#k600 in the boxplot style like Merediths figure  
gg.k600.box<-ggplot(data=subset(k600DF,!is.na(area_ha)),aes(x=bins_km2,y=k600_mday))+
  geom_boxplot()+
  geom_jitter()+
  scale_x_discrete(labels=c(bquote("<"*10^-3),bquote(10^-3~-~10^-2),bquote(10^-2~-~10^-1),bquote(10^-1~-~10^0),bquote(10^0~-~10^1),bquote(10^1~-~10^2),bquote(">"*10^2)))+
  scale_y_log10()+
  labs(y=bquote(k[600]~(m~d^-1)),x=bquote(Surface~area~(km^2)))+
  #labels=bquote(.(parse(text=paste("10^",as.numeric(as.character(levels(DielTempRange_all$bins_km2),sep=""))))
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=-45,vjust=0.95,hjust=0))
#FIGURE: daily tempreature range scatter, print out as jpg####
ggsave("plots/WIAP-FigureX-k600Boxplot.jpg", plot=gg.k600.box, width=3, height=3,units="in", dpi=300)


#metabolism figures####
#Read in Hoellein et al. 2013 lit review csv
metabolismDF<-read_csv('RawData/DataCollatedFromOtherStudies/MetabolismLiteratureCompilation.csv')%>%
  add_row(lakeArea_ha=0.0001)%>% #put in fake data to ensure the box prints out empty in the boxplots
  mutate(area_ha=lakeArea_ha,area_m2=area_ha*10000,area_km2=area_ha/100)%>% #create some new variables
  mutate(bins_km2=floor(log10(area_km2)))%>% #create a factor for the boxplot
  mutate(bins_km2=factor(as.numeric(ifelse(bins_km2=="-5"|bins_km2=="-6","-4",ifelse(bins_km2=="3","2",bins_km2)))))%>%
  mutate(bins_km2=factor(bins_km2,levels=c("-4","-3","-2","-1","0","1","2")))%>%
  mutate(NEP_mmolperm3perday=GPP_mmolperm3perday-R_mmolperm3perday)%>%
  mutate(log_area_ha=log10(area_ha))
  #dplyr::select(DaysUsed,CR_mean_gPERm2PERday,CR_var_gPERm2PERday,CR_max_gPERm2PERday,GPP_mean_gPERm2PERday,GPP_var_gPERm2PERday,GPP_max_gPERm2PERday,NEM_mean_gPERm2PERday,NEM_var_gPERm2PERday,NEM_max_gPERm2PERday,area_ha,area_m2,area_km2,bins_km2)
  
  summary(metabolismDF$bins_km2) #check out the bins to make sure they are good
  summary(metabolismDF$area_ha) #Check out the range of the area
  summary(metabolismDF$area_ha)
  
  #NEP models####
  #Trim NEPi88 nmkm,= and order to only non NA values####
  NEP.trim<-metabolismDF%>%dplyr::select(NEP_mmolperm3perday,log_area_ha,area_ha)%>%arrange(log_area_ha)%>%filter(!is.na(NEP_mmolperm3perday))%>%filter(!is.na(area_ha))
  
  #NEP: Flat model####
  NEP.flat<-lm(NEP_mmolperm3perday~1,data=NEP.trim)
  summary(NEP.flat)
  #NEP flat: rsme####
  (NEP.flat.rsme<-sqrt(c(crossprod(NEP.flat$residuals))/length(NEP.flat$residuals)))
  
  #NEP: linear model####
  NEP.lm<-lm(NEP_mmolperm3perday~log_area_ha,data=NEP.trim)
  summary(NEP.lm)
  #NEP: rsme####
  (NEP.lm.rsme<-sqrt(c(crossprod(NEP.lm$residuals))/length(NEP.lm$residuals)))
  
  #*Segmented regression####
  #Try the segmented package version: https://rpubs.com/MarkusLoew/12164####
  NEP.segmented<-segmented(lm(NEP_mmolperm3perday~log_area_ha,data=NEP.trim),seg.z=log_area_ha,psi=c(3)) #initial guess makes a difference
  summary(NEP.segmented) 
  NEP.segmented$psi #get the breakpoints
  10^NEP.segmented$psi[,"Est."] #back calculate breakpoint - this one is at 0.35 and 11.2 ha
  #get the average of the middle segment
  10^mean(NEP.segmented$psi[,"Est."]) #This is a 2.00 ha mid point of the middle segment
  slope(NEP.segmented) #get the slopes  
  NEP.segmented.fitted<-fitted(NEP.segmented) #get the fitted data
  NEP.segmentedModel<-tibble(area_ha=10^NEP.trim$log_area_ha,log_area_ha=NEP.trim$log_area_ha,NEP.segmented.fitted=NEP.segmented.fitted) #calculate the fit with the back transformed x axis
  #NEP loess: rsme####
  (NEP.segmented.rsme<-sqrt(c(crossprod(NEP.trim$NEP_mmolperm3perday-NEP.segmentedModel$NEP.segmented.fitted)/length(NEP.trim$NEP_mmolperm3perday))))
  
  #*Create the loess model for NEP####
  NEP.loess<-loess(NEP_mmolperm3perday~log_area_ha,data=NEP.trim)
  #*find the x range###  
  NEP.xrange <- range(NEP.trim$area_ha)
  #*create a sequence for the fitted values, make sure to log transform here####
  NEP.xseq <- seq(from=log10(NEP.xrange[1]), to=log10(NEP.xrange[2]), length=200)
  #*Create data frame with the loess predicted - this is what is generated in the geom_smooth output, note they are log transformed predictions####
  NEP.loess.fit<-tibble(loess_fit=predict(NEP.loess,newdata=data.frame(log_area_ha=NEP.xseq)),log_area_ha=NEP.xseq)
  #*Calculate the first derivative of the loess modeling using the differencing method, centered the areas, also log transformed####
  fd.NEP.loess<-tibble(d_NEP_mmolperm3perday=as.numeric(diff(NEP.loess.fit$loess_fit)/diff(NEP.loess.fit$log_area_ha)),area_ha_centered=rowMeans(embed(NEP.loess.fit$log_area_ha,2)))
  #*Find the minimum slope (fastest rate of change), back transform to get into the same scale and print it####
  (NEP.cutoff<-10^(fd.NEP.loess%>%filter(d_NEP_mmolperm3perday==max(d_NEP_mmolperm3perday,na.rm=TRUE))%>%dplyr::select(area_ha_centered)%>%pull()))
  #*Just to check, plot the first derivative####
  plot(fd.NEP.loess$d_NEP_mmolperm3perday~fd.NEP.loess$area_ha_centered)
  #NEP loess: rsme####
  (NEP.loess.rsme<-sqrt(c(crossprod(NEP.trim$NEP_mmolperm3perday-predict(NEP.loess))/length(NEP.trim$NEP_mmolperm3perday))))
  
  #Fit a nonlinear logistic curve to the NEP data####
  NEP.logistic<-nls(NEP_mmolperm3perday~A+(B-A)/(1+exp((xmid-log_area_ha)/scal)),
                    start=list(A=-200,B=50,xmid=1,scal=1),data=NEP.trim,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
  summary(NEP.logistic)
  #Get the cutoff from the logistic parameter as the xmid inflection point
  NEP.logistic.cutoff<-10^NEP.logistic$m$getAllPars()[3]
  ####NEP HAD NO CONVERGENCE - MODEL DOES NOT WORK####
  
  #flat model: AICc###
  NEP.flat.aicc<-AIC(NEP.flat)+ (2 * 1^2 + 2 * 1)/(length(NEP.flat$residuals) - 1 - 1)
  
  #Linear model: AICc###
  NEP.lm.aicc<-AIC(NEP.lm)+ (2 * 2^2 + 2 * 2)/(length(NEP.flat$residuals) - 2 - 1)
  
  #Segmented model: AICc####  
  NEP.segmented.aicc<-AIC(NEP.segmented)+ (2 * 4^2 + 2 * 4)/(length(NEP.flat$residuals) - 4 - 1)
  
  #Logistic model: AICc#### 
  NEP.logistic.aicc<-AIC(NEP.logistic)+ (2 * 4^2 + 2 * 4)/(length(NEP.flat$residuals) - 4 - 1)
  
  
  #Store aicc from each model as a row####
  #Keep adding to this with each analysis
  aicc.df<-bind_rows(aicc.df,tibble(type="NEP",flat=NEP.flat.aicc,linear=NEP.lm.aicc,segmented=NEP.segmented.aicc,logistic=NEP.logistic.aicc))
  
  
  #Store rsme from each model as a column####
  #Keep adding to this with each analysis
  rsme.df<-bind_rows(rsme.df,tibble(type="NEP",flat=NEP.flat.rsme,linear=NEP.lm.rsme,segmented=NEP.segmented.rsme,loess=NEP.loess.rsme))
  
  #Plot each of the models overlaid
  ggplot(data=NEP.trim,aes(y=NEP_mmolperm3perday, x=log_area_ha))+geom_point()+
    geom_hline(yintercept=mean(NEP.trim$NEP_mmolperm3perday,na.rm=TRUE),color="orange")+
    geom_smooth(method='lm', se=FALSE,color="red")+
    geom_line(data=NEP.segmentedModel,aes(y=NEP.segmented.fitted,x=log_area_ha),color="blue")+ #This is the segmented regression
    geom_smooth(se=FALSE,color="green")+ #loess
    geom_line(data=tibble(logistic_predict=predict(NEP.logistic),log_area_ha=NEP.trim$log_area_ha), aes(y=logistic_predict,x=log_area_ha),color="purple")+
    geom_text(data=data.frame(rsme=round(NEP.flat.rsme,3)), aes(label = paste("flat=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 1)+
    geom_text(data=data.frame(rsme=round(NEP.lm.rsme,3)), aes(label = paste("linear=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 3)+
    geom_text(data=data.frame(rsme=round(NEP.segmented.rsme,3)), aes(label = paste("segmented=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 5)+
    geom_text(data=data.frame(rsme=round(NEP.loess.rsme,3)), aes(label = paste("loess=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 7)+
    theme_bw()
  
  #GPP: examine linear vs. segmented regressions####
  #Trim GPP and order to only non NA values####
  GPP.trim<-metabolismDF%>%dplyr::select(GPP_mmolperm3perday,log_area_ha,area_ha)%>%arrange(log_area_ha)%>%filter(!is.na(GPP_mmolperm3perday))
  
  #GPP: Flat model####
  GPP.flat<-lm(GPP_mmolperm3perday~1,data=GPP.trim)
  summary(GPP.flat)
  #GPP flat: rsme####
  (GPP.flat.rsme<-sqrt(c(crossprod(GPP.flat$residuals))/length(GPP.flat$residuals)))
  
  #GPP: linear model####
  GPP.lm<-lm(GPP_mmolperm3perday~log_area_ha,data=GPP.trim)
  summary(GPP.lm)
  #GPP: rsme####
  (GPP.lm.rsme<-sqrt(c(crossprod(GPP.lm$residuals))/length(GPP.lm$residuals)))
  
  #*Segmented regression####
  #Try the segmented package version: https://rpubs.com/MarkusLoew/12164####
  GPP.segmented<-segmented(lm(GPP_mmolperm3perday~log_area_ha,data=GPP.trim),seg.z=log_area_ha,psi=c(3)) #initial guess makes a difference
  summary(GPP.segmented) 
  GPP.segmented$psi #get the breakpoints
  10^GPP.segmented$psi[,"Est."] #back calculate breakpoint - this one is at 0.35 and 11.2 ha
  #get the average of the middle segment
  10^mean(GPP.segmented$psi[,"Est."]) #This is a 2.00 ha mid point of the middle segment
  slope(GPP.segmented) #get the slopes  
  GPP.segmented.fitted<-fitted(GPP.segmented) #get the fitted data
  GPP.segmentedModel<-tibble(area_ha=10^GPP.trim$log_area_ha,log_area_ha=GPP.trim$log_area_ha,GPP.segmented.fitted=GPP.segmented.fitted) #calculate the fit with the back transformed x axis
  #GPP loess: rsme####
  (GPP.segmented.rsme<-sqrt(c(crossprod(GPP.trim$GPP_mmolperm3perday-GPP.segmentedModel$GPP.segmented.fitted)/length(GPP.trim$GPP_mmolperm3perday))))
  
  #*Create the loess model for GPP####
  GPP.loess<-loess(GPP_mmolperm3perday~log_area_ha,data=GPP.trim)
  #*find the x range###  
  GPP.xrange <- range(GPP.trim$area_ha)
  #*create a sequence for the fitted values, make sure to log transform here####
  GPP.xseq <- seq(from=log10(GPP.xrange[1]), to=log10(GPP.xrange[2]), length=200)
  #*Create data frame with the loess predicted - this is what is generated in the geom_smooth output, note they are log transformed predictions####
  GPP.loess.fit<-tibble(loess_fit=predict(GPP.loess,newdata=data.frame(log_area_ha=GPP.xseq)),log_area_ha=GPP.xseq)
  #*Calculate the first derivative of the loess modeling using the differencing method, centered the areas, also log transformed####
  fd.GPP.loess<-tibble(d_GPP_mmolperm3perday=as.numeric(diff(GPP.loess.fit$loess_fit)/diff(GPP.loess.fit$log_area_ha)),area_ha_centered=rowMeans(embed(GPP.loess.fit$log_area_ha,2)))
  #*Find the minimum slope (fastest rate of change), back transform to get into the same scale and print it####
  (GPP.cutoff<-10^(fd.GPP.loess%>%filter(d_GPP_mmolperm3perday==max(d_GPP_mmolperm3perday,na.rm=TRUE))%>%dplyr::select(area_ha_centered)%>%pull()))
  #*Just to check, plot the first derivative####
  plot(fd.GPP.loess$d_GPP_mmolperm3perday~fd.GPP.loess$area_ha_centered)
  #GPP loess: rsme####
  (GPP.loess.rsme<-sqrt(c(crossprod(GPP.trim$GPP_mmolperm3perday-predict(GPP.loess))/length(GPP.trim$GPP_mmolperm3perday))))
  
  #Fit a nonlinear logistic curve to the GPP data####
  GPP.logistic<-nls(GPP_mmolperm3perday~A+(B-A)/(1+exp((xmid-log_area_ha)/scal)),
                    start=list(A=0,B=150,xmid=2,scal=-0.5),data=GPP.trim,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
  summary(GPP.logistic)
  #Get the cutoff from the logistic parameter as the xmid inflection point
  GPP.logistic.cutoff<-10^GPP.logistic$m$getAllPars()[3]
  
  
  #flat model: AICc###
  GPP.flat.aicc<-AIC(GPP.flat)+ (2 * 1^2 + 2 * 1)/(length(GPP.flat$residuals) - 1 - 1)
  
  #Linear model: AICc###
  GPP.lm.aicc<-AIC(GPP.lm)+ (2 * 2^2 + 2 * 2)/(length(GPP.flat$residuals) - 2 - 1)
  
  #Segmented model: AICc####  
  GPP.segmented.aicc<-AIC(GPP.segmented)+ (2 * 4^2 + 2 * 4)/(length(GPP.flat$residuals) - 4 - 1)
  
  #Logistic model: AICc#### 
  GPP.logistic.aicc<-GPP.logistic.aicc<-AIC(GPP.logistic)+ (2 * 4^2 + 2 * 4)/(length(GPP.flat$residuals) - 4 - 1)
  
  
  #Store aicc from each model as a row####
  #Keep adding to this with each analysis
  aicc.df<-bind_rows(aicc.df,tibble(type="GPP",flat=GPP.flat.aicc,linear=GPP.lm.aicc,segmented=GPP.segmented.aicc,logistic=GPP.logistic.aicc))
  
  
  #Store rsme from each model as a column####
  #Keep adding to this with each analysis
  rsme.df<-bind_rows(rsme.df,tibble(type="GPP",flat=GPP.flat.rsme,linear=GPP.lm.rsme,segmented=GPP.segmented.rsme,loess=GPP.loess.rsme))
  
  #Plot each of the models overlaid
  ggplot(data=GPP.trim,aes(y=GPP_mmolperm3perday, x=log_area_ha))+geom_point()+
    geom_hline(yintercept=mean(GPP.trim$GPP_mmolperm3perday,na.rm=TRUE),color="orange")+
    geom_smooth(method='lm', se=FALSE,color="red")+
    geom_line(data=GPP.segmentedModel,aes(y=GPP.segmented.fitted,x=log_area_ha),color="blue")+ #This is the segmented regression
    geom_smooth(se=FALSE,color="green")+ #loess
    geom_line(data=tibble(logistic_predict=predict(GPP.logistic),log_area_ha=GPP.trim$log_area_ha), aes(y=logistic_predict,x=log_area_ha),color="purple")+
    geom_text(data=data.frame(rsme=round(GPP.flat.rsme,3)), aes(label = paste("flat=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 1)+
    geom_text(data=data.frame(rsme=round(GPP.lm.rsme,3)), aes(label = paste("linear=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 3)+
    geom_text(data=data.frame(rsme=round(GPP.segmented.rsme,3)), aes(label = paste("segmented=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 5)+
    geom_text(data=data.frame(rsme=round(GPP.loess.rsme,3)), aes(label = paste("loess=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 7)+
    theme_bw()
  
  #R: examine linear vs. segmented regressions####
  #Trim R and order to only non NA values####
  R.trim<-metabolismDF%>%dplyr::select(R_mmolperm3perday,log_area_ha,area_ha)%>%arrange(log_area_ha)%>%filter(!is.na(R_mmolperm3perday))
  
  #R: Flat model####
  R.flat<-lm(R_mmolperm3perday~1,data=R.trim)
  summary(R.flat)
  #R flat: rsme####
  (R.flat.rsme<-sqrt(c(crossprod(R.flat$residuals))/length(R.flat$residuals)))
  
  #R: linear model####
  R.lm<-lm(R_mmolperm3perday~log_area_ha,data=R.trim)
  summary(R.lm)
  #R: rsme####
  (R.lm.rsme<-sqrt(c(crossprod(R.lm$residuals))/length(R.lm$residuals)))
  
  #*Segmented regression####
  #Try the segmented package version: https://rpubs.com/MarkusLoew/12164####
  R.segmented<-segmented(lm(R_mmolperm3perday~log_area_ha,data=R.trim),seg.z=log_area_ha,psi=c(3)) #initial guess makes a difference
  summary(R.segmented) 
  R.segmented$psi #get the breakpoints
  10^R.segmented$psi[,"Est."] #back calculate breakpoint - this one is at 0.35 and 11.2 ha
  #get the average of the middle segment
  10^mean(R.segmented$psi[,"Est."]) #This is a 2.00 ha mid point of the middle segment
  slope(R.segmented) #get the slopes  
  R.segmented.fitted<-fitted(R.segmented) #get the fitted data
  R.segmentedModel<-tibble(area_ha=10^R.trim$log_area_ha,log_area_ha=R.trim$log_area_ha,R.segmented.fitted=R.segmented.fitted) #calculate the fit with the back transformed x axis
  #R loess: rsme####
  (R.segmented.rsme<-sqrt(c(crossprod(R.trim$R_mmolperm3perday-R.segmentedModel$R.segmented.fitted)/length(R.trim$R_mmolperm3perday))))
  
  #*Create the loess model for R####
  R.loess<-loess(R_mmolperm3perday~log_area_ha,data=R.trim)
  #*find the x range###  
  R.xrange <- range(R.trim$area_ha)
  #*create a sequence for the fitted values, make sure to log transform here####
  R.xseq <- seq(from=log10(R.xrange[1]), to=log10(R.xrange[2]), length=200)
  #*Create data frame with the loess predicted - this is what is generated in the geom_smooth output, note they are log transformed predictions####
  R.loess.fit<-tibble(loess_fit=predict(R.loess,newdata=data.frame(log_area_ha=R.xseq)),log_area_ha=R.xseq)
  #*Calculate the first derivative of the loess modeling using the differencing method, centered the areas, also log transformed####
  fd.R.loess<-tibble(d_R_mmolperm3perday=as.numeric(diff(R.loess.fit$loess_fit)/diff(R.loess.fit$log_area_ha)),area_ha_centered=rowMeans(embed(R.loess.fit$log_area_ha,2)))
  #*Find the minimum slope (fastest rate of change), back transform to get into the same scale and print it####
  (R.cutoff<-10^(fd.R.loess%>%filter(d_R_mmolperm3perday==max(d_R_mmolperm3perday,na.rm=TRUE))%>%dplyr::select(area_ha_centered)%>%pull()))
  #*Just to check, plot the first derivative####
  plot(fd.R.loess$d_R_mmolperm3perday~fd.R.loess$area_ha_centered)
  #R loess: rsme####
  (R.loess.rsme<-sqrt(c(crossprod(R.trim$R_mmolperm3perday-predict(R.loess))/length(R.trim$R_mmolperm3perday))))
  
  #Fit a nonlinear logistic curve to the R data####
  R.logistic<-nls(R_mmolperm3perday~A+(B-A)/(1+exp((xmid-log_area_ha)/scal)),
                    start=list(A=50,B=250,xmid=1,scal=-0.5),data=R.trim,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
  summary(R.logistic)
  #Get the cutoff from the logistic parameter as the xmid inflection point
  R.logistic.cutoff<-10^R.logistic$m$getAllPars()[3]
  
  
  #flat model: AICc###
  R.flat.aicc<-AIC(R.flat)+ (2 * 1^2 + 2 * 1)/(length(R.flat$residuals) - 1 - 1)
  
  #Linear model: AICc###
  R.lm.aicc<-AIC(R.lm)+ (2 * 2^2 + 2 * 2)/(length(R.flat$residuals) - 2 - 1)
  
  #Segmented model: AICc####  
  R.segmented.aicc<-AIC(R.segmented)+ (2 * 4^2 + 2 * 4)/(length(R.flat$residuals) - 4 - 1)
  
  #Logistic model: AICc#### 
  R.logistic.aicc<-R.logistic.aicc<-AIC(R.logistic)+ (2 * 4^2 + 2 * 4)/(length(R.flat$residuals) - 4 - 1)
  
  
  #Store aicc from each model as a row####
  #Keep adding to this with each analysis
  aicc.df<-bind_rows(aicc.df,tibble(type="R",flat=R.flat.aicc,linear=R.lm.aicc,segmented=R.segmented.aicc,logistic=R.logistic.aicc))
  
  
  #Store rsme from each model as a column####
  #Keep adding to this with each analysis
  rsme.df<-bind_rows(rsme.df,tibble(type="R",flat=R.flat.rsme,linear=R.lm.rsme,segmented=R.segmented.rsme,loess=R.loess.rsme))
  
  #Plot each of the models overlaid
  ggplot(data=R.trim,aes(y=R_mmolperm3perday, x=log_area_ha))+geom_point()+
    geom_hline(yintercept=mean(R.trim$R_mmolperm3perday,na.rm=TRUE),color="orange")+
    geom_smooth(method='lm', se=FALSE,color="red")+
    geom_line(data=R.segmentedModel,aes(y=R.segmented.fitted,x=log_area_ha),color="blue")+ #This is the segmented regression
    geom_smooth(se=FALSE,color="green")+ #loess
    geom_line(data=tibble(logistic_predict=predict(R.logistic),log_area_ha=R.trim$log_area_ha), aes(y=logistic_predict,x=log_area_ha),color="purple")+
    geom_text(data=data.frame(rsme=round(R.flat.rsme,3)), aes(label = paste("flat=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 1)+
    geom_text(data=data.frame(rsme=round(R.lm.rsme,3)), aes(label = paste("linear=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 3)+
    geom_text(data=data.frame(rsme=round(R.segmented.rsme,3)), aes(label = paste("segmented=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 5)+
    geom_text(data=data.frame(rsme=round(R.loess.rsme,3)), aes(label = paste("loess=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 7)+
    theme_bw()

    #Removed strucchange code
  # #Structural changes search with linear slopes
  # NEP_brk <- breakpoints(NEP.trim$NEP_mmolperm3perday ~ NEP.trim$log_area_ha, h = 20)
  # summary(NEP_brk) #optimal at 0.8 or 6.3 ha
  # 
  # NEP_brk.struc <- breakpoints(temp$NEP_mmolperm3perday ~ temp$log_area_ha, h = 10)
  # summary(NEP_brk.struc)
  # #Plot the breakpoint analysis
  # plot(NEP_brk.struc)
  # #Find the minimum BIC number of breakpoints
  # plot(temp$NEP_mmolperm3perday ~ temp$log_area_ha)
  # #Plots breaks even if the minimum BIC is 0 breaks
  # lines(fitted(NEP_brk.struc, breaks = ifelse(is.na(NEP_brk.struc$breakpoints),0,length(NEP_brk.struc$breakpoints)))~temp$log_area_ha, col = 4)
  # 
  #   #gives the minimum BIC number of breakpoint locations and years, will give back NA if the minimum is 0
  # NEP_brk.struc$breakpoints
  # 10^temp$log_area_ha[NEP_brk.struc$breakpoints]
  # 

 
#GPP figures####
  #Recreate woolway plot figure 
  gg.metabolismGPP.scatter<-ggplot(data=metabolismDF,aes(x=area_ha,y=GPP_mmolperm3perday))+
    geom_point(color="dark grey")+
    scale_x_continuous(limits=c(0.001,550000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
    #geom_smooth(method="lm",se=FALSE,color="black",size=1.2)+
    geom_hline(yintercept = GPP.flat$coefficients,color="black",size=1.2)+
    #scale_y_log10()+
    labs(y=bquote(GPP~(mmol~O[2]~m^-3~d^-1)),x=bquote(Surface~area~(ha)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          #legend.position = c(0.8, 0.8),
          #legend.title = element_text(size=8),
          #legend.text = element_text(size=10)
    )
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-metabolismGPPScatter.jpg", plot=gg.metabolismGPP.scatter, width=3, height=3,units="in", dpi=300)
  
  #GPP in boxplot form a la Meredith k600 graph###
  gg.metabolismGPP.box<-ggplot(data=subset(metabolismDF,!is.na(area_ha)),aes(x=bins_km2,y=GPP_mmolperm3perday))+
    geom_boxplot()+
    geom_jitter()+
    scale_x_discrete(labels=c(bquote("<"*10^-3),bquote(10^-3~-~10^-2),bquote(10^-2~-~10^-1),bquote(10^-1~-~10^0),bquote(10^0~-~10^1),bquote(10^1~-~10^2),bquote(">"*10^2)))+
    #scale_y_log10()+
    labs(y=bquote(GPP~(mmol~O[2]~m^-3~day^-1)),x=bquote(Surface~area~(km^2)))+
    #labels=bquote(.(parse(text=paste("10^",as.numeric(as.character(levels(DielTempRange_all$bins_km2),sep=""))))
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(angle=-45,vjust=0.95,hjust=0))
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-metabolismGPPBoxplot.jpg", plot=gg.metabolismGPP.box, width=3, height=3,units="in", dpi=300)

  #R figures####
  #Recreate woolway plot figure 
  gg.metabolismR.scatter<-ggplot(data=metabolismDF,aes(x=area_ha,y=R_mmolperm3perday))+
    geom_vline(xintercept=10^R.segmented$psi[,"Est."],color="black",size=1)+
    geom_point(color="dark grey")+
    scale_x_continuous(limits=c(0.001,550000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
    geom_line(data=R.segmentedModel, aes(x = area_ha, y = R.segmented.fitted),color="black",size=1.2)+
    #scale_y_log10()+
    labs(y=bquote(R~(mmol~O[2]~m^-3~d^-1)),x=bquote(Surface~area~(ha)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          #legend.position = c(0.8, 0.8),
          #legend.title = element_text(size=8),
          #legend.text = element_text(size=10)
    )
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-metabolismRScatter.jpg", plot=gg.metabolismR.scatter, width=3, height=3,units="in", dpi=300)
  
  #R in boxplot form a la Meredith k600 graph###
  gg.metabolismR.box<-ggplot(data=subset(metabolismDF,!is.na(area_ha)),aes(x=bins_km2,y=R_mmolperm3perday))+
    geom_boxplot()+
    geom_jitter()+
    scale_x_discrete(labels=c(bquote("<"*10^-3),bquote(10^-3~-~10^-2),bquote(10^-2~-~10^-1),bquote(10^-1~-~10^0),bquote(10^0~-~10^1),bquote(10^1~-~10^2),bquote(">"*10^2)))+
    #scale_y_log10()+
    labs(y=bquote(R~(mmol~O[2]~m^-3~day^-1)),x=bquote(Surface~area~(km^2)))+
    #labels=bquote(.(parse(text=paste("10^",as.numeric(as.character(levels(DielTempRange_all$bins_km2),sep=""))))
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(angle=-45,vjust=0.95,hjust=0))
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-metabolismRBoxplot.jpg", plot=gg.metabolismR.box, width=3, height=3,units="in", dpi=300)
  
  #NEP figures####
  #Recreate woolway plot figure 
  gg.metabolismNEP.scatter<-ggplot(data=metabolismDF,aes(x=area_ha,y=NEP_mmolperm3perday))+
    geom_vline(xintercept=10^NEP.segmented$psi[,"Est."],color="black",size=1)+
    geom_point(color="dark grey")+
    scale_x_continuous(limits=c(0.001,550000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
    #geom_smooth(se=FALSE,color="black",size=1.2)+
    geom_line(data=NEP.segmentedModel, aes(x = area_ha, y = NEP.segmented.fitted),color="black",size=1.2)+
    #scale_y_log10()+
    labs(y=bquote(NEP~(mmol~O[2]~m^-3~d^-1)),x=bquote(Surface~area~(ha)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          #legend.position = c(0.8, 0.8),
          #legend.title = element_text(size=8),
          #legend.text = element_text(size=10)
    )
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-metabolismNEPScatter.jpg", plot=gg.metabolismNEP.scatter, width=3, height=3,units="in", dpi=300)
  
  #NEP in boxplot form a la Meredith k600 graph###
  gg.metabolismNEP.box<-ggplot(data=subset(metabolismDF,!is.na(area_ha)),aes(x=bins_km2,y=NEP_mmolperm3perday))+
    geom_boxplot()+
    geom_jitter()+
    scale_x_discrete(labels=c(bquote("<"*10^-3),bquote(10^-3~-~10^-2),bquote(10^-2~-~10^-1),bquote(10^-1~-~10^0),bquote(10^0~-~10^1),bquote(10^1~-~10^2),bquote(">"*10^2)))+
    #scale_y_log10()+
    labs(y=bquote(NEP~(mmol~O[2]~m^-3~day^-1)),x=bquote(Surface~area~(km^2)))+
    #labels=bquote(.(parse(text=paste("10^",as.numeric(as.character(levels(DielTempRange_all$bins_km2),sep=""))))
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(angle=-45,vjust=0.95,hjust=0))
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-metabolismNEPBoxplot.jpg", plot=gg.metabolismNEP.box, width=3, height=3,units="in", dpi=300)
  
  
  #Pond/Lake/Wetland from our literature survey####
  #*load cleaned pond data ####
  singleponds <- read.csv("Data_cleaned/Singleponds_Cleaned.csv")
  
  #remove out large/deep lakes. n=1364
  singleponds <- singleponds %>% 
    filter((max_depth_m<=9 | is.na(max_depth_m)==T) &
             (mean_depth_m<=9 | is.na(mean_depth_m)==T) &
             (max_surfacearea_m2<=200000 | is.na(max_surfacearea_m2)==T) &
             (mean_surfacearea_m2<=200000 | is.na(mean_surfacearea_m2)==T))
  singleponds$type<-"pond"
  
  #*load in public lake data ####
  lagos<- read.csv("RawData/Lakes/LAGOS.csv")
  nla<- read.csv("RawData/Lakes/NLA.csv")
  waterbase<- read.csv("RawData/Lakes/Waterbase.csv")
  hydrolakes <- read.csv("RawData/Lakes/Public_data/Hydrolakes.csv") %>% # only has area and depth 
    rename('Latitude..decimal.degree.'='Latitude..decimal.degrees.', 'Longitude..decimal.degree.'='Longitude..decimal.degrees.')
  all_lakes<-gtools::smartbind(lagos, nla, hydrolakes, waterbase)
  all_lakes$type<-"lake"
  
  #*load in wetlands data ####
  wetland_siteinfo<- read.csv("RawData/Wetlands/Wetlands_siteinfo2.csv")  %>%
    dplyr::select(UID, SITE_USE, AA_CENTER_LAT, AA_CENTER_LON)
  wetland_surfacewater <- read.csv("RawData/Wetlands/Wetlands_watercharacteristic.csv") #depth (cm)
  wetland_chem <- read.csv("RawData/Wetlands/Wetlands_waterchem.csv") %>%
    dplyr::select('UID', 'COND', 'NH3', 'NO3NO2', 'PH', 'TKN', 'TN', 'TP', 'CONDUCTIVITY_FIELD', 'PH_FIELD') #TP (ug/L), TN (mg/L)
  wetland_chla <- read.csv("RawData/Wetlands/Wetlands_chla.csv") %>%
    dplyr::select(UID, CHLA) #Chla (ug/L)
  wetland_veg<-read.csv("RawData/Wetlands/Wetlands_vegtype.csv", header = TRUE) %>%
    dplyr::select("UID", "WATER_AQVEG", "WATER_EMERGVEG")
  #group by UID and average at the site for each vegetation type (all are % covers)
  wetland_veg_mean<-wetland_veg%>%
    group_by(UID)%>%
    summarise(mean_AQVEG = mean(WATER_AQVEG, na.rm=TRUE), 
              mean_EMERGVEG = mean(WATER_EMERGVEG, na.rm=TRUE))
  
  wetland_veg_mean$macrophytes_percentcover<-wetland_veg_mean$mean_EMERGVEG + wetland_veg_mean$mean_AQVEG  #add up the submerged, floating, and emergent veg
  wetland_veg_mean$macrophytes_percentcover[wetland_veg_mean$macrophytes_percentcover > 100] <- 100  #can't have over 100% coverage
  wetland_veg_mean<- dplyr::select(wetland_veg_mean, UID, macrophytes_percentcover)
  #join tables  
  merged_WL<-left_join(wetland_siteinfo, wetland_surfacewater) %>%
    left_join(wetland_chem) %>%
    left_join(wetland_chla) %>%
    left_join(wetland_veg_mean) 
  
  #select wetlands that were not re-sampled, that are freshwater, and that have enough water for a water chem sample  
  WL_singlevisit<-filter(merged_WL, SITE_USE != "NWCA_REVISITS" )
  WL_fresh<-filter(WL_singlevisit, WATER_SALINITY == "FRESH") #458 sites 
  wetland_dat<-WL_fresh[!(is.na(WL_fresh$TP)),] # 400 sites 
  wetland_dat$type<-"wetland"
  wetland_dat$max_depth_m<-wetland_dat$SW_DEPTH/100  #convert cm to m 
  wetland_dat$tn_ugpl<-wetland_dat$TN * 1000  #convert mg/L to ug/L
  
  ### select out comparable variables to compare all 3 "types" 
  ponds<-dplyr::select(singleponds, pondname, max_depth_m, mean_depth_m, max_surfacearea_m2, mean_surfacearea_m2, ph, doc_mgpl, chla_ugpl, tp_ugpl, tn_ugpl, cond_uspcm, macrophytes_percentcover, type) %>%
    rename(siteid = pondname)
  wetlands<-dplyr::select(wetland_dat, UID, max_depth_m, COND, PH, tn_ugpl, TP, CHLA, macrophytes_percentcover, type ) %>%
    rename(siteid=UID, cond_uspcm=COND, ph=PH, tp_ugpl=TP, chla_ugpl= CHLA)
  lakes<-dplyr::select(all_lakes, PondName, Max.depth_m, Mean.depth_m, Max.surface.area_m2, Mean.surface.area_m2, pH, DOC_mgpL, Chla_ugpL, TP_ugpL, TN_ugpL, Cond_uSpcm, Macrophytes_percentcover, type) %>%
    rename(siteid = PondName, max_depth_m= Max.depth_m, mean_depth_m=Mean.depth_m, max_surfacearea_m2=Max.surface.area_m2, mean_surfacearea_m2=Mean.surface.area_m2, ph=pH, doc_mgpl = DOC_mgpL, chla_ugpl= Chla_ugpL, tp_ugpl= TP_ugpL, tn_ugpl=TN_ugpL, cond_uspcm=Cond_uSpcm, macrophytes_percentcover=Macrophytes_percentcover)
  
  all_ecos<-gtools::smartbind(ponds, lakes)
  
  #change char to factors and numeric 
  all_ecos$type<-as.factor(all_ecos$type)
  all_ecos$siteid<-as.factor(all_ecos$siteid)
  all_ecos<-all_ecos %>%
    mutate_if(is.character,as.numeric) 
  
  #*merge mean and max surface area, and take max if there are both ####
  all_ecos <- all_ecos %>%
    mutate(
      suface_area = case_when(
        is.na(all_ecos$max_surfacearea_m2) ~ all_ecos$mean_surfacearea_m2, #if ~ then
        is.na(all_ecos$mean_surfacearea_m2) ~ all_ecos$max_surfacearea_m2, #if ~ then 
        TRUE      ~ all_ecos$max_surfacearea_m2))  ### the else part 
  
  #create a new colomn for hectares 
  all_ecos$surfacearea_ha<-all_ecos$suface_area/10000
  
  #Modify the dataframe to include area, molar ratios, and bins for size####
  crossSystemDF<-all_ecos%>%
            mutate(area_ha=surfacearea_ha,area_m2=area_ha*10000,area_km2=area_ha/100,log_area_ha=log10(area_ha))%>%
            mutate(TNtoTPmolarratio=(tn_ugpl/(28.014*1000))/(tp_ugpl/(123.88*1000)))%>% #caclulate molar ratios
            mutate(bins_km2=floor(log10(area_km2)))%>% #create a factor for the boxplot
            mutate(bins_km2=factor(as.numeric(ifelse(bins_km2=="-5"|bins_km2=="-6"|bins_km2=="-7","-4",ifelse(bins_km2=="3"|bins_km2=="4"|bins_km2=="5","2",bins_km2)))))%>%
            mutate(bins_km2=factor(bins_km2,levels=c("-4","-3","-2","-1","0","1","2")))
  
  #TP: examine linear vs. segmented regressions####
  #Trim R and order to only non NA values####
  TP.trim<-crossSystemDF%>%dplyr::select(tp_ugpl,log_area_ha,area_ha)%>%arrange(log_area_ha)%>%filter(!is.na(tp_ugpl))%>%filter(!is.na(log_area_ha))%>%filter(tp_ugpl>0)%>%mutate(log_tp_ugpl=log10(tp_ugpl))
  
  #TP: Flat model####
  TP.flat<-lm(TP.trim$log_tp_ugpl~1,data=TP.trim)
  summary(TP.flat)
  #TP flat: rsme####
  (TP.flat.rsme<-sqrt(c(crossprod(TP.flat$residuals))/length(TP.flat$residuals)))
  
  #TP: linear model####
  TP.lm<-lm(TP.trim$log_tp_ugpl~log_area_ha,data=TP.trim)
  summary(TP.lm)
  #TP: rsme####
  (TP.lm.rsme<-sqrt(c(crossprod(TP.lm$residuals))/length(TP.lm$residuals)))
  
  #*Segmented regression####
  #Try the segmented package version: https://rpubs.com/MarkusLoew/12164####
  TP.segmented<-segmented(lm(log_tp_ugpl~log_area_ha,data=TP.trim),seg.z=log_area_ha,psi=c(3)) #initial guess makes a difference
  summary(TP.segmented) 
  TP.segmented$psi #get the breakpoints
  10^TP.segmented$psi[,"Est."] #back calculate breakpoint - this one is at 0.35 and 11.2 ha
  #get the average of the middle segment
  10^mean(TP.segmented$psi[,"Est."]) #This is a 2.00 ha mid point of the middle segment
  slope(TP.segmented) #get the slopes  
  TP.segmented.fitted<-fitted(TP.segmented) #get the fitted data
  TP.segmentedModel<-tibble(area_ha=10^TP.trim$log_area_ha,log_area_ha=TP.trim$log_area_ha,TP.segmented.fitted=TP.segmented.fitted) #calculate the fit with the back transformed x axis
  #TP loess: rsme####
  (TP.segmented.rsme<-sqrt(c(crossprod(TP.trim$log_tp_ugpl-TP.segmentedModel$TP.segmented.fitted)/length(TP.trim$log_tp_ugpl))))
  
  #*Create the loess model for TP####
  TP.loess<-loess(log_tp_ugpl~log_area_ha,data=TP.trim)
  #*find the x range###  
  TP.xrange <- range(TP.trim$area_ha)
  #*create a sequence for the fitted values, make sure to log transform here####
  TP.xseq <- seq(from=log10(TP.xrange[1]), to=log10(TP.xrange[2]), length=200)
  #*Create data frame with the loess predicted - this is what is generated in the geom_smooth output, note they are log transformed predictions####
  TP.loess.fit<-tibble(loess_fit=predict(TP.loess,newdata=data.frame(log_area_ha=TP.xseq)),log_area_ha=TP.xseq)
  #*Calculate the first derivative of the loess modeling using the differencing method, centered the areas, also log transformed####
  fd.TP.loess<-tibble(d_TP_log_tp_ugpl=as.numeric(diff(TP.loess.fit$loess_fit)/diff(TP.loess.fit$log_area_ha)),area_ha_centered=rowMeans(embed(TP.loess.fit$log_area_ha,2)))
  #*Find the minimum slope (fastest rate of change), back transform to get into the same scale and print it####
  (TP.cutoff<-10^(fd.TP.loess%>%filter(d_TP_log_tp_ugpl==min(d_TP_log_tp_ugpl,na.rm=TRUE))%>%dplyr::select(area_ha_centered)%>%pull()))
  #*Just to check, plot the first derivative####
  plot(fd.TP.loess$d_TP_log_tp_ugpl~fd.TP.loess$area_ha_centered)
  #TP loess: rsme####
  (TP.loess.rsme<-sqrt(c(crossprod(TP.trim$log_tp_ugpl-predict(TP.loess))/length(TP.trim$log_tp_ugpl))))
  
  #Fit a nonlinear logistic curve to the TP data####
  TP.logistic<-nls(log_tp_ugpl~A+(B-A)/(1+exp((xmid-log_area_ha)/scal)),
                    start=list(A=0.5,B=3,xmid=1,scal=-0.5),data=TP.trim,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
  summary(TP.logistic)
  #Get the cutoff from the logistic parameter as the xmid inflection point
  TP.logistic.cutoff<-10^TP.logistic$m$getAllPars()[3]
  
  
  #flat model: AICc###
  TP.flat.aicc<-AIC(TP.flat)+ (2 * 1^2 + 2 * 1)/(length(TP.flat$residuals) - 1 - 1)
  
  #Linear model: AICc###
  TP.lm.aicc<-AIC(TP.lm)+ (2 * 2^2 + 2 * 2)/(length(TP.flat$residuals) - 2 - 1)
  
  #Segmented model: AICc####  
  TP.segmented.aicc<-AIC(TP.segmented)+ (2 * 4^2 + 2 * 4)/(length(TP.flat$residuals) - 4 - 1)
  
  #Logistic model: AICc#### 
  TP.logistic.aicc<-TP.logistic.aicc<-AIC(TP.logistic)+ (2 * 4^2 + 2 * 4)/(length(TP.flat$residuals) - 4 - 1)
  
  
  #Store aicc from each model as a row####
  #Keep adding to this with each analysis
  aicc.df<-bind_rows(aicc.df,tibble(type="TP",flat=TP.flat.aicc,linear=TP.lm.aicc,segmented=TP.segmented.aicc,logistic=TP.logistic.aicc))
  
  
  #Store rsme from each model as a column####
  #Keep adding to this with each analysis
  rsme.df<-bind_rows(rsme.df,tibble(type="TP",flat=TP.flat.rsme,linear=TP.lm.rsme,segmented=TP.segmented.rsme,loess=TP.loess.rsme))
  
  #Plot each of the models overlaid
  ggplot(data=TP.trim,aes(y=log_tp_ugpl, x=log_area_ha))+geom_point()+
    geom_hline(yintercept=mean(TP.trim$log_tp_ugpl,na.rm=TRUE),color="orange")+
    geom_smooth(method='lm', se=FALSE,color="red")+
    geom_line(data=TP.segmentedModel,aes(y=TP.segmented.fitted,x=log_area_ha),color="blue")+ #This is the segmented regression
    geom_smooth(se=FALSE,color="green")+ #loess
    geom_line(data=tibble(logistic_predict=predict(TP.logistic),log_area_ha=TP.trim$log_area_ha), aes(y=logistic_predict,x=log_area_ha),color="purple")+
    geom_text(data=data.frame(rsme=round(TP.flat.rsme,3)), aes(label = paste("flat=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 1)+
    geom_text(data=data.frame(rsme=round(TP.lm.rsme,3)), aes(label = paste("linear=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 3)+
    geom_text(data=data.frame(rsme=round(TP.segmented.rsme,3)), aes(label = paste("segmented=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 5)+
    geom_text(data=data.frame(rsme=round(TP.loess.rsme,3)), aes(label = paste("loess=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 7)+
    theme_bw()
  
  #flat model
  #dtr: AICc###
  AIC(TP.flat)+ (2 * 1^2 + 2 * 1)/(length(TP.flat$residuals) - 1 - 1)
  
  #Linear model
  #dtr: AICc###
  AIC(TP.lm)+ (2 * 2^2 + 2 * 2)/(length(TP.flat$residuals) - 2 - 1)
  
  #Segmented model  
  #dtr: AICc###  
  AIC(TP.segmented)+ (2 * 4^2 + 2 * 4)/(length(TP.flat$residuals) - 4 - 1)
  
  #Loess model 
  #AIC for loess
  TP.loess.aic<-akaike.l(y=TP.trim$log_tp_ugpl,x=TP.trim$log_area_ha,interval=c(0.75,0.75,0))
  print(TP.loess.aic)
  TP.loess.aic$aic.loess[1,3]+((2 * (TP.loess$trace.hat + 1)) / (TP.loess$n - TP.loess$trace.hat - 2))
  
  #TN: examine linear vs. segmented regressions####
  #Trim TN and order to only non NA values####
  TN.trim<-crossSystemDF%>%dplyr::select(tn_ugpl,log_area_ha,area_ha)%>%arrange(log_area_ha)%>%filter(!is.na(tn_ugpl))%>%filter(!is.na(log_area_ha))%>%filter(tn_ugpl>0)%>%mutate(log_tn_ugpl=log10(tn_ugpl))
  #TN: Flat model####
  TN.flat<-lm(TN.trim$log_tn_ugpl~1,data=TN.trim)
  summary(TN.flat)
  #NEP flat: rsme####
  (TN.flat.rsme<-sqrt(c(crossprod(TN.flat$residuals))/length(TN.flat$residuals)))
  
  #TN: linear model####
  TN.lm<-lm(TN.trim$log_tn_ugpl~log_area_ha,data=TN.trim)
  summary(TN.lm)
  #TN: rsme####
  (TN.lm.rsme<-sqrt(c(crossprod(TN.lm$residuals))/length(TN.lm$residuals)))
  
  #*Segmented regression####
  #Try the segmented package version: htTNs://rpubs.com/MarkusLoew/12164####
  TN.segmented<-segmented(lm(log_tn_ugpl~log_area_ha,data=TN.trim),seg.z=log_area_ha,psi=c(3)) #initial guess makes a difference
  summary(TN.segmented) 
  TN.segmented$psi #get the breakpoints
  10^TN.segmented$psi[,"Est."] #back calculate breakpoint - this one is at 0.35 and 11.2 ha
  #get the average of the middle segment
  10^mean(TN.segmented$psi[,"Est."]) #This is a 2.00 ha mid point of the middle segment
  slope(TN.segmented) #get the slopes  
  TN.segmented.fitted<-fitted(TN.segmented) #get the fitted data
  TN.segmentedModel<-tibble(area_ha=10^TN.trim$log_area_ha,log_area_ha=TN.trim$log_area_ha,TN.segmented.fitted=TN.segmented.fitted) #calculate the fit with the back transformed x axis
  #TN loess: rsme####
  (TN.segmented.rsme<-sqrt(c(crossprod(TN.trim$log_tn_ugpl-TN.segmentedModel$TN.segmented.fitted)/length(TN.trim$log_tn_ugpl))))
  
  #*Create the loess model for TN####
  TN.loess<-loess(log_tn_ugpl~log_area_ha,data=TN.trim)
  #*find the x range###  
  TN.xrange <- range(TN.trim$area_ha)
  #*create a sequence for the fitted values, make sure to log transform here####
  TN.xseq <- seq(from=log10(TN.xrange[1]), to=log10(TN.xrange[2]), length=200)
  #*Create data frame with the loess predicted - this is what is generated in the geom_smooth ouTNut, note they are log transformed predictions####
  TN.loess.fit<-tibble(loess_fit=predict(TN.loess,newdata=data.frame(log_area_ha=TN.xseq)),log_area_ha=TN.xseq)
  #*Calculate the first derivative of the loess modeling using the differencing method, centered the areas, also log transformed####
  fd.TN.loess<-tibble(d_TN_log_tn_ugpl=as.numeric(diff(TN.loess.fit$loess_fit)/diff(TN.loess.fit$log_area_ha)),area_ha_centered=rowMeans(embed(TN.loess.fit$log_area_ha,2)))
  #*Find the minimum slope (fastest rate of change), back transform to get into the same scale and print it####
  (TN.cutoff<-10^(fd.TN.loess%>%filter(d_TN_log_tn_ugpl==min(d_TN_log_tn_ugpl,na.rm=TRUE))%>%dplyr::select(area_ha_centered)%>%pull()))
  #*Just to check, plot the first derivative####
  plot(fd.TN.loess$d_TN_log_tn_ugpl~fd.TN.loess$area_ha_centered)
  #TN loess: rsme####
  (TN.loess.rsme<-sqrt(c(crossprod(TN.trim$log_tn_ugpl-predict(TN.loess))/length(TN.trim$log_tn_ugpl))))
  
  #Fit a nonlinear logistic curve to the TN data####
  TN.logistic<-nls(log_tn_ugpl~A+(B-A)/(1+exp((xmid-log_area_ha)/scal)),
                    start=list(A=1,B=3,xmid=1,scal=-0.8),data=TN.trim,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
  summary(TN.logistic)
  #Get the cutoff from the logistic parameter as the xmid inflection point
  TN.logistic.cutoff<-10^TN.logistic$m$getAllPars()[3]
  
  
  #flat model: AICc###
  TN.flat.aicc<-AIC(TN.flat)+ (2 * 1^2 + 2 * 1)/(length(TN.flat$residuals) - 1 - 1)
  
  #Linear model: AICc###
  TN.lm.aicc<-AIC(TN.lm)+ (2 * 2^2 + 2 * 2)/(length(TN.flat$residuals) - 2 - 1)
  
  #Segmented model: AICc####  
  TN.segmented.aicc<-AIC(TN.segmented)+ (2 * 4^2 + 2 * 4)/(length(TN.flat$residuals) - 4 - 1)
  
  #Logistic model: AICc#### 
  TN.logistic.aicc<-TN.logistic.aicc<-AIC(TN.logistic)+ (2 * 4^2 + 2 * 4)/(length(TN.flat$residuals) - 4 - 1)
  
  
  #Store aicc from each model as a row####
  #Keep adding to this with each analysis
  aicc.df<-bind_rows(aicc.df,tibble(type="TN",flat=TN.flat.aicc,linear=TN.lm.aicc,segmented=TN.segmented.aicc,logistic=TN.logistic.aicc))
  
  #Store rsme from each model as a column####
  #Keep adding to this with each analysis
  rsme.df<-bind_rows(rsme.df,tibble(type="TN",flat=TN.flat.rsme,linear=TN.lm.rsme,segmented=TN.segmented.rsme,loess=TN.loess.rsme))
  
  #Plot each of the models overlaid
  ggplot(data=TN.trim,aes(y=log_tn_ugpl, x=log_area_ha))+geom_point()+
    geom_hline(yintercept=mean(TN.trim$log_tn_ugpl,na.rm=TRUE),color="orange")+
    geom_smooth(method='lm', se=FALSE,color="red")+
    geom_line(data=TN.segmentedModel,aes(y=TN.segmented.fitted,x=log_area_ha),color="blue")+ #This is the segmented regression
    geom_smooth(se=FALSE,color="green")+ #loess
    geom_line(data=tibble(logistic_predict=predict(TN.logistic),log_area_ha=TN.trim$log_area_ha), aes(y=logistic_predict,x=log_area_ha),color="purple")+ #logistic
    geom_text(data=data.frame(rsme=round(TN.flat.rsme,3)), aes(label = paste("flat=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 1)+
    geom_text(data=data.frame(rsme=round(TN.lm.rsme,3)), aes(label = paste("linear=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 3)+
    geom_text(data=data.frame(rsme=round(TN.segmented.rsme,3)), aes(label = paste("segmented=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 5)+
    geom_text(data=data.frame(rsme=round(TN.loess.rsme,3)), aes(label = paste("loess=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 7)+
    theme_bw()
  
  #TN:TP: examine linear vs. segmented regressions####
  #Trim TN and order to only non NA values####
  TNTP.trim<-crossSystemDF%>%dplyr::select(TNtoTPmolarratio,log_area_ha,area_ha)%>%arrange(log_area_ha)%>%filter(!is.na(TNtoTPmolarratio))%>%filter(!is.na(log_area_ha))%>%filter(TNtoTPmolarratio>0)%>%filter(!is.infinite(TNtoTPmolarratio))%>%mutate(log_TNtoTPmolarratio=log10(TNtoTPmolarratio))
  #TN:TP Flat model####
  TNTP.flat<-lm(TNTP.trim$log_TNtoTPmolarratio~1,data=TNTP.trim)
  summary(TNTP.flat)
  #TNTP flat: rsme####
  (TNTP.flat.rsme<-sqrt(c(crossprod(TNTP.flat$residuals))/length(TNTP.flat$residuals)))
  
  #TNTP: linear model####
  TNTP.lm<-lm(TNTP.trim$log_TNtoTPmolarratio~log_area_ha,data=TNTP.trim)
  summary(TNTP.lm)
  #TNTP: rsme####
  (TNTP.lm.rsme<-sqrt(c(crossprod(TNTP.lm$residuals))/length(TNTP.lm$residuals)))
  
  #*Segmented regression####
  #Try the segmented package version: htTNTPs://rpubs.com/MarkusLoew/12164####
  TNTP.segmented<-segmented(lm(log_TNtoTPmolarratio~log_area_ha,data=TNTP.trim),seg.z=log_area_ha,psi=c(3)) #initial guess makes a difference
  summary(TNTP.segmented) 
  TNTP.segmented$psi #get the breakpoints
  10^TNTP.segmented$psi[,"Est."] #back calculate breakpoint - this one is at 0.35 and 11.2 ha
  #get the average of the middle segment
  10^mean(TNTP.segmented$psi[,"Est."]) #This is a 2.00 ha mid point of the middle segment
  slope(TNTP.segmented) #get the slopes  
  TNTP.segmented.fitted<-fitted(TNTP.segmented) #get the fitted data
  TNTP.segmentedModel<-tibble(area_ha=10^TNTP.trim$log_area_ha,log_area_ha=TNTP.trim$log_area_ha,TNTP.segmented.fitted=TNTP.segmented.fitted) #calculate the fit with the back transformed x axis
  #TNTP loess: rsme####
  (TNTP.segmented.rsme<-sqrt(c(crossprod(TNTP.trim$log_TNtoTPmolarratio-TNTP.segmentedModel$TNTP.segmented.fitted)/length(TNTP.trim$log_TNtoTPmolarratio))))
  
  #*Create the loess model for TNTP####
  TNTP.loess<-loess(log_TNtoTPmolarratio~log_area_ha,data=TNTP.trim)
  #*find the x range###  
  TNTP.xrange <- range(TNTP.trim$area_ha)
  #*create a sequence for the fitted values, make sure to log transform here####
  TNTP.xseq <- seq(from=log10(TNTP.xrange[1]), to=log10(TNTP.xrange[2]), length=200)
  #*Create data frame with the loess predicted - this is what is generated in the geom_smooth ouTNTPut, note they are log transformed predictions####
  TNTP.loess.fit<-tibble(loess_fit=predict(TNTP.loess,newdata=data.frame(log_area_ha=TNTP.xseq)),log_area_ha=TNTP.xseq)
  #*Calculate the first derivative of the loess modeling using the differencing method, centered the areas, also log transformed####
  fd.TNTP.loess<-tibble(d_TNTP_log_TNtoTPmolarratio=as.numeric(diff(TNTP.loess.fit$loess_fit)/diff(TNTP.loess.fit$log_area_ha)),area_ha_centered=rowMeans(embed(TNTP.loess.fit$log_area_ha,2)))
  #*Find the minimum slope (fastest rate of change), back transform to get into the same scale and print it####
  (TNTP.cutoff<-10^(fd.TNTP.loess%>%filter(d_TNTP_log_TNtoTPmolarratio==max(d_TNTP_log_TNtoTPmolarratio,na.rm=TRUE))%>%dplyr::select(area_ha_centered)%>%pull()))
  #*Just to check, plot the first derivative####
  plot(fd.TNTP.loess$d_TNTP_log_TNtoTPmolarratio~fd.TNTP.loess$area_ha_centered)
  #TNTP loess: rsme####
  (TNTP.loess.rsme<-sqrt(c(crossprod(TNTP.trim$log_TNtoTPmolarratio-predict(TNTP.loess))/length(TNTP.trim$log_TNtoTPmolarratio))))
  
  #Fit a nonlinear logistic curve to the TNTP data####
  TNTP.logistic<-nls(log_TNtoTPmolarratio~A+(B-A)/(1+exp((xmid-log_area_ha)/scal)),
                    start=list(A=1,B=3,xmid=1,scal=-0.5),data=TNTP.trim,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
  summary(TNTP.logistic)
  #Get the cutoff from the logistic parameter as the xmid inflection point
  TNTP.logistic.cutoff<-10^TNTP.logistic$m$getAllPars()[3]
  
  
  #flat model: AICc###
  TNTP.flat.aicc<-AIC(TNTP.flat)+ (2 * 1^2 + 2 * 1)/(length(TNTP.flat$residuals) - 1 - 1)
  
  #Linear model: AICc###
  TNTP.lm.aicc<-AIC(TNTP.lm)+ (2 * 2^2 + 2 * 2)/(length(TNTP.flat$residuals) - 2 - 1)
  
  #Segmented model: AICc####  
  TNTP.segmented.aicc<-AIC(TNTP.segmented)+ (2 * 4^2 + 2 * 4)/(length(TNTP.flat$residuals) - 4 - 1)
  
  #Logistic model: AICc#### 
  TNTP.logistic.aicc<-TNTP.logistic.aicc<-AIC(TNTP.logistic)+ (2 * 4^2 + 2 * 4)/(length(TNTP.flat$residuals) - 4 - 1)
  
  
  #Store aicc from each model as a row####
  #Keep adding to this with each analysis
  aicc.df<-bind_rows(aicc.df,tibble(type="TNTP",flat=TNTP.flat.aicc,linear=TNTP.lm.aicc,segmented=TNTP.segmented.aicc,logistic=TNTP.logistic.aicc))
  
  #Store rsme from each model as a column####
  #Keep adding to this with each analysis
  rsme.df<-bind_rows(rsme.df,tibble(type="TNTP",flat=TNTP.flat.rsme,linear=TNTP.lm.rsme,segmented=TNTP.segmented.rsme,loess=TNTP.loess.rsme))
  
  #Plot each of the models overlaid
  ggplot(data=TNTP.trim,aes(y=log_TNtoTPmolarratio, x=log_area_ha))+geom_point()+
    geom_hline(yintercept=mean(TNTP.trim$log_TNtoTPmolarratio,na.rm=TRUE),color="orange")+
    geom_smooth(method='lm', se=FALSE,color="red")+
    geom_line(data=TNTP.segmentedModel,aes(y=TNTP.segmented.fitted,x=log_area_ha),color="blue")+ #This is the segmented regression
    geom_smooth(se=FALSE,color="green")+ #loess
    geom_line(data=tibble(logistic_predict=predict(TNTP.logistic),log_area_ha=TNTP.trim$log_area_ha), aes(y=logistic_predict,x=log_area_ha),color="purple")+ #logistic
    geom_text(data=data.frame(rsme=round(TNTP.flat.rsme,3)), aes(label = paste("flat=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 1)+
    geom_text(data=data.frame(rsme=round(TNTP.lm.rsme,3)), aes(label = paste("linear=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 3)+
    geom_text(data=data.frame(rsme=round(TNTP.segmented.rsme,3)), aes(label = paste("segmented=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 5)+
    geom_text(data=data.frame(rsme=round(TNTP.loess.rsme,3)), aes(label = paste("loess=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 7)+
    theme_bw()
  
  #chla: examine linear vs. segmented regressions####
  #Trim chla and order to only non NA values####
  chla.trim<-crossSystemDF%>%dplyr::select(chla_ugpl,log_area_ha,area_ha)%>%arrange(log_area_ha)%>%filter(!is.na(chla_ugpl))%>%filter(!is.na(log_area_ha))%>%filter(chla_ugpl>0)%>%mutate(log_chla_ugpl=log10(chla_ugpl))
  #chla: Flat model####
  chla.flat<-lm(chla.trim$log_chla_ugpl~1,data=chla.trim)
  summary(chla.flat)
  #NEP flat: rsme####
  (chla.flat.rsme<-sqrt(c(crossprod(chla.flat$residuals))/length(chla.flat$residuals)))
  
  #chla: linear model####
  chla.lm<-lm(chla.trim$log_chla_ugpl~log_area_ha,data=chla.trim)
  summary(chla.lm)
  #chla: rsme####
  (chla.lm.rsme<-sqrt(c(crossprod(chla.lm$residuals))/length(chla.lm$residuals)))
  
  #*Segmented regression####
  #Try the segmented package version: htchlas://rpubs.com/MarkusLoew/12164####
  chla.segmented<-segmented(lm(log_chla_ugpl~log_area_ha,data=chla.trim),seg.z=log_area_ha,psi=c(3)) #initial guess makes a difference
  summary(chla.segmented) 
  chla.segmented$psi #get the breakpoints
  10^chla.segmented$psi[,"Est."] #back calculate breakpoint - this one is at 0.35 and 11.2 ha
  #get the average of the middle segment
  10^mean(chla.segmented$psi[,"Est."]) #This is a 2.00 ha mid point of the middle segment
  slope(chla.segmented) #get the slopes  
  chla.segmented.fitted<-fitted(chla.segmented) #get the fitted data
  chla.segmentedModel<-tibble(area_ha=10^chla.trim$log_area_ha,log_area_ha=chla.trim$log_area_ha,chla.segmented.fitted=chla.segmented.fitted) #calculate the fit with the back transformed x axis
  #chla loess: rsme####
  (chla.segmented.rsme<-sqrt(c(crossprod(chla.trim$log_chla_ugpl-chla.segmentedModel$chla.segmented.fitted)/length(chla.trim$log_chla_ugpl))))
  
  #*Create the loess model for chla####
  chla.loess<-loess(log_chla_ugpl~log_area_ha,data=chla.trim)
  #*find the x range###  
  chla.xrange <- range(chla.trim$area_ha)
  #*create a sequence for the fitted values, make sure to log transform here####
  chla.xseq <- seq(from=log10(chla.xrange[1]), to=log10(chla.xrange[2]), length=200)
  #*Create data frame with the loess predicted - this is what is generated in the geom_smooth ouchlaut, note they are log transformed predictions####
  chla.loess.fit<-tibble(loess_fit=predict(chla.loess,newdata=data.frame(log_area_ha=chla.xseq)),log_area_ha=chla.xseq)
  #*Calculate the first derivative of the loess modeling using the differencing method, centered the areas, also log transformed####
  fd.chla.loess<-tibble(d_chla_log_chla_ugpl=as.numeric(diff(chla.loess.fit$loess_fit)/diff(chla.loess.fit$log_area_ha)),area_ha_centered=rowMeans(embed(chla.loess.fit$log_area_ha,2)))
  #*Find the minimum slope (fastest rate of change), back transform to get into the same scale and print it####
  (chla.cutoff<-10^(fd.chla.loess%>%filter(d_chla_log_chla_ugpl==min(d_chla_log_chla_ugpl,na.rm=TRUE))%>%dplyr::select(area_ha_centered)%>%pull()))
  #*Just to check, plot the first derivative####
  plot(fd.chla.loess$d_chla_log_chla_ugpl~fd.chla.loess$area_ha_centered)
  #chla loess: rsme####
  (chla.loess.rsme<-sqrt(c(crossprod(chla.trim$log_chla_ugpl-predict(chla.loess))/length(chla.trim$log_chla_ugpl))))
  
  #Fit a nonlinear logistic curve to the chla data####
  chla.logistic<-nls(log_chla_ugpl~A+(B-A)/(1+exp((xmid-log_area_ha)/scal)),
                    start=list(A=0,B=2,xmid=1,scal=-0.2),data=chla.trim,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
  summary(chla.logistic)
  #Get the cutoff from the logistic parameter as the xmid inflection point
  chla.logistic.cutoff<-10^chla.logistic$m$getAllPars()[3]
  
  
  #flat model: AICc###
  chla.flat.aicc<-AIC(chla.flat)+ (2 * 1^2 + 2 * 1)/(length(chla.flat$residuals) - 1 - 1)
  
  #Linear model: AICc###
  chla.lm.aicc<-AIC(chla.lm)+ (2 * 2^2 + 2 * 2)/(length(chla.flat$residuals) - 2 - 1)
  
  #Segmented model: AICc####  
  chla.segmented.aicc<-AIC(chla.segmented)+ (2 * 4^2 + 2 * 4)/(length(chla.flat$residuals) - 4 - 1)
  
  #Logistic model: AICc#### 
  chla.logistic.aicc<-chla.logistic.aicc<-AIC(chla.logistic)+ (2 * 4^2 + 2 * 4)/(length(chla.flat$residuals) - 4 - 1)
  
  
  #Store aicc from each model as a row####
  #Keep adding to this with each analysis
  aicc.df<-bind_rows(aicc.df,tibble(type="chla",flat=chla.flat.aicc,linear=chla.lm.aicc,segmented=chla.segmented.aicc,logistic=chla.logistic.aicc))
  
  #Store rsme from each model as a column####
  #Keep adding to this with each analysis
  rsme.df<-bind_rows(rsme.df,tibble(type="chla",flat=chla.flat.rsme,linear=chla.lm.rsme,segmented=chla.segmented.rsme,loess=chla.loess.rsme))
  

  #Plot each of the models overlaid
  ggplot(data=chla.trim,aes(y=log_chla_ugpl, x=log_area_ha))+geom_point()+
    geom_hline(yintercept=mean(chla.trim$log_chla_ugpl,na.rm=TRUE),color="orange")+
    geom_smooth(method='lm', se=FALSE,color="red")+
    geom_line(data=chla.segmentedModel,aes(y=chla.segmented.fitted,x=log_area_ha),color="blue")+ #This is the segmented regression
    geom_smooth(se=FALSE,color="green")+ #loess
    geom_line(data=tibble(logistic_predict=predict(chla.logistic),log_area_ha=chla.trim$log_area_ha), aes(y=logistic_predict,x=log_area_ha),color="purple")+ #logistic
    geom_text(data=data.frame(rsme=round(chla.flat.rsme,3)), aes(label = paste("flat=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 1)+
    geom_text(data=data.frame(rsme=round(chla.lm.rsme,3)), aes(label = paste("linear=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 3)+
    geom_text(data=data.frame(rsme=round(chla.segmented.rsme,3)), aes(label = paste("segmented=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 5)+
    geom_text(data=data.frame(rsme=round(chla.loess.rsme,3)), aes(label = paste("loess=",rsme), x = Inf, y = Inf),hjust = 1, vjust = 7)+
    theme_bw()
  
  
  #pH analysis####
  #*ph: trim and order to only non NA values####
  ph.trim<-crossSystemDF%>%dplyr::select(ph,log_area_ha,area_ha)%>%arrange(log_area_ha)%>%filter(!is.na(ph))%>%filter(!is.na(log_area_ha))%>%filter(ph>0)
  
  #*pH: Flat model####
  ph.flat<-lm(ph~1,data=ph.trim)
  summary(ph.flat)
  
  #*ph: linear model####
  ph.lm<-lm(ph~log_area_ha,data=ph.trim)
  summary(ph.lm)
  
  #*ph: Segmented regression####
  #Try the segmented package version: https://rpubs.com/MarkusLoew/12164#
  ph.segmented<-segmented(lm(ph ~ log_area_ha,data=ph.trim),seg.z=log_area_ha,psi=c(0)) #initial guess makes a difference
  summary(ph.segmented) 
  10^ph.segmented$psi[,"Est."] #back calculate breakpoint
  slope(ph.segmented) #get the slopes  
  ph.segmented.fitted<-fitted(ph.segmented) #get the fitted data
  ph.segmentedModel<-tibble(area_ha=10^ph.trim$log_area_ha,log_area_ha=ph.trim$log_area_ha,ph.segmented.fitted=ph.segmented.fitted) #calculate the fit with the back transformed x axis
  
  #*ph: fit a nonlinear logistic curve####
  ph.logistic<-nls(ph~A+(B-A)/(1+exp((xmid-log_area_ha)/scal)),
                              start=list(A=6,B=8,xmid=0,scal=-0.2),data=ph.trim,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
  summary(ph.logistic)
  #Get the cutoff from the logistic parameter as the xmid inflection point
  ph.logistic.cutoff<-10^ph.logistic$m$getAllPars()[3]
  
  #**ph: flat model AICc####
  ph.flat.aicc<-AIC(ph.flat)+ (2 * 1^2 + 2 * 1)/(length(ph.flat$residuals) - 1 - 1)
  
  #**ph: linear model AICc####
  ph.lm.aicc<-AIC(ph.lm)+ (2 * 2^2 + 2 * 2)/(length(ph.flat$residuals) - 2 - 1)
  
  #**ph: Segmented model AICc####  
  ph.segmented.aicc<-AIC(ph.segmented)+ (2 * 4^2 + 2 * 4)/(length(ph.flat$residuals) - 4 - 1)
  
  #**ph: logistic model AICc#### 
  ph.logistic.aicc<-AIC(ph.logistic)+ (2 * 4^2 + 2 * 4)/(length(ph.flat$residuals) - 4 - 1)
  
  #**ph: Store aicc from each model as a row####
  #Keep adding to this with each analysis
  aicc.df<-bind_rows(aicc.df,tibble(type="ph",flat=ph.flat.aicc,linear=ph.lm.aicc,segmented=ph.segmented.aicc,logistic=ph.logistic.aicc))
  
  #*ph: plot models overlaid####
  ggplot(data=ph.trim,aes(y=ph, x=log_area_ha))+geom_point()+
    geom_hline(yintercept=mean(ph.trim$ph,na.rm=TRUE),color="orange")+
    geom_smooth(method='lm', se=FALSE,color="red")+
    geom_line(data=ph.segmentedModel,aes(y=ph.segmented.fitted,x=log_area_ha),color="blue")+ #This is the segmented regression
    geom_line(data=tibble(logistic_predict=predict(ph.logistic),log_area_ha=ph.trim$log_area_ha), aes(y=logistic_predict,x=log_area_ha),color="purple")+
    geom_text(data=data.frame(aicc=round(ph.flat.aicc,1)), aes(label = paste("flat=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 1)+
    geom_text(data=data.frame(aicc=round(ph.lm.aicc,1)), aes(label = paste("linear=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 3)+
    geom_text(data=data.frame(aicc=round(ph.segmented.aicc,1)), aes(label = paste("segmented=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 5)+
    geom_text(data=data.frame(aicc=round(ph.logistic.aicc,1)), aes(label = paste("logistic=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 7)+
    theme_bw()
  
  #*ph: plot optimal fit#### 
  gg.ph.scatter<-ggplot(data=ph.trim,aes(x=area_ha,y=ph))+
    geom_vline(xintercept=ph.logistic.cutoff,color="black",linetype=1,size=1)+ #vertical line indicating the maximum first derivative from the loess model
    geom_point(color="dark grey")+
    scale_x_continuous(limits=c(0.001,550000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
    geom_line(data=tibble(logistic_predict=predict(ph.logistic),log_area_ha=ph.trim$log_area_ha), aes(y=logistic_predict,x=10^log_area_ha),color="black",size=1.2)+
    #geom_line(data=methaneFluxes.segmentedModel,aes(y=10^methaneFluxes.segmented.fitted,x=10^log_area_ha),color="black",size=1.2)+ #This is the segmented regression
    #geom_smooth(method="loess",se=FALSE,size=1.2,color="black")+ #put in the loess smoothed line
    #geom_line(aes(y=predict),color="black",size=1.2)+
    #geom_line(data=methanefluxes.loess.fit,aes(x=10^log_area_ha,y=loess_fit),color="purple")+
    #geom_line(data=temp,aes(x=area_ha,y=loess_fit),color="green")+
    scale_y_log10()+
    labs(y=bquote(pH),x=bquote(Surface~area~(ha)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          #legend.position = c(0.8, 0.8),
          #legend.title = element_text(size=8),
          #legend.text = element_text(size=10)
    )
  #**pH: FIGURE export as jpg####
  ggsave("plots/WIAP-FigureX-pHScatter.jpg", plot=gg.ph.scatter, width=3, height=3,units="in", dpi=300)
  
  
  #TP figures####
  #Recreate woolway plot figure 
  gg.nutrientTP.scatter<-ggplot(data=TP.trim,aes(x=area_ha,y=tp_ugpl))+
    geom_vline(xintercept=TP.logistic.cutoff,color="black",size=1)+
    geom_point(color="dark grey")+
    scale_x_continuous(limits=c(0.001,550000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
    geom_line(data=tibble(logistic_predict=predict(TP.logistic),log_area_ha=TP.trim$log_area_ha), aes(y=10^logistic_predict,x=10^log_area_ha),color="black",size=1.2)+
    #geom_line(data=TP.segmentedModel, aes(x = area_ha, y = 10^TP.segmented.fitted),color="black",size=1.2)+
    #geom_smooth(se=TRUE)+ #LOESS smoothing
    scale_y_log10()+
    labs(y=bquote(TP~(mu*g~L^-1)),x=bquote(Surface~area~(ha)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          #legend.position = c(0.8, 0.8),
          #legend.title = element_text(size=8),
          #legend.text = element_text(size=10)
    )
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-nutrientTP.jpg", plot=gg.nutrientTP.scatter, width=3, height=3,units="in", dpi=300)
  
  #TP in boxplot form a la Meredith k600 graph###
  gg.nutrientTP.box<-ggplot(data=subset(crossSystemDF,!is.na(area_ha)),aes(x=bins_km2,y=tp_ugpl))+
    geom_boxplot()+
    geom_jitter()+
    scale_x_discrete(labels=c(bquote("<"*10^-3),bquote(10^-3~-~10^-2),bquote(10^-2~-~10^-1),bquote(10^-1~-~10^0),bquote(10^0~-~10^1),bquote(10^1~-~10^2),bquote(">"*10^2)))+
    scale_y_log10()+
    labs(y=bquote(TP~(mu*g/L)),x=bquote(Surface~area~(km^2)))+
    #labels=bquote(.(parse(text=paste("10^",as.numeric(as.character(levels(DielTempRange_all$bins_km2),sep=""))))
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(angle=-45,vjust=0.95,hjust=0))
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-nutrientTPBoxplot.jpg", plot=gg.nutrientTP.box, width=3, height=3,units="in", dpi=300)
  
  #TN figures####
  #Recreate woolway plot figure 
  gg.nutrientTN.scatter<-ggplot(data=TN.trim,aes(x=area_ha,y=tn_ugpl))+
    geom_vline(xintercept=10^TN.segmented$psi[,"Est."],color="black",size=1)+
    geom_point(color="dark grey")+
    scale_x_continuous(limits=c(0.001,550000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
    geom_line(data=TN.segmentedModel, aes(x = area_ha, y = 10^TN.segmented.fitted),color="black",size=1.2)+
    #geom_smooth(se=TRUE)+
    scale_y_log10()+
    labs(y=bquote(TN~(mu*g~L^-1)),x=bquote(Surface~area~(ha)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          #legend.position = c(0.8, 0.8),
          #legend.title = element_text(size=8),
          #legend.text = element_text(size=10)
    )
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-nutrientTN.jpg", plot=gg.nutrientTN.scatter, width=3, height=3,units="in", dpi=300)
  
  #TN in boxplot form a la Meredith k600 graph###
  gg.nutrientTN.box<-ggplot(data=subset(crossSystemDF,!is.na(area_ha)),aes(x=bins_km2,y=tn_ugpl))+
    geom_boxplot()+
    geom_jitter()+
    scale_x_discrete(labels=c(bquote("<"*10^-3),bquote(10^-3~-~10^-2),bquote(10^-2~-~10^-1),bquote(10^-1~-~10^0),bquote(10^0~-~10^1),bquote(10^1~-~10^2),bquote(">"*10^2)))+
    scale_y_log10()+
    labs(y=bquote(TN~(mu*g/L)),x=bquote(Surface~area~(km^2)))+
    #labels=bquote(.(parse(text=paste("10^",as.numeric(as.character(levels(DielTempRange_all$bins_km2),sep=""))))
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(angle=-45,vjust=0.95,hjust=0))
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-nutrientTNBoxplot.jpg", plot=gg.nutrientTN.box, width=3, height=3,units="in", dpi=300)
  
  #TN:TP molar ratios figures####
  #Recreate woolway plot figure 
  gg.nutrientNtoP.scatter<-ggplot(data=TNTP.trim,aes(x=area_ha,y=TNtoTPmolarratio))+
    geom_vline(xintercept=10^TNTP.segmented$psi[,"Est."],color="black",size=1)+
    geom_point(color="dark grey")+
    scale_x_continuous(limits=c(0.001,550000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
    geom_line(data=TNTP.segmentedModel, aes(x = area_ha, y = 10^TNTP.segmented.fitted),color="black",size=1.2)+
    #geom_smooth(se=TRUE)+
    #geom_hline(yintercept=10^TNTP.flat$coefficients,color="black",size=1.2)+
    scale_y_log10()+
    labs(y=bquote(TN:TP),x=bquote(Surface~area~(ha)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          #legend.position = c(0.8, 0.8),
          #legend.title = element_text(size=8),
          #legend.text = element_text(size=10)
    )
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-nutrientNtoP.jpg", plot=gg.nutrientNtoP.scatter, width=3, height=3,units="in", dpi=300)
  
  #TN:TP in boxplot form a la Meredith k600 graph###
  gg.nutrientNtoP.box<-ggplot(data=subset(crossSystemDF,!is.na(area_ha)),aes(x=bins_km2,y=TNtoTPmolarratio))+
    geom_boxplot()+
    geom_jitter()+
    scale_x_discrete(labels=c(bquote("<"*10^-3),bquote(10^-3~-~10^-2),bquote(10^-2~-~10^-1),bquote(10^-1~-~10^0),bquote(10^0~-~10^1),bquote(10^1~-~10^2),bquote(">"*10^2)))+
    scale_y_log10()+
    labs(y=bquote(TN:TP),x=bquote(Surface~area~(km^2)))+
    #labels=bquote(.(parse(text=paste("10^",as.numeric(as.character(levels(DielTempRange_all$bins_km2),sep=""))))
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(angle=-45,vjust=0.95,hjust=0))
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-nutrientNtoPBoxplot.jpg", plot=gg.nutrientNtoP.box, width=3, height=3,units="in", dpi=300)
  
  #chl a figures####
  #Recreate woolway plot figure 
  gg.nutrientchla.scatter<-ggplot(data=chla.trim,aes(x=area_ha,y=chla_ugpl))+
    geom_vline(xintercept=chla.logistic.cutoff,color="black",size=1)+
    geom_point(color="dark grey")+
    scale_x_continuous(limits=c(0.001,550000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
    geom_line(data=tibble(logistic_predict=predict(chla.logistic),log_area_ha=chla.trim$log_area_ha), aes(y=10^logistic_predict,x=10^log_area_ha),color="black",size=1.2)+
    #geom_smooth(se=TRUE)+
    #geom_hline(yintercept=10^chla.flat$coefficients,color="black",size=1.2)+
    scale_y_log10()+
    labs(y=bquote(Chl~italic(a)~(mu*g~L^-1)),x=bquote(Surface~area~(ha)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          #legend.position = c(0.8, 0.8),
          #legend.title = element_text(size=8),
          #legend.text = element_text(size=10)
    )
  
  
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-nutrientchlA.jpg", plot=gg.nutrientchla.scatter, width=3, height=3,units="in", dpi=300)
  
  #chla in boxplot form a la Meredith k600 graph###
  gg.nutrientchla.box<-ggplot(data=subset(crossSystemDF,!is.na(area_ha)),aes(x=bins_km2,y=chla_ugpl))+
    geom_boxplot()+
    geom_jitter()+
    scale_x_discrete(labels=c(bquote("<"*10^-3),bquote(10^-3~-~10^-2),bquote(10^-2~-~10^-1),bquote(10^-1~-~10^0),bquote(10^0~-~10^1),bquote(10^1~-~10^2),bquote(">"*10^2)))+
    scale_y_log10()+
    labs(y=bquote(Chl~a~(mu*g/L)),x=bquote(Surface~area~(km^2)))+
    #labels=bquote(.(parse(text=paste("10^",as.numeric(as.character(levels(DielTempRange_all$bins_km2),sep=""))))
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(angle=-45,vjust=0.95,hjust=0))
  #FIGURE: daily tempreature range scatter, print out as jpg####
  ggsave("plots/WIAP-FigureX-nutrientchlABoxplot.jpg", plot=gg.nutrientchla.box, width=3, height=3,units="in", dpi=300)
  
  #Summarize the rsme with the percent declines from the flat line####
  #choose some number of 3% to indicate a better model fit?
  rsme.df%>%mutate(linear_per=100-linear*100/flat,segmented_per=100-segmented*100/flat,loess_per=100-loess*100/flat)
  #Summarize AICc adding in the delta from the minimum for each entry####
  #Add in segmented and logistic cutoffs
  aicc.df.export<-aicc.df%>%rowwise()%>%mutate(delta_flat=flat-min(flat,linear,segmented,logistic,na.rm=TRUE),delta_linear=linear-min(flat,linear,segmented,logistic,na.rm=TRUE),delta_segmented=segmented-min(flat,linear,segmented,logistic,na.rm=TRUE),delta_logistic=logistic-min(flat,linear,segmented,logistic,na.rm=TRUE))%>%
    bind_cols(.,tibble(segmented.cutoff=c(10^dtr.segmented$psi[,"Est."],10^methaneFluxes.segmented$psi[,"Est."],10^k600.segmented$psi[,"Est."],10^NEP.segmented$psi[,"Est."],10^GPP.segmented$psi[,"Est."],10^R.segmented$psi[,"Est."],10^TP.segmented$psi[,"Est."],10^TN.segmented$psi[,"Est."],10^TNTP.segmented$psi[,"Est."],10^chla.segmented$psi[,"Est."],10^ph.segmented$psi[,"Est."]),
                       logistic.cutoff=c(dtr.logistic.cutoff,methaneFluxes.logistic.cutoff,k600.logistic.cutoff,NEP.logistic.cutoff,GPP.logistic.cutoff,R.logistic.cutoff,TP.logistic.cutoff,TN.logistic.cutoff,TNTP.logistic.cutoff,chla.logistic.cutoff,ph.logistic.cutoff),
                       segmented.cutoff.SE=c(10^dtr.segmented$psi[,"St.Err"],10^methaneFluxes.segmented$psi[,"St.Err"],10^k600.segmented$psi[,"St.Err"],10^NEP.segmented$psi[,"St.Err"],10^GPP.segmented$psi[,"St.Err"],10^R.segmented$psi[,"St.Err"],10^TP.segmented$psi[,"St.Err"],10^TN.segmented$psi[,"St.Err"],10^TNTP.segmented$psi[,"St.Err"],10^chla.segmented$psi[,"St.Err"],10^ph.segmented$psi[,"St.Err"]),
                       logistic.cutoff.SE=c(10^coef(summary(dtr.logistic))[,"Std. Error"][3],10^coef(summary(methaneFluxes.logistic))[,"Std. Error"][3],10^coef(summary(k600.logistic))[,"Std. Error"][3],NA,NA,10^coef(summary(R.logistic))[,"Std. Error"][3],10^coef(summary(TP.logistic))[,"Std. Error"][3],10^coef(summary(TN.logistic))[,"Std. Error"][3],NA,10^coef(summary(chla.logistic))[,"Std. Error"][3],10^coef(summary(ph.logistic))[,"Std. Error"][3])
                       ) #end of tibble
              
              ) #end of bind cols

  
  
  #FIGURE: Massive 12 panel figure with the first column as the flat/linear; second column as segmented regression; third column as the sigmoidal####  
  #First column which is the linear or flat trends####
  (gg.column1<-ggarrange(gg.linear+
              theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    plot.margin=unit(c(2,0,1,1), "lines"),
                    axis.ticks.length.x = unit(0, "pt"))+
              labs(y="Ecosystem struc./func.", title=NULL)+
              geom_text(data=panelLetter.normal,
                        aes(x=xpos,
                            y=ypos,
                            hjust=hjustvar,
                            vjust=vjustvar,
                            label="a",
                            fontface="bold")),
            gg.metabolismGPP.scatter+
              theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank()), #Rotate and center the x axis labels
            gg.nutrientNtoP.scatter+
              theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
              scale_y_continuous(limits=c(1,1000),trans="log10",breaks=c(1,10,100,1000),labels=trans_format("log10",math_format(10^.x))),
            gg.nutrientchla.scatter+
              theme(axis.text.y = element_text(angle = 90,hjust=0.5))+ #Rotate and center the x axis labels
              scale_y_continuous(limits=c(0.01,1000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x))),
            nrow=4,align="v"))
  
  
     (gg.column2<-ggarrange(gg.segmented+
              theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    plot.margin=unit(c(2,0,1,1), "lines"),
                    axis.ticks.length.x = unit(0, "pt"))+
              labs(y="Ecosystem struc./func.", title=NULL)+
              geom_text(data=panelLetter.normal,
                        aes(x=xpos,
                            y=ypos,
                            hjust=hjustvar,
                            vjust=vjustvar,
                            label="b",
                            fontface="bold")),
            gg.metabolismR.scatter+
              theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank()), #Rotate and center the x axis labels
            gg.nutrientTN.scatter+
              theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
              scale_y_continuous(limits=c(10,10000),trans="log10",breaks=c(10,100,1000,10000),labels=trans_format("log10",math_format(10^.x))),
            gg.nutrientTP.scatter+
              theme(axis.text.y = element_text(angle = 90,hjust=0.5))+ #Rotate and center the x axis labels
              scale_y_continuous(limits=c(1,10000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x))),
            nrow=4,align="v"))
  
     (gg.column3<-ggarrange(gg.nonlinear+
                           theme(axis.title.x=element_blank(),
                                 axis.text.x=element_blank(),
                                 axis.ticks.x=element_blank(),
                                 plot.margin=unit(c(2,0,1,1), "lines"),
                                 axis.ticks.length.x = unit(0, "pt"))+
                           labs(y="Ecosystem struc./func.", title=NULL)+
                           geom_text(data=panelLetter.normal,
                                     aes(x=xpos,
                                         y=ypos,
                                         hjust=hjustvar,
                                         vjust=vjustvar,
                                         label="c",
                                         fontface="bold")),
                         gg.dtr.scatter+
                           theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank()), #Rotate and center the x axis labels
                         gg.ch4flux.scatter+
                           theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
                           scale_y_continuous(limits=c(1,1200),trans="log10",breaks=c(1,10,100,1000),labels=trans_format("log10",math_format(10^.x))),
                         gg.k600.scatter+
                           theme(axis.text.y = element_text(angle = 90,hjust=0.5))+ #Rotate and center the x axis labels
                           scale_y_continuous(limits=c(0.1,3),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x))),
                         nrow=4,align="v"))

  
  #Stitch all three columns together
  gg.largefigure<-ggarrange(gg.column1,gg.column2,gg.column3,ncol=3,align="h")
  #FIGURE: all 12 panels, print out as jpg####
  ggsave("plots/WIAP-FigureX-Obj5LargeMultipanelFigure.jpg", plot=gg.largefigure, width=6.6, height=8,units="in", dpi=300)
  
  #Try the patchwork approach to make the figres the same size####
  #Create a panel letter for the rest of the plots####
  #Modify with panel letters###
  panelLetter.data <- data.frame(
    xpos = c(0.001),
    ypos =  c(Inf),
    hjustvar = c(0) ,
    vjustvar = c(1.5))
  
  #goes left to right
  List<-list(gg.linear+
         theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               plot.margin=unit(c(2,0,1,1), "lines"),
               axis.ticks.length.x = unit(0, "pt"))+
         labs(y="Ecosystem struc./func.", title=NULL)+
         geom_text(data=panelLetter.normal,
                   aes(x=xpos,
                       y=ypos,
                       hjust=hjustvar,
                       vjust=vjustvar,
                       label="a",
                       fontface="bold")),
       gg.segmented+
         theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               plot.margin=unit(c(2,0,1,1), "lines"),
               axis.ticks.length.x = unit(0, "pt"))+
         labs(y="Ecosystem struc./func.", title=NULL)+
         geom_text(data=panelLetter.normal,
                   aes(x=xpos,
                       y=ypos,
                       hjust=hjustvar,
                       vjust=vjustvar,
                       label="b",
                       fontface="bold")),
       gg.nonlinear+
         theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               plot.margin=unit(c(2,0,1,1), "lines"),
               axis.ticks.length.x = unit(0, "pt"))+
         labs(y="Ecosystem struc./func.", title=NULL)+
         geom_text(data=panelLetter.normal,
                   aes(x=xpos,
                       y=ypos,
                       hjust=hjustvar,
                       vjust=vjustvar,
                       label="c",
                       fontface="bold")),
       gg.metabolismGPP.scatter+
         theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
         geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="d",fontface="bold")), 
       gg.metabolismR.scatter+
         theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
         geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="e",fontface="bold")), 
       gg.dtr.scatter+
         theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
         geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="f",fontface="bold")),
       gg.nutrientNtoP.scatter+
         theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
         scale_y_continuous(limits=c(1,1000),trans="log10",breaks=c(1,10,100,1000),labels=trans_format("log10",math_format(10^.x)))+ 
         geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="g",fontface="bold")),
       gg.nutrientTN.scatter+
         theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
         scale_y_continuous(limits=c(10,10000),trans="log10",breaks=c(10,100,1000,10000),labels=trans_format("log10",math_format(10^.x)))+
         geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="h",fontface="bold")),
       gg.ch4flux.scatter+
         theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
         scale_y_continuous(limits=c(1,1200),trans="log10",breaks=c(1,10,100,1000),labels=trans_format("log10",math_format(10^.x)))+ 
         geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="i",fontface="bold")),
       gg.nutrientchla.scatter+
         theme(axis.text.y = element_text(angle = 90,hjust=0.5))+ #Rotate and center the x axis labels
         scale_y_continuous(limits=c(0.01,1000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
         geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="j",fontface="bold")),
       gg.nutrientTP.scatter+
         theme(axis.text.y = element_text(angle = 90,hjust=0.5))+ #Rotate and center the x axis labels
         scale_y_continuous(limits=c(1,10000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
         geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="k",fontface="bold")),
       gg.k600.scatter+
         theme(axis.text.y = element_text(angle = 90,hjust=0.5))+ #Rotate and center the x axis labels
         scale_y_continuous(limits=c(0.1,3),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
         geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="l",fontface="bold"))
       )
  #Plot them using patchwork####
  gg.largefigure.patchwork<-wrap_plots(List,ncol = 3,nrow = 4)
  #FIGURE: all 12 panels, print out as jpg####
  ggsave("plots/WIAP-FigureX-Obj5LargeMultipanelFigure.jpg", plot=gg.largefigure.patchwork, width=6.6, height=8,units="in", dpi=300)
  
  
  #goes left to right
  List<-list(gg.metabolismGPP.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
               geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="a",fontface="bold")),
             gg.metabolismNEP.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
               geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="b",fontface="bold")),
             gg.nutrientTP.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
               scale_y_continuous(limits=c(1,10000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
               geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="c",fontface="bold")),
             gg.metabolismR.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
               geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="d",fontface="bold")), 
             gg.nutrientchla.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
               scale_y_continuous(limits=c(0.01,1000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
               geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="e",fontface="bold")),
             gg.nutrientTN.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
               scale_y_continuous(limits=c(10,10000),trans="log10",breaks=c(10,100,1000,10000),labels=trans_format("log10",math_format(10^.x)))+
               geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="f",fontface="bold")),
             gg.dtr.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
               geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="g",fontface="bold")),
             #gg.nutrientNtoP.scatter+
              # theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
               #scale_y_continuous(limits=c(1,1000),trans="log10",breaks=c(1,10,100,1000),labels=trans_format("log10",math_format(10^.x)))+ 
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="h",fontface="bold")),
             gg.k600.scatter+
               theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10))+ #Rotate and center the x axis labels
               scale_y_continuous(limits=c(0.1,3),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
               geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="i",fontface="bold")),
             gg.ch4flux.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10))+ #Rotate and center the x axis labels
               scale_y_continuous(limits=c(1,1200),trans="log10",breaks=c(1,10,100,1000),labels=trans_format("log10",math_format(10^.x)))+ 
               geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="j",fontface="bold"))
             )
  
  #Plot them using patchwork####
  (gg.largefigure.patchwork.5x2<-wrap_plots(List,ncol = 2,nrow = 5))
  
  #FIGURE: all 12 panels, print out as jpg####
  #Could do a 3x3 with width 6, height = 5
  ggsave("plots/WIAP-FigureX-Obj5LargeMultipanelFigure5x2.jpg", plot=gg.largefigure.patchwork.5x2, width=3, height=8,units="in", dpi=300)
  
  
  #goes left to right
  #panel letter size
  panel.size<-10
  List<-list(gg.metabolismGPP.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="a",fontface="bold"))+
               ggtitle("(a)"),
             gg.nutrientTP.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               scale_y_continuous(limits=c(1,10000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="b",fontface="bold"))+
               ggtitle("(b)"),
             gg.metabolismNEP.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="c",fontface="bold"))+
               ggtitle("(c)"),
             gg.ch4flux.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               scale_y_continuous(limits=c(1,1200),trans="log10",breaks=c(1,10,100,1000),labels=trans_format("log10",math_format(10^.x)))+ 
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="d",fontface="bold"))+
               ggtitle("(d)"),
             gg.metabolismR.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="e",fontface="bold"))+
               ggtitle("(e)"), 
             
             gg.nutrientchla.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               scale_y_continuous(limits=c(0.01,1000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="f",fontface="bold"))+
               ggtitle("(f)"),
             gg.nutrientTN.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               scale_y_continuous(limits=c(10,10000),trans="log10",breaks=c(10,100,1000,10000),labels=trans_format("log10",math_format(10^.x)))+
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="g",fontface="bold"))+
               ggtitle("(g)"),
             gg.dtr.scatter+
               theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="h",fontface="bold"))+
                ggtitle("(h)"),
             #gg.nutrientNtoP.scatter+
             # theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank())+ #Rotate and center the x axis labels
             #scale_y_continuous(limits=c(1,1000),trans="log10",breaks=c(1,10,100,1000),labels=trans_format("log10",math_format(10^.x)))+ 
             #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="h",fontface="bold")),
             gg.k600.scatter+
               theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               scale_y_continuous(limits=c(0.1,3),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="i",fontface="bold"))+
               ggtitle("(i)")
            
  )
  
  #Plot them using patchwork####
  (gg.largefigure.patchwork.3x3<-wrap_plots(List,ncol = 3,nrow = 3)&theme(plot.margin = unit(c(3,3,3,3),"pt")))
    #Code to tag panels in patchwork
    #+plot_annotation(tag_levels = c('a'), tag_prefix = '(',tag_suffix = ')')
  #FIGURE: all 12 panels, print out as jpg####
  #Could do a 3x3 with width 6, height = 5
  ggsave("plots/WIAP-FigureX-Obj5LargeMultipanelFigure3x3.jpg", plot=gg.largefigure.patchwork.3x3, width=6, height=5.5,units="in", dpi=300)
  
  #Export the rsme table for supplemental####
  #write_csv(rsme.df,"outputs/WIAP-SupplementalTable-rsme.csv")
  #Export the aicc table for supplemental####
  write_csv(aicc.df.export,"outputs/WIAP-SupplementalTable-aicc-surfaceArea.csv")

  #Do stats on selected models####
  #First combine the cutoffs based on which was selected for segmented vs. logistic
    #This adds in a mutual column called cutoff
  aicc.df.summary<-bind_rows(aicc.df.export%>%filter(type%in%c("NEP","R","TN"))%>%dplyr::select(type,segmented.cutoff)%>%mutate(cutoff=segmented.cutoff),
                             aicc.df.export%>%filter(type%in%c("dielTempRange","TP","chla","ph","k600","methaneFluxes"))%>%dplyr::select(type,logistic.cutoff)%>%mutate(cutoff=logistic.cutoff))
  #Now summarize all cutoffs for manuscript
  aicc.df.summary%>%ungroup()%>%summarize(mean=mean(cutoff),stderr=sd(cutoff)/sqrt(length(cutoff)),median=median(cutoff))                        
    
