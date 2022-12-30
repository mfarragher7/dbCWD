# Create some theoretical and empirical figures based on our pond definition ideas
# This one carries script 11 forward by doing the analysis on depth
# This script needs script 11 run first to get the data sets in
# Created 02Oct2021 by Dave Richardson (hereafter: DCR)

#Things to do####
#Run all analyses for depth vs. metrics for 
  #GPP-n=27, not enough data
  #R-n=27, not enough data
  #NEP-n=27, not enough data
  #methane flux-DONE
  #DTR-DONE
  #k600 - NOT ENOUGH DATA
  #pH-DONE
  #TP-DONE
  #TN-DONE
  #chl a-DONE


#Export all AICc's 
#Select optimal model for each
#Create large figure
#Export AICC results
#summarize breakpoints (if they exist)

#Initialize aicc.df.depth####
aicc.df.depth<-tibble(type=character(),flat=numeric(),linear=numeric(),segmented=numeric(),logistic=numeric(),segmented.cutoff=numeric(), logistic.cutoff=numeric(), segmented.cutoff.SE=numeric(), logistic.cutoff.SE=numeric(),)

#pH analysis for depth####
#*ph: trim and order to only non NA values####
ph.trim.depth<-crossSystemDF%>%dplyr::select(ph,max_depth_m)%>%arrange(max_depth_m)%>%filter(!is.na(ph))%>%filter(!is.na(max_depth_m))%>%filter(ph>0)%>%mutate(log_max_depth_m=log10(max_depth_m))

#*pH: Flat model####
ph.flat.depth<-lm(ph~1,data=ph.trim.depth)
summary(ph.flat.depth)

#*ph: linear model####
ph.lm.depth<-lm(ph~log_max_depth_m,data=ph.trim.depth)
summary(ph.lm.depth)

#*ph: Segmented regression####
#Try the segmented package version: https://rpubs.com/MarkusLoew/12164#
ph.segmented.depth<-segmented(lm(ph ~ log_max_depth_m,data=ph.trim.depth),seg.z=log_max_depth_m,psi=c(0.25)) #initial guess makes a difference
summary(ph.segmented.depth) 
10^ph.segmented.depth$psi[,"Est."] #back calculate breakpoint
slope(ph.segmented.depth) #get the slopes  
ph.segmented.depth.fitted<-fitted(ph.segmented.depth) #get the fitted data
ph.segmented.depthModel<-tibble(max_depth_m=10^ph.trim.depth$log_max_depth_m,log_max_depth_m=ph.trim.depth$log_max_depth_m,ph.segmented.depth.fitted=ph.segmented.depth.fitted) #calculate the fit with the back transformed x axis

#*ph: fit a nonlinear logistic curve####
ph.logistic.depth<-nls(ph~A+(B-A)/(1+exp((xmid-log_max_depth_m)/scal)),
                 start=list(A=6,B=8,xmid=0.25,scal=-0.2),data=ph.trim.depth,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
summary(ph.logistic.depth)
#Get the cutoff from the logistic parameter as the xmid inflection point
ph.logistic.depth.cutoff<-10^ph.logistic.depth$m$getAllPars()[3]

#**ph: flat model AICc####
ph.flat.depth.aicc<-AIC(ph.flat.depth)+ (2 * 1^2 + 2 * 1)/(length(ph.flat.depth$residuals) - 1 - 1)

#**ph: linear model AICc####
ph.lm.depth.aicc<-AIC(ph.lm.depth)+ (2 * 2^2 + 2 * 2)/(length(ph.flat.depth$residuals) - 2 - 1)

#**ph: Segmented model AICc####  
ph.segmented.depth.aicc<-AIC(ph.segmented.depth)+ (2 * 4^2 + 2 * 4)/(length(ph.flat.depth$residuals) - 4 - 1)

#**ph: logistic model AICc#### 
ph.logistic.depth.aicc<-AIC(ph.logistic.depth)+ (2 * 4^2 + 2 * 4)/(length(ph.flat.depth$residuals) - 4 - 1)

#**ph: Store aicc from each model as a row####
#Keep adding to this with each analysis
aicc.df.depth<-aicc.df.depth%>%add_row(type="ph",flat=ph.flat.depth.aicc,linear=ph.lm.depth.aicc,segmented=ph.segmented.depth.aicc,logistic=ph.logistic.depth.aicc,
                                        segmented.cutoff=10^ph.segmented.depth$psi[,"Est."],logistic.cutoff=ph.logistic.depth.cutoff, 
                                        segmented.cutoff.SE=10^ph.segmented.depth$psi[,"St.Err"], logistic.cutoff.SE=10^coef(summary(ph.logistic.depth))[,"Std. Error"][3])

#*ph: plot models overlaid####
ggplot(data=ph.trim.depth,aes(y=ph, x=log_max_depth_m))+geom_point()+
  geom_hline(yintercept=mean(ph.trim.depth$ph,na.rm=TRUE),color="orange")+
  geom_smooth(method='lm', se=FALSE,color="red")+
  geom_line(data=ph.segmented.depthModel,aes(y=ph.segmented.depth.fitted,x=log_max_depth_m),color="blue")+ #This is the segmented regression
  geom_line(data=tibble(logistic_predict=predict(ph.logistic.depth),log_max_depth_m=ph.trim.depth$log_max_depth_m), aes(y=logistic_predict,x=log_max_depth_m),color="purple")+
  geom_text(data=data.frame(aicc=round(ph.flat.depth.aicc,1)), aes(label = paste("flat=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 1)+
  geom_text(data=data.frame(aicc=round(ph.lm.depth.aicc,1)), aes(label = paste("linear=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 3)+
  geom_text(data=data.frame(aicc=round(ph.segmented.depth.aicc,1)), aes(label = paste("segmented=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 5)+
  geom_text(data=data.frame(aicc=round(ph.logistic.depth.aicc,1)), aes(label = paste("logistic=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 7)+
  theme_bw()

#*ph: plot optimal fit#### 
gg.ph.depth.scatter<-ggplot(data=ph.trim.depth,aes(x=max_depth_m,y=ph))+
  geom_vline(xintercept=10^ph.segmented.depth$psi[,"Est."],color="black",linetype=1,size=1)+ #vertical line indicating cutoff from optimal model if it exists, change that here
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(0.1,200),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  #geom_line(data=tibble(logistic_predict=predict(ph.logistic.depth),log_max_depth_m=ph.trim.depth$log_max_depth_m), aes(y=logistic_predict,x=10^log_max_depth_m),color="black",size=1.2)+ #add the logistic fit
  geom_line(data=ph.segmented.depthModel,aes(y=ph.segmented.depth.fitted,x=10^log_max_depth_m),color="black",size=1.2)+ #This is the segmented regression, have to raise the y variable 10^ if it has been transformed
  #geom_line(aes(y=predict),color="black",size=1.2)+
  #geom_line(data=methanefluxes.loess.fit,aes(x=10^log_area_ha,y=loess_fit),color="purple")+
  #geom_line(data=temp,aes(x=area_ha,y=loess_fit),color="green")+
  #scale_y_log10()+ #comment this out if the y axis is not transformed
  labs(y=bquote(pH),x=bquote(Max~depth~(m)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
  )
#**pH: FIGURE export as jpg####
ggsave("plots/WIAP-FigureX-pHScatter-depth.jpg", plot=gg.ph.depth.scatter, width=3, height=3,units="in", dpi=300)

#chla analysis for depth####
#*chla: trim and order to only non NA values####
chla.trim.depth<-crossSystemDF%>%dplyr::select(chla_ugpl,max_depth_m)%>%arrange(max_depth_m)%>%filter(!is.na(chla_ugpl))%>%filter(!is.na(max_depth_m))%>%filter(chla_ugpl>0)%>%mutate(log_max_depth_m=log10(max_depth_m),log_chla_ugpl=log10(chla_ugpl))
  #ggplot(data=chla.trim.depth,aes(y=log_chla_ugpl,x=log_max_depth_m))+geom_point() #plot to check if data needs to be log transformed 

#*chla: Flat model####
chla.flat.depth<-lm(log_chla_ugpl~1,data=chla.trim.depth)
summary(chla.flat.depth)

#*chla: linear model####
chla.lm.depth<-lm(log_chla_ugpl~log_max_depth_m,data=chla.trim.depth)
summary(chla.lm.depth)

#*chla: Segmented regression####
#Try the segmented package version: https://rpubs.com/MarkusLoew/12164#
chla.segmented.depth<-segmented(lm(log_chla_ugpl ~ log_max_depth_m,data=chla.trim.depth),seg.z=log_max_depth_m,psi=c(0.25)) #initial guess makes a difference
summary(chla.segmented.depth) 
10^chla.segmented.depth$psi[,"Est."] #back calculate breakpoint
slope(chla.segmented.depth) #get the slopes  
chla.segmented.depth.fitted<-fitted(chla.segmented.depth) #get the fitted data
chla.segmented.depthModel<-tibble(max_depth_m=10^chla.trim.depth$log_max_depth_m,log_max_depth_m=chla.trim.depth$log_max_depth_m,chla.segmented.depth.fitted=chla.segmented.depth.fitted) #calculate the fit with the back transformed x axis

#*chla: fit a nonlinear logistic curve####
chla.logistic.depth<-nls(log_chla_ugpl~A+(B-A)/(1+exp((xmid-log_max_depth_m)/scal)),
                       start=list(A=0,B=1.5,xmid=0,scal=-0.2),data=chla.trim.depth,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
summary(chla.logistic.depth)
#Get the cutoff from the logistic parameter as the xmid inflection point
chla.logistic.depth.cutoff<-10^chla.logistic.depth$m$getAllPars()[3]

#**chla: flat model AICc####
chla.flat.depth.aicc<-AIC(chla.flat.depth)+ (2 * 1^2 + 2 * 1)/(length(chla.flat.depth$residuals) - 1 - 1)

#**chla: linear model AICc####
chla.lm.depth.aicc<-AIC(chla.lm.depth)+ (2 * 2^2 + 2 * 2)/(length(chla.flat.depth$residuals) - 2 - 1)

#**chla: Segmented model AICc####  
chla.segmented.depth.aicc<-AIC(chla.segmented.depth)+ (2 * 4^2 + 2 * 4)/(length(chla.flat.depth$residuals) - 4 - 1)

#**chla: logistic model AICc#### 
chla.logistic.depth.aicc<-AIC(chla.logistic.depth)+ (2 * 4^2 + 2 * 4)/(length(chla.flat.depth$residuals) - 4 - 1)

#**chla: Store aicc from each model as a row####
#Keep adding to this with each analysis
aicc.df.depth<-aicc.df.depth%>%add_row(type="chla",flat=chla.flat.depth.aicc,linear=chla.lm.depth.aicc,segmented=chla.segmented.depth.aicc,logistic=chla.logistic.depth.aicc,
                                       segmented.cutoff=10^chla.segmented.depth$psi[,"Est."],logistic.cutoff=chla.logistic.depth.cutoff, 
                                       segmented.cutoff.SE=10^chla.segmented.depth$psi[,"St.Err"], logistic.cutoff.SE=10^coef(summary(chla.logistic.depth))[,"Std. Error"][3])

#*chla: plot models overlaid####
ggplot(data=chla.trim.depth,aes(y=log_chla_ugpl, x=log_max_depth_m))+geom_point()+
  geom_hline(yintercept=mean(chla.trim.depth$log_chla_ugpl,na.rm=TRUE),color="orange")+
  geom_smooth(method='lm', se=FALSE,color="red")+
  geom_line(data=chla.segmented.depthModel,aes(y=chla.segmented.depth.fitted,x=log_max_depth_m),color="blue")+ #This is the segmented regression
  geom_line(data=tibble(logistic_predict=predict(chla.logistic.depth),log_max_depth_m=chla.trim.depth$log_max_depth_m), aes(y=logistic_predict,x=log_max_depth_m),color="purple")+
  geom_text(data=data.frame(aicc=round(chla.flat.depth.aicc,1)), aes(label = paste("flat=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 1)+
  geom_text(data=data.frame(aicc=round(chla.lm.depth.aicc,1)), aes(label = paste("linear=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 3)+
  geom_text(data=data.frame(aicc=round(chla.segmented.depth.aicc,1)), aes(label = paste("segmented=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 5)+
  geom_text(data=data.frame(aicc=round(chla.logistic.depth.aicc,1)), aes(label = paste("logistic=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 7)+
  theme_bw()

#*chla: plot optimal fit#### 
gg.chla.depth.scatter<-ggplot(data=chla.trim.depth,aes(x=max_depth_m,y=chla_ugpl))+
  geom_vline(xintercept=chla.logistic.depth.cutoff,color="black",linetype=1,size=1)+ #vertical line indicating cutoff from optimal model if it exists, change that here
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(0.1,200),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  geom_line(data=tibble(logistic_predict=predict(chla.logistic.depth),log_max_depth_m=chla.trim.depth$log_max_depth_m), aes(y=10^logistic_predict,x=10^log_max_depth_m),color="black",size=1.2)+ #add the logistic fit, have to raise the y variable 10^ if it has been transformed
  #geom_line(data=chla.segmented.depthModel,aes(y=chla.segmented.depth.fitted,x=10^log_max_depth_m),color="black",size=1.2)+ #This is the segmented regression, have to raise the y variable 10^ if it has been transformed
  #geom_line(aes(y=predict),color="black",size=1.2)+
  #geom_line(data=methanefluxes.loess.fit,aes(x=10^log_area_ha,y=loess_fit),color="purple")+
  #geom_line(data=temp,aes(x=area_ha,y=loess_fit),color="green")+
  scale_y_log10()+
  labs(y=bquote(Chl~italic(a)~(mu*g~L^-1)),x=bquote(Max~depth~(m)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
  )
#**chla: FIGURE export as jpg####
ggsave("plots/WIAP-FigureX-chlaScatter-depth.jpg", plot=gg.chla.depth.scatter, width=3, height=3,units="in", dpi=300)

#tn analysis for depth####
#*tn: trim and order to only non NA values####
tn.trim.depth<-crossSystemDF%>%dplyr::select(tn_ugpl,max_depth_m)%>%arrange(max_depth_m)%>%filter(!is.na(tn_ugpl))%>%filter(!is.na(max_depth_m))%>%filter(tn_ugpl>0)%>%mutate(log_max_depth_m=log10(max_depth_m),log_tn_ugpl=log10(tn_ugpl))
#ggplot(data=tn.trim.depth,aes(y=log_tn_ugpl,x=log_max_depth_m))+geom_point() #plot to check if data needs to be log transformed 

#*tn: Flat model####
tn.flat.depth<-lm(log_tn_ugpl~1,data=tn.trim.depth)
summary(tn.flat.depth)

#*tn: linear model####
tn.lm.depth<-lm(log_tn_ugpl~log_max_depth_m,data=tn.trim.depth)
summary(tn.lm.depth)

#*tn: Segmented regression####
#Try the segmented package version: https://rpubs.com/MarkusLoew/12164#
tn.segmented.depth<-segmented(lm(log_tn_ugpl ~ log_max_depth_m,data=tn.trim.depth),seg.z=log_max_depth_m,psi=c(0.25)) #initial guess makes a difference
summary(tn.segmented.depth) 
10^tn.segmented.depth$psi[,"Est."] #back calculate breakpoint
slope(tn.segmented.depth) #get the slopes  
tn.segmented.depth.fitted<-fitted(tn.segmented.depth) #get the fitted data
tn.segmented.depthModel<-tibble(max_depth_m=10^tn.trim.depth$log_max_depth_m,log_max_depth_m=tn.trim.depth$log_max_depth_m,tn.segmented.depth.fitted=tn.segmented.depth.fitted) #calculate the fit with the back transformed x axis

#*tn: fit a nonlinear logistic curve####
tn.logistic.depth<-nls(log_tn_ugpl~A+(B-A)/(1+exp((xmid-log_max_depth_m)/scal)),
                         start=list(A=0,B=1.5,xmid=0,scal=-0.2),data=tn.trim.depth,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
summary(tn.logistic.depth)
#Get the cutoff from the logistic parameter as the xmid inflection point
tn.logistic.depth.cutoff<-10^tn.logistic.depth$m$getAllPars()[3]

#**tn: flat model AICc####
tn.flat.depth.aicc<-AIC(tn.flat.depth)+ (2 * 1^2 + 2 * 1)/(length(tn.flat.depth$residuals) - 1 - 1)

#**tn: linear model AICc####
tn.lm.depth.aicc<-AIC(tn.lm.depth)+ (2 * 2^2 + 2 * 2)/(length(tn.flat.depth$residuals) - 2 - 1)

#**tn: Segmented model AICc####  
tn.segmented.depth.aicc<-AIC(tn.segmented.depth)+ (2 * 4^2 + 2 * 4)/(length(tn.flat.depth$residuals) - 4 - 1)

#**tn: logistic model AICc#### 
tn.logistic.depth.aicc<-AIC(tn.logistic.depth)+ (2 * 4^2 + 2 * 4)/(length(tn.flat.depth$residuals) - 4 - 1)

#**tn: Store aicc from each model as a row####
#Keep adding to this with each analysis
aicc.df.depth<-aicc.df.depth%>%add_row(type="tn",flat=tn.flat.depth.aicc,linear=tn.lm.depth.aicc,segmented=tn.segmented.depth.aicc,logistic=tn.logistic.depth.aicc,
                                       segmented.cutoff=10^tn.segmented.depth$psi[,"Est."],logistic.cutoff=tn.logistic.depth.cutoff, 
                                       segmented.cutoff.SE=10^tn.segmented.depth$psi[,"St.Err"], logistic.cutoff.SE=10^coef(summary(tn.logistic.depth))[,"Std. Error"][3])

#*tn: plot models overlaid####
ggplot(data=tn.trim.depth,aes(y=log_tn_ugpl, x=log_max_depth_m))+geom_point()+
  geom_hline(yintercept=mean(tn.trim.depth$log_tn_ugpl,na.rm=TRUE),color="orange")+
  geom_smooth(method='lm', se=FALSE,color="red")+
  geom_line(data=tn.segmented.depthModel,aes(y=tn.segmented.depth.fitted,x=log_max_depth_m),color="blue")+ #This is the segmented regression
  geom_line(data=tibble(logistic_predict=predict(tn.logistic.depth),log_max_depth_m=tn.trim.depth$log_max_depth_m), aes(y=logistic_predict,x=log_max_depth_m),color="purple")+
  geom_text(data=data.frame(aicc=round(tn.flat.depth.aicc,1)), aes(label = paste("flat=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 1)+
  geom_text(data=data.frame(aicc=round(tn.lm.depth.aicc,1)), aes(label = paste("linear=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 3)+
  geom_text(data=data.frame(aicc=round(tn.segmented.depth.aicc,1)), aes(label = paste("segmented=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 5)+
  geom_text(data=data.frame(aicc=round(tn.logistic.depth.aicc,1)), aes(label = paste("logistic=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 7)+
  theme_bw()

#*tn: plot optimal fit#### 
gg.tn.depth.scatter<-ggplot(data=tn.trim.depth,aes(x=max_depth_m,y=tn_ugpl))+
  geom_vline(xintercept=10^tn.segmented.depth$psi[,"Est."],color="black",linetype=1,size=1)+ #vertical line indicating cutoff from optimal model if it exists, change that here, either 10^tn.segmented.depth$psi[,"Est."] for segemented or tn.logistic.depth.cutoff for logistic
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(0.1,200),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  #geom_line(data=tibble(logistic_predict=predict(tn.logistic.depth),log_max_depth_m=tn.trim.depth$log_max_depth_m), aes(y=10^logistic_predict,x=10^log_max_depth_m),color="black",size=1.2)+ #add the logistic fit, have to raise the y variable 10^ if it has been transformed
  geom_line(data=tn.segmented.depthModel,aes(y=10^tn.segmented.depth.fitted,x=10^log_max_depth_m),color="black",size=1.2)+ #This is the segmented regression, have to raise the y variable 10^ if it has been transformed
  #geom_line(aes(y=predict),color="black",size=1.2)+
  #geom_line(data=methanefluxes.loess.fit,aes(x=10^log_area_ha,y=loess_fit),color="purple")+
  #geom_line(data=temp,aes(x=area_ha,y=loess_fit),color="green")+
  scale_y_log10()+
  labs(y=bquote(TN~(mu*g~L^-1)),x=bquote(Max~depth~(m)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
  )
#**tn: FIGURE export as jpg####
ggsave("plots/WIAP-FigureX-tnScatter-depth.jpg", plot=gg.tn.depth.scatter, width=3, height=3,units="in", dpi=300)

#tp analysis for depth####
#*tp: trim and order to only non NA values####
tp.trim.depth<-crossSystemDF%>%dplyr::select(tp_ugpl,max_depth_m)%>%arrange(max_depth_m)%>%filter(!is.na(tp_ugpl))%>%filter(!is.na(max_depth_m))%>%filter(tp_ugpl>0)%>%mutate(log_max_depth_m=log10(max_depth_m),log_tp_ugpl=log10(tp_ugpl))
#ggplot(data=tp.trim.depth,aes(y=log_tp_ugpl,x=log_max_depth_m))+geom_point() #plot to check if data needs to be log transformed 

#*tp: Flat model####
tp.flat.depth<-lm(log_tp_ugpl~1,data=tp.trim.depth)
summary(tp.flat.depth)

#*tp: linear model####
tp.lm.depth<-lm(log_tp_ugpl~log_max_depth_m,data=tp.trim.depth)
summary(tp.lm.depth)

#*tp: Segmented regression####
#Try the segmented package version: https://rpubs.com/MarkusLoew/12164#
tp.segmented.depth<-segmented(lm(log_tp_ugpl ~ log_max_depth_m,data=tp.trim.depth),seg.z=log_max_depth_m,psi=c(0.25)) #initial guess makes a difference
summary(tp.segmented.depth) 
10^tp.segmented.depth$psi[,"Est."] #back calculate breakpoint
slope(tp.segmented.depth) #get the slopes  
tp.segmented.depth.fitted<-fitted(tp.segmented.depth) #get the fitted data
tp.segmented.depthModel<-tibble(max_depth_m=10^tp.trim.depth$log_max_depth_m,log_max_depth_m=tp.trim.depth$log_max_depth_m,tp.segmented.depth.fitted=tp.segmented.depth.fitted) #calculate the fit with the back transformed x axis

#*tp: fit a nonlinear logistic curve####
tp.logistic.depth<-nls(log_tp_ugpl~A+(B-A)/(1+exp((xmid-log_max_depth_m)/scal)),
                       start=list(A=0,B=1.5,xmid=0,scal=-0.2),data=tp.trim.depth,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
summary(tp.logistic.depth)
#Get the cutoff from the logistic parameter as the xmid inflection point
tp.logistic.depth.cutoff<-10^tp.logistic.depth$m$getAllPars()[3]

#**tp: flat model AICc####
tp.flat.depth.aicc<-AIC(tp.flat.depth)+ (2 * 1^2 + 2 * 1)/(length(tp.flat.depth$residuals) - 1 - 1)

#**tp: linear model AICc####
tp.lm.depth.aicc<-AIC(tp.lm.depth)+ (2 * 2^2 + 2 * 2)/(length(tp.flat.depth$residuals) - 2 - 1)

#**tp: Segmented model AICc####  
tp.segmented.depth.aicc<-AIC(tp.segmented.depth)+ (2 * 4^2 + 2 * 4)/(length(tp.flat.depth$residuals) - 4 - 1)

#**tp: logistic model AICc#### 
tp.logistic.depth.aicc<-AIC(tp.logistic.depth)+ (2 * 4^2 + 2 * 4)/(length(tp.flat.depth$residuals) - 4 - 1)

#**tp: Store aicc from each model as a row####
#Keep adding to this with each analysis
aicc.df.depth<-aicc.df.depth%>%add_row(type="tp",flat=tp.flat.depth.aicc,linear=tp.lm.depth.aicc,segmented=tp.segmented.depth.aicc,logistic=tp.logistic.depth.aicc,
                                       segmented.cutoff=10^tp.segmented.depth$psi[,"Est."],logistic.cutoff=tp.logistic.depth.cutoff, 
                                       segmented.cutoff.SE=10^tp.segmented.depth$psi[,"St.Err"], logistic.cutoff.SE=10^coef(summary(tp.logistic.depth))[,"Std. Error"][3])

#*tp: plot models overlaid####
ggplot(data=tp.trim.depth,aes(y=log_tp_ugpl, x=log_max_depth_m))+geom_point()+
  geom_hline(yintercept=mean(tp.trim.depth$log_tp_ugpl,na.rm=TRUE),color="orange")+
  geom_smooth(method='lm', se=FALSE,color="red")+
  geom_line(data=tp.segmented.depthModel,aes(y=tp.segmented.depth.fitted,x=log_max_depth_m),color="blue")+ #This is the segmented regression
  geom_line(data=tibble(logistic_predict=predict(tp.logistic.depth),log_max_depth_m=tp.trim.depth$log_max_depth_m), aes(y=logistic_predict,x=log_max_depth_m),color="purple")+
  geom_text(data=data.frame(aicc=round(tp.flat.depth.aicc,1)), aes(label = paste("flat=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 1)+
  geom_text(data=data.frame(aicc=round(tp.lm.depth.aicc,1)), aes(label = paste("linear=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 3)+
  geom_text(data=data.frame(aicc=round(tp.segmented.depth.aicc,1)), aes(label = paste("segmented=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 5)+
  geom_text(data=data.frame(aicc=round(tp.logistic.depth.aicc,1)), aes(label = paste("logistic=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 7)+
  theme_bw()

#*tp: plot optimal fit#### 
gg.tp.depth.scatter<-ggplot(data=tp.trim.depth,aes(x=max_depth_m,y=tp_ugpl))+
  geom_vline(xintercept=10^tp.segmented.depth$psi[,"Est."],color="black",linetype=1,size=1)+ #vertical line indicating cutoff from optimal model if it exists, change that here, either 10^tp.segmented.depth$psi[,"Est."] for segemented or tp.logistic.depth.cutoff for logistic
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(0.1,200),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  #geom_line(data=tibble(logistic_predict=predict(tp.logistic.depth),log_max_depth_m=tp.trim.depth$log_max_depth_m), aes(y=10^logistic_predict,x=10^log_max_depth_m),color="black",size=1.2)+ #add the logistic fit, have to raise the y variable 10^ if it has been transformed
  geom_line(data=tp.segmented.depthModel,aes(y=10^tp.segmented.depth.fitted,x=10^log_max_depth_m),color="black",size=1.2)+ #This is the segmented regression, have to raise the y variable 10^ if it has been transformed
  #geom_line(aes(y=predict),color="black",size=1.2)+
  #geom_line(data=methanefluxes.loess.fit,aes(x=10^log_area_ha,y=loess_fit),color="purple")+
  #geom_line(data=temp,aes(x=area_ha,y=loess_fit),color="green")+
  scale_y_log10()+
  labs(y=bquote(TP~(mu*g~L^-1)),x=bquote(Max~depth~(m)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
  )
#**tp: FIGURE export as jpg####
ggsave("plots/WIAP-FigureX-tpScatter-depth.jpg", plot=gg.tp.depth.scatter, width=3, height=3,units="in", dpi=300)

#NEP analysis for depth####
#Not enough data points
NEP.trim.depth<-metabolismDF%>%dplyr::select(NEP_mmolperm3perday,maxDepth_m)%>%arrange(maxDepth_m)%>%filter(!is.na(NEP_mmolperm3perday))%>%filter(!is.na(maxDepth_m))%>%mutate(log_max_depth_m=log10(maxDepth_m))
#ggplot(data=NEP.trim.depth,aes(y=NEP_mmolperm3perday,x=log_max_depth_m))+geom_point() #plot to check if data needs to be log transformed 

#GPP analysis for depth####
#Not enough data points
GPP.trim.depth<-metabolismDF%>%dplyr::select(GPP_mmolperm3perday,maxDepth_m)%>%arrange(maxDepth_m)%>%filter(!is.na(GPP_mmolperm3perday))%>%filter(!is.na(maxDepth_m))%>%mutate(log_max_depth_m=log10(maxDepth_m))
ggplot(data=GPP.trim.depth,aes(y=GPP_mmolperm3perday,x=log_max_depth_m))+geom_point() #plot to check if data needs to be log transformed 

#R analysis for depth####
#Not enough data points
R.trim.depth<-metabolismDF%>%dplyr::select(R_mmolperm3perday,maxDepth_m)%>%arrange(maxDepth_m)%>%filter(!is.na(R_mmolperm3perday))%>%filter(!is.na(maxDepth_m))%>%mutate(log_max_depth_m=log10(maxDepth_m))
ggplot(data=R.trim.depth,aes(y=R_mmolperm3perday,x=log_max_depth_m))+geom_point() #plot to check if data needs to be log transformed 


#methaneFluxes analysis for depth####
#*methaneFluxes: trim and order to only non NA values####
methaneFluxes.trim.depth<-methaneFluxesDF%>%mutate(max_depth_m=Depth_max_m,log_max_depth_m=log10(Depth_max_m),log_fch4_mgCm2d=log10(fch4_mgCm2d))%>%arrange(max_depth_m)%>%filter(!is.na(fch4_mgCm2d))%>%filter(!is.na(max_depth_m))
#ggplot(data=methaneFluxes.trim.depth,aes(y=log_fch4_mgCm2d,x=log_max_depth_m))+geom_point() #plot to check if data needs to be log transformed 

#*methaneFluxes: Flat model####
methaneFluxes.flat.depth<-lm(log_fch4_mgCm2d~1,data=methaneFluxes.trim.depth)
summary(methaneFluxes.flat.depth)

#*methaneFluxes: linear model####
methaneFluxes.lm.depth<-lm(log_fch4_mgCm2d~log_max_depth_m,data=methaneFluxes.trim.depth)
summary(methaneFluxes.lm.depth)

#*methaneFluxes: Segmented regression####
#Try the segmented package version: htmethaneFluxess://rpubs.com/MarkusLoew/12164#
methaneFluxes.segmented.depth<-segmented(lm(log_fch4_mgCm2d ~ log_max_depth_m,data=methaneFluxes.trim.depth),seg.z=log_max_depth_m,psi=c(0.25)) #initial guess makes a difference
summary(methaneFluxes.segmented.depth) 
10^methaneFluxes.segmented.depth$psi[,"Est."] #back calculate breakpoint
slope(methaneFluxes.segmented.depth) #get the slopes  
methaneFluxes.segmented.depth.fitted<-fitted(methaneFluxes.segmented.depth) #get the fitted data
methaneFluxes.segmented.depthModel<-tibble(max_depth_m=10^methaneFluxes.trim.depth$log_max_depth_m,log_max_depth_m=methaneFluxes.trim.depth$log_max_depth_m,methaneFluxes.segmented.depth.fitted=methaneFluxes.segmented.depth.fitted) #calculate the fit with the back transformed x axis

#*methaneFluxes: fit a nonlinear logistic curve####
methaneFluxes.logistic.depth<-nls(log_fch4_mgCm2d~A+(B-A)/(1+exp((xmid-log_max_depth_m)/scal)),
                       start=list(A=0.5,B=2.5,xmid=0.5,scal=-0.2),data=methaneFluxes.trim.depth,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
summary(methaneFluxes.logistic.depth)
#Get the cutoff from the logistic parameter as the xmid inflection point
methaneFluxes.logistic.depth.cutoff<-10^methaneFluxes.logistic.depth$m$getAllPars()[3]

#**methaneFluxes: flat model AICc####
methaneFluxes.flat.depth.aicc<-AIC(methaneFluxes.flat.depth)+ (2 * 1^2 + 2 * 1)/(length(methaneFluxes.flat.depth$residuals) - 1 - 1)

#**methaneFluxes: linear model AICc####
methaneFluxes.lm.depth.aicc<-AIC(methaneFluxes.lm.depth)+ (2 * 2^2 + 2 * 2)/(length(methaneFluxes.flat.depth$residuals) - 2 - 1)

#**methaneFluxes: Segmented model AICc####  
methaneFluxes.segmented.depth.aicc<-AIC(methaneFluxes.segmented.depth)+ (2 * 4^2 + 2 * 4)/(length(methaneFluxes.flat.depth$residuals) - 4 - 1)

#**methaneFluxes: logistic model AICc#### 
methaneFluxes.logistic.depth.aicc<-AIC(methaneFluxes.logistic.depth)+ (2 * 4^2 + 2 * 4)/(length(methaneFluxes.flat.depth$residuals) - 4 - 1)

#**methaneFluxes: Store aicc from each model as a row####
#Keep adding to this with each analysis
aicc.df.depth<-aicc.df.depth%>%add_row(type="methaneFluxes",flat=methaneFluxes.flat.depth.aicc,linear=methaneFluxes.lm.depth.aicc,segmented=methaneFluxes.segmented.depth.aicc,logistic=methaneFluxes.logistic.depth.aicc,
                                       segmented.cutoff=10^methaneFluxes.segmented.depth$psi[,"Est."],logistic.cutoff=methaneFluxes.logistic.depth.cutoff, 
                                       segmented.cutoff.SE=10^methaneFluxes.segmented.depth$psi[,"St.Err"], logistic.cutoff.SE=10^coef(summary(methaneFluxes.logistic.depth))[,"Std. Error"][3])

#*methaneFluxes: plot models overlaid####
ggplot(data=methaneFluxes.trim.depth,aes(y=log_fch4_mgCm2d, x=log_max_depth_m))+geom_point()+
  geom_hline(yintercept=mean(methaneFluxes.trim.depth$log_fch4_mgCm2d,na.rm=TRUE),color="orange")+
  geom_smooth(method='lm', se=FALSE,color="red")+
  geom_line(data=methaneFluxes.segmented.depthModel,aes(y=methaneFluxes.segmented.depth.fitted,x=log_max_depth_m),color="blue")+ #This is the segmented regression
  geom_line(data=tibble(logistic_predict=predict(methaneFluxes.logistic.depth),log_max_depth_m=methaneFluxes.trim.depth$log_max_depth_m), aes(y=logistic_predict,x=log_max_depth_m),color="purple")+
  geom_text(data=data.frame(aicc=round(methaneFluxes.flat.depth.aicc,1)), aes(label = paste("flat=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 1)+
  geom_text(data=data.frame(aicc=round(methaneFluxes.lm.depth.aicc,1)), aes(label = paste("linear=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 3)+
  geom_text(data=data.frame(aicc=round(methaneFluxes.segmented.depth.aicc,1)), aes(label = paste("segmented=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 5)+
  geom_text(data=data.frame(aicc=round(methaneFluxes.logistic.depth.aicc,1)), aes(label = paste("logistic=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 7)+
  theme_bw()

#*methaneFluxes: plot optimal fit#### 
gg.methaneFluxes.depth.scatter<-ggplot(data=methaneFluxes.trim.depth,aes(x=max_depth_m,y=fch4_mgCm2d))+
  #geom_vline(xintercept=10^methaneFluxes.segmented.depth$psi[,"Est."],color="black",linetype=1,size=1)+ #vertical line indicating cutoff from optimal model if it exists, change that here, either 10^methaneFluxes.segmented.depth$psi[,"Est."] for segemented or methaneFluxes.logistic.depth.cutoff for logistic
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(0.1,200),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  #geom_line(data=tibble(logistic_predict=predict(methaneFluxes.logistic.depth),log_max_depth_m=methaneFluxes.trim.depth$log_max_depth_m), aes(y=10^logistic_predict,x=10^log_max_depth_m),color="black",size=1.2)+ #add the logistic fit, have to raise the y variable 10^ if it has been transformed
  #geom_line(data=methaneFluxes.segmented.depthModel,aes(y=10^methaneFluxes.segmented.depth.fitted,x=10^log_max_depth_m),color="black",size=1.2)+ #This is the segmented regression, have to raise the y variable 10^ if it has been transformed
  geom_line(data=tibble(fitted_values=methaneFluxes.lm.depth$fitted.values,max_depth_m=methaneFluxes.trim.depth$max_depth_m),aes(y=10^fitted_values,x=max_depth_m),size=1.2)+ #linear fit
  #geom_line(aes(y=predict),color="black",size=1.2)+
  #geom_line(data=temp,aes(x=area_ha,y=loess_fit),color="green")+
  scale_y_log10()+
  labs(y=bquote(CH[4]~flux~(mg~C~m^-2~d^-1)),x=bquote(Max~depth~(m)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
  )
#**methaneFluxes: FIGURE export as jpg####
ggsave("plots/WIAP-FigureX-methaneFluxesScatter-depth.jpg", plot=gg.methaneFluxes.depth.scatter, width=3, height=3,units="in", dpi=300)



#Diel Temperature Range analysis for depth####
#*Diel Temperature Range: trim and order to only non NA values####
dtr.trim.depth<-DielTempRange_all%>%mutate(log_max_depth_m=log10(max_depth_m))%>%arrange(max_depth_m)%>%filter(!is.na(diel_rng))%>%filter(!is.na(max_depth_m))
#ggplot(data=dtr.trim.depth,aes(y=diel_rng,x=log_max_depth_m))+geom_point() #plot to check if data needs to be log transformed 

#*dtr: Flat model####
dtr.flat.depth<-lm(diel_rng~1,data=dtr.trim.depth)
summary(dtr.flat.depth)

#*dtr: linear model####
dtr.lm.depth<-lm(diel_rng~log_max_depth_m,data=dtr.trim.depth)
summary(dtr.lm.depth)

#*dtr: Segmented regression####
#Try the segmented package version: htdtrs://rpubs.com/MarkusLoew/12164#
dtr.segmented.depth<-segmented(lm(diel_rng ~ log_max_depth_m,data=dtr.trim.depth),seg.z=log_max_depth_m,psi=c(0.25)) #initial guess makes a difference
summary(dtr.segmented.depth) 
10^dtr.segmented.depth$psi[,"Est."] #back calculate breakpoint
slope(dtr.segmented.depth) #get the slopes  
dtr.segmented.depth.fitted<-fitted(dtr.segmented.depth) #get the fitted data
dtr.segmented.depthModel<-tibble(max_depth_m=10^dtr.trim.depth$log_max_depth_m,log_max_depth_m=dtr.trim.depth$log_max_depth_m,dtr.segmented.depth.fitted=dtr.segmented.depth.fitted) #calculate the fit with the back transformed x axis

#*dtr: fit a nonlinear logistic curve####
dtr.logistic.depth<-nls(diel_rng~A+(B-A)/(1+exp((xmid-log_max_depth_m)/scal)),
                                  start=list(A=2,B=8,xmid=0.75,scal=-0.2),data=dtr.trim.depth,trace=TRUE,control=nls.control(tol=1e-05, minFactor=1/100000000,warnOnly = TRUE))
summary(dtr.logistic.depth)
#Get the cutoff from the logistic parameter as the xmid inflection point
dtr.logistic.depth.cutoff<-10^dtr.logistic.depth$m$getAllPars()[3]

#**dtr: flat model AICc####
dtr.flat.depth.aicc<-AIC(dtr.flat.depth)+ (2 * 1^2 + 2 * 1)/(length(dtr.flat.depth$residuals) - 1 - 1)

#**dtr: linear model AICc####
dtr.lm.depth.aicc<-AIC(dtr.lm.depth)+ (2 * 2^2 + 2 * 2)/(length(dtr.flat.depth$residuals) - 2 - 1)

#**dtr: Segmented model AICc####  
dtr.segmented.depth.aicc<-AIC(dtr.segmented.depth)+ (2 * 4^2 + 2 * 4)/(length(dtr.flat.depth$residuals) - 4 - 1)

#**dtr: logistic model AICc#### 
dtr.logistic.depth.aicc<-AIC(dtr.logistic.depth)+ (2 * 4^2 + 2 * 4)/(length(dtr.flat.depth$residuals) - 4 - 1)

#**dtr: Store aicc from each model as a row####
#Keep adding to this with each analysis
aicc.df.depth<-aicc.df.depth%>%add_row(type="dtr",flat=dtr.flat.depth.aicc,linear=dtr.lm.depth.aicc,segmented=dtr.segmented.depth.aicc,logistic=dtr.logistic.depth.aicc,
                                       segmented.cutoff=10^dtr.segmented.depth$psi[,"Est."],logistic.cutoff=dtr.logistic.depth.cutoff, 
                                       segmented.cutoff.SE=10^dtr.segmented.depth$psi[,"St.Err"], logistic.cutoff.SE=10^coef(summary(dtr.logistic.depth))[,"Std. Error"][3])

#*dtr: plot models overlaid####
ggplot(data=dtr.trim.depth,aes(y=diel_rng, x=log_max_depth_m))+geom_point()+
  geom_hline(yintercept=mean(dtr.trim.depth$diel_rng,na.rm=TRUE),color="orange")+
  geom_smooth(method='lm', se=FALSE,color="red")+
  geom_line(data=dtr.segmented.depthModel,aes(y=dtr.segmented.depth.fitted,x=log_max_depth_m),color="blue")+ #This is the segmented regression
  geom_line(data=tibble(logistic_predict=predict(dtr.logistic.depth),log_max_depth_m=dtr.trim.depth$log_max_depth_m), aes(y=logistic_predict,x=log_max_depth_m),color="purple")+
  geom_text(data=data.frame(aicc=round(dtr.flat.depth.aicc,1)), aes(label = paste("flat=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 1)+
  geom_text(data=data.frame(aicc=round(dtr.lm.depth.aicc,1)), aes(label = paste("linear=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 3)+
  geom_text(data=data.frame(aicc=round(dtr.segmented.depth.aicc,1)), aes(label = paste("segmented=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 5)+
  geom_text(data=data.frame(aicc=round(dtr.logistic.depth.aicc,1)), aes(label = paste("logistic=",aicc), x = Inf, y = Inf),hjust = 1, vjust = 7)+
  theme_bw()

#*dtr: plot optimal fit#### 
gg.dtr.depth.scatter<-ggplot(data=dtr.trim.depth,aes(x=max_depth_m,y=diel_rng))+
  geom_vline(xintercept=dtr.logistic.depth.cutoff,color="black",linetype=1,size=1)+ #vertical line indicating cutoff from optimal model if it exists, change that here, either 10^dtr.segmented.depth$psi[,"Est."] for segemented or dtr.logistic.depth.cutoff for logistic
  geom_point(color="dark grey")+
  scale_x_continuous(limits=c(0.1,200),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+
  geom_line(data=tibble(logistic_predict=predict(dtr.logistic.depth),log_max_depth_m=dtr.trim.depth$log_max_depth_m), aes(y=logistic_predict,x=10^log_max_depth_m),color="black",size=1.2)+ #add the logistic fit, have to raise the y variable 10^ if it has been transformed
  #geom_line(data=dtr.segmented.depthModel,aes(y=10^dtr.segmented.depth.fitted,x=10^log_max_depth_m),color="black",size=1.2)+ #This is the segmented regression, have to raise the y variable 10^ if it has been transformed
  #geom_line(data=tibble(fitted_values=dtr.lm.depth$fitted.values,max_depth_m=dtr.trim.depth$max_depth_m),aes(y=10^fitted_values,x=max_depth_m),size=1.2)+
  #geom_line(aes(y=predict),color="black",size=1.2)+
  #geom_line(data=temp,aes(x=area_ha,y=loess_fit),color="green")+
  #scale_y_log10()+
  labs(y=bquote(Diel~temp.~range~(degree*C)),x=bquote(Max~depth~(m)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.position = c(0.8, 0.8),
        #legend.title = element_text(size=8),
        #legend.text = element_text(size=10)
  )
#**dtr: FIGURE export as jpg####
ggsave("plots/WIAP-FigureX-dtrScatter-depth.jpg", plot=gg.dtr.depth.scatter, width=3, height=3,units="in", dpi=300)


#Do stats on selected models####
#First combine the cutoffs based on which was selected for segmented vs. logistic
#This adds in a mutual column called cutoff
aicc.df.depth.summary<-bind_rows(aicc.df.depth%>%filter(type=="tn"|type=="tp"|type=="ph")%>%dplyr::select(type,segmented.cutoff)%>%mutate(cutoff=segmented.cutoff),
                           aicc.df.depth%>%filter(type%in%c("dtr","chla"))%>%dplyr::select(type,logistic.cutoff)%>%mutate(cutoff=logistic.cutoff))%>%arrange(cutoff)
#Now summarize all cutoffs for manuscript
aicc.df.depth.summary%>%ungroup()%>%summarize(mean=mean(cutoff),stderr=sd(cutoff)/sqrt(length(cutoff)),median=median(cutoff))                        

#Export the aicc table for supplemental####
write_csv(aicc.df.depth,"outputs/WIAP-SupplementalTable-aicc-maxDepth.csv")


#goes left to right
#panel letter size
panel.size<-10
List<-list(gg.methaneFluxes.depth.scatter+
             theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             scale_y_continuous(limits=c(1,1200),trans="log10",breaks=c(1,10,100,1000),labels=trans_format("log10",math_format(10^.x)))+ 
             ggtitle("(a)"),
           gg.ph.depth.scatter+
             theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             scale_y_continuous(breaks=c(3,6,9))+ 
             #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="b",fontface="bold"))+
             ggtitle("(b)"),
           gg.tp.depth.scatter+
             theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             scale_y_continuous(limits=c(1,10000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
             ggtitle("(c)"),
           gg.tn.depth.scatter+
             theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             scale_y_continuous(limits=c(10,10000),trans="log10",breaks=c(10,100,1000,10000),labels=trans_format("log10",math_format(10^.x)))+
             #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="d",fontface="bold"))+
             ggtitle("(d)"),
           gg.dtr.depth.scatter+
             theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="h",fontface="bold"))+
             ggtitle("(e)"),
           gg.chla.depth.scatter+
             theme(axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             scale_y_continuous(limits=c(0.1,1000),trans="log10",breaks=c(0.1,1,10,100,1000),labels=trans_format("log10",math_format(10^.x)))+
             #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="i",fontface="bold"))+
             ggtitle("(f)")
           
)

#Plot them using patchwork####
(gg.largefiguredepth.patchwork.3x2<-wrap_plots(List,ncol = 2,nrow = 3)&theme(plot.margin = unit(c(3,3,3,3),"pt")))
#Could do a 3x3 with width 6, height = 5
ggsave("plots/WIAP-FigureX-EcosystemFunctionVDepth3x2.jpg", plot=gg.largefiguredepth.patchwork.3x2, width=4, height=5.5,units="in", dpi=300)
