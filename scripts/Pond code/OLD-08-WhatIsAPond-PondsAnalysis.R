#Pond data analysis from datamining 
#created 2020-12-11 by DCR and MJF

#check for required packages
if(!require(dplyr)){install.packages("dplyr")}
if(!require(plyr)){install.packages("plyr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(ggplot2)){install.packages("ggplot2")}

#load libraries
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

#load cleaned pond data
singleponds = read.csv("Data_cleaned/Singleponds_Cleaned.csv")
str(singleponds)

#remove large/deep lakes. n=1591
singleponds = singleponds %>% 
  filter((max_depth_m<20 | is.na(max_depth_m)==T) &
         (mean_depth_m<50 | is.na(mean_depth_m)==T) &
         (max_surfacearea_m2<500000 | is.na(max_surfacearea_m2)==T) &
         (mean_surfacearea_m2<5000000 | is.na(mean_surfacearea_m2)==T))
  

#TO-DO
#log transform some but not all plots?
#remove large/deep lakes



#add derived columns ####

#add log transformed cols
singleponds$log_mean_sa = log10(singleponds$mean_surfacearea_m2)
singleponds$log_max_sa = log10(singleponds$max_surfacearea_m2)
singleponds$log_doc = log10(singleponds$doc_mgpl)
singleponds$log_tn = log10(singleponds$tn_ugpl)
singleponds$log_tp = log10(singleponds$tp_ugpl)
singleponds$log_cond = log10(singleponds$cond_uspcm)
singleponds$log_chla = log10(singleponds$chla_ugpl)
#TN:TP 
singleponds$tn_tp_ratio = singleponds$tn / singleponds$tp
#TSI equations from https://www.nalms.org/secchidipin/monitoring-methods/trophic-state-equations/
#TSI TP
singleponds$tp_tsi = (log(singleponds$tp_ugpl)*14.42) + 4.15
#TSI TN
singleponds$tn_mgpl = singleponds$tn_ugpl/1000
singleponds$tn_tsi = (log(singleponds$tn_mgpl)*14.43) + 54.45
#TSI chl a
singleponds$chla_tsi = (log(singleponds$chla_ugpl)*9.81) + 30.6




#generate figures ####

#test plot
#test = ggplot(singleponds,aes(x=managed,y=tp_ugpl)) + geom_boxplot(); test


#save list of derived variables
deriv_vars = c("max_depth_m","mean_depth_m","max_surfacearea_m2","log_max_sa","mean_surfacearea_m2","log_mean_sa",
               "ph","doc_mgpl","log_doc","tn_ugpl","log_tn","tp_ugpl","log_tp","cond_uspcm","log_cond",
               "macrophytes_percentcover","tn_tp_ratio","tp_tsi","tn_tsi","chla_tsi")


#*Ponduse ####

#empty list of plots
plot_list = list()

#loop ponduse vs. derived vars 
for (i in 1:length(deriv_vars)){ 
  
  #temp df where column i has NA removed
  temp = singleponds %>% drop_na(ponduse)
  temp_pred = 'ponduse'
  
  #generate plot of predictor vs derived
  p = ggplot(temp,aes_string(x=temp_pred,y=deriv_vars[[i]][1])) +
    geom_boxplot() +
    geom_jitter(position=position_jitter(width=0.2, height=0))
  
  #paste plot in list
  plot_list[[i]] = p
  
  #save pdf 
  pdf("Plots/Pond_analysis_plots/Ponduse_plots.pdf")
  for (i in 1:length(plot_list)) {
    print(plot_list[[i]])
  }
  dev.off()  
}




#cursory look at other predictor vars

#Managed ####
#empty list of plots
plot_list = list()
#loop ponduse vs. derived vars 
for (i in 1:length(deriv_vars)){
  #temp df where column i has NA removed
  temp = singleponds %>% drop_na(managed)
  temp_pred = 'managed'
  #generate plot of predictor vs derived
  p = ggplot(temp,aes_string(x=temp_pred,y=deriv_vars[[i]][1])) +
    geom_boxplot() +
    geom_jitter(position=position_jitter(width=0.2, height=0))
  #paste plot in list
  plot_list[[i]] = p
  #save pdf 
  pdf("Plots/Pond_analysis_plots/Managed_plots.pdf")
  for (i in 1:length(plot_list)) {
    print(plot_list[[i]])
  }
  dev.off()  
}


#Hydrology ####
#empty list of plots
plot_list = list()
#loop ponduse vs. derived vars 
for (i in 1:length(deriv_vars)){
  #temp df where column i has NA removed
  temp = singleponds %>% drop_na(hydrology)
  temp_pred = 'hydrology'
  #generate plot of predictor vs derived
  p = ggplot(temp,aes_string(x=temp_pred,y=deriv_vars[[i]][1])) +
    geom_boxplot() +
    geom_jitter(position=position_jitter(width=0.2, height=0))
  #paste plot in list
  plot_list[[i]] = p
  #save pdf 
  pdf("Plots/Pond_analysis_plots/Hydrology_plots.pdf")
  for (i in 1:length(plot_list)) {
    print(plot_list[[i]])
  }
  dev.off()  
}



#Human built / manipulated ####
#empty list of plots
plot_list = list()
#loop ponduse vs. derived vars 
for (i in 1:length(deriv_vars)){
  #temp df where column i has NA removed
  temp = singleponds %>% drop_na(humanbuilt_manipulated)
  temp_pred = 'humanbuilt_manipulated'
  #generate plot of predictor vs derived
  p = ggplot(temp,aes_string(x=temp_pred,y=deriv_vars[[i]][1])) +
    geom_boxplot() +
    geom_jitter(position=position_jitter(width=0.2, height=0))
  #paste plot in list
  plot_list[[i]] = p
  #save pdf 
  pdf("Plots/Pond_analysis_plots/Artificial_manipulated_plots.pdf")
  for (i in 1:length(plot_list)) {
    print(plot_list[[i]])
  }
  dev.off()  
}



#One pdf
pred_vars = c('ponduse','hydrology','managed','humanbuilt_manipulated')

#empty list of plots
plot_list = list()


#for loop. loop through both lists of vars and generate 4*14 plots
for (i in 1:length(deriv_vars)){ #for every item in deriv var list
  
  for (j in 1:length(pred_vars)){ #and for every item in pred var list
    
    #generate plot of predictor vs derived
    p = ggplot(filter(singleponds,!is.na(pred_vars[[j]])),
               aes_string(x=pred_vars[[j]][1],y=deriv_vars[[i]][1])) +
      geom_boxplot() +
      geom_jitter(position=position_jitter(width=0.2, height=0))
    
    #paste plot in list
    plot_list[[i*j]] = p
  }
}

#print pdf ####
#sort plot list
plot_list[order(names(plot_list))]
pdf("Plots/Pond_analysis_plots/Pond_overview_plots.pdf")
sort(plot_list,)

for (i in 1:length(plot_list)) {
  print(plot_list[[i]])
}
dev.off()  





#OLD version

#prints plots out of order, and only 28/44 plots. Others are NULL


#make lists of predictor variables
pred_vars = c('ponduse','hydrology','managed','humanbuilt_manipulated')
#list of derived variables
deriv_vars = c('mean_depth_m','max_depth_m','mean_surfacearea_m2','max_surfacearea_m2','ph','doc_mgpl',
               'tn_ugpl','tp_ugpl','macrophytes_percentcover','cond_uspcm','ratio_tn_tp')
#need to add calculated derived vars 

#empty list of plots
plot_list = list()

#for loop. loop through both lists of vars and generate 4*14 plots
for (i in 1:length(pred_vars)){ #for every item in predictor var list
  
  for (j in 1:length(deriv_vars)){ #and for every item in derived var list
    
    #generate plot of predictor vs derived
    p = ggplot(filter(singleponds,!is.na(pred_vars[[i]])),
                      aes_string(x=pred_vars[[i]][1],y=deriv_vars[[j]][1])) +
      geom_boxplot() +
      geom_jitter(position=position_jitter(width=0.2, height=0))
    
    #paste plot in list
    plot_list[[i*j]] = p
  }
}

#print pdf ####
#sort plot list
my_list[order(names(my_list))]
pdf("Plots/Pond_analysis_plots/Pond_overview_plots.pdf")
sort(plot_list,)
for (i in 1:length(plot_list)) {
  print(plot_list[[i]])
}
dev.off()  


 
     
#from stack overflow:
# Make list of variable names to loop over.
 var_list= combn(names(iris)[1:3], 2, simplify=FALSE)
# Make plots.
plot_list = list()
for (i in 1:3) {
  p = ggplot(iris, aes_string(x=var_list[[i]][1], y=var_list[[i]][2])) +
    geom_point(size=3, aes(colour=Species))
  plot_list[[i]] = p
}

#create pdf where each page is a separate plot
pdf("plots.pdf")
for (i in 1:3) {
  print(plot_list[[i]])
}
dev.off()