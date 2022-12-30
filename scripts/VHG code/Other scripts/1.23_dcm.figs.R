#DCM figures
#make 

#load data

#load survey data
#run first part of 1.12_dcm.thickness
#run 1.13_kd.R

#eventually change this to load 1.12_dcm.thickness. add 1% par info
#represent DCM as DCM:Depth1%PAR

#libraries
library(dplyr)
library(ggplot2)
library(ggpubr)



#combine dataframes
chl.doc = chl.doc %>% select(-Date,-LakeID,-seasonID)
dcm_z = join(summary.c3.loess,chl.doc,by="lakedate")
#add lake name
dcm_z = dcm_z %>% 
  mutate(lakename=ifelse(grepl('JP',LakeID),'Jordan Pond',NA)) %>% #add lake name column 
  mutate(lakename=replace(lakename,grepl('SC',LakeID), 'Seal Cove Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('WH',LakeID), 'Witch Hole Pond')) %>% 
  mutate(lakename=replace(lakename,grepl('BB',LakeID), 'Bubble Pond')) 
#calculate ratio of DCM : Z 1% PAR
dcm_z$ratio_dcm_z = dcm_z$chl_max_depth/dcm_z$z1


#Third try 
#DCM : Z 1% PAR #### 

#set colors
dcm_z$lakename = factor(dcm_z$lakename,levels=c("Jordan Pond","Bubble Pond","Seal Cove Pond","Witch Hole Pond"))
#4 colors pairs
color.code = c('#80CDC1','#01665E','#BF812D','#543005')

#use better colors so no one gets mad at me!!!!



plot.ratio_dcm_z = 
  ggplot(dcm_z,aes(x=chl_max_depth,y=z1,color=lakename)) +
  geom_point(shape=19,size=3) +
  scale_x_continuous(limits=c(0,21),n.breaks=5) +
  scale_y_continuous(limits=c(0,21),n.breaks=5) +
  scale_color_manual(values=color.code) +
  #geom_segment(mapping=aes(x=0,xend=21,y=0,yend=21),linetype=2,color="gray60") +
  labs(title='',color="Lake",x="zChl max (m)",y="z 1% PAR (m)") + 
  theme_classic(); plot.ratio_dcm_z

  
#ignore this one
#mean fChl for each date with +/- sd to show thickness
#JP
plot.jp.dcm = 
  ggplot(filter(dcm_z,LakeID=='JP'),
         aes(x=Date,y=chl_mean)) +
  geom_point(shape=19,size=3) +
  geom_line(color='honeydew4',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=chl_mean-chl_sd,ymax=chl_mean+chl_sd),width=5) +
  #scale_y_continuous(limits=c(10,22),breaks=c(10,12,14,16,18,20,22)) +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b") +
  ylim(0,600) +
  labs(title='Jordan Pond',color=" ",x="Date",y="Depth mean fChl (m)") + 
  theme_classic(); plot.jp.dcm



#JP
plot.jp.dcm = 
  ggplot(filter(dcm_z,LakeID=='JP'),aes(x=Date,y=chl_mean,color=chl_max_depth)) +
  geom_point(shape=19,size=3) +
  geom_line(color='honeydew4',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=chl_mean-chl_sd,ymax=chl_mean+chl_sd),width=5) +
  scale_color_gradient2(limits=c(10,22),breaks=c(10,12,14,16,18,20,22),midpoint=16,
                        low="green",mid='gold',high="red") +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b") +
  ylim(0,400) +
  guides(color=guide_colorbar(reverse=T)) + #reverse color/number order
  labs(title='Jordan Pond',color="Depth max fChl (m)",x="",y="Mean fChl") + 
  theme_classic(); plot.jp.dcm

#SC
plot.sc.dcm = 
  ggplot(filter(dcm_z,LakeID=='SC'),aes(x=Date,y=chl_mean,color=chl_max_depth)) +
  geom_point(shape=19,size=3) +
  geom_line(color='honeydew4',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=chl_mean-chl_sd,ymax=chl_mean+chl_sd),width=5) +
  scale_color_gradient2(limits=c(0,10),breaks=c(0,2,4,6,8,10),midpoint=5,
                       low="green",mid='gold',high="red") +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b") +
  ylim(0,400) +
  guides(color=guide_colorbar(reverse=T)) + 
  labs(title='Seal Cove Pond',color="Depth max fChl (m)",x="",y="Mean fChl") + 
  theme_classic(); plot.sc.dcm

#BB
plot.bb.dcm = 
  ggplot(filter(dcm_z,LakeID=='BB'),aes(x=Date,y=chl_mean,color=chl_max_depth)) +
  geom_point(shape=19,size=3) +
  geom_line(color='honeydew4',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=chl_mean-chl_sd,ymax=chl_mean+chl_sd),width=5) +
  scale_color_gradient2(limits=c(0,12),breaks=c(0,2,4,6,8,10,12),midpoint=6,
                         low="green",mid='gold',high="red") +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b") +
  ylim(0,400) +
  guides(color=guide_colorbar(reverse=T)) + 
  labs(title='Bubble Pond',color="Depth max fChl (m)",x="",y="Mean fChl") + 
  theme_classic(); plot.bb.dcm

#WH
plot.wh.dcm = 
  ggplot(filter(dcm_z,LakeID=='WH'),aes(x=Date,y=chl_mean,color=chl_max_depth)) +
  geom_point(shape=19,size=3) +
  geom_line(color='honeydew4',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=chl_mean-chl_sd,ymax=chl_mean+chl_sd),width=5) +
  scale_color_gradient2(limits=c(0,8),breaks=c(0,2,4,6,8),midpoint=4,
                         low="green",mid='gold',high="red") +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),
               breaks='2 months',date_labels="%d-%b") +
  ylim(0,800) +
  guides(color=guide_colorbar(reverse=T)) + 
  labs(title='Witch Hole Pond',color="Depth max fChl (m)",x="",y="Mean fChl") + 
  theme_classic(); plot.wh.dcm

#combine
z = ggarrange(plot.jp.dcm,plot.sc.dcm,plot.bb.dcm,plot.wh.dcm,nrow=2,ncol=2);z
z = ggarrange(plot.jp.dcm,plot.bb.dcm,plot.sc.dcm,plot.wh.dcm,nrow=4,ncol=1);z














#v1 ####
#max chl for each date, DCM on color scale. 
#JP
plot.jp.dcm = 
  ggplot(filter(summary.profiles,LakeID=='JP'),aes(x=Date,y=chl_max,color=chl_max_depth)) +
  geom_point(shape=19,size=3) +
  geom_line(color='honeydew4',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=chl_max-chl_sd,ymax=chl_max+chl_sd),width=5) +
  scale_color_continuous(limits=c(10,22),breaks=c(10,12,14,16,18,20,22),low="#C7EAE5",high="#003C30") +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),breaks='2 months',date_labels="%d-%b") +
  ylim(0,600) +
  guides(color=guide_colorbar(reverse=T)) + #reverse color/number order
  labs(title='Jordan Pond',color="Depth max fChl (m)",x="Date",y="Max fChl") + 
  theme_classic(); plot.jp.dcm
#SC
plot.sc.dcm = 
  ggplot(filter(summary.profiles,LakeID=='SC'),aes(x=Date,y=chl_max,color=chl_max_depth)) +
  geom_point(shape=19,size=3) +
  geom_line(color='honeydew4',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=chl_max-chl_sd,ymax=chl_max+chl_sd),width=5) +
  scale_color_continuous(limits=c(0,10),breaks=c(0,2,4,6,8,10),low="#C7EAE5",high="#003C30") +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),breaks='2 months',date_labels="%d-%b") +
  ylim(0,600) +
  guides(color=guide_colorbar(reverse=T)) + 
  labs(title='Seal Cove Pond',color="Depth max fChl (m)",x="Date",y="Max fChl") + 
  theme_classic(); plot.sc.dcm
#BB
plot.bb.dcm = 
  ggplot(filter(summary.profiles,LakeID=='BB'),aes(x=Date,y=chl_max,color=chl_max_depth)) +
  geom_point(shape=19,size=3) +
  geom_line(color='honeydew4',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=chl_max-chl_sd,ymax=chl_max+chl_sd),width=5) +
  scale_color_continuous(limits=c(0,12),breaks=c(0,2,4,6,8,10,12),low="#C7EAE5",high="#003C30") +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),breaks='2 months',date_labels="%d-%b") +
  ylim(0,600) +
  guides(color=guide_colorbar(reverse=T)) + 
  labs(title='Bubble Pond',color="Depth max fChl (m)",x="Date",y="Max fChl") + 
  theme_classic(); plot.bb.dcm
#WH
plot.wh.dcm = 
  ggplot(filter(summary.profiles,LakeID=='WH'),aes(x=Date,y=chl_max,color=chl_max_depth)) +
  geom_point(shape=19,size=3) +
  geom_line(color='honeydew4',linetype=4,inherit.aes=T) +
  geom_errorbar(aes(ymin=chl_max-chl_sd,ymax=chl_max+chl_sd),width=5) +
  scale_color_continuous(limits=c(0,8),breaks=c(0,2,4,6,8),low="#C7EAE5",high="#003C30") +
  scale_x_date(limits=c(as.Date('2020-02-01'),as.Date('2020-11-06')),breaks='2 months',date_labels="%d-%b") +
  ylim(0,1000) +
  guides(color=guide_colorbar(reverse=T)) + 
  labs(title='Witch Hole Pond',color="Depth max fChl (m)",x="Date",y="Max fChl") + 
  theme_classic(); plot.wh.dcm
#combine
z = ggarrange(plot.jp.dcm,plot.sc.dcm,plot.bb.dcm,plot.wh.dcm,nrow=2,ncol=2);z



#v2 ####


