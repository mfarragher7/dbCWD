# Overview figures for profile stats
# updated 2020-11-25

#load survey data 


pro = read.csv('library/profiles.all.csv',header=T)

#libraries
library(dplyr)
library(ggplot2)
library(ggpubr)

#Thermocline####
#Compare thermocline depths from EXO and C3
#JP
a=ggplot(filter(pro,LakeID=='JP'),
         aes(x=Date,y=thermocline_depth_exo)) + 
  geom_point(size=2,shape=19) +
  scale_y_reverse() +
  labs(title="JP thermocline depth", y='Depth') +
  geom_point(data=filter(pro,LakeID=='JP'),
             aes(x=Date, y=thermocline_depth_c3),size=3,shape=1);a

#SC
b=ggplot(filter(pro,LakeID=='SC'),aes(x=Date,y=thermocline_depth_exo)) + 
  geom_point(size=2,shape=19) +
  scale_y_reverse() +
  labs(title="SC thermocline depth", y='Depth') + 
  geom_point(data=filter(pro,LakeID=='SC'),aes(x=Date, y=thermocline_depth_c3),size=3,shape=1);b

#BB
c=ggplot(filter(pro,LakeID=='BB'),aes(x=Date,y=thermocline_depth_exo)) +
  geom_point(size=2,shape=19) + 
  scale_y_reverse() +
  labs(title="BB thermocline depth", y='Depth') +
  geom_point(data=filter(pro,LakeID=='BB'),aes(x=Date, y=thermocline_depth_c3),size=3,shape=1);c

#WH
d=ggplot(filter(pro,LakeID=='WH'),aes(x=Date,y=thermocline_depth_exo)) + 
  geom_point(size=2,shape=19) + 
  scale_y_reverse() +
  labs(title="WH thermocline depth", y='Depth') +
  geom_point(data=filter(pro,LakeID=='WH'),aes(x=Date, y=thermocline_depth_c3),size=3,shape=1);d

#combine
z=ggarrange(a,b,c,d,ncol=2,nrow=2);z






#Depth Max DO ####
#JP
a=ggplot(filter(pro,LakeID=='JP'),aes(x=Date,y=do_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() + 
  labs(title="JP max DO depth", y='Depth') + 
  theme_classic();a

#SC
b=ggplot(filter(pro,LakeID=='SC'),aes(x=Date,y=do_max_depth)) +
  geom_point(size=2,shape=19) +
  geom_line(linetype=3) + 
  scale_y_reverse() + 
  labs(title="SC max DO depth", y='Depth') +
  theme_classic();b

#BB
c=ggplot(filter(pro,LakeID=='BB'),aes(x=Date,y=do_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() +
  labs(title="BB max DO depth", y='Depth') + 
  theme_classic();c

#WH
d=ggplot(filter(pro,LakeID=='WH'),aes(x=Date,y=do_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() +
  labs(title="WH max DO depth", y='Depth') + 
  theme_classic();d

#combine
z=ggarrange(a,b,c,d,ncol=2,nrow=2);z



#Mean DO ####
a=ggplot(filter(pro,LakeID=='JP'),aes(x=Date,y=do_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  scale_y_reverse() + 
  labs(title="JP", y='DO saturation') + 
  theme_classic();a

#SC
b=ggplot(filter(pro,LakeID=='SC'),aes(x=Date,y=do_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() + 
  labs(title="SC", y='DO saturation') + 
  theme_classic();b

#BB
c=ggplot(filter(pro,LakeID=='BB'),aes(x=Date,y=do_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  scale_y_reverse() + 
  labs(title="BB", y='DO saturation') + 
  theme_classic();c
#WH
d=ggplot(filter(pro,LakeID=='WH'),aes(x=Date,y=do_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  scale_y_reverse() +
  labs(title="WH", y='DO saturation') + 
  theme_classic();d

#combine
z=ggarrange(a,b,c,d,ncol=2,nrow=2);z



#DCM ####
#JP
a=ggplot(filter(pro,LakeID=='JP'),aes(x=Date,y=chl_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() + 
  labs(title="JP", y='Depth') +
  theme_classic();a

#SC
b=ggplot(filter(pro,LakeID=='SC'),aes(x=Date,y=chl_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  scale_y_reverse() + 
  labs(title="SC", y='Depth') +
  theme_classic();b

#BB
c=ggplot(filter(pro,LakeID=='BB'),aes(x=Date,y=chl_max_depth)) + 
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  scale_y_reverse() + 
  labs(title="BB", y='Depth') +
  theme_classic();c

#WH
d=ggplot(filter(pro,LakeID=='WH'),aes(x=Date,y=chl_max_depth)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  scale_y_reverse() +
  labs(title="WH", y='Depth') +
  theme_classic();d

#combine
z=ggarrange(a,b,c,d,ncol=2,nrow=2);z



#mean chl
#JP
a=ggplot(filter(pro,LakeID=='JP'),aes(x=Date,y=chl_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) + 
  labs(title="JP", y='Chl a RFU') +
  theme_classic();a

#SC
b=ggplot(filter(pro,LakeID=='SC'),aes(x=Date,y=chl_mean)) + 
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  labs(title="SC", y='Chl a RFU') + 
  theme_classic();b

#BB
c=ggplot(filter(pro,LakeID=='BB'),aes(x=Date,y=chl_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  labs(title="BB", y='Chl a RFU') + 
  theme_classic();c

#WH
d=ggplot(filter(pro,LakeID=='WH'),aes(x=Date,y=chl_mean)) +
  geom_point(size=2,shape=19) + 
  geom_line(linetype=3) +
  labs(title="WH", y='Chl a RFU') +
  theme_classic();d

#combine
z=ggarrange(a,b,c,d,ncol=2,nrow=2);z




