#model kD and z 1% PAR from DOC and chlorophyll concentrations for each sample
#using jasmine's limnology class notes

# Kd = 0.22[DOC] + 0.07[chla] - 0.05
# z1%par = 4.6/Kd




#chl.doc = full.nooch %>% filter(Date <= "2020-10-01") #just use dates I have for now
full.nooch = read.csv('library/full.nooch.csv',header=T)
full.c3 = read.csv('library/full.c3.csv',header=T)
secchi = read.csv('library/full.secchi.csv',header=T)


#average 3 depth samples of DOC and chl for each date
chl.doc = ddply(full.nooch, .(lakedate,LakeID,Date,seasonID), summarize, 
                mean_doc=mean(DOC_mgpl), mean_chla=mean(chla_ugpl))

#calculate kd
chl.doc$kd = (0.22*chl.doc$mean_doc) + (0.07*chl.doc$mean_chla) - 0.05

#calculate depth of 1% par
chl.doc$z1 = 4.6/chl.doc$kd


#save df
write.csv(chl.doc,'library/kd.csv',row.names=F)







#compare to nps secchi. 
#figures from 8.20_chlbiomass.profiles.R

#** a few meters more shallow than nps secchi. **

library(ggplot2)
#select seasons. 
szns=c('winter1','spring3','summer3','fall4')
c3 = full.c3 %>% filter(seasonID %in% szns)
chl = full.nooch %>% filter(seasonID %in% szns)
secchi = secchi %>% filter(seasonID %in% szns)

#winter
jp.winter=ggplot(filter(c3,lakedate=='JP_2020-02-21'),
                 aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) +
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='JP_2020-02-21'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) +  #add extracted chl. use mapping=aes()' to avoid error
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(data=filter(chl.doc,lakedate=='JP_2020-02-21'),
               mapping=aes(x=z1,xend=z1,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color="Chl a (ug/L)",x="Depth (m)",y=" ") + 
  coord_flip() + 
  theme_classic(); jp.winter 

#spring3
jp.spring3=ggplot(filter(c3,lakedate=='JP_2020-05-28'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(27,0),n.breaks=6) + 
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='JP_2020-05-28'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) + 
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(data=filter(chl.doc,lakedate=='JP_2020-05-28'),
               mapping=aes(x=z1,xend=z1,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color="Chl a (ug/L)",x="",y=" ") + 
  coord_flip() + 
  theme_classic(); jp.spring3


#sealcove - pretty close

#summer3
sc.summer3=ggplot(filter(c3,lakedate=='SC_2020-08-03'),
                  aes(x=Depth,y=Chlorophyll_a)) + 
  geom_point(shape=1,size=1,alpha=0.25) +  
  stat_smooth(method="loess",size=0.75,color='black',se=F) + 
  scale_x_reverse(limits=c(12,0),n.breaks=7) +
  ylim(0,450) +
  geom_point(filter(chl,lakedate=='SC_2020-08-03'),
             mapping=aes(x=Depth,y=chla_ugpl,color=chla_ugpl),
             position=position_nudge(y=400),size=5) + 
  scale_color_gradient2(limits=c(0,3),midpoint=1.5,
                        breaks=c(0,1,2,3),
                        low="green",mid='gold',high="red") +
  geom_segment(data=filter(chl.doc,lakedate=='SC_2020-08-03'),
               mapping=aes(x=z1,xend=z1,y=0,yend=375),
               linetype=6,color="gray60") +
  labs(color="Chl a (ug/L)",x=" ",y=" ") + 
  coord_flip() + 
  theme_classic(); sc.summer3 



#compare to DCM. ratio