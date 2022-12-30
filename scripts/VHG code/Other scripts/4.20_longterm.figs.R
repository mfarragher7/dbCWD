#long term DOC, Secchi, and SO4 figs
#2020-12-01

#load  data 
lt.doc = read.csv("library/lt.doc.csv",header=T)
lt.secchi = read.csv("library/lt.secchi.csv",header=T)
lt.so4 = read.csv("library/lt.so4.csv",header=T)

library(ggplot2)
library(ggpubr)

#reorder lakes
lt.doc$lakename = factor(lt.doc$lakename,levels=c("Jordan Pond","Bubble Pond","Seal Cove Pond","Witch Hole Pond"))
lt.secchi$lakename = factor(lt.secchi$lakename,levels=c("Jordan Pond","Bubble Pond","Seal Cove Pond","Witch Hole Pond"))
#lt.secchi$lakename = factor(lt.secchi$lakename,levels=c("Witch Hole Pond","Seal Cove Pond","Bubble Pond","Jordan Pond"))
lt.so4$lakename = factor(lt.so4$lakename,levels=c("Jordan Pond","Bubble Pond","Seal Cove Pond","Witch Hole Pond"))
#4 colors pairs
color.code = c('#35978F','#01665E','#BF812D','#543005')
# reversed
#color.code = c('#543005','#BF812D','#01665E','#80CDC1')

#DOC
plot.lt.doc =
  ggplot(lt.doc, aes(x=as.Date(Date),y=DOC_mgpl,color=lakename)) +
  geom_point(size=2,shape=19,alpha=0.8) +  
  stat_smooth(aes(group=lakename),method="loess",
              linetype=2,size=1,se=F,show.legend=F) + 
  scale_y_continuous(limits=c(1,8),n.breaks=4) + 
  scale_x_date(date_labels="%Y",limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),by='5 years'))) +  #adjust date origin
  labs(title="DOC concentration",color="Lake",x=" ") +
  ylab(bquote("DOC (mg "*L^-1*")")) +
  scale_color_manual(values=color.code) +
  guides(color=guide_legend(override.aes=list(shape=19,size=2))) +
  theme_classic(); plot.lt.doc  

#Secchi
plot.lt.secchi = 
  ggplot(lt.secchi,aes(x=as.Date(Date),y=Secchi_m,color=lakename)) +
  geom_point(size=2,shape=19,alpha=0.8) +  
  stat_smooth(aes(group=lakename),method="loess",
              linetype=2,size=1,se=F,show.legend=F) + 
  scale_y_reverse(limits=c(22,0)) + 
  scale_x_date(date_labels="%Y",limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),by='5 years'))) + 
  labs(title="Secchi depth",color="Lake",x=" ",y="Secchi depth (m)") +
  scale_color_manual(values=color.code) +
  guides(color=guide_legend(override.aes=list(shape=19,size=2))) +
  theme_classic(); plot.lt.secchi 


#combine 
plot.lt.doc.secchi = ggarrange(plot.lt.doc,plot.lt.secchi,
                       nrow=1,ncol=2,common.legend=TRUE,legend='bottom'); plot.lt.doc.secchi



#format
plot.lt.doc.secchi.formatted = plot.lt.doc.secchi +
  theme(title=element_text(size=4,hjust=0.5),
        axis.title=element_text(size=4),
        axis.text=element_text(size=4))

#save
ggsave(filename="plots/longterm/lt.doc.secchi.v2.png",
       plot=plot.lt.doc.secchi.formatted,
       device="png",
       height=6,
       width=13.5,
       units="in",
       dpi=500)



#just secchi
plot.secchi.formatted = plot.lt.secchi +
  theme(title=element_text(size=10,hjust=0.5),
        axis.title=element_text(size=10),
        axis.text=element_text(size=10))
#save
ggsave(filename="lt.secchi.png", plot=plot.secchi.formatted,device="png",
       height=6, width=10, units="in", dpi=800)







#SO4 ####
#label help:  xlab(bquote('Assimilation ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')'))

plot.lt.so4 =
  ggplot(lt.so4, aes(x=Date,y=SO4_ueqpl,color=lakename)) +
  geom_point(size=1,shape=19,alpha=0.8) +  
  stat_smooth(aes(group=lakename),method="loess",
              linetype=2,size=0.6,se=F,show.legend=F) + 
  #scale_y_continuous(limits=c(1,8),n.breaks=4) + 
  scale_x_date(date_labels="%Y",limits=as.Date(c("1995-01-01","2020-12-01")),
               breaks=c(seq(from=as.Date("1995-01-01"),
                            to=as.Date("2020-12-01"),by='5 years'))) +  #adjust date origin
  labs(title="Sulfate concentration",color="Lake",x="Date") +
  ylab(bquote(~SO[4]~" ("*mu"eq "*L^-1*")")) +
  scale_color_manual(values=color.code) +
  guides(color=guide_legend(override.aes=list(size=2))) +
  theme_classic(); plot.lt.so4  

#format
plot.lt.so4.formatted = plot.lt.so4 +
  theme(title=element_text(size=12,hjust=0.5),
        axis.title=element_text(size=12),
        axis.text=element_text(size=12))
#save
ggsave(filename="lt.so4.png", plot=plot.lt.so4.formatted,device="png",
       height=6, width=9, units="in", dpi=800)


#combine all
plot.lt = ggarrange(plot.lt.so4, plot.lt.doc,plot.lt.secchi,
                    nrow=1,ncol=3,common.legend=TRUE,legend='bottom'); plot.lt

#format
plot.lt.formatted = plot.lt +
  theme(title=element_text(size=0.1,hjust=0.5),
        axis.title=element_text(size=1),
        axis.text=element_text(size=1))
#save
ggsave(filename="lt.png", plot=plot.lt.formatted,device="png",
       height=5, width=12, units="in", dpi=500)






