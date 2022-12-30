
# FOA August 2020 board meeting
# Jordan Pond summary profiles

### UPDATED 
#used jp figure for Borns Symposium 2020 also 





#color palette
brownblues = c( '#DFC27D',   #2
                '#BF812D',   #3
                '#8C510A',   #4
                '#000000',   #7
                '#003C30',   #9
                '#01665E',   #10
                '#35978F',   #11
                '#80CDC1')   #12
                
#temp
plot.jp.c3.temp = ggplot(filter(full.c3,LakeID=='JP'), aes(x=Depth, y=Temp, color=factor(Date))) + 
  geom_point(shape=1,size=2) +  
  scale_x_reverse(limits=c(27,1),n.breaks=6) +
  ylim(0,25) +
  labs(color="Date", y=expression("Temperature " ( degree*C)), x="Depth (m)") +
  scale_color_manual(values=brownblues) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() + 
  ggtitle("Jordan Pond - Temperature") + 
  coord_flip() 
plot.jp.c3.temp

#chlorophyll
plot.c3.jp.chl = ggplot(filter(full.c3,LakeID=='JP'), aes(x=Depth, y=Chlorophyll_a, color=factor(Date))) + 
  geom_point(shape=1,size=1.5) +  
  stat_smooth(method = "loess",  size = 1, se=F) +
  scale_x_reverse(limits=c(27,1),n.breaks=6) +
  ylim(0,165) +
  labs(color="Date", title=expression("Chlorophyll"~italic(a)), y=expression("Chlorophyll"~italic(a)~"(RFU)"), x="Depth (m)") +
  scale_color_manual(values=brownblues) +
  theme_classic() + 
  coord_flip() 
plot.c3.jp.chl

#DO Sat
#subset
jp.exo = subset(full.exo, LakeID %in% "JP")
#SUBSET LAKEDATE, add one more date, 8/3 add another color below too
jp.exo = jp.exo %>% filter(!lakedate=='JP_2020-08-19')
#plot
plot.exo.jp.dosat = ggplot(jp.exo, aes(x=DO_sat, y=Depth, color=factor(Date))) + 
  geom_point() +  
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=6) +
  xlim(85,105) +
  scale_color_manual(values=brownblues) +
  labs(color="Date", x="Dissolved Oxygen (% Saturation)", y="Depth (m)") +
  theme_classic() + 
  ggtitle("Dissolved Oxygen")
plot.exo.jp.dosat 

#fdom
plot.exo.jp.fdom = ggplot(jp.exo, aes(x=fDOM_corr, y=Depth, color=factor(Date))) + 
  geom_point() +
  geom_path(linetype=2) +
  scale_y_reverse(n.breaks=6) +
  xlim(0,3.5) +
  labs(color="Date", x="fDOM (RFU)", y="Depth (m)") +
  scale_color_manual(values=brownblues) +
  theme_classic() + 
  ggtitle("fDOM")
plot.exo.jp.fdom

#combine
plot.summary.jp = ggarrange(plot.jp.c3.temp,
                            plot.c3.jp.chl,
                            plot.exo.jp.dosat,
                            plot.exo.jp.fdom,
                            ncol=2,nrow=2,common.legend = TRUE, legend='bottom')
plot.summary.jp








#jp buoy minus missing data. run hobo script first
plot.jp.hobo = ggplot(jp.hobo, aes(x=DateTime, y=Depth, color=Temp)) +
  geom_tile() +
  scale_y_reverse(n.breaks=20, limits=c(28,4)) +
  scale_x_datetime(breaks="1 month", date_labels="%b-%d") +
  labs(color="Temp C", x="Date", y="Depth (m)") +
  scale_color_gradient2(midpoint=10,high="yellow",mid="red",low="navy") +
  theme_classic() +
  ggtitle("Jordan Pond Thermal Structure Feb 26 - June 12 2020")
plot.jp.hobo


