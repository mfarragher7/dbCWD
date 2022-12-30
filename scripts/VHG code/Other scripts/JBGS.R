#species composition#
setwd("~/Documents/greenland/2019")
library(vegan);library(ggplot2);library(plyr);library(dplyr);library(corrr);library(reshape2)
sp <- read.csv("sp-SS2andSS901-ABstandardized.csv", header = T)

sp.x <- read.csv("sp-SS2andSS901-ABstandardized-lakesonly.csv", header = T)
sp.x <- t(sp.x)/3
sp.x.matrix <- as.matrix(vegdist(sp.x, method = "horn", upper = T))
write.csv(sp.x.matrix, file = "sp.horn.lakes.csv")

taxon.abb <- abbreviate(sp[,1], minlength = 10) #make abbreviations of taxon names
row.names(sp) <- taxon.abb 
sp <- sp[,-1]

#load traits and phyto groups
grp <- read.csv("traitsSS2&SS901.csv")
grp$abb <- taxon.abb
counts <- sp %>% 
  group_by(grp$group) %>%
  summarise_all(sum) # count # of individuals per group in samples

counts <- as.data.frame(t(counts)) # prepare transposed table to merge with env
colnames(counts) <- as.character(unlist(counts[1,]))
counts <- counts[-1,]

trophy <- sp %>% 
  group_by(grp$trophy) %>%
  summarise_all(sum) # count # of individuals per group = PA = photoauto; MX = mixotropyh, HT =hetero, NA = unknown

trophy <- as.data.frame(t(trophy)) # prepare transposed table to merge with env
colnames(trophy) <- as.character(unlist(trophy[1,]))
trophy <- trophy[-1,]

flagella <- sp %>% 
  group_by(grp$flagella) %>%
  summarise_all(sum) # count # of individuals/mL per group

flagella <- as.data.frame(t(flagella)) # prepare transposed table to merge with env
colnames(flagella) <- as.character(unlist(flagella[1,]))
flagella <- flagella[-1,]

# covert to numeric
counts <- as.data.frame(lapply(counts, function(x) as.numeric(as.character(x))))
trophy <- as.data.frame(lapply(trophy, function(x) as.numeric(as.character(x))))
flagella <- as.data.frame(lapply(flagella, function(x) as.numeric(as.character(x))))
trophy <- decostand(trophy, method = "total", margin = 1) # % of each pathway
trophy <- trophy[,1:2]*100 # select only photoautotrophy and mixotrophy, *100
flagella <- decostand(flagella, method = "total", margin = 1)
flagella <- flagella[,2]*100 # select only %cells with flagella


### calculate weighted mean for each sample sum of (abundance x GALDcolony) / sum of abundance

sp.GALD <- data.frame(matrix(nrow=111, ncol=39))
x.col <- colnames(sp)
colnames(sp.GALD) <- x.col

mean.GALD <- data.frame(matrix(nrow=2, ncol=39))
colnames(mean.GALD) <- x.col
rownames(mean.GALD) <- c("w.meanGALD", "w.medianGALD")
library("limma")

for (i in colnames(sp)) {
  sp.GALD[,i] <- sp[,i] * grp$GALDcolony
  sum.GALD <- sum(sp.GALD[,i])
  sum.sp <- sum(sp[,i])
  mean.GALD[1,i] <- sum.GALD/sum.sp
  mean.GALD[2,i] <- matrixStats::weightedMedian(grp$GALDcolony, sp[,i], na.rm = FALSE)
}

for (i in colnames(sp)) {
  sp.GALD[,i] <- sp[,i] * grp$GALDcolony
  sum.GALD <- sum(sp.GALD[,i])
  sum.sp <- sum(sp[,i])
  mean.GALD[1,i] <- sum.GALD/sum.sp
  mean.GALD[2,i] <- matrixStats::weightedMedian(grp$GALDcolony, sp[,i], na.rm = FALSE)
}

mean.GALD <- as.data.frame(t(mean.GALD))
env <- cbind(env, mean.GALD)

sp <- as.data.frame(t(sp)) #transpose the table for vegan with appropriate rownames

names.reduced <- names(which(specnumber(sp, MARGIN = 2) > 3)) # filter out species with more than 4 occurrences
sp.red <- sp[,which(colnames(sp) %in% names.reduced)] # reduce sp dataset to only those species with >4 frequency of occurr.

ind <- rowSums(sp) # number of ind per ml - per sample

# load biomass
bm <- read.csv("sp-SS2andSS901biomass.csv")
row.names(bm) <- bm[,1] 
bm <- bm[,-1]

biomass <- bm %>% 
  group_by(grp$group) %>%
  summarise_all(sum)

biomass <- as.data.frame(t(biomass)) # prepare transposed table to merge with env
colnames(biomass) <- as.character(unlist(biomass[1,]))
biomass <- biomass[-1,]
biomass <- as.data.frame(lapply(biomass, function(x) as.numeric(as.character(x))))

trophy.bio <- bm %>% 
  group_by(grp$trophy) %>%
  summarise_all(sum)

trophy.bio <- as.data.frame(t(trophy.bio)) # prepare transposed table to merge with env
colnames(trophy.bio) <- as.character(unlist(trophy.bio[1,]))
trophy.bio <- trophy.bio[-1,]
trophy.bio <- as.data.frame(lapply(trophy.bio, function(x) as.numeric(as.character(x))))
trophy.bio <- decostand(trophy.bio, method = "total", margin = 1) # % of each pathway
trophy.bio <- trophy.bio[,1:2]*100 # sel
names(trophy.bio)[1]<-"MXV"
names(trophy.bio)[2]<-"PAV"

flagella.bio <- bm %>% 
  group_by(grp$flagella) %>%
  summarise_all(sum)

flagella.bio <- as.data.frame(t(flagella.bio)) # prepare transposed table to merge with env
colnames(flagella.bio) <- as.character(unlist(flagella.bio[1,]))
flagella.bio <- flagella.bio[-1,]
flagella.bio <- as.data.frame(lapply(flagella.bio, function(x) as.numeric(as.character(x))))
flagella.bio <- decostand(flagella.bio, method = "total", margin = 1) # % of each pathway
flagella.bio <- flagella.bio[,1:2]*100 # sel
names(flagella.bio)[1]<-"flagNO"
names(flagella.bio)[2]<-"flagellaV"

bm <- as.data.frame(t(bm))
bio <- rowSums(bm)

####rarefaction#### (included in new env - but wrong)
counts <- read.csv("sp-SS2andSS901-counts.csv", header = T)
sp.r <- t(round(counts, digits = 0))
raremax <- min(rowSums(sp.r))
Srare <- rarefy(sp.r, raremax)
env$richR <- Srare
boxplot(richR ~ Period*Lake, data = env)
rarecurve(sp.r, step = 1, sample = raremax, xlab = "Sample Size", ylab = "Species",
          label = T, col = env$Lake)

# load env 
env <- read.csv("env-for-sp-all.csv", row.names = 1)
env$Date <- as.Date(env$Date)
env$Layer <- factor(env$Layer, levels = c("epi", "meta", "hypo"))
#env$jul <- as.numeric(format(env$Date, "%j"))

env$shannon <- diversity(sp, index = "shannon") # Shannon H
env$rich <- specnumber(sp) # number of species
env$J <- env$shannon/log(env$rich) # Pielou evenness

envbio <- cbind(env[,-c(31:41)], biomass)

env <- env[,-c(42,43,47,48)]
env <- cbind(env, trophy$MX, trophy.bio$MXV)
names(env)[53]<-"MX"
names(env)[54]<-"MXV"


boxplot(rich ~ Period*Lake, env)
boxplot(shannon ~ Period*Lake, env)
par(mfrow = c(3,1))
boxplot(ind ~ Period*Lake, env) #compare # of individuals among periods and lakes
boxplot(env$MXV ~ Period*Lake, env)
boxplot(chl ~ Period*Lake, env)

env.med <- env %>% 
  group_by(Period, Lake) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

env.iqr <- env %>% 
  group_by(Period, Lake) %>%
  summarise_if(is.numeric, IQR, na.rm = TRUE)

env.mean <- env %>% 
  group_by(Period, Lake) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

env.sd <- env %>% 
  group_by(Period, Lake) %>%
  summarise_if(is.numeric, sd, na.rm = TRUE)

env.sd <- env %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)



env.mean.layer <- env %>% 
  group_by(Period, Lake, Layer) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

write.csv(env.mean, "env.mean.csv")
write.csv(env.med, "env.med.csv")
write.csv(env.sd, "env.sd.csv")
write.csv(env.mean.layer, "env.mean.layer.csv")
write.csv(env.mean.layer.sd, "env.mean.layer.sd.csv")




#### zooplankton data are already attached to env (mesh counts) ####
### load zoop community data ####
zoop <- read.csv("zoop-GL-com.csv")
env.zoop <- read.csv("env.mean.zoop.csv")

zoop.tr <- as.data.frame(t(zoop))
grp.zoop <- read.csv("groups-zoops.csv")
zoop.order <- zoop.tr %>% 
  group_by(grp.zoop$Order) %>%
  summarise_all(sum) # count # of individuals per group in samples

zoop.order <- as.data.frame(t(zoop.order)) # prepare transposed table to merge with env
colnames(zoop.order) <- as.character(unlist(zoop.order[1,]))
zoop.order <- zoop.order[-1,]
zoop.order <- mutate_all(zoop.order, function(x) as.numeric(as.character(x)))

zoop.phylum <- zoop.tr %>% 
  group_by(grp.zoop$Phylum) %>%
  summarise_all(sum) # count # of individuals per group in samples

zoop.phylum <- as.data.frame(t(zoop.phylum)) # prepare transposed table to merge with env
colnames(zoop.phylum) <- as.character(unlist(zoop.phylum[1,]))
zoop.phylum <- zoop.phylum[-1,]
zoop.phylum <- mutate_all(zoop.phylum, function(x) as.numeric(as.character(x)))

env.zoop.comb <- cbind(env.zoop, zoop.order)
env.zoop.comb <- env.zoop[,-c(49)]

zoo <- ggplot(env.zoop.comb, aes(x = jul, y = Cyclopoida)) +
  labs(x = "julian day", y = "copepods") +
  geom_point() + 
  geom_path() + 
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141), size = 0.5) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145), size = 0.5) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)
zoo

zoop <- zoop[-c(8,12:13),]
env.zoop.ord <- env.zoop.comb[-c(8,12:13),] # deleting NA rows

zoop.nmds <- metaMDS(zoop, distance = "horn", k=2, trymax = 1000)
zoop.nmds <- metaMDS(zoop, distance = "horn", k=2, previous.best = zoop.nmds)
#Dimensions: 2 
#Stress:      0.1161218 
#Stress type 1, weak ties

stressplot(zoop.nmds)
ordiplot(zoop.nmds, type = "none")
ordisurf(zoop.nmds,env.zoop.ord$chl, main="",col="forestgreen")
ordihull(zoop.nmds, env.zoop.ord$Period)
points(zoop.nmds, "sites", pch = 19, col = env.zoop.ord$Lake)
points(zoop.nmds, "species", pch = 2, col = grp.zoop$Order)
fit <- envfit(zoop.nmds ~ ind + jul + Calanoida + Cyclopoida + Cladocera + Ploima + Flosculariaceae, env.zoop.ord, perm = 999, na.rm = T)
plot(fit,  col = "gold")



#### pigments ####
pig <- read.csv("env-for-sp-pigments.csv")
pig$Date <- as.Date(env$Date)
pig$Layer <- factor(env$Layer, levels = c("epi", "meta", "hypo"))
pig$jul <- as.numeric(format(env$Date, "%j"))
pig$period <- env$Period

pig$total <- rowSums(pig[,6:18])
pig$chl.phaeo <- rowSums(pig[,c(16,17)])

cor(pig$Alloxanthin, env$Cryptophyceae, use = "complete.obs") #0.52
cor(pig$Chl.c2, env$Chrysophyceae, use = "complete.obs")
cor(pig$Peridinin, env$Dinophyta, use = "complete.obs")

cor(pig$total, env$ind, use = "complete.obs") # 0.6362621
cor(pig$total, env$bio, use = "complete.obs", method = "spearman") # 0.3522727

cor(pig$nochl, env$ind, use = "complete.obs") # 0.5974304
cor(pig$nochl, env$bio, use = "complete.obs", method = "spearman") # 0.3532754

cor(pig$Chl.a, env$chl, use = "complete.obs") #0.59
cor(pig$chl.phaeo, env$chl, use = "complete.obs") #0.95; chlorophyll a measured in the lab = chl a + phaeo from HPLC

cor(pig$total, env$ind, use = "complete.obs") #0.64
cor(log(pig$total), log(env$bio), use = "complete.obs") #0.58
plot(log(env$bio), log(pig$total))
plot(log(env$ind), log(pig$total))

pig.env <- cbind(pig, env[,6:49])
pig.env <- pig.env[,-c(19:20)]

ggplot(pig.env, aes(x = log(bio), y = log(total))) + 
  geom_point() + 
  geom_smooth(method = "lm")

pig.env$propchl <- (chl.phaeo/tot)*100

pchl <- ggplot(pig.env, aes(x = jul, y = chl.phaeo)) +
  labs(x = "julian day", y = "prop chl+phaeo") +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141), size = 0.5) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145), size = 0.5) +
  geom_point(aes(shape = Layer, color = Layer)) +
  geom_line(aes(group = Layer, color = Layer), size = 1) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)
pchl

table <- pig.env[,6:26] %>% 
  correlate(method = "pearson")


pig.env %>% 
  group_by(Lake, Period) %>% 
  summarise_at(vars(propchl), mean, na.rm = T)
pig.env %>% 
  group_by(Lake, Period) %>% 
  summarise_at(vars(propchl), sd, na.rm = T)

pig.melt <- melt(pig, id.vars = c("jul", "Layer", "Lake", "Date", "period"),
                 measure.vars = c("Chl.c2", "Peridinin" , "Fucoxanthin", "Neoxanthin", "Violaxanthin",
                                  "Diadinoxanthin", "Alloxanthin", "Lutein",  "Zeaxanthin", "Chl.b",
                                  "Chl.a", "Pheophytin.a", "B.carotene"))

pig.melt$period <- factor(pig.melt$period, levels = c("ice", "early", "mid-summer"))
pig.cols <- c("Chl.c2" = "brown", "Peridinin" = "grey80",
              "Fucoxanthin" = "yellowgreen", "Neoxanthin" = "orange",
              "Violaxanthin" = "forestgreen", "Diadinoxanthin" = "turquoise3",
              "Alloxanthin" = "purple", "Lutein" = "blue",
              "Zeaxanthin" = "red", "Chl.b" = "darkblue",
              "Chl.a" = "grey60", "Pheophytin.a" = "darkorange", "B.carotene" = "orangered2")

xx <- ggplot(pig.melt, aes(x = Layer, y=value)) +
  geom_bar(aes(fill = variable), stat="identity", position = "fill") +
  scale_x_discrete(limits=c("hypo","meta","epi")) +
  scale_fill_manual(values = pig.cols) +
  labs(x = NULL, y = expression(Pigment~concentration~(nmol.L^"-1"))) +
  coord_flip() + 
  facet_grid(Lake~period, scales = "free") +
  theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
        axis.title.y = element_text(face='plain',size=12,vjust=1),
        axis.text.x = element_text(face='plain',size=9,color='black',angle=0, hjust=0.5,vjust=0.5),
        axis.text.y = element_text(face='plain',size=9,color='black'), 
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        strip.placement = "outside",
        axis.line.y = element_line(size = 0.25, colour = "black"),
        axis.line.x = element_line(size = 0.25, colour = "black"),
        panel.grid.major = element_blank(),
        panel.background =  element_blank())

xx
ggsave("pigmentsRA.pdf", path = "~/Documents/greenland/2019/JGR", width = 3.5, 
       height = 2, units = "in", useDingbats = F)

pig.vars <- c("Chl.a", "Pheophytin.a")
"%ni%" <- Negate("%in%")
pig.melt.nochl <- pig.melt[pig.melt$variable %ni% pig.vars, ]

xx <- ggplot(pig.melt.nochl, aes(x = Layer, y=value)) +
  geom_bar(aes(fill = variable), stat="identity") +
  scale_x_discrete(limits=c("hypo","meta","epi")) +
  scale_fill_manual(values = pig.cols) +
  labs(x = NULL, y = expression(Pigment~concentration~(nmol.L^"-1"))) +
  coord_flip() + 
  facet_grid(Lake~period, scales = "free") +
  theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
        axis.title.y = element_text(face='plain',size=12,vjust=1),
        axis.text.x = element_text(face='plain',size=9,color='black',angle=0, hjust=0.5,vjust=0.5),
        axis.text.y = element_text(face='plain',size=9,color='black'), 
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        strip.placement = "outside",
        axis.line.y = element_line(size = 0.25, colour = "black"),
        axis.line.x = element_line(size = 0.25, colour = "black"),
        panel.grid.major = element_blank(),
        panel.background =  element_blank())

xx
ggsave("pigments-no-chl-and-ph2.pdf", path = "~/Documents/greenland/2019/JGR", width = 6, 
       height = 4, units = "in", useDingbats = F)


pig.env.ice <- pig.env[which(pig.env$Period == "ice"), ]
ggplot(pig.env.ice, aes(x = a440, y = total)) + 
  geom_point() +
  geom_smooth(method="nls",formula = y~a*exp(x*b),method.args=list(start=c(a=2,b=0.5)), se=F,color="red")


####
#### Ordination #####################################################################################
library(vegan)

cca <- rda(sp.red ~ DOC + chl + MX + MXV + jul, env, scale = T)
plot(cca, scaling = 1)
ordihull(cca, env$Period, col = "green", label = T)
ordihull(cca, env$Lake, col = "gold", label = T)

#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
#library(pairwiseAdonis)


#significance of within Time and within space variability - PERMANOVA
ado <- adonis2(sp ~ Lake/Period, strata = Lake, data = env, method = "horn", permutations = 9999)
ado

#Df SumOfSqs      R2      F Pr(>F)    
#Lake         1   1.3023 0.12589 7.3700  1e-04 ***
#Lake:Period  4   3.2110 0.31040 4.5428  1e-04 ***
#Residual    33   5.8313 0.56371                  
#Total       38  10.3446 1.00000 

sp.ice <- sp[which(rownames(sp) %in% rownames(ice.only)),]
ado <- adonis2(sp.ice ~ Lake, data = ice.only, method = "horn", permutations = 9999)
ado

#Df SumOfSqs      R2      F Pr(>F)
#Lake      1  0.19135 0.10506 1.1739 0.3232
#Residual 10  1.62994 0.89494              
#Total    11  1.82129 1.00000              

sp.early <- sp[which(rownames(sp) %in% rownames(early.only)),]
ado <- adonis2(sp.early ~ Lake, data = early.only, method = "horn", permutations = 9999)
ado
#Lake      1   0.6080 0.15551 2.3939  0.093 .

sp.mid <- sp[which(rownames(sp) %in% rownames(mid.only)),]
ado <- adonis2(sp.mid ~ Lake, data = mid.only, method = "horn", permutations = 9999)
ado
#Lake      1  1.00675 0.52801 11.187 0.0049 **

env.open <- rbind(early.only, mid.only) # differences in comm structure between ss2 and ss901 during the whole open-water
sp.open <- sp[which(rownames(sp) %in% rownames(env.open)),]
rownames(env.open) <- rownames(sp.open)
ado <- adonis2(sp.open ~ Layer/Lake, data = env.open, method = "horn", permutations = 9999)
ado
#Lake      1   0.6880 0.0939 2.5909 0.0395 *

sp.horn <- vegdist(sp, method = "horn")
beta.horn <- betadisper(sp.horn, env$Period, type = 'centroid')
boxplot(beta.horn$distances~env$Period)
permutest(beta.horn)

mod <- betadisper(sp.horn, env$Period, type = "median", bias.adjust=TRUE)
permutest(mod, pairwise = T) #if the dispersion is different between groups, then examine
plot(mod)
boxplot(mod)
mod.HSD <- TukeyHSD(mod)
mod.HSD

#diff        lwr        upr     p adj
#ice-early        -0.10544765 -0.2566610 0.04576576 0.2173878
#mid-summer-early -0.11396789 -0.2651813 0.03724551 0.1705042
#mid-summer-ice   -0.00852025 -0.1679132 0.15087267 0.9906349



ice.1 <- metaMDS(sp, distance = "jaccard", k=3, trymax = 100)
ice <- metaMDS(sp, distance = "jaccard", k=3, previous.best = ice.1)
#Dimensions: 2 
#Stress:     0.255316 
#Stress type 1, weak ties

stressplot(ice)
ordiplot(ice, type = "none")
ordisurf(ice,env$DOC, main="",col="forestgreen")
points(ice, "sites", pch = 19, col = env$Lake)
#points(ice, "species", pch = 2, col = grp$group)
fit <- envfit(ice ~ DOC + NO3 + NH4 + cladocera + jul, env, perm = 999, na.rm = T)
ordiArrowMul(fit)
plot(fit,  col = "gold", p.max = 0.05)
ordihull(ice, env$Period, draw = "polygon", conf.int = 0.95, alpha = 20, border = NULL, col = "purple", label = T)
#with(grp, legend("topright", legend = levels(group), fill = group, pch = 21))



#### make a nice plot####

data.scores <- as.data.frame(scores(ice))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$Lake <- env$Lake  #  add Lake
data.scores$Period <- env$Period 
data.scores$ind <- env$ind
species.scores <- as.data.frame(scores(ice, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
species.scores$grp <- grp$group
# make hulls
grp.ice <- data.scores[data.scores$Period == "ice", ][chull(data.scores[data.scores$Period == 
                                                                          "ice", c("NMDS1", "NMDS2")]), ]  

grp.early <- data.scores[data.scores$Period == "early", ][chull(data.scores[data.scores$Period == 
                                                                   "early", c("NMDS1", "NMDS2")]), ] 

grp.mid <- data.scores[data.scores$Period == "mid-summer", ][chull(data.scores[data.scores$Period == 
                                                                              "mid-summer", c("NMDS1", "NMDS2")]), ] 
hulls <- rbind(grp.ice, grp.early, grp.mid)  #combine 

#extract envfit
library(grid)

efit <- as.data.frame(scores(fit, display = "vectors"))
efit$var <- rownames(efit)
efit$p <- fit$vectors$pvals

####
library(ggnewscale)
cols <- c("SS2" = "#E69F00", "SS901" = "#0072B2")
grp.cols <- c("Bacillariophyceae" = "brown",
              "Chlorophyta" = "yellowgreen", "Chrysophyceae" = "orange",
              "Cryptophyceae" = "forestgreen", "Cyanobacteria" = "turquoise3",
              "Desmidiales" = "purple", "Dinophyta" = "blue", "Haptophyta" = "orangered2",
              "Euglenophyta" = "red", "Eustigmatophyceae" = "darkblue",
              "Heterokonta" = "grey60", "Synurophyceae" = "darkorange",
              "Xantophyceae" = "yellow")
nmds.all <- (ggplot() + 
  geom_polygon(data=hulls, aes(x = NMDS1, y = NMDS2, fill = Period, group = Period), alpha=0.3) +
    scale_fill_manual(values = c("skyblue3", "darkblue", "goldenrod")) +
  new_scale_colour() +
  geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, colour = Lake), size=log(env$ind)/1.5) +
  scale_colour_manual(values = cols) +  
  geom_segment(data = efit,
               aes(x = 0, xend = NMDS1*1.44, y = 0, yend = NMDS2*1.44),
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
  geom_text(data = efit, aes(x = NMDS1*1.44, y = NMDS2*1.44, label = var, hjust = 0.5, vjust = 1), size = 4) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=10), # remove x-axis labels
        axis.title.y = element_text(size=10), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        legend.position = "none",
        plot.background = element_blank()))
nmds.all
ggsave("nmds-all-done.pdf", path = "~/Documents/greenland/2019/JGR", width = 3.5, height = 2.75, units = "in", useDingbats = F)
 
######################################################################################
######################################################################################
######################################################################################


melt.all <- melt(env, id.vars = c("jul", "Layer", "Lake", "Date", "Ice", "Period"), 
                 measure.vars = c("Bacillariophyceae", "Chlorophyta", "Chrysophyceae", "Cryptophyceae", 
                                  "Synurophyceae", "Cyanobacteria", "Euglenophyta", "Eustigmatophyceae", "Desmidiales", "Dinophyta", "Haptophyta"))

xx <- ggplot(melt.all, aes(x = jul, y = value)) +
  geom_point(aes(x = jul, y = value, shape = Layer, color = Layer)) +
  geom_line(aes(x = jul, y = value, group = Layer, color = Layer)) +
  labs(y = "phytoplankton density (ml^-1)", x = "julian day") +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141)) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145)) +
  facet_grid(variable~Lake, scales = "free_y") + 
  theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
         axis.title.y = element_text(face='plain',size=12,vjust=1),
         axis.text.x = element_text(face='plain',size=9,color='black',angle=0, hjust=0.5,vjust=0.5),
         axis.text.y = element_text(face='plain',size=9,color='black'), 
         strip.background = element_blank(),
         strip.text = element_text(size = 8),
         strip.placement = "outside",
         axis.line.y = element_line(size = 0.25, colour = "black"),
         axis.line.x = element_line(size = 0.25, colour = "black"),
         panel.grid.major = element_blank(),
         panel.background =  element_blank())
xx
ggsave("all2.pdf", path = "~/Documents/greenland/2019/JGR", width = 15, height = 15, units = "cm", useDingbats = F)

### density figure ####
grp.cols <- c("Bacillariophyceae" = "chocolate",
              "Chlorophyta" = "yellowgreen", "Chrysophyceae" = "gold",
              "Cryptophyceae" = "forestgreen",  "Synurophyceae" = "darkorange",
              "Cyanobacteria" = "turquoise3", "Euglenophyta" = "red3",
              "Eustigmatophyceae" = "darkblue", "Desmidiales" = "purple",
              "Dinophyta" = "plum4", "Haptophyta" = "bisque3")


melt.all$variable <- factor(melt.all$variable, levels = c("Bacillariophyceae",
                                                        "Chlorophyta", "Chrysophyceae",
                                                        "Cryptophyceae",  "Synurophyceae",
                                                        "Cyanobacteria", "Euglenophyta",
                                                        "Eustigmatophyceae", "Desmidiales",
                                                        "Dinophyta", "Haptophyta"))
melt.all$Period <- factor(melt.all$Period, levels = c("ice", "early", "mid-summer"))


xx <- ggplot(melt.all, aes(x = Layer, y=value)) +
  geom_bar(aes(fill = variable), stat="identity", position = "fill") +
  scale_fill_manual(values = grp.cols) +
  scale_x_discrete(limits=c("hypo","meta","epi")) +
  labs(x = NULL, y = expression(Density~(cells.mL^"-1"))) +
  coord_flip() + 
  facet_grid(Period~Lake, scales = "free") +
  theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
        axis.title.y = element_text(face='plain',size=12,vjust=1),
        axis.text.x = element_text(face='plain',size=9,color='black',angle=0, hjust=0.5,vjust=0.5),
        axis.text.y = element_text(face='plain',size=9,color='black'), 
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        strip.placement = "outside",
        axis.line.y = element_line(size = 0.25, colour = "black"),
        axis.line.x = element_line(size = 0.25, colour = "black"),
        panel.grid.major = element_blank(),
        panel.background =  element_blank())
xx
ggsave("densityreverse-axes.pdf", path = "~/Documents/greenland/2019/JGR", width = 15, 
       height = 10, units = "cm", useDingbats = F)

  

envbiomelt <- melt(envbio, id.vars = c("jul", "Layer", "Lake", "Date", "Ice", "Period"), 
                   measure.vars = c("Bacillariophyceae", "Chlorophyta", "Chrysophyceae", "Cryptophyceae", 
                                    "Synurophyceae",
                                    "Cyanobacteria", "Desmidiales", "Dinophyta", "Haptophyta"))
xx <- ggplot(envbiomelt, aes(x = jul, y = value)) +
  geom_point(aes(x = jul, y = value, shape = Layer, color = Layer)) +
  geom_line(aes(x = jul, y = value, group = Layer, color = Layer)) +
  labs(y = "phytoplankton biovolume (um^-3.ml-1)", x = "julian day") +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141)) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145)) +
  facet_grid(variable~Lake, scales = "free_y") + 
  theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
        axis.title.y = element_text(face='plain',size=12,vjust=1),
        axis.text.x = element_text(face='plain',size=9,color='black',angle=0, hjust=0.5,vjust=0.5),
        axis.text.y = element_text(face='plain',size=9,color='black'), 
        strip.background = element_blank(),
        strip.text = element_text(size = 8),
        strip.placement = "outside",
        axis.line.y = element_line(size = 0.25, colour = "black"),
        axis.line.x = element_line(size = 0.25, colour = "black"),
        panel.grid.major = element_blank(),
        panel.background =  element_blank())
xx
ggsave("all2bio.pdf", path = "~/Documents/greenland/2019/JGR", width = 15, height = 15, units = "cm", useDingbats = F)

envbiomelt$Period <- factor(envbiomelt$Period, levels = c("ice", "early", "mid-summer"))
xx <- ggplot(envbiomelt, aes(x = Layer, y=value)) +
  geom_bar(aes(fill = variable),  stat="identity", position = "fill") +
  scale_fill_manual(values = grp.cols) +
  scale_x_discrete(limits=c("hypo","meta","epi")) +
  labs(x = NULL, y = expression(Biovolume~("\u03bc"*m^"3"~.mL^"-1"))) +
  coord_flip() + 
  facet_grid(Period~Lake, scales = "free") +
  theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
        axis.title.y = element_text(face='plain',size=12,vjust=1),
        axis.text.x = element_text(face='plain',size=9,color='black',angle=0, hjust=0.5,vjust=0.5),
        axis.text.y = element_text(face='plain',size=9,color='black'), 
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        strip.placement = "outside",
        axis.line.y = element_line(size = 0.25, colour = "black"),
        axis.line.x = element_line(size = 0.25, colour = "black"),
        panel.grid.major = element_blank(),
        panel.background =  element_blank())
xx
ggsave("biovolumeRAreverse-axes.pdf", path = "~/Documents/greenland/2019/JGR", width = 15, 
       height = 10, units = "cm", useDingbats = F)


#### make separate ordinations for SS2 and SS901 to see drivers of change there ####
SS2 <- env[which(env$Lake == "SS2"), ]
sp2 <- sp[1:24,]
names.2.red <- names(which(specnumber(sp2, MARGIN = 2) > 2)) # filter out species with more than 4 occurrences
sp2.red <- sp2[,which(colnames(sp2) %in% names.2.red)]
grp2 <- grp[which(grp$abb %in% names.2.red),]

ano <- adonis2(sp2 ~ Period, data = SS2, method = "horn", permutations = 999) #differences between times
ano
sp2.horn <- vegdist(sp2.red, method = "horn")
mod <- betadisper(sp2.horn, SS2$Period, type = "median")
permutest(mod, pairwise = TRUE) #if the dispersion is different between groups, then examine
plot(mod)
boxplot(mod)
mod.HSD <- TukeyHSD(mod)
mod.HSD
plot(mod.HSD)


## Tukey's Honest Significant Differences
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)

ice <- metaMDS(sp2.red, distance = "horn", trymax = 200)
#Dimensions: 2 
#Stress:     0.1866979 
stressplot(ice)
ordiplot(ice, type = "points")
ordisurf(ice,SS2$MXV, main="",col="forestgreen")
points(ice, "sites", pch = 19, col = SS2$Period)
text(ice, "species", cex = 0.5)
ordihull(ice, SS2$Period, draw = "polygon", conf.int = 0.95, alpha = 20, border = NULL, col = "purple", label = T)
fit <- envfit(ice ~ a380 + NO3 + DIN.TP + dSi + cladocera + Chlorophyta, SS2, perm = 999)
fit
plot(fit,  col = "black", p.max = 0.05)
ordiArrowMul(fit) #2.080099

#### make a nice plot####

data.scores <- as.data.frame(scores(ice))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$Period <- SS2$Period 
data.scores$ind <- SS2$ind
species.scores <- as.data.frame(scores(ice, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
species.scores$grp <- grp2$group
# make hulls
grp.ice <- data.scores[data.scores$Period == "ice", ][chull(data.scores[data.scores$Period == 
                                                                          "ice", c("NMDS1", "NMDS2")]), ]  

grp.early <- data.scores[data.scores$Period == "early", ][chull(data.scores[data.scores$Period == 
                                                                              "early", c("NMDS1", "NMDS2")]), ] 

grp.mid <- data.scores[data.scores$Period == "mid-summer", ][chull(data.scores[data.scores$Period == 
                                                                                 "mid-summer", c("NMDS1", "NMDS2")]), ] 
hulls <- rbind(grp.ice, grp.early, grp.mid)  #combine 

#extract envfit
library(grid)

efit <- as.data.frame(scores(fit, display = "vectors"))
efit$var <- rownames(efit)
efit$p <- fit$vectors$pvals

####
library(ggnewscale)

grp.cols <- c("Bacillariophyceae" = "brown", "Bicosoecaceae" = "grey80",
              "Chlorophyta" = "yellowgreen", "Chrysophyceae" = "orange",
              "Cryptophyceae" = "forestgreen", "Cyanobacteria" = "turquoise3",
              "Desmidiales" = "purple", "Dinophyta" = "blue",
              "Euglenophyta" = "red", "Eustigmatophyceae" = "darkblue",
              "Heterokonta" = "grey60", "Synurophyceae" = "darkorange", "Haptophyta" = "orangered2",
              "unknown" == "black", "Xantophyceae" = "yellow")
nmds.ss2 <- (ggplot() + 
               geom_polygon(data=hulls, aes(x = NMDS1, y = NMDS2, fill = Period, group = Period), alpha=0.4) +
               scale_fill_manual(values = c("skyblue3", "darkblue", "goldenrod")) +
               new_scale_colour() +
               geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2, colour = grp), shape = 1, size = 2, alpha = 0.9) + 
               scale_colour_manual(values = grp.cols) +
               new_scale_colour() +
               geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, size=log(SS2$ind)), colour = "#E69F00") +
               scale_colour_manual(values = cols) +  
               geom_segment(data = efit,
                            aes(x = 0, xend = NMDS1*2, y = 0, yend = NMDS2*2),
                            arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
               geom_text(data = efit, aes(x = NMDS1*2, y = NMDS2*2, label = var, hjust = 0.75, vjust = 1), size = 3) +
               coord_equal() +
               theme_bw() + 
               theme(axis.text.x = element_blank(),  # remove x-axis text
                     axis.text.y = element_blank(), # remove y-axis text
                     axis.ticks = element_blank(),  # remove axis ticks
                     axis.title.x = element_text(size=10), # remove x-axis labels
                     axis.title.y = element_text(size=10), # remove y-axis labels
                     panel.background = element_blank(), 
                     panel.grid.major = element_blank(),  #remove major-grid labels
                     panel.grid.minor = element_blank(),  #remove minor-grid labels
                     legend.position = "right",
                     plot.background = element_blank()))
nmds.ss2
ggsave("nmds-ss2.pdf", path = "~/Documents/greenland/2019/JGR", width = 3.5, height = 3.5, units = "in", useDingbats = F)

######################################################################################
######################################################################################



SS901 <- env[which(env$Lake == "SS901"), ]
sp901 <- sp[25:39,]
names.901.red <- names(which(specnumber(sp901, MARGIN = 2) > 2))
sp901.red <- sp901[,which(colnames(sp901) %in% names.901.red)]
sp901.red <- cbind(sp901.red, sp901[,5])
names(sp901.red)[38] <- "Chrcccsaph"
grp901 <- grp[which(grp$abb %in% colnames(sp901.red)),]

ano <- adonis2(sp901.red ~ Period, data = SS901, distance = "horn", permutations = 999)
ano
sp901.horn <- vegdist(sp901.red, method = "horn")
mod <- betadisper(sp901.horn, SS901$Period, type = "centroid")
permutest(mod, pairwise = TRUE, permutations = 999) #if the dispersion is different between groups, then examine
plot(mod)
boxplot(mod)
mod.HSD <- TukeyHSD(mod)
mod.HSD
plot(mod.HSD)


ice <- metaMDS(sp901.red, distance = "horn", trymax = 200)
#Dimensions: 2 
#Stress:     0.1623078 
stressplot(ice)
ordiplot(ice, type = "points")
ordisurf(ice,SS901$DOC, main="",col="forestgreen")
points(ice, "sites", pch = 19, col = SS901$Period)
text(ice, "species", cex = 0.5)
ordihull(ice, SS901$Period, draw = "polygon", conf.int = 0.95, alpha = 20, border = NULL, col = "purple", label = T)

fit <- envfit(ice ~ DOC + DIN.TP + dSi + NH4, SS901, perm = 999)
fit
ordiArrowMul(fit)
# 0.67
plot(fit, col = "black")

#### make a nice plot####

data.scores <- as.data.frame(scores(ice))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$Period <- SS901$Period 
data.scores$ind <- SS901$ind
species.scores <- as.data.frame(scores(ice, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
species.scores$grp <- grp901$group
# make hulls
grp.ice <- data.scores[data.scores$Period == "ice", ][chull(data.scores[data.scores$Period == 
                                                                          "ice", c("NMDS1", "NMDS2")]), ]  

grp.early <- data.scores[data.scores$Period == "early", ][chull(data.scores[data.scores$Period == 
                                                                              "early", c("NMDS1", "NMDS2")]), ] 

grp.mid <- data.scores[data.scores$Period == "mid-summer", ][chull(data.scores[data.scores$Period == 
                                                                                 "mid-summer", c("NMDS1", "NMDS2")]), ] 
hulls <- rbind(grp.ice, grp.early, grp.mid)  #combine 

#extract envfit
library(grid)

efit <- as.data.frame(scores(fit, display = "vectors"))
efit$var <- rownames(efit)
efit$p <- fit$vectors$pvals

####
library(ggnewscale)
cols <- c("SS2" = "#E69F00", "SS901" = "#0072B2")
grp.cols <- c("Bacillariophyceae" = "brown", "Bicosoecaceae" = "grey80",
              "Chlorophyta" = "yellowgreen", "Chrysophyceae" = "orange",
              "Cryptophyceae" = "forestgreen", "Cyanobacteria" = "turquoise3",
              "Desmidiales" = "purple", "Dinophyta" = "blue",
              "Euglenophyta" = "red", "Eustigmatophyceae" = "darkblue",
              "Heterokonta" = "grey60", "Synurophyceae" = "darkorange", "Haptophyta" = "orangered2", 
              "unknown" == "grey90", "Xantophyceae" = "yellow")
nmds.ss901 <- (ggplot() + 
               geom_polygon(data=hulls, aes(x = NMDS1, y = NMDS2, fill = Period, group = Period), alpha=0.4) +
               scale_fill_manual(values = c("skyblue3", "darkblue", "goldenrod")) +
               new_scale_colour() +
               geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2, colour = grp), shape = 1, size = 2, alpha = 0.9) + 
               scale_colour_manual(values = grp.cols) +
               new_scale_colour() +
               geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, size=log(SS901$ind)), colour = "#0072B2") +
               scale_colour_manual(values = cols) +  
               geom_segment(data = efit,
                            aes(x = 0, xend = NMDS1*0.67, y = 0, yend = NMDS2*0.67),
                            arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
               geom_text(data = efit, aes(x = NMDS1*0.67, y = NMDS2*0.67, label = var, hjust = 0.5, vjust = 1), size = 4) +
               coord_equal() +
               theme_bw() + 
               theme(axis.text.x = element_blank(),  # remove x-axis text
                     axis.text.y = element_blank(), # remove y-axis text
                     axis.ticks = element_blank(),  # remove axis ticks
                     axis.title.x = element_text(size=10), # remove x-axis labels
                     axis.title.y = element_text(size=10), # remove y-axis labels
                     panel.background = element_blank(), 
                     panel.grid.major = element_blank(),  #remove major-grid labels
                     panel.grid.minor = element_blank(),  #remove minor-grid labels
                     legend.position="none",
                     plot.background = element_blank()))
nmds.ss901
ggsave("nmds-ss901.pdf", path = "~/Documents/greenland/2019/JGR", width = 3.5, height = 3.5, units = "in", useDingbats = F)

######################################################################################
######################################################################################
### CCAs ####
# explain variance in ice-out / early / and mid-summer separately???
sp.red.hell <- decostand(sp.red, method = "hellinger")
cca1 <- cca(sp.red.hell ~ a380. + chl + NO3 + DIN.TP + MXV + flagella + Condition(Lake), env, scale = T)
plot(cca1, scaling = 2)
ordihull(cca1, env$Period, col = "green", label = T)
anova(cca1, step=1000, by = "term")
summary(cca1)
RsquareAdj(cca1)
lake <- env[,c(1,4)]
nutr <- env[,c(8:12)]
doc <- env[,c(7,16:21)]
biol <- env[,c(6,44:47)]
varp <- varpart(sp.red.hell, nutr, doc, biol)
plot(varp)

plot(cca(sp.red.hell, doc))



################################################################################
###############################################################################
# changes in beta-diversity over time
sp.horn.beta <- vegdist(sp, method = "horn", upper = T)
sp.horn.beta <- as.matrix(sp.horn.beta)
write.csv(sp.horn.beta, "sp.horn.csv") ### would have to be re-done

sp.jaccard <- vegdist(sp, method = "jaccard", binary = T, upper = T)
sp.jaccard <- as.matrix(sp.jaccard)
write.csv(sp.jaccard, "sp.jaccard-binary.csv") 

jac.nmds <- metaMDS(sp.jaccard, k=2, trymax = 1000)
ordiplot(jac.nmds, type = "none")
ordihull(jac.nmds, env$Period)
points(jac.nmds, "sites", pch = 19, col = env$Lake)

# changes in beta
beta <- read.csv("sp.horn.beta.csv")
env <- cbind(env,beta[,2])
names(env)[41]<-"beta"

xx <- ggplot(env, aes(x = jul, y = beta.horn)) +
  geom_point(aes(x = jul, y = beta.horn, shape = Layer, color = Layer)) +
  geom_line(aes(x = jul, y = beta.horn, group = Layer, color = Layer)) +
  labs(y = "beta", x = "julian day") +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141)) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145)) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)
xx

jac <- read.csv("jac-gl.csv")
env <- cbind(env,jac[,2:3])
xx <- ggplot(env, aes(x = jul, y = jacBin)) +
  geom_point(aes(shape = Layer, color = Layer)) +
  geom_line(aes(group = Layer, color = Layer), size = 1) +
  labs(x = "Julian day", y =  expression(Temporal~beta~diversity~changes~(Jaccard~index)))+
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141)) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145)) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)
jac.plot <- xx + theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
            axis.title.y = element_text(face='plain',size=12,vjust=1),
            axis.text.x = element_text(face='plain',size=12,color='black',angle=0, hjust=0.5,vjust=0.5),
            axis.text.y = element_text(face='plain',size=12,color='black'), 
            strip.background = element_blank(),
            strip.text = element_text(size = 12),
            strip.placement = "outside",
            legend.position=c(0.9,0.9),
            legend.background = element_blank(),
            legend.title = element_blank(),
            legend.key = element_blank(),
            axis.line.y = element_line(size = 0.25, colour = "black"),
            axis.line.x = element_line(size = 0.25, colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background =  element_blank())
jac.plot
ggsave("jac-plot.pdf", path = "~/Documents/greenland/2019/JGR", width = 13, height = 9, units = "cm", useDingbats = F)

# changes in beta - Legendre and de Caceres?
source ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/beta.div.R')
ss2.beta <- beta.div(sp2, "ab.sorensen", samp = T, nperm = 999)
ss901.beta <- beta.div(sp901, "ab.sorensen", samp = T, nperm = 999)
beta.lcbd <- c(ss2.beta$LCBD, ss901.beta$LCBD)
beta.lcbd.p <- c(ss2.beta$p.LCBD, ss901.beta$p.LCBD)
env <- cbind(env, beta.lcbd)
env <- cbind(env, beta.lcbd.p)

ss2.beta$SStotal_BDtotal
ss901.beta$SStotal_BDtotal

xx <- ggplot(env, aes(x = jul, y = beta.lcbd)) +
  geom_vline(xintercept = 115, linetype=1, size = 30, color = "skyblue3", alpha = 0.3) +
  geom_vline(xintercept = 145, linetype=1, size = 45, color = "darkblue", alpha = 0.3) +
  geom_vline(xintercept = 180, linetype=1, size = 45, color = "goldenrod", alpha = 0.3) +
  geom_point(aes(x = jul, y = beta.lcbd, shape = Layer, color = Layer)) +
  labs(y = "LCBD", x = "julian day") +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141)) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145)) +
  geom_line(aes(x = jul, y = beta.lcbd, group = Layer, color = Layer), size = 1) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)

xx + theme(axis.title.x = element_blank(),
           axis.title.y = element_text(face='plain',size=12,vjust=1),
           axis.text.x = element_text(face='plain',size=12,color='black',angle=0, hjust=0.5,vjust=0.5),
           axis.text.y = element_text(face='plain',size=12,color='black'), 
           strip.background = element_blank(),
           strip.text = element_text(size = 12),
           strip.placement = "outside",
           legend.position=c(0.9,0.9),
           legend.background = element_blank(),
           legend.title = element_blank(),
           legend.key = element_blank(),
           axis.line.y = element_line(size = 0.25, colour = "black"),
           axis.line.x = element_line(size = 0.25, colour = "black"),
           panel.grid.major = element_blank(),
           panel.background =  element_blank())

ggsave("lcbd.pdf", path = "~/Documents/greenland/2019/JGR", width = 13, height = 9, units = "cm", useDingbats = F)

###############################################################################
###############################################################################
# relationship between biovolume and chlorophyll
coef(lm(chl ~ log(bio), data = env))

cor(log(env$bio), env$chl, method = "pearson") #0.5830174
cor(log(env$ind), env$chl, method = "pearson") #0.6929473

env$Period <- factor(env$Period, levels = c("ice", "early", "mid-summer"))
xx <- ggplot(env, aes(x = ind, y = chl, group = Lake)) +
  geom_point(aes(shape = Lake, color = Period), size = pig$Chl.c2*10) +
  scale_colour_manual(values = c("darkblue", "skyblue3", "goldenrod")) +
  labs(y = "chl", x = "log(density)") +
  theme(axis.title.x = element_text(face='plain',size=8,vjust=1),
          axis.title.y = element_text(face='plain',size=8,vjust=1),
          axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.5,vjust=0.5),
          axis.text.y = element_text(face='plain',size=8,color='black'), 
          strip.background = element_blank(),
          strip.text = element_text(size = 8),
          strip.placement = "outside",
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.18,0.75),
          axis.line.y = element_line(size = 0.25, colour = "black"),
          axis.line.x = element_line(size = 0.25, colour = "black"),
          panel.grid.major = element_blank(),
          panel.background =  element_blank())
xx
ggsave("bio-chl.pdf", path = "~/Documents/greenland/2019/JGR", width = 9, height = 9, units = "cm", useDingbats = F)


#### lm model and then explain residuals? ####
shapiro.test(env$chl)
lm.mod <- lm(log(ind) ~ chl, data = env)
summary(lm.mod)
plot(lm.mod)
plot(env$chl, log(env$ind))
lm.res <- lm(lm.mod$residuals ~ Haptophyta + Chrysophyceae, data = env)
summary(lm.res)
lm.res <- lm(lm.mod$residuals ~ Chl.c2, data = pig.env)
summary(lm.res)
plot(lm.res)
plot(lm.mod$residuals, env$Cryptophyceae)
plot(lm.mod$residuals, env$Chrysophyceae)
plot(lm.mod$residuals, env$Haptophyta)
plot(lm.mod$residuals, env$MX)

lm.mod <- lm(log(bio) ~ chl, data = env)
summary(lm.mod)
plot(lm.mod)

#### individuals & biovolume - additional figure ####
ind <- ggplot(env, aes(x = jul, y = ind)) +
  #geom_vline(xintercept = 115, linetype=1, size = 30, color = "skyblue3", alpha = 0.3) +
  #geom_vline(xintercept = 145, linetype=1, size = 45, color = "darkblue", alpha = 0.3) +
  #geom_vline(xintercept = 180, linetype=1, size = 45, color = "goldenrod", alpha = 0.3) +
  labs(x = "julian day", y = expression(Density~(cells.mL^"-1"))) +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141), size = 0.5) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145), size = 0.5) +
  geom_point(aes(x = jul, y = ind, color = Layer)) +
  geom_line(aes(x = jul, y = ind, group = Layer, color = Layer), size = 1) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)

ind + theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
           axis.title.y = element_text(face='plain',size=12,vjust=1),
           axis.text.x = element_text(face='plain',size=12,color='black',angle=0, hjust=0.5,vjust=0.5),
           axis.text.y = element_text(face='plain',size=12,color='black'), 
           strip.background = element_blank(),
           strip.text = element_text(size = 12),
           strip.placement = "outside",
           legend.position=c(0.9,0.9),
           legend.background = element_blank(),
           legend.title = element_blank(),
           legend.key = element_blank(),
           axis.line.y = element_line(size = 0.25, colour = "black"),
           axis.line.x = element_line(size = 0.25, colour = "black"),
           panel.grid.major = element_blank(),
           panel.background =  element_blank()) +
           annotation_logticks(sides = "l", scaled = T)

ggsave("individuals-clean.pdf", path = "~/Documents/greenland/2019/JGR", width = 13, height = 9, units = "cm", useDingbats = F)

volume <- ggplot(env, aes(x = jul, y = bio)) +
  labs(x = "julian day", y =  expression(Biovolume~(mu*'m'^"3"*.mL^"-1")))+
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141), size = 0.5) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145), size = 0.5) +
  geom_point(aes(x = jul, y = bio, color = Layer)) +
  geom_line(aes(x = jul, y = bio, group = Layer, color = Layer), size = 1) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)

volume <- volume + theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
            axis.title.y = element_text(face='plain',size=12,vjust=1),
            axis.text.x = element_text(face='plain',size=12,color='black',angle=0, hjust=0.5,vjust=0.5),
            axis.text.y = element_text(face='plain',size=12,color='black'), 
            strip.background = element_blank(),
            strip.text = element_text(size = 12),
            strip.placement = "outside",
            legend.position=c(0.9,0.15),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.title = element_blank(),
            axis.line.y = element_line(size = 0.25, colour = "black"),
            axis.line.x = element_line(size = 0.25, colour = "black"),
            panel.grid.major = element_blank(),
            panel.background =  element_blank()) +
            annotation_logticks(sides = "l", scaled = T)

volume
ggsave("biovolume-clean.pdf", path = "~/Documents/greenland/2019/JGR", width = 13, height = 9, units = "cm", useDingbats = F)


### zooplankton over time ####
zoo.total <- env %>%
  group_by(Lake, jul) %>%
  summarise_at(vars(copepoda, cladocera), mean) ### mean, not total!
zoo.total.sd <- env %>%
  group_by(Lake, jul) %>%
  summarise_at(vars(copepoda, cladocera), sd) ### mean, not total!
zoo.total$total <- zoo.total$copepoda + zoo.total$cladocera  
  

zooplankton.tot <- ggplot(NULL, aes(x = jul, y = copepoda)) +
  geom_line(data = env, aes(group = Layer, color = Layer), size = 1) +
  geom_line(data = zoo.total, aes(x = jul, y = total), linetype = 2) +
  labs(x = "julian day", y =  "Copepoda (ind per L)") +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141), size = 0.5) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145), size = 0.5) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)

zooplankton.tot + theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
               axis.title.y = element_text(face='plain',size=12,vjust=1),
               axis.text.x = element_text(face='plain',size=12,color='black',angle=0, hjust=0.5,vjust=0.5),
               axis.text.y = element_text(face='plain',size=12,color='black'), 
               strip.background = element_blank(),
               strip.text = element_text(size = 12),
               strip.placement = "inside",
               legend.position=c(0.9,0.15),
               legend.background = element_blank(),
               legend.key = element_blank(),
               legend.title = element_blank(),
               axis.line.y = element_line(size = 0.25, colour = "black"),
               axis.line.x = element_line(size = 0.25, colour = "black"),
               panel.grid.major = element_blank(),
               panel.background =  element_blank())

ggsave("zoopl-tot.pdf", path = "~/Documents/greenland/2019/JGR", width = 13, height = 9, units = "cm", useDingbats = F)

zooplankton.clad <- ggplot(NULL, aes(x = jul, y = cladocera)) +
  geom_line(data = env, aes(group = Layer, color = Layer), size = 1) +
  geom_line(data = zoo.total, linetype = 2) +
  labs(x = "julian day", y =  "Cladocera (ind per L)") +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141), size = 0.5) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145), size = 0.5) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)


zooplankton.clad <- zooplankton.clad + theme(axis.title.x = element_text(face='plain',size=8,vjust=1),
                        axis.title.y = element_text(face='plain',size=8,vjust=1),
                        axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.5,vjust=0.5),
                        axis.text.y = element_text(face='plain',size=8,color='black'), 
                        strip.background = element_blank(),
                        strip.text = element_text(size = 8),
                        strip.placement = "outside",
                        legend.position= "none",
                        legend.background = element_blank(),
                        legend.key = element_blank(),
                        legend.title = element_blank(),
                        axis.line.y = element_line(size = 0.25, colour = "black"),
                        axis.line.x = element_line(size = 0.25, colour = "black"),
                        panel.grid.major = element_blank(),
                        panel.background =  element_blank())
zooplankton.clad
ggsave("zoopl-cla.pdf", path = "~/Documents/greenland/2019/JGR", width = 13, height = 9, units = "cm", useDingbats = F)

#only total for zoops
zooplankton.tot.only <- ggplot() +
  geom_line(data = zoo.total, aes(x = jul, y = total), linetype = 2) +
  labs(x = "julian day", y =  "zooplankton (ind per L)") +
  ylim(0,NA) +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141), size = 0.5) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145), size = 0.5) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)


zooplankton.tot.only <- zooplankton.tot.only + theme(axis.title.x = element_text(face='plain',size=8,vjust=1),
                                             axis.title.y = element_text(face='plain',size=8,vjust=1),
                                             axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.5,vjust=0.5),
                                             axis.text.y = element_text(face='plain',size=8,color='black'), 
                                             strip.background = element_blank(),
                                             strip.text = element_text(size = 8),
                                             strip.placement = "outside",
                                             legend.position= "right",
                                             legend.background = element_blank(),
                                             legend.key = element_blank(),
                                             legend.title = element_blank(),
                                             axis.line.y = element_line(size = 0.25, colour = "black"),
                                             axis.line.x = element_line(size = 0.25, colour = "black"),
                                             panel.grid.major = element_blank())
zooplankton.tot.only
ggsave("zooplankton-tot-only-phyla.pdf", path = "~/Documents/greenland/2019/JGR", width = 13, height = 9, units = "cm", useDingbats = F)

#### #### ####
zoo.total.melt <- melt(zoo.total, id = c("Lake", "jul"))
zooplankton.tot.only <- ggplot(zoo.total.melt, aes(x = jul, y = value)) +
  geom_line(aes(x = jul, y = value, color = variable), linetype = 2) +
  labs(x = "julian day", y =  "zooplankton (ind per L)") +
  ylim(0,NA) +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141), size = 0.5) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145), size = 0.5) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)


zooplankton.tot.only <- zooplankton.tot.only + theme(axis.title.x = element_text(face='plain',size=8,vjust=1),
                                                     axis.title.y = element_text(face='plain',size=8,vjust=1),
                                                     axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.5,vjust=0.5),
                                                     axis.text.y = element_text(face='plain',size=8,color='black'), 
                                                     strip.background = element_blank(),
                                                     strip.text = element_text(size = 8),
                                                     strip.placement = "outside",
                                                     legend.position= "none",
                                                     legend.background = element_blank(),
                                                     legend.key = element_blank(),
                                                     legend.title = element_blank(),
                                                     axis.line.y = element_line(size = 0.25, colour = "black"),
                                                     axis.line.x = element_line(size = 0.25, colour = "black"),
                                                     panel.grid.major = element_blank())
zooplankton.tot.only


#### GALD ####
gald <- ggplot(env, aes(x = jul, y = w.medianGALD)) +
  geom_line(data = env, aes(group = Layer, color = Layer), size = 1) +
  labs(x = "julian day", y =  "abundance-weghted average GALD") +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141), size = 0.5) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145), size = 0.5) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)
gald

gald.means <- env %>%
  group_by(Lake, Date) %>%
  summarise_at(vars(w.meanGALD), list(mean, sd))

gald <- ggplot(gald.means, aes(x = Date, y = fn1)) +
  geom_point() +
  geom_line(size = 0.5, linetype = 2) +
  labs(x = "julian day", y =  "abundance-weighted average GALD") +
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2), width=0.25) +
  scale_y_continuous(position = "right", limits = c(0,NA)) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)
gald

ggsave("gald.pdf", path = "~/Documents/greenland/2019/JGR", width = 13, height = 9, units = "cm", useDingbats = F)


###############################################################################
###############################################################################
#### vertical profiles of species abundances over time ####
#sp.order <- apply(sp2, 2, sum)
#sp.order <- sort(sp.order, decreasing = T)
#sp.20.names <- names(sp.order[1:20])
#names(which(specnumber(sp2, MARGIN = 2) > 9))

ss901.names.hm <- c("Chryscccst", "centrcdtms", "Elktthrsp.", "Cryptmcf.c", 
                    "Dnphytcyst", "Prdnpsselp", "Astrnllfrm", "Aphncpssp.",
                    "Dnbryndvrg", "Kephyrnbrl", "Oocystssp.", "Dnbrynbvrc")

ss2.names.hm <- c("Aphncpssp.", "Dnbryndvrg", "Kphyrnmnlf", "Chryscccst", "centrcdtms",
                  "Btrycccsp.", "Oocystssp.", "Elktthrsp.",
                  "Cryptmcf.c", "Plgslmsnnn", "centrcdtms", "Dnphytcyst", "Wlszynssp.")

sp.901.hm <- sp901[,which(colnames(sp901) %in% ss901.names.hm)]
sp.2.hm <- sp2[,which(colnames(sp2) %in% ss2.names.hm)]

env.901.hm <- cbind(SS901[,c(1,4,15,20)], sp.901.hm)
env.2.hm <- cbind(SS2[,c(1,4,15,20)], sp.2.hm) # connect with envrionmental variables 

env.901.hm$Layer <- factor(env.901.hm$Layer, c("hypo", "meta", "epi"))
env.2.hm$Layer <- factor(env.2.hm$Layer, c("hypo", "meta", "epi"))
env.hm.melt.901 <- melt(env.901.hm, id.vars = c("jul", "Layer", "Lake", "Period"))
env.hm.melt.2 <- melt(env.2.hm, id.vars = c("jul", "Layer", "Lake", "Period"))
env.hm.melt.901$jul <- as.factor(env.hm.melt.901$jul)
env.hm.melt.2$jul <- as.factor(env.hm.melt.2$jul)


heatmap.ind <- ggplot(env.hm.melt.2, aes(x = jul, y = variable)) +
  labs(x = "julian day", y = NULL) +
  geom_tile(aes(x = jul, y = Layer, fill = log(value))) +
  scale_fill_viridis_c() +
  facet_wrap(~variable, ncol = 2)
heatmap.ind + theme(axis.title.x = element_text(face='plain',size=10,vjust=1),
            axis.title.y = element_text(face='plain',size=10,vjust=1),
            axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.5,vjust=0.5),
            axis.text.y = element_text(face='plain',size=8,color='black'), 
            strip.background = element_blank(),
            strip.text = element_text(size = 10, margin = margin(0.5, 0, 0.5, 0)),
            strip.placement = "outside",
            legend.background = element_blank(),
            legend.title = element_blank(),
            legend.key = element_blank(),
            axis.line.y = element_line(size = 0.25, colour = "black"),
            axis.line.x = element_line(size = 0.25, colour = "black"),
            panel.spacing = unit(0.1, "cm"),
            panel.grid.major = element_blank(),
            panel.background =  element_blank())

ggsave("heatmap-ss2.pdf", path = "~/Documents/greenland/2019/JGR", width = 5, height = 6, units = "in", useDingbats = F)


heatmap.ind <- ggplot(env.hm.melt.901, aes(x = jul, y = variable)) +
  labs(x = "julian day", y = NULL) +
  geom_tile(aes(x = jul, y = Layer, fill = log(value))) +
  scale_fill_viridis_c() +
  facet_wrap(~variable, ncol = 2)
heatmap.ind + theme(axis.title.x = element_text(face='plain',size=10,vjust=1),
                    axis.title.y = element_text(face='plain',size=10,vjust=1),
                    axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.5,vjust=0.5),
                    axis.text.y = element_text(face='plain',size=8,color='black'), 
                    strip.background = element_blank(),
                    strip.text = element_text(size = 10, margin = margin(0.5, 0, 0.5, 0)),
                    strip.placement = "outside",
                    legend.background = element_blank(),
                    legend.title = element_blank(),
                    legend.key = element_blank(),
                    axis.line.y = element_line(size = 0.25, colour = "black"),
                    axis.line.x = element_line(size = 0.25, colour = "black"),
                    panel.spacing = unit(0.1, "cm"),
                    panel.grid.major = element_blank(),
                    panel.background =  element_blank())

ggsave("heatmap-ss901.pdf", path = "~/Documents/greenland/2019/JGR", width = 5, height = 6, units = "in", useDingbats = F)


#########FIGURES#############

###############################################################################
###############################################################################


############### PAR and DO graph ################
### load data from 2019
setwd("~/Documents/greenland/2019/Fiobuoy/fullsets")
library(reshape2)
ss901.19 <- read.csv("ss901_2019_PAR.csv")
ss2.19 <- read.csv("ss2_2019_PAR.csv")
code <- rep("SS901", 1853)
ss901.19 <- cbind(ss901.19, code)
code <- rep("SS2", 1719)
ss2.19 <- cbind(ss2.19, code)

comb.19 <- rbind(ss901.19, ss2.19)
comb.19 <- comb.19[,c(1,3,5,7)]

comb.19 <- melt(comb.19, id = c("code", "UTC"))
comb.19$UTC <- strptime(comb.19$UTC, "%Y-%m-%d %H:%M:%S")
comb.19$h <- as.numeric(format(comb.19$UTC, "%H"))
comb.19$UTC <- as.Date(comb.19$UTC, format="%Y-%m-%d")
comb.19$jul <- as.numeric(format(comb.19$UTC, "%j"))

cols <- c("SS2" = "#E69F00", "SS901" = "#0072B2")
comb.19$variable2 <- factor(comb.19$variable, 
                                  labels = c('PAR ('*mu~'mol'~m^-2~s^-1*')',
                                             expression(DO~(m*g.L^-1))))

write.csv(comb.19, "combPAR&DO2019.csv")

light.DO.sum <- comb.19 %>% 
  group_by(jul, variable, code) %>%
  summarise_at(vars(value), mean)
write.csv(light.DO.sum, "light-DO.csv")

light.DO.sum <- read.csv("light-DO.csv")

light.DO.sum.m <- light.DO.sum %>% 
  group_by(SS901, variable, code) %>%
  summarise_at(vars(value), list(min, max))

# select only day times
comb.19 <- comb.19[which(comb.19$h > 7 & comb.19$h < 20),]


combined19 <- ggplot(comb.19, aes(jul, value)) +
  geom_point(aes(color = code), alpha = 0.3, size = 0.5, shape = 1) +
  stat_smooth(aes(color = code), se = T, method = "gam", size = 1, 
              formula = y ~ s(x, bs = "cs", k = 16), span = 0.8) +
  #scale_x_date(date_breaks = "1 month", strftime(comb.19$UTC, "%m%d"))  +
  geom_vline(xintercept = 141, linetype=4, color = "#E69F00") +
  geom_vline(xintercept = 145, linetype=4, color = "#0072B2") +
  ylab(NULL) +
  xlab("Julian day") +
  facet_wrap(~variable2, ncol = 1, scales = "free_y", labeller = label_parsed, strip.position = "left") +
  scale_colour_manual(values = cols)

combined19 <- combined19 + theme(axis.title = element_text(face='plain',size=9,vjust=1),
                                         axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.5,vjust=0.5),
                                         axis.text.y = element_text(face='plain',size=8,color='black'), 
                                         strip.background = element_blank(),
                                         strip.text = element_text(size = 12),
                                         strip.placement = "outside",
                                         legend.position=c(0.08,0.9),
                                         legend.background = element_blank(),
                                         legend.key = element_blank(),
                                         legend.title = element_blank(),
                                         axis.line.y = element_line(size = 0.25, colour = "black"),
                                         axis.line.x = element_line(size = 0.25, colour = "black"),
                                         panel.grid.major = element_blank(),
                                         panel.background =  element_blank())
combined19
ggsave("fig2-8-19h.pdf", plot = combined19, path = "~/Documents/greenland/2019/JGR", device = "pdf",
       width = 11.5, height = 9.5, units = "cm", dpi = 500)


comb.19 <- comb.19 %>%
  group_by(code, variable) %>%
  mutate(VR = value / max (value, na.rm = TRUE))

combined19 <- ggplot(comb.19, aes(jul, VR)) +
  geom_point(aes(color = code), alpha = 0.3, size = 0.5) +
  stat_smooth(aes(color = code), se = T, method = "gam", size = 2, 
              formula = y ~ s(x, bs = "cs", k = 16), span = 0.8) +
  #scale_x_date(date_breaks = "1 month", strftime(comb.19$UTC, "%m%d"))  +
  geom_vline(xintercept = 141, linetype=4, color = "#E69F00") +
  geom_vline(xintercept = 145, linetype=4, color = "#0072B2") +
  ylab(NULL) +
  xlab("Julian day") +
  facet_wrap(~variable2, ncol = 1, scales = "free_y", labeller = label_parsed, strip.position = "left") +
  scale_colour_manual(values = cols)

combined19 <- combined19 + theme(axis.title = element_text(face='plain',size=8,vjust=1),
                                 axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.5,vjust=0.5),
                                 axis.text.y = element_text(face='plain',size=8,color='black'), 
                                 strip.background = element_blank(),
                                 strip.text = element_text(size = 12),
                                 strip.placement = "outside",
                                 legend.position=c(0.08,0.9),
                                 legend.background = element_blank(),
                                 legend.key = element_blank(),
                                 legend.title = element_blank(),
                                 axis.line.y = element_line(size = 0.25, colour = "black"),
                                 axis.line.x = element_line(size = 0.25, colour = "black"),
                                 panel.grid.major = element_blank(),
                                 panel.background =  element_blank())
combined19


######### FIG 3 ##########
setwd("~/Documents/greenland/2019/turner-profiles/plots")
lakes <- read.csv("lakes.csv")
lakes$week <- factor(lakes$week, levels = c("Apr 22-23", "Apr 27-29", "May 16-20 (ice out)", 
                                            "May 21-25 (ice-out)", "May 27-30", "May 31-June 2",
                                            "June 24-30", "July 1-7", "July 8"))

#skyblue - darkblue - goldenrod
library(colorRamps)
# "#0000AA", "#0055FF", "#00AAFF", "#55FFFF", "#AAFFAA", "#FFFF55", "#FFAA00", "#FF5500", "#AA0000"
ml <- c("skyblue1", "skyblue3", "cornflowerblue", "cornflowerblue", "royalblue", "darkblue",  "gold1", "goldenrod", "goldenrod4")
       
profiles_c3 <- ggplot(lakes, aes(x=depth, y=value, group = week)) +
  geom_point(aes(x=depth, y=value, color = week), size = 0.7, alpha = 0.4) +
  scale_x_reverse() + 
  stat_smooth(aes(x=depth, y=value, color = week), method = "gam", formula = y ~ s(x, k = 12, bs = "cs")) +
  coord_flip() +
  ylab(NULL) + 
  facet_wrap(lake~variable, scales = "free")  +
  scale_color_manual(values = ml)
profiles_c3 <- profiles_c3 + theme(
  axis.title = element_text(face='plain',size=12,vjust=1),
  axis.text.x = element_text(face='plain',size=10,color='black',angle=0, hjust=0.6,vjust=0.5),
  axis.text.y = element_text(face='plain',size=10,color='black',hjust=1), 
  strip.background = element_blank(),
  strip.text = element_text(face='plain',size=12,vjust=1),
  legend.background = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(face='plain',size=12,hjust=0),
  legend.key = element_blank(),
  legend.position = "none",
  panel.border = element_rect(color = "gray80", fill = NA),
  axis.line.y = element_line(size = 0.25, colour = "black"),
  axis.line.x = element_line(size = 0.25, colour = "black"),
  panel.grid.major.x = element_line(size = 0.2, colour = "gray"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background =  element_blank())
profiles_c3

ggsave("fig3.pdf", profiles_c3, path = "~/Documents/greenland/2019/JGR", height = 4, width = 8.5, units = "in")
ggsave("fig3a.pdf", profiles_c3, path = "~/Documents/greenland/2019/JGR", height = 3.5, width = 5.5, units = "in")
############# FIG 4 ################
# use env from above
env.2 <- env
env.2$Period <- factor(env.2$Period, levels = c("mid-summer", "early", "ice"))
melt.ice <- melt(env.2, id = c("Lake", "Layer", "Period"))
melt.ice$Period <- factor(melt.ice$Period, levels = c("mid-summer", "early", "ice"))
vars <- c("chl", "TP", "NO3", "NH4", "DIN.TP", "DOC", "Cond", "dSi", 
          "a380", "SR", "SUVA254", 
          "MX", "MXV", "flagella")
#vars1 <- c("chl", "TP", "NO3", "NH4", "DIN.TP", "DOC", "a380.", "a380N", "SR", "SUVA254")
melt.ice.vars <- melt.ice[melt.ice$variable %in% vars,]
melt.ice.vars$value <- as.numeric(melt.ice.vars$value)
melt.ice.vars$Layer <- factor(melt.ice.vars$Layer, levels = c("epi", "meta", "hypo"))
melt.ice.vars$variable <- factor(melt.ice.vars$variable, levels = c("chl", "DOC", "NO3", "a380",
                                                                    "NH4", "SR", "TP", "SUVA254", 
                                                                    "DIN.TP", "Cond", "dSi",
                                                                     "MX", "MXV", "flagella"))
#melt.ice.vars$variable <- factor(melt.ice.vars$variable, levels = c("chl", "DOC", "NO3", "a380.",
#                                                                    "a380N", "NH4", "SR", "TP", "SUVA254", 
#                                                                    "DIN.TP"))

melt.ice.vars$variable2 <- factor(melt.ice.vars$variable, 
                                  labels = c(expression(chl~a~(mu*g.L^{"-"})),
                                             expression(DOC~(mg.L^{"-"})),
                                             expression(NO[3]^{"-"}~(mu*g.L^{"-"})),
                                             expression(a[380]~(m^{"-"})),
                                             expression(NH[4]^{"+"}~(mu*g.L^{"-"})),
                                             expression(S[R]),
                                             expression(TP~(mu*g.L^{"-"})),
                                             expression(SUVA[254]~(L~m*g*C^{"-"}*m^{"-"})),
                                             "DIN:TP",
                                             expression(Conductivity~(mu*S.cm^{"-"})),
                                             expression(dSi~(mu*g.L^{"-"})),
                                             expression('%'*mixotrophy~D),
                                             expression('%'*mixotrophy~V),
                                             expression('%'*motile)
                                              ))
#expression(Biovolume~(mu*'m'^"3"~10^"-4"~.mL^"-1")),
#expression(Density~(cells.mL^"-1"))


melt.ice.vars$variable2 <- factor(melt.ice.vars$variable, 
                                  labels = c(expression(chl~a~(mu*g.L^{"-"})),
                                             expression(DOC~(mg.L^{"-"})),
                                             expression(NO[3]^{"-"}~(mu*g.L^{"-"})),
                                             expression(a[380]~(m^{"-"})),
                                             expression(a^{"*"}[380]~(L~m*g*C^{"-"}*m^{"-"})),
                                             expression(NH[4]^{"+"}~(mu*g.L^{"-"})),
                                             expression(S[R]),
                                             expression(TP~(mu*g.L^{"-"})),
                                             expression(SUVA[254]~(L~m*g*C^{"-"}*m^{"-"})),
                                             "DIN:TP"))


#melt.ice.vars.ss2 <- melt.ice.vars[which(melt.ice.vars$Lake == "SS2"), ]

ice.diff.s2 <- ggplot(data = melt.ice.vars, aes(x = Layer, y = value)) +
  geom_boxplot(aes(x = Layer, y = value, color = Period, fill = Period)) +
  scale_fill_manual(values = alpha(c("goldenrod", "darkblue", "skyblue3"), 0.4)) +
  scale_color_manual(values = c("goldenrod", "darkblue", "skyblue3")) +
  scale_x_discrete(limits=c("hypo","meta","epi")) +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + 
  facet_wrap (variable2~Lake, scales = "free_x", labeller = label_parsed, nrow = 4, dir = "v")
  
ice.diff.s2 <- ice.diff.s2 + theme(axis.title = element_blank(),
                                   axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.65, vjust=1.5),
                                   axis.text.y = element_text(face='plain',size=9,color='black',hjust=1), 
                                   strip.background = element_blank(),
                                   legend.justification = "right",
                                   legend.background = element_blank(),
                                   legend.title = element_blank(),
                                   legend.text.align = 0,
                                   strip.text = element_text(face='bold',size=9,color='black',hjust=1, margin = margin(1, 0, 1, 0)), 
                                   legend.text = element_text(face='plain',size=9,color='black',hjust=1), 
                                   legend.key = element_blank(),
                                   axis.line.y = element_line(size = 0.25, colour = "black"),
                                   axis.line.x = element_line(size = 0.25, colour = "black"),
                                   panel.spacing = unit(.15, "cm"),
                                   panel.grid.major = element_blank(),
                                   panel.border = element_rect(color = "gray", fill = NA),
                                   panel.background =  element_blank())
ice.diff.s2
ggsave("fig4-new.pdf", path = "~/Documents/greenland/2019/JGR", ice.diff.s2, device = "pdf", 
       height = 7, width = 12, units = "in")

melt.ice.vars.ss901 <- melt.ice.vars[which(melt.ice.vars$Lake == "SS901"), ]

ice.diff.s901 <- ggplot(data = melt.ice.vars.ss901, aes(x = Layer, y = value)) +
  geom_boxplot(aes(x = Layer, y = value, color = ice, fill = ice)) +
  scale_fill_manual(values = alpha(c("goldenrod", "palegreen4", "darkblue"), 0.4)) +
  scale_color_manual(values = c("goldenrod", "palegreen4", "darkblue")) +
  scale_x_discrete(limits=c("hypo","meta","epi")) +
  coord_flip() + 
  facet_grid(~variable2, labeller = label_parsed, scales = "free")

ice.diff.s901 <- ice.diff.s901 + theme(axis.title = element_blank(),
                                   axis.text.x = element_text(face='plain',size=12,color='black',angle=0, hjust=0.5,vjust=0.5),
                                   axis.text.y = element_text(face='plain',size=10,color='black',hjust=1), 
                                   strip.background = element_blank(),
                                   legend.justification = "right",
                                   legend.background = element_blank(),
                                   legend.title = element_blank(),
                                   legend.text.align = 0,
                                   strip.text = element_text(face='bold',size=12,color='black',hjust=1), 
                                   legend.text = element_text(face='plain',size=12,color='black',hjust=1), 
                                   axis.line.y = element_line(size = 0.25, colour = "black"),
                                   axis.line.x = element_line(size = 0.25, colour = "black"),
                                   panel.grid.major = element_blank(),
                                   panel.border = element_rect(color = "gray", fill = NA),
                                   panel.background =  element_blank())
ice.diff.s901
ggsave("fig4B.pdf", path = "~/Documents/greenland/2019/JGR", ice.diff.s901, device = "pdf", 
       height = 4, width = 18, units = "in")


########## fig 4 for presentation #############


#### fig with bio vars for fig.5 ####
biovars <- c("MX", "MXV", "flagella", "flagellaV")
melt.ice.biovars <- melt.ice[melt.ice$variable %in% biovars,]
melt.ice.biovars$value <- as.numeric(melt.ice.biovars$value)
melt.ice.biovars$Layer <- factor(melt.ice.biovars$Layer, levels = c("epi", "meta", "hypo"))
melt.ice.biovars$Period <- factor(melt.ice.biovars$Period, levels = c("ice", "early", "mid-summer"))
melt.ice.biovars$variable <- factor(melt.ice.biovars$variable, levels = c("MX", "MXV", "flagella", "flagellaV"))

biovars.D <- c("MX")  #, "flagella"
melt.ice.biovars.density <- melt.ice.biovars[melt.ice.biovars$variable %in% biovars.D,]

fig4.biovars <- ggplot(data = melt.ice.biovars.density, aes(x = Layer, y = value)) +
  geom_boxplot(aes(x = Layer, y = value, color = Layer, fill = Period, linetype = variable), size = 0.25) +
  scale_fill_manual(values = alpha(c("skyblue3", "darkblue", "goldenrod"), 0.4)) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  scale_linetype_manual(values = c(1,3)) + 
  scale_x_discrete(limits=c("hypo","meta","epi")) +
  coord_flip() + 
  ylim(1,100) +
  facet_wrap (Lake~Period, scales = "free_x", ncol = 3, dir = "h")

fig4.biovars <- fig4.biovars + theme(axis.title = element_blank(),
                                   axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.65, vjust=1.5),
                                   axis.text.y = element_text(face='plain',size=9,color='black',hjust=1), 
                                   strip.background = element_blank(),
                                   legend.justification = "right",
                                   legend.background = element_blank(),
                                   legend.title = element_blank(),
                                   legend.text.align = 0,
                                   strip.text = element_text(face='bold',size=9,color='black',hjust=1, margin = margin(1, 0, 1, 0)), 
                                   legend.text = element_text(face='plain',size=9,color='black',hjust=1), 
                                   legend.key = element_blank(),
                                   axis.line.y = element_line(size = 0.25, colour = "black"),
                                   axis.line.x = element_line(size = 0.25, colour = "black"),
                                   panel.spacing = unit(.15, "cm"),
                                   panel.grid.major = element_blank(),
                                   panel.border = element_rect(color = "gray", fill = NA),
                                   panel.background =  element_blank())
fig4.biovars
ggsave("fig4-biovars-density-2.pdf", path = "~/Documents/greenland/2019/JGR", fig4.biovars, device = "pdf", 
       height = 6, width = 11, units = "cm")


biovars.B <- c("MXV") #, "flagellaV"
melt.ice.biovars.biovolume <- melt.ice.biovars[melt.ice.biovars$variable %in% biovars.B,]

fig4.biovars <- ggplot(data = melt.ice.biovars.biovolume, aes(x = Layer, y = value)) +
  geom_boxplot(aes(x = Layer, y = value, color = Layer, fill = Period, linetype = variable), size = 0.25) +
  scale_fill_manual(values = alpha(c("skyblue3", "darkblue", "goldenrod"), 0.4)) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  scale_linetype_manual(values = c(1,3)) + 
  scale_x_discrete(limits=c("hypo","meta","epi")) +
  coord_flip() + 
  ylim(1,100) +
  facet_wrap (Lake~Period, scales = "free_x", ncol = 3, dir = "h")

fig4.biovars <- fig4.biovars + theme(axis.title = element_blank(),
                                     axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.65, vjust=1.5),
                                     axis.text.y = element_text(face='plain',size=9,color='black',hjust=1), 
                                     strip.background = element_blank(),
                                     legend.justification = "right",
                                     legend.background = element_blank(),
                                     legend.title = element_blank(),
                                     legend.text.align = 0,
                                     strip.text = element_text(face='bold',size=9,color='black',hjust=1, margin = margin(1, 0, 1, 0)), 
                                     legend.text = element_text(face='plain',size=9,color='black',hjust=1), 
                                     legend.key = element_blank(),
                                     axis.line.y = element_line(size = 0.25, colour = "black"),
                                     axis.line.x = element_line(size = 0.25, colour = "black"),
                                     panel.spacing = unit(.15, "cm"),
                                     panel.grid.major = element_blank(),
                                     panel.border = element_rect(color = "gray", fill = NA),
                                     panel.background =  element_blank())
fig4.biovars
ggsave("fig4-biovars-biovol-2.pdf", path = "~/Documents/greenland/2019/JGR", fig4.biovars, device = "pdf", 
       height = 6, width = 11, units = "cm")



#####################################################
############# MODELS ################################
#####################################################
m <- lm(log(TP) ~ chl, data = env)

plot(chl ~ DOC, data = ice.log)
plot(chl ~ TP, data = ice.log)
plot(chl ~ DIN.TP, data = ice.log)

library(car)

shapiro.test(ice$chl)
shapiro.test(ice$TP)
shapiro.test(ice$DOC) 
shapiro.test(ice$NO3)
shapiro.test(ice$NH4)
shapiro.test(ice$DIN.TP)

ice.log <- ice
for (i in c(6:11)) {
  ice.log[i] <- log(ice[i])
} # log norm all vars
ice.log$chl <- log(ice$chl + 1) # chl log(x+1)
shapiro.test(ice.log$DIN.TP) 



### after transformation, chl OK, TP Ok-ish, DIN.TP OK-ish
### DOC, NO3, and NH4 need something different

lm <- lm(chl ~ DOC + DIN.TP, data = ice.log)
summary(lm)
aov(lm)
plot(lm)

leveneTest(chl ~ ice*Layer, data = ice.log, center = mean) 
boxplot(chl ~ ice*Layer, data = ice.log)
fit <- aov(chl ~ ice*Layer + Lake, data = ice.log) 
summary(fit)
TukeyHSD(fit)

#The following code will run another ANOVA model with a define interaction term and a tukey's test comparison
ixn <- with(ice.log, interaction(ice,Layer))
fit2 <- aov(chl ~ ixn + Lake, data = ice.log)
tukey <- glht(fit2, linfct = mcp(ixn = "Tukey"))
summary(tukey)
tukey.cld <- cld(tukey) 

#You can view the letter comparisons for each group by graphing:
opar <- par(mai=c(1,1,1.5,1))
plot(tukey.cld)
par(opar)

###### PREDICTORS OF CHL ICE x UNDER ICE ########

### RT
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(randomForest)
vars <- ice[,6:16]

rt <- rpart(chl ~ DOC + TP + NO3 + NH4 + DIN.TP + epi, data = ice)
par(mfrow = c(1,2), xpd = NA)
plot(rt)
text(rt, use.n = TRUE)

rf <- randomForest(chl ~ ., data = ice[,c(5:12,15,16)], 
                   importance = T, ntree = 5000, na.action=na.omit) # all 2019
rf
plot(rf)
varImpPlot(rf, cex = 1)


ice.only <- ice[which(ice$ice == "ice"),]
rf <- randomForest(chl ~ ., data = ice.only[,c(5:12,15,16)], 
                   importance = T, ntree = 5000, na.action=na.omit) # all 2019
rf
plot(rf)
varImpPlot(rf, cex = 1)

plot(chl ~ TP, data = ice.only)




p <- ggplot(env, aes(x = a380, y = log(ind), group = Ice)) +
geom_point(aes(x = a380, y = log(ind), color = Ice)) +
  geom_smooth()

p

ice.only <- env[which(env$Ice == "ice"),]
early.only <- env[which(env$Period == "early"),]
mid.only <- env[which(env$Period == "mid-summer"),]
ice.early <- env[which(env$Ice == "ice" |
                      env$Period == "early"),]

p <- ggplot(ice.only, aes(x = a380, y = chl)) +
  geom_point(aes(color = Lake, shape = Layer)) +
  geom_smooth(method = "gam", formula = y ~ poly(x, 2))

p

p <- ggplot(ice.only, aes(x = a380, y = bio)) +
  geom_point(aes(color = Lake, shape = Layer)) +
  geom_smooth(method = "gam", formula = y ~ log(x))

p



p <- ggplot(env, aes(x = jul, y = log(ind), group = Lake)) +
  geom_point(aes(x = jul, y = log(ind), color = Lake)) +
  geom_smooth(method = "gam", formula = y ~ poly(x, 2), aes(color = Lake))

p


melt.ice.jul <- melt(env.2, id = c("Lake", "Layer", "Period", "jul"))
vars <- c("chl", "TP", "NO3", "NH4", "DIN.TP", "DOC", "a380", "a380.", "SUVA254", "shannon", "beta", "richR", "MX", "MXV", "ind", "bio", "flagella")
melt.ice.jul.v <- melt.ice.jul[melt.ice.jul$variable %in% vars,]
melt.ice.jul.v$value <- as.numeric(melt.ice.jul.v$value)
p <- ggplot(melt.ice.jul.v, aes(x = jul, y = value, group = Lake)) +
  geom_point(aes(x = jul, y = value, color = Lake, shape = Layer)) +
  geom_smooth(method = "gam", formula = y ~ poly(x, 2), aes(color = Lake)) +
  facet_wrap(~variable, scales = "free_y")
p + theme_classic()

p <- ggplot(melt.ice.jul.v, aes(x = jul, y = value, group = Lake)) +
  geom_point(aes(x = jul, y = value, color = Lake, shape = Layer)) +
  geom_path(aes(x = jul, y = value, color = Lake, shape = Layer)) +
  facet_wrap(~variable, scales = "free_y")
p + theme_classic()

xx <- ggplot(env, aes(x = jul, y = a380)) +
  geom_point(aes(shape = Layer, color = Layer)) +
  labs(y = "a380", x = "julian day") +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141)) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145)) +
  geom_line(aes(group = Layer, color = Layer), size = 1) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)

xx + theme(axis.title.x = element_blank(),
           axis.title.y = element_text(face='plain',size=12,vjust=1),
           axis.text.x = element_text(face='plain',size=12,color='black',angle=0, hjust=0.5,vjust=0.5),
           axis.text.y = element_text(face='plain',size=12,color='black'), 
           strip.background = element_blank(),
           strip.text = element_text(size = 12),
           strip.placement = "outside",
           legend.position=c(0.9,0.9),
           legend.background = element_blank(),
           legend.title = element_blank(),
           legend.key = element_blank(),
           axis.line.y = element_line(size = 0.25, colour = "black"),
           axis.line.x = element_line(size = 0.25, colour = "black"),
           panel.grid.major = element_blank(),
           panel.background =  element_blank())


p <- ggplot(ice.only, aes(x = log(DOC), y = ind)) +
  geom_point(aes(color = Lake, shape = Layer)) +
  geom_smooth(method = "gam", formula = y ~ poly(x, 2))
p

shapiro.test(env$a380)
shapiro.test(log(env$DIN.TP))
shapiro.test(log(env$bio))

lm.m <- lm(log(bio) ~ log(a380) , data = ice.early)
summary(lm.m)

plot(lm.m)





library(corrr)
library(here)
library(dplyr)

env.x <- env[,c(6:12, 44:45, 53:56)]
ss2.x <- SS2[,c(6:12, 14, 15, 42, 43, 49:50, 52:56)]

rs <- correlate(env.x, method = "spearman", diagonal = 1)
rs %>%
  focus(MXV) %>%
  mutate(rowname = reorder(rowname, MXV)) %>%
  ggplot(aes(rowname, MXV)) +
  geom_col() + coord_flip()

rs %>%
  focus(bio) %>%
  mutate(rowname = reorder(rowname, bio)) %>%
  ggplot(aes(rowname, bio)) +
  geom_col() + coord_flip()

env.x %>% # all
  correlate(method = "spearman") %>% 
  focus(ind, bio)

ice.x <- ice.only[,c(6:12, 44:45, 53:54)]
ice.x %>% 
  correlate(method = "spearman") %>% 
  focus(ind, bio)
early.x <- early.only[,c(6:12, 44:45, 53:54)]
early.x %>% 
  correlate(method = "spearman") %>% 
  focus(ind, bio)
mid.x <- mid.only[,c(6:12, 44:45, 53:54)]
md <- mid.x %>% 
  correlate(method = "spearman") %>% 
  focus(ind, bio)


mid.ss2 <- ss2.x[c(16:24),]
  mid.ss2 %>% correlate(method = "spearman") %>% 
  focus(ind, bio)


spear <- read.csv("spearman.csv")

vars.spear <- c("DOC", "a380", "DIN.TP", "NH4", "NO3", "TP", "copepoda", "cladocera")
spear.sel <- spear[spear$var %in% vars.spear,]
spear.sel$var <- factor(spear.sel$var, levels = c("DOC", "a380", "DIN.TP", "NH4", "NO3", "TP", "copepoda", "cladocera"))

spear.sel$var2 <- factor(spear.sel$var, 
                           labels = c( expression(DOC),
                                       expression(a[380]),
                                       "DIN:TP",
                                       expression(NH[4]^{"+"}),
                                       expression(NO[3]^{"-"}),
                                       expression(TP),
                                       expression(Copepoda),
                                       expression(Cladocera)))


spear.sel$period <- factor(spear.sel$period, levels = c("ice", "early", "mid-summer"))
spearman.plot.ind <- ggplot(data = spear.sel, aes(x = var, y = ind)) +
  geom_col(aes(color = period, fill = period), position = position_dodge() , width = 0.7) +
  scale_fill_manual(values = alpha(c("skyblue3", "darkblue", "goldenrod"), 0.4)) +
  scale_color_manual(values = c("skyblue3", "darkblue", "goldenrod")) +
  ylab(expression(Spearman~rho)) +
  xlab(NULL) +
  scale_x_discrete(labels = c(expression(DOC),
                   expression(a[380]),
                   "DIN:TP",
                   expression(NH[4]^{"+"}),
                   expression(NO[3]^{"-"}),
                   expression(TP),
                   expression(Cop),
                   expression(Cla)))

sp.ind <- spearman.plot.ind + theme(axis.title.x = element_text(face='plain',size=8,vjust=1),
      axis.title.y = element_text(face='plain',size=8,vjust=1),
      axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.5,vjust=0.5),
      axis.text.y = element_text(face='plain',size=8,color='black'), 
      axis.line.y = element_line(size = 0.25, colour = "black"),
      axis.line.x = element_line(size = 0.25, colour = "black"),
      panel.grid.major = element_blank(),
      legend.position = "none",
      panel.background =  element_blank())
sp.ind
ggsave("sp.ind.pdf", path = "~/Documents/greenland/2019/JGR", width = 8.5, 
       height = 4, units = "cm", useDingbats = F)



spearman.plot.bio <- ggplot(data = spear.sel, aes(x = var, y = bio)) +
  geom_col(aes(color = period, fill = period), position = position_dodge(), width = 0.7) +
  scale_fill_manual(values = alpha(c("skyblue3", "darkblue", "goldenrod"), 0.4)) +
  scale_color_manual(values = c("skyblue3", "darkblue", "goldenrod")) +
  ylab(expression(Spearman~rho)) +
  xlab(NULL) +
  scale_x_discrete(labels = c(expression(DOC),
                              expression(a[380]),
                              "DIN:TP",
                              expression(NH[4]^{"+"}),
                              expression(NO[3]^{"-"}),
                              expression(TP),
                              expression(Cop),
                              expression(Cla)))
sp.bio <- spearman.plot.bio + theme(axis.title.x = element_text(face='plain',size=8,vjust=1),
                          axis.title.y = element_text(face='plain',size=8,vjust=1),
                          axis.text.x = element_text(face='plain',size=8,color='black',angle=0, hjust=0.5,vjust=0.5),
                          axis.text.y = element_text(face='plain',size=8,color='black'), 
                          axis.line.y = element_line(size = 0.25, colour = "black"),
                          axis.line.x = element_line(size = 0.25, colour = "black"),
                          panel.grid.major = element_blank(),
                          legend.position = "none",
                          panel.background =  element_blank())
sp.bio
ggsave("sp.bio.pdf", path = "~/Documents/greenland/2019/JGR", width = 8.5, 
       height = 4, units = "cm", useDingbats = F)


ice.early.x <- ice.early[,c(6:12, 16:17, 44:45)]
ice.early.x %>% 
  correlate(method = "spearman") %>% 
  focus(ind, bio)

x <- vector("numeric", 25L)
for (i in 1:25) {
  x[i] <- i^2
  sum(x)
}


pchl <- ggplot(env, aes(x = jul, y = richR)) +
  labs(x = "julian day", y = "rich") +
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141), size = 0.5) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145), size = 0.5) +
  geom_point(aes(shape = Layer, color = Layer)) +
  geom_line(aes(group = Layer, color = Layer), size = 1) +
  scale_color_manual(values = c("gray70", "gray40", "gray20")) +
  facet_wrap(~Lake, scales = "free_y", ncol = 1)
pchl


xx <- ggplot(melt.all, aes(x = Layer, y=value)) +
  geom_bar(aes(fill = variable), stat="identity") +
  scale_fill_manual(values = grp.cols) +
  scale_x_discrete(limits=c("hypo","meta","epi")) +
  labs(x = NULL, y = expression(Density~(cells.mL^"-1"))) +
  coord_flip() + 
  facet_grid(Date~Lake, scales = "free") +
  theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
        axis.title.y = element_text(face='plain',size=12,vjust=1),
        axis.text.x = element_text(face='plain',size=9,color='black',angle=0, hjust=0.5,vjust=0.5),
        axis.text.y = element_text(face='plain',size=9,color='black'), 
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        strip.placement = "outside",
        axis.line.y = element_line(size = 0.25, colour = "black"),
        axis.line.x = element_line(size = 0.25, colour = "black"),
        panel.grid.major = element_blank(),
        panel.background =  element_blank())
xx


##################################################################
# prep only species present under ice

remove_zero_cols <- function(df) {
  rem_vec <- NULL
  for(i in 1:ncol(df)){
    this_sum <- summary(df[,i])
    zero_test <- length(which(this_sum == 0))
    if(zero_test == 6) {
      rem_vec[i] <- names(df)[i]
    }
  }
  features_to_remove <- rem_vec[!is.na(rem_vec)]
  rem_ind <- which(names(df) %in% features_to_remove)
  df <- df[,-rem_ind]
  return(df)
}

sp.ice.red <- remove_zero_cols(sp.ice)
sp.ice.red <- as.data.frame(t(sp.ice.red))
names.ice <- rownames(sp.ice.red)

grp.ice.size <- grp[which(grp$abb %in% names.ice), ]

grp.ice.size %>% group_by(trophy) %>% summarise_at(vars(GALD, GALDcolony), list(mean, sd, median, IQR), na.rm = T)


sp.open.red <- remove_zero_cols(sp.open)
sp.open.red <- as.data.frame(t(sp.open.red))
names.open <- rownames(sp.open.red)

grp.open.size <- grp[which(grp$abb %in% names.open), ]
grp.open.size %>% group_by(trophy) %>% summarise_at(vars(GALD, GALDcolony), list(mean, sd, median, IQR), na.rm = T)


### calculate beta-diversity
library(betapart)

# swithc to binary
sp.red.bin <-ifelse(sp.red > 0,1,0)

# Now, we'll calculate the Jaccard index and its partitions of turnover and nestedness. We can calculate Sorensen index instead by using the argument     index.family="sorensen"    .

dist <- beta.pair(sp.red.bin, index.family="jaccard") # binary jac
turn <- as.matrix(dist[[1]]) # pairwise Jaccard index turnover partition between communities
nest <- as.matrix(dist[[2]]) # nestedness
all <- as.matrix(dist[[3]]) # all beta

bd <- betadisper(dist[[3]], env$Period)
plot(bd)
boxplot(bd)

write.csv(turn, file = "replacement-jac-bin.csv")
write.csv(nest, file = "nest-jac-bin.csv")
write.csv(all, file = "all-jac-bin.csv")

##### combined layers #####
###########################

sp.x.bin <-ifelse(sp.x > 0,1,0)
rowSums(sp.x.bin)
# calculate the Jaccard index and its partitions of turnover and nestedness

dist <- beta.pair(sp.x.bin, index.family="jaccard") # binary jac
turn <- as.matrix(dist[[1]]) # pairwise Jaccard index turnover partition between communities
nest <- as.matrix(dist[[2]]) # nestedness
all <- as.matrix(dist[[3]]) # all beta

group <- factor(c(rep(1,2), rep(2,3), rep(3,3), rep(1,2), rep(2,2), rep(3,1)), labels = c("ice","early","mid"))
bd <- betadisper(dist[[3]], group)
plot(bd)

write.csv(turn, file = "replacement-jac-bin2.csv")
write.csv(nest, file = "nest-jac-bin2.csv")
write.csv(all, file = "all-jac-bin2.csv")

# with abundances
dist <- beta.pair.abund(sp.x, index.family = "ruzicka")
dist[[1]] # pairwise Jaccard index turnover partition between communities
dist[[2]] # nestedness
dist[[3]] # all beta

bd <- betadisper(dist[[3]], env$Period) # jacc based on abundance
plot(bd)


bd <- betadisper(sp.horn, env$Period) # compare with Morisita-Horn
plot(bd)


jac <- read.csv("jac-gl-all.csv")
jac.melt <- melt(jac, id.vars = c("lake", "jul"), measure.vars = c("all", "rep", "nest"))

xx <- ggplot(jac.melt, aes(x = jul, y = value)) +
  geom_line(aes(group = variable, color = variable), size = 1) +
  labs(x = "Julian day", y =  expression(Temporal~beta~diversity~changes~(Jaccard~index)))+
  geom_vline(data=filter(env, Lake =="SS2"), aes(xintercept = 141)) +
  geom_vline(data=filter(env, Lake =="SS901"), aes(xintercept = 145)) +
  scale_color_manual(values = c("black", "red", "blue")) +
  xlim(110,190) +
  ylim(0,1) +
  facet_wrap(~lake, scales = "free_y", ncol = 1)
jac.plot <- xx + theme(axis.title.x = element_text(face='plain',size=12,vjust=1),
                       axis.title.y = element_text(face='plain',size=12,vjust=1),
                       axis.text.x = element_text(face='plain',size=12,color='black',angle=0, hjust=0.5,vjust=0.5),
                       axis.text.y = element_text(face='plain',size=12,color='black'), 
                       strip.background = element_blank(),
                       strip.text = element_text(size = 12),
                       strip.placement = "outside",
                       legend.position=c(0.9,0.9),
                       legend.background = element_blank(),
                       legend.title = element_blank(),
                       legend.key = element_blank(),
                       axis.line.y = element_line(size = 0.25, colour = "black"),
                       axis.line.x = element_line(size = 0.25, colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.background =  element_blank())
jac.plot
ggsave("jac-plot2.pdf", path = "~/Documents/greenland/2019/JGR", width = 13, height = 9, units = "cm", useDingbats = F)


nphy <- read.csv("LTER_phytoplankton84-15.csv")
df <- nphy %>% group_by(sampledate, lakename) %>% summarise_at(vars(total_biovol), sum)

ggplot(df, aes(sampledate, total_biovol)) +
  geom_point() +
  facet_wrap(~lakename, scales = "free_y", ncol = 1)
