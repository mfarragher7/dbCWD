# Script for running SO (based on Mouillot et al 2005, Oecologia)
# ---------------------------------------

# Beatrix Beisner
# Jan 12 2017 update
# compares 2 groups of phytoplankton at a time and then sums the values

# Mary Lofton
# 27JUL19 update
# tidying code for publication

rm(list=ls(all=TRUE))

#load packages
#the 'pacman' package automatically installs and loads all required packages
#install.packages('pacman')
pacman::p_load(tidyverse)

#############################################################################################################
# function to compute the niche overlap between N species along a trait axis (functional or environmental)  #
#     generalization of the Niche Overlap index (NO) based on kernel density estimation                     #
#     of Mouillot et al. 2005 (Oecologia, 145, 345-353)                                                     #
#                                                                                                           #
#                                                                                                           #
# inputs:                                                                                                   #
#        - trab: a matrix (x, N+1) with the abudances of the N species at x values of the considered trait  #  
#               NA are not allowed in traits values                                                         #
#        - graph: logical value indicating whether a graphical illustration of niche overlap is wanted      #
#                 (for graphical convenience N must not exceed 6)                                           #
#         - colorsN: vector (length=N) for colors of niches in hexadecimal code (if NA default colors)      #
#                                                                                                           #
# outputs:                                                                                                  #
#         - the value of the overlap between corrected densities: 0 (no overlap) to 1(N identic densities)  #
#           i.e. the N densities along the finite range of values considered                                #
#               are corrected according to their respective integrals,                                       #
#               so that integrals of corrected densities along the range equal 1                            #
#                                                                                                           #
#         - if graph=T, each species niche have its own color, their overlap being shaded by lines          #            
#                                                                                                           #
#############################################################################################################

nicheoverlap<-function(trab,graph=T,colorsN=NA) {

# data extraction and checking

# traits values
tr<-trab[,1]
if (length(which(is.na(tr)))!=0)  stop("error : NA are not allowed in traits values")
mintr<-min(tr)
maxtr<-max(tr)

# abundances
abN<-trab[,-1]
if (dim(abN)[2]<2)  stop("error : there must be at least 2 species")

# number of species
N<-dim(abN)[2]


###############################################################################################################
#                     function to compute kernel density estimates

nichedens<-function(trab1)    {

# data extraction
tr1<-trab1[,1]
ab1<-trab1[,2]

# transformation of abundances into relative abundances and then conversion into whole numbers (precision of 0.001)
abrel<-ab1/sum(ab1,na.rm=T)
abrelW<-round(abrel,3)*1000

# transformation of data: traits values are replicated according to their abundances
trrep<-vector()
for (i in 1:length(abrelW))
if (abrelW[i]!=0 & is.na(abrelW[i])==F) trrep<-c( trrep,rep(tr1[i],abrelW[i]) )

#Bandwidth
bandwdth<-1.06*sd(trrep)*(length(trrep)^-0.2)

# computation of density based on gaussian kernel along the range of trait
resdens<-density(trrep,bw=bandwdth,kernel="gaussian",from=min(tr1),to=max(tr1),n=1024)
kx<-resdens$x
ky<-resdens$y

dens<-cbind(kx,ky)

return(dens)

} # end of function nichedens
###############################################################################################################

# computation of the N densities along the traits axis
for (i in 1:N)
{
densi<-nichedens(cbind(tr,abN[,i]))
if (i==1) resdens<-densi[,1:2]
else resdens<-cbind(resdens,densi[,2])
} # end of i

# minimum of the N densities and interpolation
x<-resdens[,1]
densN<-resdens[,2:(N+1)]

# computation of the N integrals and correction of densities
integ<-vector()
densNcorr<-densN

for (i in 1:N)
{
fdensk<-splinefun(x,densN[,i])
integ[i]<-integrate(fdensk,min(x),max(x),subdivisions=1000)$value
densNcorr[,i]<-densN[,i]/integ[i]
} # end of i

# overlap computation
minN<-apply(densNcorr,1,min)
fminN<-splinefun(x,minN)
overlap<-integrate(fminN,min(x),max(x),subdivisions=1000)$value
NO<-round(overlap,3)

#############################################################################################################
# graphic

if (graph==T) {
if (N>6)  stop("error : maximum 6 densities for graphical representation")

# for lakes with no greens (i.e. Caribou, Heney, Ludger):
# colors:  							 blue,     brown,      red,      violet,     grey
#if (is.na(colorsN[1])==T) color<-c("#4B8AEF", "#966410",  "#FF570A", "#761EA1","#9D939C")

# for lakes with no bluegreens (i.e. Bowker, Orford):
# colors:  							 green,     brown,      red,      violet,     grey
#if (is.na(colorsN[1])==T) color<-c("#11A123", "#966410",  "#FF570A", "#761EA1","#9D939C")

# colors:  							green,       blue,     brown,      red,      violet,     grey
if (is.na(colorsN[1])==T) color<-c("#11A123", "#4B8AEF", "#966410",  "#FF570A", "#761EA1","#9D939C")
if (is.na(colorsN[1])==F) color<-colorsN
if (is.na(colorsN[1])==F & length(colorsN)!=N) stop("'colorsN' must be of same length than number of species")
transp=50

# axes scale
labx<-pretty(x, n=5, min.n=3)
laby<-pretty(c(0,as.vector(densNcorr)), n=5, min.n=3)

# plot of axes
plot(mean(labx),mean(laby), type="n",xlim=range(x), ylim=range(laby), xlab="", ylab="", xaxt="n", yaxt="n",axes=F)
title(sub=paste("Niche overlap=",round(NO,3),sep=""),line=2,cex.sub=1.2 )
axis(side=1, labx[labx<=max(x) & labx>=min(x)], tcl=-0.2, pos=0, labels=F)
mtext(labx[labx<=max(x) & labx>=min(x)], side=1, at=labx[labx<=max(x) & labx>=min(x)], las=1,line=-0.2, cex=1)

axis(side=2, laby, tcl=-0.2, pos=min(x),labels=F) ; mtext(laby, side=2, at=laby, las=1, line=-0.4, cex=1)

# plot of densities
for (k in 1:N )
polygon( rbind(c(min(x),0), c(min(x),densNcorr[1,k]) , cbind(x,densNcorr[,k]), c(max(x),0) ),border=color[k],col=paste(color[k],transp,sep="") ) # end of k

# shading lines
polygon( rbind(c(min(x),0), c(min(x),minN[1]) , cbind(x,minN), c(max(x),0) ),density=10 )

# border
rect(min(x),min(laby),max(x),max(laby))

} # end of if (graph==T) 

#############################################################################################################

return(NO)
NO

} 
# end of function nicheoverlap


# START loop to get load files
#set working directory
getwd()
setwd("./")

#load data
data <- read_csv("./Manuscript/Example_fluorescence_profiles_Script_S3.csv")

###LOOP FOR LAKES WITH 4 SPECTRAL GROUPS
# NOTE: INCLUDE ONLY LAKES WITH 4 SPECTRAL GROUPS

# Import Diversity Index data and call function in loop
my.list <- list()
k<-0

casts <- unique(data[,c('Lake')])
final_SO <- NULL


for (x in 1:length(casts$Lake)) {

	k<-k+1
	
	#read in data file to use (looped)
	mydata <- filter(data, Lake == casts$Lake[x])
	
	#open pdf file to which you will save the graph produced by nicheoverlap function
	#pdf(paste("/Users/beatrixbeisner/Documents/students&postdocs/PhD students/Lorena Longhi/Lorena BB work/all lakes round2/Fluoroprobe data/NO_clacs&results/",substr(x,1,nchar(x)-4),".pdf",sep=""))
	
	#call nicheoverlap function, send mydata and write NO output to NOout: Note that NO was later called SO in our paper
	
	# set some counters
	kk<-1
	cc<-4
	my.list2 <- numeric()

	for (i in 3:5) {
		for (j in cc:6) {
				
	    	shortdata<-mydata[,c(2,i,j)] #first i, then j
	    	
	    #set up plotting device
	    filename = paste(mydata$Lake[1],colnames(mydata)[i],colnames(mydata)[j],"SO.png",sep = "_")
	    png(filename = paste0("./Manuscript/",filename))
	    	
	    #call nicheoverlap function, send mydata and write NO output to NOout
			NOout <-nicheoverlap(shortdata)
				
			#turn off plotting device
			dev.off()
			
			#write NO value to my.list
			my.list2[[ kk ]]<- NOout
			#print(NOout)
			kk<-kk+1
		
			}	
		cc<-cc+1
		#take average of NO values and output
		avg<-mean(my.list2)
		#take sum of NO values and output
		tot<-sum(my.list2)
		#print(avg)
		#print(tot) #should output these values to a file outside of this loop to accumulate over all runs of the larger loop
		}	
	
	#get lake name and NO and put in same line
	lake.name<- mydata$Lake[1]
	yy<-c(lake.name,my.list2,avg,tot)
	print(yy)
	final_SO <- rbind(final_SO, yy)
	
	#close quartz
	#dev.off()
	
	#clear the datafile
	rm(mydata)
}	

final_SO <- data.frame(final_SO)

colnames(final_SO) <- c("Lake","SO_Group1_Group2","SO_Group1_Group3",
                        "SO_Group1_Group4","SO_Group2_Group3","SO_Group2_Group4","SO_Group3_Group4",
                        "SO_avg","SO_tot")

write.csv(final_SO, "./Manuscript/SO_results.csv",row.names = FALSE)





