################################################################
####                    remove objects                      ####
################################################################
rm(list = ls())   # removes everything stored in R

# bring in dataframe
flower <- read.delim ("/Users/amandaklemmer/Documents/Teaching/UMaine classes/Library R workshop/2019/datasheets/flower.csv", sep= ",")


################################################################
####                      simple math                       ####
################################################################






################################################################
####                  creating objects                      ####
################################################################

# vector using concantenate function
A <- c(1,2,3)
B <- c(4,5,6)
C <- c("blue","green","red")

# matrix using cbind function
D <- cbind (A,B)
D.2 <- rbind (A,B)

# data.frame using data.frame function
E <- data.frame(A,B,C)

################################################################
####                      functions                         ####
################################################################

# class of object
class(E)

# class of element in object
class(E$A)
class(E$C)

# remove an object
rm(B)

# math functions
sum(E$A)
mean(E$B)
mean(c(2,3,4))

################################################################
####                bring in dataframes                     ####
################################################################

# using read.delim function
flower <- read.delim(file="/Users/amandaklemmer/Documents/Teaching/UMaine classes/Library R workshop/2019/datasheets/flower.csv",sep = ",")
flower2 <-read.delim(file="/Users/amandaklemmer/Documents/Teaching/UMaine classes/Library R workshop/2019/datasheets/flower.txt",header = T)

flower_color <-read.delim(file="/Users/amandaklemmer/Documents/Teaching/UMaine classes/Library R workshop/2019/datasheets/color.txt",header = TRUE)


################################################################
####                    data summary                        ####
################################################################

# check first 6 rows
head(flower)
head(flower_color)

#if you need to know the names of different levels in a factor - can be really handy to see if you have mispelled anything in your datasheet
levels(flower$Species)

#if you want to know all the names of the columns
names(flower)


################################################################
####                  merging dataframes                    ####
################################################################

#can use the merge function to merge dataframes together

flower_combined<-merge(flower, #data frame mergining onto
                       flower_color[,c("Species","Color")], #data frame merging on
                       by="Species", 
                       all=T)
head(flower_combined)



# to merge and not repeat the variable = all=F
flower_combined_2<-merge(flower, 
                         flower_color[,c("Species","Color")], 
                         by="Species", 
                         all=F)


head(flower_combined_2)


################################################################
####                    selecting data                      ####
################################################################

#subset allows you to take a subset of your data based on some critera
# ==
# !=
# >= or <
names(flower_combined_2)
levels(flower_combined_2$Color)


subset(flower_combined_2, 
       Color !="purple")

subset(flower_combined_2, 
       Color !="purple" | Sepal.Width >= 3)


subset(flower_combined_2, Color=="purple")

#if you want to use that subset for further analysis, can assign in a name
setosa <- subset(flower_combined_2, Color=="purple")



################################################################
####                  aggregating data                      ####
################################################################

# like pivot tables in excel
# first off aggregate can use formula as inputs for how you want it to handle your data

# responses go to the left of the tilde '~' and categoricals/factors go to the right:
# Petal.Length ~ Species

aggregate(Petal.Length ~ Species, 
          data=flower_combined_2, 
          FUN=mean)

aggregate(Petal.Length ~ Species, data=flower_combined_2, FUN=sum)
aggregate(Petal.Length ~ Species, data=flower_combined_2, FUN=sd)
aggregate(Petal.Length ~ Species, data=flower_combined_2, FUN=max)
aggregate(Petal.Length ~ Species, data=flower_combined_2, FUN=length)


petalLength_mean <- aggregate(Petal.Length ~ Species, data=flower_combined_2, FUN=mean)


flower_combined_2$Collector <- c("HSG", "AJK")
collector_average <- aggregate(Petal.Length ~ Collector * Species, 
                               data=flower_combined_2, 
                               FUN=mean)

################################################################
####                  manipulating data                     ####
################################################################

# adding columns - repeat the values
flower_combined_2$Collector <- c("HSG", "AJK") 

# can add a vector as a column
ID <- 1:150 #making a vector to add
flower_combined_2$flower_ID <- ID

#can do calculations for new variables
flower_combined_2$Petal.Area <- flower_combined_2$Petal.Width * flower_combined_2$Petal.Length

#deleting columns
flower_combined_2$flower_ID <- NULL



