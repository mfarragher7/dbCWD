################################################################
####                    remove objects                      ####
################################################################
rm(list = ls())   # removes everything stored in R

# bring in dataframe
flower_combined_2 <- read.delim ("/Users/amandaklemmer/Documents/Teaching/UMaine classes/Library R workshop/2019/datasheets/flower_combined_2.csv", sep= ",")


################################################################
####              writing out dataframes                    ####
################################################################


flower_combined_2$Sepal.Area <- flower_combined_2$Sepal.Length * flower_combined_2$Sepal.Width

write.csv(flower_combined_2,"/Users/amandaklemmer/Documents/Teaching/UMaine classes/Library R workshop/2019/datasheets/flower_combined_3.csv" )



################################################################
####                      packages                          ####
################################################################


# installing packages although I recommend using the packages tab
install.packages("nlme")

# calling packages
library(nlme)



###############################################################################
####                                  t tests                              ####
###############################################################################

# Quick look at data
plot(Petal.Area~Species, data=flower_combined_2 )

# performing t test between setosa and versicolor
model_1 <- t.test (flower_combined_2$Petal.Area[flower_combined_2$Species=="setosa"],
                       flower_combined_2$Petal.Area[flower_combined_2$Species=="versicolor"])
model_1


###############################################################################
####                               1-way ANOVA                             ####
###############################################################################

# Quick look at data
plot(Petal.Area~Species, data=flower_combined_2 )

# model with 1 categorical predictor
model_2 <- lm (Petal.Area ~ Species, data = flower_combined_2)
summary(model_2) # gives you coefficient values (t-test) and R^2
anova(model_2) # significance of predictors (F-test)

# saving stats table as object
model_2_table <- anova(model_2)


# can use aov for ANOVA
model_3 <- aov (Petal.Area ~ Species, data = flower_combined_2)
summary(model_3) # now gives you the same as anova()
anova(model_3) # significance of predictors (F-test)


###############################################################################
####                               regression                              ####
###############################################################################

# Quick look at the data
plot(Petal.Length ~ Petal.Width, data = flower_combined_2)

# model with continuous predictor
model_4 <- lm (Petal.Length ~ Petal.Width, data = flower_combined_2)
summary(model_4) # gives you coefficient values (t-test) and R^2
anova(model_4) # significance of predictors (F-test)


# saving stats table as object
model_4_table <- anova(model_4)


# linear regression on setosa Species
plot(Petal.Length ~ Petal.Width, data = subset(flower_combined_2, Species=="versicolor")) # Quick look at the data

model_5 <- lm (Petal.Length ~ Petal.Width, data = subset(flower_combined_2, Species=="versicolor"))
summary(model_5) # gives you coefficient values (t-test) and R^2
anova(model_5) # significance of predictors (F-test)


###############################################################################
####                      prettier plots                                  ####
###############################################################################

model_4 <- lm (Petal.Length ~ Petal.Width, data = flower_combined_2)
plot(Petal.Length ~ Petal.Width, data = flower_combined_2)
plot(Petal.Length ~ Petal.Width, data = subset(flower_combined_2, Species=="versicolor"))

colors()

# changing arguments within the plot function
plot(Petal.Length ~ Petal.Width,                                # x and y variables
     data = subset(flower_combined_2, Species=="versicolor"),   # dataframe    
     pch=25,                                                    # point symbol (1-25)
     col="blue",                                                # color of points (outline for certain symbols)
     bg = "salmon",                                             # color of point fill (for certain symbols)
     cex=1.5,                                                   # size of points
     lwd=1.5,                                                   # size of point outline
     xlim=c(min(0),max(flower_combined_2$Petal.Width)),         # x axis limits
     ylim=c(min(0),max(flower_combined_2$Petal.Length)),        # y axis limits
     bty="l",                                                   # plot box
     ylab="Sepal Length",                                       # y axis label
     xlab="Sepal Width")                                        # x axis label

#makes a line of best fit for linear models
abline(model_4)




###############################################################################
####                        exporting plots                                ####
###############################################################################


#### white background versicolor plot ####

png(filename = "/Users/amandaklemmer/Documents/Teaching/UMaine classes/Library R workshop/2019/plots/versicolor.png",
    width = 297, height = 210, units ="mm", pointsize = 12,
    bg = "white", res = 400,
    type="quartz")

plot(Petal.Length ~ Petal.Width,                                # x and y variables
     data = subset(flower_combined_2, Species=="versicolor"),   # dataframe    
     pch=25,                                                    # point symbol (1-25)
     col="blue",                                                # color of points (outline for certain symbols)
     bg = "salmon",                                             # color of point fill (for certain symbols)
     cex=1.5,                                                   # size of points
     lwd=1.5,                                                   # size of point outline
     xlim=c(min(0),max(flower_combined_2$Petal.Width)),         # x axis limits
     ylim=c(min(0),max(flower_combined_2$Petal.Length)),        # y axis limits
     bty="l",                                                   # plot box
     ylab="Sepal Length",                                       # y axis label
     xlab="Sepal Width")    


dev.off()



#### black background versicolor plot ####

png(filename = "/Users/amandaklemmer/Documents/Teaching/UMaine classes/Library R workshop/2019/plots/versicolor.png",
    width = 297, height = 210, units ="mm", pointsize = 12,
    bg = "black", res = 400,
    type="quartz")

plot(Petal.Length ~ Petal.Width,                                # x and y variables
     data = subset(flower_combined_2, Species=="versicolor"),   # dataframe    
     pch=25,                                                    # point symbol (1-25)
     col="white",                                               # color of points (outline for certain symbols)
     bg = "salmon",                                             # color of point fill (for certain symbols)
     cex=1.5,                                                   # size of points
     lwd=1.5,                                                   # size of point outline
     xlim=c(min(0),max(flower_combined_2$Petal.Width)),         # x axis limits
     ylim=c(min(0),max(flower_combined_2$Petal.Length)),        # y axis limits
     bty="l",                                                   # plot box
     ylab="Sepal Length",                                       # y axis label
     xlab="Sepal Width",                                        # x axis label
     col.axis = "white",                                        # making axis white
     col.lab = "white",                                         # making label white
     fg = "white")                                              # making foreground white


dev.off()



