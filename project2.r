# Author: August Semrau Andersen - s183918

# Set working directory
setwd("C:/Users/August/Dropbox/3semester/Projekt_i_statistisk_evaluering/Statistical_Evaluation_Project2")



##########################################
# Start of project 2.
##########################################

### Loading data
load("fosfor_data.RData")


# Removing field '011' from data set
Phosphorous <- Phosphorous[Phosphorous$location != "011",]
Phosphorous$location <- as.factor(Phosphorous$location)
# Attach makes it unnescesary to write 'Phosphorous$'location 
attach(Phosphorous)

  
# Defining different subsets of the different locations
loca1 <- Phosphorous[location == "001",]
loca2 <- Phosphorous[location == "002",]
loca3 <- Phosphorous[location == "003",]
loca4 <- Phosphorous[location == "004",]
loca6 <- Phosphorous[location == "006",]
loca7 <- Phosphorous[location == "007",]
loca8 <- Phosphorous[location == "008",]
loca9 <- Phosphorous[location == "009",]
loca10 <- Phosphorous[location == "010",]
# Location 11 is excluded as it only contains two complete observations
#loca11 <- Phosphorous[location == "011",]



##########################################
# First focus: Choosing between olsen P and DGT measurements.
##########################################

### Plotting data for general overview
par(mfrow = c(1,1))

# Boxplot
plot(location,yield,xlab="Field",ylab="Yield (hkg/ha)",main="Boxplot of yield for each field")

# Plotting correlation between yield, DGT and olsenP. Location is not relevant here
plot(Phosphorous[c("yield","DGT","olsenP")],main="Correlation plots of Phosphorous data set")
cor(Phosphorous[c("yield","DGT","olsenP")]) #Correlation matrix



### Making linear model from both DGT and olsenP
par(mfrow = c(2,2))

modelDGT <- lm(yield ~ DGT)
summary(modelDGT)
plot(modelDGT, main="yield ~ DGT")

modelOlsenP <- lm(yield ~ olsenP)
summary(modelOlsenP)
plot(modelOlsenP, main="yield ~ olsenP")

modelLocation <- lm(yield ~ location)
summary(modelLocation)
plot(modelLocation, main="yield ~ location")

# Combined model
modelAll <- lm(yield ~ location + DGT + olsenP)
summary(modelAll)
plot(modelDGT_OlsenP, main="yield ~ DGT + olsenP")

anova(Phosphorous)


###  Michaelies-Menten model
#nls()
#phos.model <- nls(yield ~ alfa * DGT/(beta + DGT) , data = Phosphorous, start = list(alfa = 90 , beta = 1)












##########################################
# Second focus: Does the amount of bioavailable phosphorous influence the harvest yield?.
##########################################











