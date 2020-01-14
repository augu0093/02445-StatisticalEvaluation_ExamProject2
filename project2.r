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
#as.factor(Phosphorous$location)
# Attach makes it unnescesary to write 'Phosphorous$'location 
attach(Phosphorous)

# Give short data summary of quartiles and min/max
summary(Phosphorous)
  
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
plot(location,yield, xlab="Field", ylab="Yield (hkg/ha)", main="Boxplot of yield for each field")

# Variance of all locations' yield
var(loca1["yield"]) # = 7.905626
var(loca2["yield"]) # = 10.83938
var(loca3["yield"]) # = 9.270759
var(loca4["yield"]) # = 0.2335097
var(loca6["yield"]) # = 25.28641
var(loca7["yield"]) # = 3.022686
var(loca8["yield"]) # = 14.26606
var(loca10["yield"]) # = 5.436162
# Mean of these = 7.905626
mean(var(loca1["yield"]),var(loca2["yield"]),var(loca3["yield"]),var(loca4["yield"]),var(loca6["yield"]),var(loca7["yield"]),var(loca8["yield"]),var(loca10["yield"]))


# Plotting correlation between yield, DGT and olsenP. Location is not relevant here
plot(Phosphorous[c("yield","DGT","olsenP")],main="Correlation plots of Phosphorous data set")
#plot(Phosphorous)

# Correlation matrix
cor(Phosphorous[c("yield","DGT","olsenP")]) #Correlation matrix
#           yield       DGT    olsenP
#yield  1.0000000 0.3635117 0.2460804
#DGT    0.3635117 1.0000000 0.8569596
#olsenP 0.2460804 0.8569596 1.0000000








### Making linear models for differentiation between measurement methods and prediction
par(mfrow = c(2,2))

# Model: yield ~ DGT
modelDGT <- lm(yield ~ DGT)
summary(modelDGT)
plot(modelDGT, main="yield ~ DGT")
anova(modelDGT)

# Model: yield ~ olsenP
modelOlsenP <- lm(yield ~ olsenP)
summary(modelOlsenP)
plot(modelOlsenP, main="yield ~ olsenP")
anova(modelOlsenP)




### Here we train the linear model on 75% of data and test on 25% CV
set.seed(1)
# Now Selecting 75% of data as sample from total 'n' rows of the data

sample <- sample.int(n = nrow(Phosphorous), size = floor(.75*nrow(Phosphorous)), replace = F)
PhosphorousTrain <- Phosphorous[sample, ]
PhosphorousTest  <- Phosphorous[-sample, ]

# Train linar model regression on DGT and validate accuracy
modelTrainDGT <- lm(PhosphorousTrain$yield ~ PhosphorousTrain$DGT)
summary(modelTrainDGT)

predictionsDGT <- predict(modelTrainDGT, PhosphorousTest)
mean((PhosphorousTest$yield - predictionsDGT)^2) ## Mean squared prediction error


# Train linar model regression on olsenP and validate accuracy
modelTrainOlsenP <- lm(PhosphorousTrain$yield ~ PhosphorousTrain$olsenP)
summary(modelTrainOlsenP)

predictionsOlsenP <- predict(modelTrainOlsenP, PhosphorousTest)
mean((PhosphorousTest$yield - predictionsOlsenP)^2) ## Mean squared prediction error


### The same kind of training is performed in a range of percentages
perc <- c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.96875)
DGTac <- rep(NA,length(perc))
olsenPac <- rep(NA,length(perc))

set.seed(50)
for (i in (1:length(perc))){
  sample <- sample.int(n = nrow(Phosphorous), size = floor(perc[i]*nrow(Phosphorous)), replace = F)
  PhosphorousTrain <- Phosphorous[sample, ]
  PhosphorousTest  <- Phosphorous[-sample, ]
  modelTrainDGT <- lm(yield ~ DGT, data = PhosphorousTrain)
  modelTrainolsenP <- lm(yield ~ olsenP, data = PhosphorousTrain)
  # Append MSE DGT
  predictionsDGT <- predict(modelTrainDGT, PhosphorousTest)
  DGTac[i] <- mean((PhosphorousTest$yield - predictionsDGT)^2)
  # Append MSE olsenP
  predictionsOlsenP <- predict(modelTrainOlsenP, PhosphorousTest)
  olsenPac[i] <- mean((PhosphorousTest$yield - predictionsOlsenP)^2)
}

# Plotting accuracies found for predictions
par(mfrow = c(1,1))
#plot(perc, DGTac, xlab="Percent training data", ylab="Accuracy DGT")
#plot(perc, olsenPac, xlab="Percent training data", ylab="Accuracy olsenP")

xyplot(DGTac + olsenPac ~ perc, xlab="Percent training data", ylab="Accuracy of DGT and olsenP", auto.key=TRUE)


###  Michaelies-Menten model
#nls()
#phos.model <- nls(yield ~ alfa * DGT/(beta + DGT) , data = Phosphorous, start = list(alfa = 90 , beta = 1)












##########################################
# Second focus: Does the amount of bioavailable phosphorous influence the harvest yield?.
##########################################











