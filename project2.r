# Author: August Semrau Andersen - s183918

library(drc)
library(lattice)
library(ggplot2)

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

# Create standardized data set
PhosphorousStandard <- Phosphorous
PhosphorousStandard$DGT <- scale(Phosphorous$DGT)
PhosphorousStandard$olsenP <- scale(Phosphorous$olsenP)


# Give short data summary of quartiles and min/max
summary(Phosphorous)
  
##########################################
# First focus: Choosing between olsen P and DGT measurements.
##########################################

### Plotting data for general overview
par(mfrow = c(1,1))

# Boxplot
plot(location,yield, xlab="Field", ylab="Yield (hkg/ha)", main="Boxplot of yield for each field")

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

# Combined model of DGT and olsenP: yield ~ DGT + olsenP
modelDGTolsenP <- lm(yield ~ DGT + olsenP)
summary(modelDGTolsenP)
plot(modelDGTolsenP, main="yield ~ DGT + olsenP")
#anova(modelDGTolsenP)

#Pooled model of DGT and olsenP: yield ~ DGT * olsenP
modelDGTolsenPpool <- lm(yield ~ DGT * olsenP)
summary(modelDGTolsenPpool)
plot(modelDGTolsenPpool, main="yield ~ DGT * olsenP")
#anova(modelDGTolsenP)




# Model standardized data
modelDGTstandard <- lm(yield ~ DGT, data=PhosphorousStandard)
summary(modelDGTstandard)
plot(modelDGTstandard, main="yield ~ DGT")
anova(modelDGT)
# Makes absolutely no differnece


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

a <- PhosphorousTrain$yield
x <- PhosphorousTrain$DGT

#here you use x as a name
modelTrainDGT <- lm(a ~ x) 
#here you use x again as a name in newdata.
predict(modelTrainDGT, data.frame(x = mean(x)))#, interval = "confidence") 



# Train linar model regression on olsenP and validate accuracy
modelTrainOlsenP <- lm(PhosphorousTrain$yield ~ PhosphorousTrain$olsenP)
summary(modelTrainOlsenP)

predictionsOlsenP <- predict(modelTrainOlsenP, PhosphorousTest)
mean((PhosphorousTest$yield - predictionsOlsenP)^2) ## Mean squared prediction error

# Other method
b <- PhosphorousTrain$yield
y <- PhosphorousTrain$olsenP

#here you use x as a name
modelTrainOlsenP <- lm(b ~ y) 
#here you use x again as a name in newdata.
predict(modelTrainOlsenP, data.frame(y = mean(y)))#, interval = "confidence") 








# leave-one-out Cross Validation
pred.errorsDGT <- rep(NA, 32)
pred.errorsOlsenP <- rep(NA, 32)

for (i in 1:32) {
  # train_data <- Phosphorous[-i, ] 
  # test_data <- Phosphorous[i, ]
  # Standardized data
  train_data <- PhosphorousStandard[-i, ] 
  test_data <- PhosphorousStandard[i, ]
  
  
  # Train model, make prediction, calculate MSE
  mDGT <- lm(yield ~ DGT, data = train_data)
  predictionDGT <- predict(mDGT, test_data)
  pred.errorsDGT[i] <- test_data$yield - predictionDGT
  
  mOlsenP <- lm(yield ~ olsenP, data = train_data)  
  predictionOlsenP <- predict(mOlsenP, test_data)
  pred.errorsOlsenP[i] <- test_data$yield - predictionOlsenP
}
# Define MSE
mean(pred.errorsDGT^2) # = 114.3345
mean(pred.errorsOlsenP^2) # = 127.8291





###  Non-linear regression model: Michaelies-Menten model
#nls()
#phos.model <- nls((yield ~ alfa * DGT/(beta + DGT)), data = Phosphorous, start = list(alfa = 90 , beta = 1)

# Model DGT
modelDGT.nls <- nls(yield ~ a * DGT/(b+DGT), data = Phosphorous, start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield)))
summary(modelDGT.nls)

# Model OlsenP
modelOlsenP.nls <- nls(yield ~ a * olsenP/(b+olsenP), data = Phosphorous, start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield)))
summary(modelOlsenP.nls)

#predict(modelDGT.nls, newdata = c(60))

# par(mfrow = c(1,1))
# model.drmDGT <- drm(yield ~ DGT, data = Phosphorous, fct = MM.2())
# summary(model.drmDGT)
# plot(model.drmDGT)


# Leave-one-out CV for testing accuray of non-linear model
pred.errorsDGTMM <- rep(NA, 32)
pred.errorsOlsenPMM <- rep(NA, 32)
for (i in 1:32) {
  train_data <- Phosphorous[-i, ]
  test_data <- Phosphorous[i, ]
  # Standardized data
  # train_data <- PhosphorousStandard[-i, ] 
  # test_data <- PhosphorousStandard[i, ]
  
  # Train model, make prediction, calculate MSE
  #mDGTMM <- drm(yield ~ DGT, data = train_data, fct = MM.2())
  modelDGT.nls <- nls(yield ~ a * DGT/(b+DGT), data = train_data, start = list(b = max(train_data$yield)/2, a = max(train_data$yield)))
  dfDGT <- data.frame(
    a <- c(coef(modelDGT.nls)[2]),
    b <- c(coef(modelDGT.nls)[1]),
    DGT <- c(test_data$DGT)
  )
  predictionDGTMM <- predict(modelDGT.nls, newdata = dfDGT)
  pred.errorsDGTMM[i] <- test_data$yield - predictionDGTMM
  
  modelOlsenP.nls <- nls(yield ~ a * olsenP/(b+DGT), data = train_data, start = list(b = max(train_data$yield)/2, a = max(train_data$yield)))
  dfOlsenP <- data.frame(
    a <- c(coef(modelOlsenP.nls)[2]),
    b <- c(coef(modelOlsenP.nls)[1]),
    olsenP <- c(test_data$olsenP)
  )
  predictionOlsenPMM <- predict(modelOlsenP.nls, newdata = dfOlsenP)
  pred.errorsOlsenPMM[i] <- test_data$yield - predictionOlsenPMM
  
}
# Define MSE from CV
mean(pred.errorsDGTMM^2) # = 123.5609
mean(pred.errorsOlsenPMM^2) # = 253.0265





# Plotting non-linear DGT
modelDGT.nls <- nls(yield ~ a * DGT/(b+DGT), data = Phosphorous, start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield)))
DGTl <- data.frame(DGT = seq(0, max(Phosphorous$DGT), length.out = 100))
DGTl$yield <- predict(modelDGT.nls, newdata = DGTl)

ggplot(Phosphorous, aes(x = DGT, y = yield)) +
  theme_bw() +
  xlab("DGT [mug/L]") +
  ylab("Yield [100kg]") +
  ggtitle("Michaelis-Menten kinetics") +
  geom_point(alpha = 0.5) +
  geom_line(data = DGTl, aes(x = DGT, y = yield), colour = "red")


# Plotting non-linear OlsenP
modelOlsenP.nls <- nls(yield ~ a * olsenP/(b+olsenP), data = Phosphorous, start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield)))
olsenPl <- data.frame(olsenP = seq(0, max(Phosphorous$olsenP), length.out = 100))
olsenPl$yield <- predict(modelOlsenP.nls, newdata = olsenPl)

ggplot(Phosphorous, aes(x = olsenP, y = yield)) +
  theme_bw() +
  xlab("Olsen-P [mg/100g]") +
  ylab("Yield [100kg]") +
  ggtitle("Michaelis-Menten kinetics") +
  geom_point(alpha = 0.5) +
  geom_line(data = olsenPl, aes(x = olsenP, y = yield), colour = "red")



##########################################
# Second focus: Does the amount of bioavailable phosphorous influence the harvest yield?.
##########################################

par(mfrow=c(2,2))
# Combined model: yield ~ location + DGT + olsenP
modelAll <- lm(yield ~ as.factor(location) + DGT + olsenP)
summary(modelAll)
plot(modelAll, main="yield ~ location + DGT + olsenP")
anova(modelAll)

modelAll <- lm(yield ~ as.numeric(location) + DGT + olsenP)
summary(modelAll)



modelDGT <- lm(yield ~ DGT)
summary(modelDGT)

pValueDGT <- rep(NA, 32)
for (i in 1:32) {
  train_data <- Phosphorous[-i, ]
  modelDGTL <- lm(yield ~ DGT, data = train_data)
  pValueDGT[i] <- summary(modelDGTL)$coefficients[2,4]
}
print(pValueDGT)
mean(pValueDGT)



