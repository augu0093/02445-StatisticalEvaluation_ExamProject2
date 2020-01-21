### This script contains all functions that were simply clutter on the main script




# Model: yield ~ location
modelLocation <- lm(yield ~ location)
summary(modelLocation)
plot(modelLocation, main="yield ~ location")
anova(modelLocation)


# Combined model of DGT and olsenP: yield ~ DGT + olsenP
modelDGTolsenP <- lm(yield ~ DGT + olsenP)
summary(modelDGTolsenP)
plot(modelDGTolsenP, main="yield ~ location + DGT + olsenP")
#anova(modelDGTolsenP)




# Pooled model: yield ~ location * DGT * olsenP
modelAllPool <- lm(yield ~ location * DGT * olsenP)
summary(modelAllPool)



### The same kind of training is performed in a range of percentages
perc <- c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.96875)
DGTac <- rep(NA,length(perc))
olsenPac <- rep(NA,length(perc))

set.seed(101)
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
xyplot(DGTac + olsenPac ~ perc, xlab="Percent training data", ylab="Accuracy of DGT and olsenP", auto.key=TRUE)

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



# Leave-one-out CV for testing accuray of non-linear model
pred.errorsDGTMM <- rep(NA, 8)
pred.errorsOlsenPMM <- rep(NA, 8)
for (i in 1:8) {
  train_data <- PhosphorousMean[-i, ]
  test_data <- PhosphorousMean[i, ]
  # Standardized data
  # train_data <- PhosphorousStandard[-i, ] 
  # test_data <- PhosphorousStandard[i, ]
  
  # Train model, make prediction, calculate MSE
  #mDGTMM <- drm(yield ~ DGT, data = train_data, fct = MM.2())
  modelDGT.nls <- nls(yield ~ a * DGT/(b+DGT), data = train_data, start = list(b = max(train_data$yield)/2, a = max(train_data$yield)))
  # dfDGT <- data.frame(
  #   a <- c(coef(modelDGT.nls)[2]),
  #   b <- c(coef(modelDGT.nls)[1]),
  #   DGT <- c(test_data$DGT)
  # )
  predictionDGTMM <- predict(modelDGT.nls,test_data$olsenP)#, newdata = dfDGT)#
  pred.errorsDGTMM[i] <- test_data$yield - predictionDGTMM
  
  modelOlsenP.nls <- nls(yield ~ a * olsenP/(b+olsenP), data = train_data, start = list(b = max(train_data$yield)/2, a = max(train_data$yield)))
  # dfOlsenP <- data.frame(
  #   a <- c(coef(modelOlsenP.nls)[2]),
  #   b <- c(coef(modelOlsenP.nls)[1]),
  #   olsenP <- c(test_data$olsenP)
  # )
  predictionOlsenPMM <- predict(modelOlsenP.nls, test_data$olsenP)#, newdata = dfOlsenP)##
  pred.errorsOlsenPMM[i] <- test_data$yield - predictionOlsenPMM
  
}
t.test(pred.errorsDGTMM^2, pred.errorsOlsenPMM^2)

# Define MSE from CV
mean(pred.errorsDGTMM^2) # = 138.6358
mean(pred.errorsOlsenPMM^2) # = 153.9979




