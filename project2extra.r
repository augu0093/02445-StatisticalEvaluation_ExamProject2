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




