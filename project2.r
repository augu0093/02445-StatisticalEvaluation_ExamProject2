# Author: August Semrau Andersen - s183918

library(drc)
library(lattice)
library(ggplot2)

library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)

rm(list=ls())

x# Set working directory
setwd("C:/Users/August/Dropbox/3semester/Projekt_i_statistisk_evaluering/Statistical_Evaluation_Project2")



##########################################
# Start of project 2.
##########################################

### Loading data
load("fosfor_data.RData")


# Removing field '011' from data set
Phosphorous <- Phosphorous[Phosphorous$location != "011",]

# Making dataframe that contaisn mean values of each location
PhosphorousMean <- Phosphorous
v <- rep(NA,8)
for (i in 1:8){
  v[i] <- (i-1)*4+1
}
PhosphorousMean <- PhosphorousMean[--c(v),]

for (i in 1:8){
  meani <- mean(Phosphorous$yield[((i-1)*4+1):((i-1)*4+4)])
  PhosphorousMean[i,"yield"] <- meani
}


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



##### Plotting DGT and olsenP for better overview of data
##### Plotting 
# Model: yield ~ DGT
modelDGT <- lm(yield ~ DGT)
summary(modelDGT) # p-value = 0.0408 * 
plot(modelDGT, main="yield ~ DGT")
anova(modelDGT)


# Model: yield ~ olsenP
modelOlsenP <- lm(yield ~ olsenP)
summary(modelOlsenP) # p-value = 0.175 
plot(modelOlsenP, main="yield ~ olsenP")
anova(modelOlsenP)

#### Plotting scatterplots
par(mfrow = c(1,2))
plot(DGT,yield, ylim=c(0,100), xlab="DGT [microgram/liter]",ylab="yield [100 kg/ha]", main="Scatterplot of DGT and yield")
abline(modelDGT)
plot(olsenP,yield, ylim=c(0,100), xlab="Olsen-P [milligram/100 gram]",ylab="yield [100 kg/ha]", main="Scatterplot of Olsen-P and yield")
abline(modelOlsenP)

par(mfrow = c(1,2))
plot(PhosphorousMean$DGT,PhosphorousMean$yield, ylim=c(0,100), xlab="DGT [microgram/liter]",ylab="yield [100 kg/ha]", main="Scatterplot of DGT and yield")
abline(modelDGT)
plot(PhosphorousMean$olsenP,PhosphorousMean$yield, ylim=c(0,100), xlab="Olsen-P [milligram/100 gram]",ylab="yield [100 kg/ha]", main="Scatterplot of Olsen-P and yield")
abline(modelOlsenP)

######## Make prediction of yield to evaluate precision of measurements

### First leave-one-out Cross Validation
pred.errorsDGT <- rep(NA, 8)
pred.errorsOlsenP <- rep(NA, 8)

for (i in 1:8) {
  train_data <- PhosphorousMean[-i, ]
  test_data <- PhosphorousMean[i, ]
  # Standardized data
  #train_data <- PhosphorousStandard[-i, ] 
  #test_data <- PhosphorousStandard[i, ]
  
  
  # Train model, make prediction, calculate MSE
  mDGT <- lm(yield ~ DGT, data = train_data)
  predictionDGT <- predict(mDGT, test_data)
  pred.errorsDGT[i] <- test_data$yield - predictionDGT
  
  mOlsenP <- lm(yield ~ olsenP, data = train_data)  
  predictionOlsenP <- predict(mOlsenP, test_data)
  pred.errorsOlsenP[i] <- test_data$yield - predictionOlsenP
}
# Performing t.test on squared errors to find if they differ signifcantly
t.test(pred.errorsDGT^2, pred.errorsOlsenP^2, paried=TRUE)
# Define MSE
mean(pred.errorsDGT^2) # = 197.8192
mean(pred.errorsOlsenP^2) # = 238.6616



### Second is to try leave-one-out Cross Validation with 32 but holding out same -ocation data
pred.errorsDGT2 <- rep(NA, 32)
pred.errorsOlsenP2 <- rep(NA, 32)

for (i in 1:8) {
  train_data <- Phosphorous[-(((i-1)*4)+1):-(((i-1)*4)+4), ]
  test_data <- Phosphorous[(((i-1)*4)+1):(((i-1)*4)+4), ]
  # train_data <- PhosphorousMean[-i, ]
  # test_data <- PhosphorousMean[i, ]
  # Standardized data
  #train_data <- PhosphorousStandard[-i, ]
  #test_data <- PhosphorousStandard[i, ]
  #print(train_data)

  #Train model, make prediction, calculate SE
  mDGT <- lm(yield ~ DGT, data = train_data)
  #predictionsDGT = rep(NA,4)
  pred.errorsDGT2[1+((i-1)*4)] <- (test_data$yield[1] - predict(mDGT, test_data[1,]))^2
  pred.errorsDGT2[2+((i-1)*4)] <- (test_data$yield[2] - predict(mDGT, test_data[2,]))^2
  pred.errorsDGT2[3+((i-1)*4)] <- (test_data$yield[3] - predict(mDGT, test_data[3,]))^2
  pred.errorsDGT2[4+((i-1)*4)] <- (test_data$yield[4] - predict(mDGT, test_data[4,]))^2
  # for (a in 1:4){
  #   pred.errorsDGT2[a+((i-1)*4)] <- (test_data$yield[a] - predict(mDGT, test_data[a]))^2
  # }

  mOlsenP <- lm(yield ~ olsenP, data = train_data)
  #predictionsOlsenP = rep(NA,4)
  for (a in 1:4){
    pred.errorsOlsenP2[a+((i-1)*4)] <- (test_data$yield[a] - predict(mOlsenP, test_data[a,]))^2
    #pred.errorsOlsenP2[a+((i-1)*4)] <- predict(mOlsenP, test_data[a])
  }
}
pred.errorsDGT1 <- pred.errorsDGT2
t.test(pred.errorsDGT2,pred.errorsOlsenP2, paired=TRUE)


# Define MSE
mean(pred.errorsDGT^2) # = 216.5326
mean(pred.errorsOlsenP^2) # = 268.5599
# 
# 
# 
# 
# ### Third is to try leave-one-out Cross Validation making mean after prediction
# pred.errorsDGT1 <- rep(NA, 8)
# pred.errorsOlsenP1 <- rep(NA, 8)
# 
# for (i in 1:8) {
#   train_data <- Phosphorous[-(((i-1)*4)+1):-(((i-1)*4)+4), ]
#   test_data <- Phosphorous[(((i-1)*4)+1):(((i-1)*4)+4), ]
#   # train_data <- PhosphorousMean[-i, ]
#   # test_data <- PhosphorousMean[i, ]
#   # Standardized data
#   #train_data <- PhosphorousStandard[-i, ] 
#   #test_data <- PhosphorousStandard[i, ]
#   #print(train_data)
#   
#   #Train model, make prediction, calculate MSE
#   mDGT <- lm(yield ~ DGT, data = train_data)
#   predictionsDGT = rep(NA,4)
#   for (i in 1:4){
#     predictionsDGT[i] <- predict(mDGT, test_data[i])
#   }
#   pred.errorsDGT[i] <- mean(test_data$yield - predictionsDGT)
# 
#   mOlsenP <- lm(yield ~ olsenP, data = train_data)
#   predictionsOlsenP = rep(NA,4)
#   for (i in 1:4){
#     predictionsOlsenP[i] <- predict(mOlsenP, test_data[i])
#   }
#   pred.errorsOlsenP[i] <- mean(test_data$yield - predictionsOlsenP)
# }
# 
# # Define MSE
# mean(pred.errorsDGT^2) # = 216.5326
# mean(pred.errorsOlsenP^2) # = 268.5599



### We first made CV using all data-point, but training on the same values of
### DGT and olsenP that we want to predict from does not work
# for (i in 1:32) {
#   # train_data <- Phosphorous[-i, ] 
#   # test_data <- Phosphorous[i, ]
#   # Standardized data
#   train_data <- PhosphorousStandard[-i, ] 
#   test_data <- PhosphorousStandard[i, ]
#   
#   
#   # Train model, make prediction, calculate MSE
#   mDGT <- lm(yield ~ DGT, data = train_data)
#   predictionDGT <- predict(mDGT, test_data)
#   pred.errorsDGT[i] <- test_data$yield - predictionDGT
#   
#   mOlsenP <- lm(yield ~ olsenP, data = train_data)  
#   predictionOlsenP <- predict(mOlsenP, test_data)
#   pred.errorsOlsenP[i] <- test_data$yield - predictionOlsenP
# }
# # Define MSE
# mean(pred.errorsDGT^2) # = 114.3345
# mean(pred.errorsOlsenP^2) # = 127.8291





##### Now we use the non-linear model to see if it better fits the data 
##### and helps ud predict yield

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








pred.errorsDGT2 <- rep(NA, 32)
pred.errorsOlsenP2 <- rep(NA, 32)

for (i in 1:8) {
  train_data <- Phosphorous[-(((i-1)*4)+1):-(((i-1)*4)+4), ]
  test_data <- Phosphorous[(((i-1)*4)+1):(((i-1)*4)+4), ]

  
  #Train model, make prediction, calculate squared error
  modelDGT.nls <- nls(yield ~ a * DGT/(b+DGT), data = train_data, start = list(b = max(train_data$yield)/2, a = max(train_data$yield)))

  
  pred.errorsDGT2[1+((i-1)*4)] <- (test_data$yield[1] - predict(modelDGT.nls, test_data[1,]))^2
  pred.errorsDGT2[2+((i-1)*4)] <- (test_data$yield[2] - predict(modelDGT.nls, test_data[2,]))^2
  pred.errorsDGT2[3+((i-1)*4)] <- (test_data$yield[3] - predict(modelDGT.nls, test_data[3,]))^2
  pred.errorsDGT2[4+((i-1)*4)] <- (test_data$yield[4] - predict(modelDGT.nls, test_data[4,]))^2
  #predictionsDGT = rep(NA,4)
  # for (de in 1:4){
  #   # dfDGT <- data.frame(
  #   #   a <- c(coef(modelDGT.nls)[2]),
  #   #   b <- c(coef(modelDGT.nls)[1]),
  #   #   DGT <- c(test_data$DGT[de])
  #   # )
  #   pred.errorsDGT2[de+((i-1)*4)] <- (test_data$yield[de] - predict(modelDGT.nls,test_data[de,]))^2#, newdata = dfDGT))^2
  # }
  
  modelOlsenP.nls <- nls(yield ~ a * olsenP/(b+olsenP), data = train_data, start = list(b = max(train_data$yield)/2, a = max(train_data$yield)))
  # dfOlsenP <- data.frame(
  #   a <- c(coef(modelOlsenP.nls)[2]),
  #   b <- c(coef(modelOlsenP.nls)[1]),
  #   olsenP <- c(test_data$olsenP)
  # )
  #predictionOlsenPMM <- predict(modelOlsenP.nls, newdata = dfOlsenP)
  #pred.errorsOlsenPMM[i] <- test_data$yield - predictionOlsenPMM
  
  pred.errorsOlsenP2[1+((i-1)*4)] <- (test_data$yield[1] - predict(modelOlsenP.nls, test_data[1,]))^2
  pred.errorsOlsenP2[2+((i-1)*4)] <- (test_data$yield[2] - predict(modelOlsenP.nls, test_data[2,]))^2
  pred.errorsOlsenP2[3+((i-1)*4)] <- (test_data$yield[3] - predict(modelOlsenP.nls, test_data[3,]))^2
  pred.errorsOlsenP2[4+((i-1)*4)] <- (test_data$yield[4] - predict(modelOlsenP.nls, test_data[4,]))^2
  
  #predictionsOlsenP = rep(NA,4)
  # for (de in 1:4){
  #   # dfOlsenP <- data.frame(
  #   #   a <- c(coef(modelOlsenP.nls)[2]),
  #   #   b <- c(coef(modelOlsenP.nls)[1]),
  #   #   olsenP <- c(test_data$olsenP[de])
  #   # )
  #   pred.errorsOlsenP2[de+((i-1)*4)] <- (test_data$yield[de] - predict(mOlsenP, test_data[de,]))^2
  # }
}
t.test(pred.errorsDGT2, pred.errorsOlsenP2, paired=TRUE)


t.test(pred.errorsDGT1, pred.errorsDGT2, paired=TRUE)
mean(pred.errorsDGT2) # = 194.3606
mean(pred.errorsOlsenP2) # = 236.4644



par(mfrow = c(1,2))
# Plotting non-linear DGT
modelDGT.nls <- nls(yield ~ a * DGT/(b+DGT), data = Phosphorous, start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield)))
DGTl <- data.frame(DGT = seq(0, max(Phosphorous$DGT), length.out = 100))
DGTl$yield <- predict(modelDGT.nls, newdata = DGTl)

ggplot(PhosphorousMean, aes(x = DGT, y = yield)) +
  theme_bw() +
  xlab("DGT [microgram/liter]") +
  ylab("Yield [100 kg/ha]") +
  ggtitle("Non-linear regression DGT") +
  geom_point(alpha = 0.5) +
  geom_line(data = DGTl, aes(x = DGT, y = yield), colour = "red")


# Plotting non-linear OlsenP
modelOlsenP.nls <- nls(yield ~ a * olsenP/(b+olsenP), data = Phosphorous, start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield)))
olsenPl <- data.frame(olsenP = seq(0, max(Phosphorous$olsenP), length.out = 100))
olsenPl$yield <- predict(modelOlsenP.nls, newdata = olsenPl)

ggplot(PhosphorousMean, aes(x = olsenP, y = yield)) +
  theme_bw() +
  xlab("Olsen-P [milligram/100 gram]") +
  ylab("Yield [100 kg/ha]") +
  ggtitle("Non-linear regression Olsen-P") +
  geom_point(alpha = 0.5) +
  geom_line(data = olsenPl, aes(x = olsenP, y = yield), colour = "red")



##########################################
# Second focus: Does the amount of bioavailable phosphorous influence the harvest yield?.
##########################################

# Significance of DGT, olsenP and DGT + olsenP

### Making linear models for differentiation between measurement methods and prediction
par(mfrow = c(2,2))

# Model: yield ~ DGT
modelDGT <- lm(yield ~ DGT)
summary(modelDGT) # p-value = 0.0408 * 
plot(modelDGT, main="yield ~ DGT")
anova(modelDGT)

# Model: yield ~ DGT
modelDGTmean <- lm(yield ~ DGT, data = PhosphorousMean)
summary(modelDGTmean) # p-value = 0.0408 * 
plot(modelDGT, main="yield ~ DGT")
anova(modelDGT)

# Model: yield ~ olsenP
modelOlsenP <- lm(yield ~ olsenP)
summary(modelOlsenP) # p-value = 0.175 
plot(modelOlsenP, main="yield ~ olsenP")
anova(modelOlsenP)
Response: yield

# Model: yield ~ location
modelLocation <- lm(yield ~ location)
summary(modelLocation)


# Combined model of DGT and olsenP: yield ~ DGT + olsenP
modelDGTolsenP <- lm(yield ~ DGT + olsenP)
summary(modelDGTolsenP) # p-valueDGT = 0.0946, p-valueOlsenP = 0.4648
plot(modelDGTolsenP, main="yield ~ DGT + olsenP")
anova(modelDGTolsenP)
#           Df  Sum Sq Mean Sq F value  Pr(>F)  
# DGT        1  490.26  490.26  4.4991 0.04258 *
# olsenP     1   59.81   59.81  0.5488 0.46475  
# Residuals 29 3160.06  108.97                  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Combined model of DGT and olsenP: yield ~ DGT * olsenP
modelDGTolsenPm <- lm(yield ~ DGT * olsenP)
summary(modelDGTolsenPm)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 93.39543    8.56600  10.903 1.38e-11 ***
#   DGT         -0.27738    0.13499  -2.055  0.04933 *  
#   olsenP      -7.57720    2.31088  -3.279  0.00279 ** 
#   DGT:olsenP   0.07568    0.02179   3.472  0.00169 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 8.882 on 28 degrees of freedom
# Multiple R-squared:  0.4046,	Adjusted R-squared:  0.3408 
# F-statistic: 6.343 on 3 and 28 DF,  p-value: 0.00203
plot(modelDGTolsenP, main="yield ~ DGT * olsenP")
anova(modelDGTolsenPm)
# Response: yield
#             Df  Sum Sq Mean Sq F value   Pr(>F)   
# DGT         1  490.26  490.26  6.2145 0.018856 * 
# olsenP      1   59.81   59.81  0.7581 0.391330   
# DGT:olsenP  1  951.16  951.16 12.0569 0.001694 **
# Residuals  28 2208.90   78.89                    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



par(mfrow=c(2,2))
# Combined model: yield ~ location + DGT + olsenP
modelAll <- lm(yield ~ DGT + olsenP + as.factor(location))
summary(modelAll)
alias(modelAll)
plot(modelAll, main="yield ~ location + DGT + olsenP")
anova(modelAll)

#modelAll <- lm(yield ~ as.numeric(location) + DGT + olsenP)
summary(modelAll)


ancova(modelAll)


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



### Permutation DGT, olsenP and the combined models
par(mfrow=c(1,1))
DGTmodel_perm <- rep(NA, 10000)

for (i in 1:10000) {
  DGT_perm <- DGT[sample(32)] # permute DGT's
  L <- lm(yield ~ DGT_perm)
  DGTmodel_perm[i] <- coef(L)[2]
}

hist(DGTmodel_perm,breaks=100, xlab="Slope", main="Density of permutated Linear Model slopes DGT")
abline(v = -0.08021827, col="red", lwd=3, lty=2)
abline(v = 0.08167824, col="red", lwd=3, lty=2)
abline(v=0.08397093, col="blue")

#abline(quantile(DGTmodel_perm, c(0.025, 0.975)))
DGT_obs <- coef(lm(yield ~ DGT))[2]
DGT_obs
quantile(DGTmodel_perm, c(0.025, 0.975)) ## see if beta_obs is inside interval

## p value
#
p0 <- sum(DGTmodel_perm < DGT_obs) / 10000 ## get position in distribution
{
  if (p0 > 0.5) 
    p <- 2*(1-p0)
  else p <- 2*p0
}
p # p value





### Permutation
olsenPmodel_perm <- rep(NA, 10000)

for (i in 1:10000) {
  olsenP_perm <- olsenP[sample(32)] # permute x's
  L <- lm(yield ~ olsenP_perm)
  olsenPmodel_perm[i] <- coef(L)[2]
}

hist(olsenPmodel_perm ,breaks=50, xlab="Slope", main="Density of permutated Linear Model slopes Olsen-P")
abline(v = -1.742545, col="red", lwd=3, lty=2)
abline(v = 1.766044, col="red", lwd=3, lty=2)
abline(v= 1.23009, col="blue")


olsenP_obs <- coef(lm(yield ~ olsenP))[2]
olsenP_obs
quantile(olsenPmodel_perm, c(0.025, 0.975)) ## see if beta_obs is inside interval

## p value
#
p0 <- sum(olsenPmodel_perm < olsenP_obs) / 10000 ## get position in distribution
{
  if (p0 > 0.5) p <- 2*(1-p0)
  else p <- 2*p0
}
p # p value










### Non-linear model permutation evaluation

# NL-Model DGT
modelDGT.nls <- nls(yield ~ a * DGT/(b+DGT), data = Phosphorous, start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield)))
summary(modelDGT.nls)




DGTNLmodel_permA <- rep(NA, 10000)
DGTNLmodel_permB <- rep(NA, 10000)

set.seed(10)
for (i in 1:10000) {
  DGTNL_perm <- DGT[sample(32)] # permute DGT's
  L <-  nls(yield ~ a * DGTNL_perm/(b+DGTNL_perm),nls.control((warnOnly = TRUE)), start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield)))
  DGTNL_perm <- NA
  DGTNLmodel_permA[i] <- coef(L)[2]
  DGTNLmodel_permB[i] <- coef(L)[1]
}
DGTNLmodel_permA <- na.omit(DGTNLmodel_permA)
DGTNLmodel_permB <- na.omit(DGTNLmodel_permB)

coef(nls(yield ~ a * olsenP/(b+olsenP), data = Phosphorous, start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield))))[2]
quantile(DGTNLmodel_permA, c(0.025, 0.975)) ## see if beta_obs is inside interval
hist(DGTNLmodel_permA,breaks=100, xlab="alpha", main="Alpha coefficient of non-linear regression model DGT")
abline(v = 63.74288, col="red", lwd=3, lty=2)
abline(v = 77.74456, col="red", lwd=3, lty=2)
abline(v= 72.34983, col="blue")

coef(nls(yield ~ a * olsenP/(b+olsenP), data = Phosphorous, start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield))))[1]
quantile(DGTNLmodel_permB, c(0.025, 0.975)) ## see if beta_obs is inside interval
hist(DGTNLmodel_permB,breaks=100, xlab="beta", main="Beta coefficient of non-linear regression model DGT")
abline(v = -4.123488, col="red", lwd=3, lty=2)
abline(v = 4.982314, col="red", lwd=3, lty=2)
abline(v= 0.1042812, col="blue")


mean(is.na(DGTNLmodel_permA))

# ## p value
# #
# p0 <- sum(DGTmodel_perm < DGT_obs) / 10000 ## get position in distribution
# {
#   if (p0 > 0.5) 
#     p <- 2*(1-p0)
#   else p <- 2*p0
# }
# p # p value





# Model OlsenP
modelOlsenP.nls <- nls(yield ~ a * olsenP/(b+olsenP), data = Phosphorous, start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield)))
summary(modelOlsenP.nls)




#################################################
# Predict location eleven
modelDGT.nls <- nls(yield ~ a * DGT/(b+DGT), data = Phosphorous, start = list(b = max(Phosphorous$yield)/2, a = max(Phosphorous$yield)))
predict(modelDGT.nls, Phosphorous[36,])

mDGT <- lm(yield ~ DGT, data = PhosphorousWO)
predict(mDGT, Phosphorous[36,])


Phosphorous[Phosphorous$location == "011",]
