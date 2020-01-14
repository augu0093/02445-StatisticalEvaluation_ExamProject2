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

# Combined model: yield ~ location + DGT + olsenP
modelAll <- lm(yield ~ location + DGT + olsenP)
summary(modelAll)
plot(modelAll, main="yield ~ location + DGT + olsenP")
#anova(modelAll)


# Pooled model: yield ~ location * DGT * olsenP
modelAllPool <- lm(yield ~ location * DGT * olsenP)
summary(modelAllPool)








