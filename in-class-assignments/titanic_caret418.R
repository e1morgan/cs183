setwd("C:/Users/Emily/Desktop/Documents/_Spring 2015/cs183")
trainData <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)

################ 1. Preprocess Data ##################
age_mean = round(mean(trainData$Age, na.rm=TRUE),digits=2)

for (i in 1:nrow(trainData)) {
  if (is.na(trainData$Age[i])){
    trainData$Age[i] = age_mean
  }
}

pclass_mean = round(mean(trainData$Pclass, na.rm=TRUE),digits=2)

for (i in 1:nrow(trainData)) {
  if (is.na(trainData$Pclass[i])){
    trainData$Pclass[i] = pclass_mean
  }
}

fare_mean = round(mean(trainData$Fare, na.rm=TRUE),digits=2)

for (i in 1:nrow(trainData)) {
  if (is.na(trainData$Fare[i])){
    trainData$Fare[i] = fare_mean
  }
}

test_age_mean = round(mean(testData$Fare, na.rm=TRUE),digits=2)

for (i in 1:nrow(testData)){
  if (is.na(testData$Age[i])){
    testData$Age[i] = test_age_mean
  }
}

#################### 2. Understand Tree ##############

library(rpart)
fit <- rpart(Survived ~ Pclass + Age + Fare + Sex, method="class", data=trainData)
# printcp(fit)
# plotcp(fit)
# summary(fit)

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


#################### 3. Fit a Model ###################
# install.packages('doMC')
# library(doMC)
# registerDoMC(cores = 6)

library(caret)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verbose=TRUE)

#gmb model
library(gbm)

gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3, 4),
                        n.trees = (1:5)*200,
                        shrinkage = c(.1,  .01,  .001))
x = nrow(gbmGrid)
set.seed(x)
gbmFit2 <- train(Survived ~ Pclass + Age + SibSp + Parch + Fare + Sex, 
                 data = trainData,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid = gbmGrid)


#################### Inspect Model #################

# trellis.par.set(caretTheme())
# plot(gbmFit2)

################### Submit Kaggle #################


probs <- predict(gbmFit2, newdata = testData)
probs[418]=0
probs <- round(probs)
testData$Survived <- probs
 
# inclasskaggle.sub <- cbind(PassengerId,Survived)
# colnames(inclasskaggle.sub) <- c("PassengerId", "Survived")
# write.csv(inclasskaggle.sub, file = "inclasskaggle.csv", row.names = FALSE)

submit <- data.frame(PassengerId = testData$PassengerId, Survived = testData$Survived)
write.csv(submit, file = "titanic.csv", row.names=FALSE)
