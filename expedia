#setwd("C:/Users/Emily/Desktop/Documents/_Spring 2015/cs183/kaggle1")

trainData <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)

######################## PreProcess Data ##############################
#visitor_hist_starrating
for (i in 1:nrow(trainData)){
  if (trainData[,"visitor_hist_starrating"][i]==0){
    trainData[,"visitor_hist_starrating"][i] = NA
  }
}
  
rating_mean = round(mean(trainData$visitor_hist_starrating, na.rm=TRUE),digits=2)
  
for (i in 1:nrow(trainData)){
  if (is.na(trainData$visitor_hist_starrating[i])){
    trainData$visitor_hist_starrating[i] = rating_mean
  }
}

#visitor_hist_adr_usd
for (i in 1:nrow(trainData)){
  if (trainData[,"visitor_hist_adr_usd"][i]==0){
    trainData[,"visitor_hist_adr_usd"][i] = NA
  }
}

adr_mean = round(mean(trainData$visitor_hist_adr_usd, na.rm=TRUE),digits=2)

for (i in 1:nrow(trainData)){
  if (is.na(trainData$visitor_hist_adr_usd[i])){
    trainData$visitor_hist_adr_usd[i] = adr_mean
  }
}

#prop_starrating
for (i in 1:nrow(trainData)){
  if (trainData[,"prop_starrating"][i]==0){
    trainData[,"prop_starrating"][i] = NA
  }
}

prop_star_mean = round(mean(trainData$prop_starrating, na.rm=TRUE),digits=2)

for (i in 1:nrow(trainData)){
  if (is.na(trainData$prop_starrating[i])){
    trainData$prop_starrating[i] = prop_star_mean
  }
}

#prop_review_score
for (i in 1:nrow(trainData)){
  if (trainData[,"prop_review_score"][i]==0){
    trainData[,"prop_review_score"][i] = NA
  }
}

prop_review_mean = round(mean(trainData$prop_review_score, na.rm=TRUE),digits=2)

for (i in 1:nrow(trainData)){
  if (is.na(trainData$prop_review_score[i])){
    trainData$prop_review_score[i] = prop_review_mean
  }
}

#srch_query_affinity_score
for (i in 1:nrow(trainData)){
  if (trainData[,"srch_query_affinity_score"][i]==0){
    trainData[,"srch_query_affinity_score"][i] = NA
  }
}

srch_mean = round(mean(trainData$srch_query_affinity_score, na.rm=TRUE),digits=2)

for (i in 1:nrow(trainData)){
  if (is.na(trainData$srch_query_affinity_score[i])){
    trainData$srch_query_affinity_score[i] = srch_mean
  }
}

#orig_destination_distance
for (i in 1:nrow(trainData)){
  if (trainData[,"orig_destination_distance"][i]==0){
    trainData[,"orig_destination_distance"][i] = NA
  }
}

orig_mean = round(mean(trainData$orig_destination_distance, na.rm=TRUE),digits=2)

for (i in 1:nrow(trainData)){
  if (is.na(trainData$orig_destination_distance[i])){
    trainData$orig_destination_distance[i] = orig_mean
  }
}

############### PreProcess Test Data ###################
#visitor_hist_starrating
for (i in 1:nrow(testData)){
  if (testData[,"visitor_hist_starrating"][i]==0){
    testData[,"visitor_hist_starrating"][i] = NA
  }
}

rating_mean_test = round(mean(testData$visitor_hist_starrating, na.rm=TRUE),digits=2)

for (i in 1:nrow(testData)){
  if (is.na(testData$visitor_hist_starrating[i])){
    testData$visitor_hist_starrating[i] = rating_mean_test
  }
}

#visitor_hist_adr_usd
for (i in 1:nrow(testData)){
  if (testData[,"visitor_hist_adr_usd"][i]==0){
    testData[,"visitor_hist_adr_usd"][i] = NA
  }
}

adr_mean_test = round(mean(testData$visitor_hist_adr_usd, na.rm=TRUE),digits=2)

for (i in 1:nrow(testData)){
  if (is.na(testData$visitor_hist_adr_usd[i])){
    testData$visitor_hist_adr_usd[i] = adr_mean_test
  }
}

#prop_starrating
for (i in 1:nrow(testData)){
  if (testData[,"prop_starrating"][i]==0){
    testData[,"prop_starrating"][i] = NA
  }
}

prop_star_mean_test = round(mean(testData$prop_starrating, na.rm=TRUE),digits=2)

for (i in 1:nrow(testData)){
  if (is.na(testData$prop_starrating[i])){
    testData$prop_starrating[i] = prop_star_mean_test
  }
}

#prop_review_score
for (i in 1:nrow(testData)){
  if (testData[,"prop_review_score"][i]==0){
    testData[,"prop_review_score"][i] = NA
  }
}

prop_review_mean_test = round(mean(testData$prop_review_score, na.rm=TRUE),digits=2)

for (i in 1:nrow(testData)){
  if (is.na(testData$prop_review_score[i])){
    testData$prop_review_score[i] = prop_review_mean_test
  }
}


#srch_query_affinity_score
for (i in 1:nrow(testData)){
  if (testData[,"srch_query_affinity_score"][i]==0){
    testData[,"srch_query_affinity_score"][i] = NA
  }
}

srch_mean_test = round(mean(testData$srch_query_affinity_score, na.rm=TRUE),digits=2)

for (i in 1:nrow(testData)){
  if (is.na(testData$srch_query_affinity_score[i])){
    testData$srch_query_affinity_score[i] = srch_mean_test
  }
}

#orig_destination_distance
for (i in 1:nrow(testData)){
  if (testData[,"orig_destination_distance"][i]==0){
    testData[,"orig_destination_distance"][i] = NA
  }
}

orig_mean_test = round(mean(testData$orig_destination_distance, na.rm=TRUE),digits=2)

for (i in 1:nrow(testData)){
  if (is.na(testData$orig_destination_distance[i])){
    testData$orig_destination_distance[i] = orig_mean_test
  }
}
# 
# 
# ############## Exploratory Analysis ##########################
library(ggplot2)
library(caret)
# 
# #qplot(prop_review_score,booking_bool,colour=srch_id,data=trainData)
trainData$stars <- trainData$visitor_hist_starrating * trainData$prop_starrating
testData$stars <- testData$visitor_hist_starrating * testData$prop_starrating
trainData$price_compare <- trainData$visitor_hist_adr_usd * trainData$price_usd
testData$price_compare <- testData$visitor_hist_adr_usd * testData$price_usd
trainData$comp_rates <- (trainData$comp1_rate + trainData$comp2_rate +trainData$comp3_rate +trainData$comp4_rate +trainData$comp5_rate +trainData$comp6_rate +trainData$comp7_rate +trainData$comp8_rate)/8
testData$comp_rates <- (testData$comp1_rate + testData$comp2_rate +testData$comp3_rate +testData$comp4_rate +testData$comp5_rate +testData$comp6_rate +testData$comp7_rate +testData$comp8_rate)/8
# #qplot(stars,booking_bool,data=trainData)
# #qplot(visitor_hist_adr_usd,booking_bool,data=trainData)
# 
# # 
# # library(rpart)
# # fit <- rpart(booking_bool ~ stars + Age + Fare + Sex, method="class", data=trainData)
# # printcp(fit)
# # plotcp(fit)
# # summary(fit)
# # 
# # # plot tree 
# # plot(fit, uniform=TRUE, 
# #      main="Classification Tree")
# # text(fit, use.n=TRUE, all=TRUE, cex=.8)
# 
# 
# ############## Fit a Model ###########
# 

library("randomForest")
set.seed(1234) 
modfit<-train(booking_bool ~ stars + price_compare + promotion_flag + comp_rates + prop_brand_bool,
              data=trainData,
              method="rf",
              tuneGrid=data.frame(mtry=3))
pred<-predict(modfit,testData)
testData$booking_bool<-pred

testData$srchpropid<- paste(testData$srch_id, testData$prop_id, sep="-")

kaggle2.sub <- cbind(testData$srchpropid,testData$booking_bool)
colnames(kaggle2.sub) <- c("srch-prop_id", "booking_bool")
write.csv(kaggle2.sub, file = "kaggle2.csv", row.names = FALSE)
