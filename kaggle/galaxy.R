setwd("C:/Users/Emily/Desktop/Documents/_Spring 2015/cs183/kaggle2")

library("EBImage")
library("ripa")
library("jpeg")


############### Read in and format Train/Test Images/Data #################

trainData<-read.csv("galaxy_train.csv", header = TRUE, stringsAsFactors = FALSE)
allImages<-read.csv("image_index.csv", header = TRUE,stringsAsFactors = FALSE)

myImages <- list.files("./images_training_rev1", pattern="*jpg$", full.name=TRUE)

# img_title <- function(s){
#   s=strsplit(s,".")
#   
# } 
# imagenames=lapply(imagenames,img_title)

# 
# # set an empty matrix 
# X = matrix(,nrow=length(myImages),ncol=900)
# 
# resize, greyscale, and insert images into matrix X
for (i in 1:2){
  I=readJPEG(myImages[i])
  I=rgb2grey(I,coefs=c(0.30,0.59,0.11))
  I=resize(I,30,30)
  print(I)
}

########## Test vs Train ##############

# separate test and train data

# testData<-subset(allData,GalaxyID %in% fixedData$GalaxyID)




########## Fit a Model ################

# Use matlab code for neural networks
features<-read.csv("new_features.csv", header = FALSE, stringsAsFactors = FALSE)


# Create a random forest based on new features

library("randomForest")
set.seed(1234) 
modfit<-train(prob ~ features,
              data=trainData,
              method="rf",
              tuneGrid=data.frame(mtry=3))
pred<-predict(modfit,testData)
testData$booking_bool<-pred

testData$srchpropid<- paste(testData$srch_id, testData$prop_id, sep="-")

kaggle2.sub <- cbind(testData$srchpropid,testData$booking_bool)
colnames(kaggle2.sub) <- c("GalaxyID", "Prob_Smooth")
write.csv(kaggle2.sub, file = "kaggle2.csv", row.names = FALSE)
