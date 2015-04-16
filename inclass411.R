setwd("C:/Users/Emily/Desktop/Documents/_Spring 2015/cs183")

btheftData <- read.csv("biketheftlog.csv", header = TRUE, stringsAsFactors = FALSE)
btheftData <- na.omit(btheftData)

############ Question 1 ############

for (i in 1:nrow(btheftData)){
  if (grepl("Swig .",btheftData$Location[i])){
    btheftData$Location[i] = "Swig"
  } else if (grepl("Sobrato .",btheftData$Location[i])){
    btheftData$Location[i] = "Sobrato"
  } else if (grepl("Graham .",btheftData$Location[i])){
    btheftData$Location[i] = "Graham"
  } else if (grepl("Villas .",btheftData$Location[i])){
    btheftData$Location[i] = "Villas"
  } else if (grepl("Walsh .",btheftData$Location[i])){
    btheftData$Location[i] = "Walsh"
  } else if (grepl("Sanfilippo .",btheftData$Location[i])){
    btheftData$Location[i] = "Sanfilippo"
  } else if (grepl("Nobili .",btheftData$Location[i])){
    btheftData$Location[i] = "Nobili"
  } else if (grepl("Campisi .",btheftData$Location[i])){
    btheftData$Location[i] = "Campisi"
  } else if (grepl("Dunne .",btheftData$Location[i])){
    btheftData$Location[i] = "Dunne"
  } else if (grepl("Bellarmine .",btheftData$Location[i])){
    btheftData$Location[i] = "Bellarmine"
  } else{
    btheftData$Location[i] = "Other"
  }
}

#Swig is the highest with 23 incidents

######### Question 2 #############

library(lubridate)

btheftData$wday <- wday(mdy(btheftData$Date))
table(btheftData$wday)

#Monday is highest with 40 incidents

######## Question 3 #############
btheftData$month <- month(mday(btheftData$Date))
table(btheftData$month)

#February is highest with 31 incidents








