setwd("C:/Users/Emily/Desktop/Documents/_Spring 2015/cs183")

btheftData <- read.csv("biketheftlog.csv", header = TRUE, stringsAsFactors = FALSE)
btheftData <- na.omit(btheftData)

############ Create time series plot of SCU bike thefts ############

# Create a .tsv file of SCU bike thefts

#bike thefts/month/location
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
  } else if (grepl("Benson .",btheftData$Location[i])){
    btheftData$Location[i] = "Benson"
  } else if (grepl("Engineering .",btheftData$Location[i])){
    btheftData$Location[i] = "Engineering"
  } else if (grepl("Bannan .",btheftData$Location[i])){
    btheftData$Location[i] = "Bannan"
  } else if (grepl("Learning .",btheftData$Location[i])){
     btheftData$Location[i] = "Library"
  } else if (grepl("Kenna .",btheftData$Location[i])){
    btheftData$Location[i] = "Benson"
  } else if (grepl("Malley .",btheftData$Location[i])){
    btheftData$Location[i] = "Malley"
  } else{
    btheftData$Location[i] = "Other"
  }
}


# Organize by month 

library(lubridate)

btheftData$Month <- month(mdy(btheftData$Date))
btheftData$Year <- year(mdy(btheftData$Date))
btheftData$YrMo<- paste(btheftData$Year, btheftData$Month,sep="-")

newData <- as.data.frame(cbind(btheftData$YrMo,btheftData$Location))
newData$count=1
colnames(newData) <- c("YrMo","Location","Count")
aggdata <- aggregate(newData$Count,by=list(newData$Location,newData$YrMo),FUN=sum,na.rm=FALSE)

library(reshape2)

final = dcast(aggdata, Group.1 ~ Group.2)
final[is.na(final)] <- 0

write.table(final, file="biketheft.tsv",sep="\t",row.names=FALSE)
