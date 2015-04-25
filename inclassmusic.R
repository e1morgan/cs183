setwd("C:/Users/Emily/Desktop/Documents/_Spring 2015/cs183/inclass_425")

musicData <- read.csv("music-all.csv", header = TRUE, stringsAsFactors = FALSE)

musicData[is.na(musicData)] <- 0

my_pr=prcomp(musicData[,4:ncol(musicData)],retx=TRUE,scale.=TRUE)

p1<-plot(my_pr$sdev,
     main="Principal Components Std Dev",
     xlab="PrComp", ylab="Standard Deviation")

new_comp <- as.data.frame(my_pr$x)
new_comp$artist <- musicData$artist

library(ggplot2)

p2<-ggplot(new_comp,aes(x=PC1,y=PC2))+geom_text(aes(label=artist))
print(p2)
