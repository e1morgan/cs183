data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))

head(data1)

#categorizes users by age
data1$age_group <- cut(data1$Age,c(-Inf,18,24,34,44,54,64,Inf))

#only signed in users have ages and genders
data1_s <- subset(data1, data1$Signed_In==1)

#signed in users click through rate
data1_s$ctr <- data1_s$Clicks/data1_s$Impressions

#categorize users based on click behavior
data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions >0] <- "Imps"
data1$scode[data1$Clicks >0] <-"Clicks"

library('doBy')
siterange <- function(x){c(length(x), min(x), mean(x), max(x))}
summaryBy(Age~age_group, data=data1_s, FUN=siterange)

data1$hasimps <- cut(data1$Impressions, c(-Inf,0,Inf))
summaryBy(Clicks~hasimps, data =data1, FUN=siterange)

library(ggplot2)
#signed in, click through rate by age group
p1=ggplot(subset(data1_s, Clicks>0), aes(x=Clicks/Impressions, colour=age_group)) +geom_density()
p1=p1+ggtitle("Signed in, Click Through Rate by Age Group")
print(p1)

#signed in, number of impressions by age group
p2=ggplot(data1_s, aes(x=age_group, y=Impressions, fill=age_group))+geom_boxplot()
p2=p2+ggtitle("Age Group vs Impressions")
print(p2)

AveAge=mean(data1_s$Age)
print(AveAge)

AveGen=mean(data1_s$Gender)
print(AveGen)

clen <-function(x){c(length(x))}
etable1 <- summaryBy(Impressions~scode+Gender+age_group+AveAge+AveGen, data = data1_s, FUN=clen)

