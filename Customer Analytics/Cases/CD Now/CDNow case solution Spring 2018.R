#Fernbach
#University of Colorado Leeds School of Business
#MKTG 3050: Customer Analytics
#Updated April 2018

#set working directory
setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Cases/CD Now")

rm(list=ls())

#load transaction data and retention data
CDNow.summary.all<-read.csv("CDNow Restructured.csv")




#install.packages("dplyr")

library(dplyr)
library(visreg)
#install.packages("cvTools")
library(cvTools)



CDNow.summary.all$frequencyquantile <- ntile(CDNow.summary.all$Frequency,3)  

#quantile analysis

CDNow.summary.all$monetaryquantile <- ntile(CDNow.summary.all$Monetary,10)  
CDNow.summary.all$recencyquantile <- ntile(CDNow.summary.all$recency,10)  

monetary.quantile.summary<-aggregate(CDNow.summary.all$retained, by = list(CDNow.summary.all$monetaryquantile), FUN = mean)
colnames(monetary.quantile.summary)<-c("quantile","retention.percentage")
monetary.quantile.summary$n<-table(CDNow.summary.all$monetaryquantile)
plot(monetary.quantile.summary$quantile, monetary.quantile.summary$retention.percentage, type="l")

recency.quantile.summary<-aggregate(CDNow.summary.all$retained, by = list(CDNow.summary.all$recencyquantile), FUN = mean)
colnames(recency.quantile.summary)<-c("quantile","retention.percentage")
recency.quantile.summary$n<-table(CDNow.summary.all$recencyquantile)
plot(recency.quantile.summary$quantile, recency.quantile.summary$retention.percentage, type="l")

#cumulative lift charts

#monetary


overall.responserate<-mean(CDNow.summary.all$retained)

monetary.quantile.summary<-monetary.quantile.summary[order(-monetary.quantile.summary$quantile),]
monetary.quantile.summary$cum.n<-cumsum(monetary.quantile.summary$n)
monetary.quantile.summary$n.retained<-monetary.quantile.summary$n*monetary.quantile.summary$retention.percentage
monetary.quantile.summary$cum.n.retained<-cumsum(monetary.quantile.summary$n.retained)
monetary.quantile.summary$cumlift<-(monetary.quantile.summary$cum.n.retained/monetary.quantile.summary$cum.n)/overall.responserate

plot(-monetary.quantile.summary$quantile, monetary.quantile.summary$cumlift, type="l", )

plot(monetary.quantile.summary$quantile, monetary.quantile.summary$cumlift, type="l", lwd=5, xlab="monetary quantile", ylab="cumulative lift",ylim=c(1,3), xlim=rev(range(monetary.quantile.summary$quantile)))
abline(1,0, col="red", lwd=5)




#recency

recency.quantile.summary$cum.n<-cumsum(recency.quantile.summary$n)
recency.quantile.summary$n.retained<-recency.quantile.summary$n*recency.quantile.summary$retention.percentage
recency.quantile.summary$cum.n.retained<-cumsum(recency.quantile.summary$n.retained)
recency.quantile.summary$cumlift<-(recency.quantile.summary$cum.n.retained/recency.quantile.summary$cum.n)/overall.responserate

plot(recency.quantile.summary$quantile, recency.quantile.summary$cumlift, type="l",lwd=5, xlab="recency quantile", ylab="cumulative lift",ylim=c(1,3) )
abline(1,0, col="red", lwd=5)


#Linear regression
CDNow.linearfit <-lm(retained ~ recency + Frequency + Monetary, data=CDNow.summary.all)
summary(CDNow.linearfit)
CDNow.summary.all$linearfit <- predict(CDNow.linearfit, type="response")
hist(CDNow.summary.all$linearfit)

plot(CDNow.summary.all$Monetary,CDNow.summary.all$linearfit)
plot(CDNow.summary.all$Frequency,CDNow.summary.all$linearfit)
plot(CDNow.summary.all$recency,CDNow.summary.all$linearfit)


#logistic regression

CDNow.logisticfit <- glm(retained ~ recency + Frequency + Monetary, data=CDNow.summary.all, family = "binomial")
summary(CDNow.logisticfit)
CDNow.summary.all$logisticfit <- predict(CDNow.logisticfit,type="response")
hist(CDNow.summary.all$logisticfit)

plot(CDNow.summary.all$Monetary,CDNow.summary.all$logisticfit)
plot(CDNow.summary.all$Frequency,CDNow.summary.all$logisticfit)
plot(CDNow.summary.all$recency,CDNow.summary.all$logisticfit)

#lift chart for logistic regression

CDNow.summary.all$logisticquantile <- ntile(CDNow.summary.all$logisticfit,10)  

logistic.quantile.summary<-aggregate(CDNow.summary.all$retained, by = list(CDNow.summary.all$logisticquantile), FUN = mean)
colnames(logistic.quantile.summary)<-c("quantile","retention.percentage")
logistic.quantile.summary$n<-table(CDNow.summary.all$logisticquantile)
plot(logistic.quantile.summary$quantile, logistic.quantile.summary$retention.percentage, type="l")

logistic.quantile.summary<-logistic.quantile.summary[order(-logistic.quantile.summary$quantile),]

logistic.quantile.summary$cum.n<-cumsum(logistic.quantile.summary$n)
logistic.quantile.summary$n.retained<-logistic.quantile.summary$n*logistic.quantile.summary$retention.percentage
logistic.quantile.summary$cum.n.retained<-cumsum(logistic.quantile.summary$n.retained)
logistic.quantile.summary$cumlift<-(logistic.quantile.summary$cum.n.retained/logistic.quantile.summary$cum.n)/overall.responserate

plot(logistic.quantile.summary$quantile, logistic.quantile.summary$cumlift, type="l", lwd=5, xlab="quantile", ylab="cumulative lift",ylim=c(1,3), xlim=rev(range(logistic.quantile.summary$quantile)))
abline(1,0, col="red", lwd=5)


#Interaction Modeling and Cross-validation:

CDNow.logisticfit.int <- glm(retained ~ recency * Frequency + Monetary, data=CDNow.summary.all, family = "binomial")
summary(CDNow.logisticfit.int)
CDNow.summary.all$logisticfit.int <- predict(CDNow.logisticfit.int,type="response")

visreg(CDNow.logisticfit.int,"Frequency", by="recency", overlay="TRUE",  scale="response", xlim=c(1,20))


set.seed(12345)
cvFit(CDNow.logisticfit,y=CDNow.summary.all$retained, data=CDNow.summary.all,K=5, R=5)

cvFit(CDNow.logisticfit.int,y=CDNow.summary.all$retained, data=CDNow.summary.all,K=5, R=5)

