CDNow <- read.csv("CDNow Restructured.csv")
View (CDNow) 
#install.packages("dplyr")
library("dplyr")
library(ggplot2)


#PART 1


CDNow$ntileMonetary <- ntile(CDNow$Monetary, 10)
CDNow$ntileRecency <- ntile(CDNow$recency, 10)
CDNow$ntileFrequency <- ntile(CDNow$Frequency, 10)

Recency <- aggregate(CDNow$retained, 
                     by = list(CDNow$ntileRecency),
                     FUN = mean )

plot(Recency)


Monetary <- aggregate(CDNow$retained, 
                      by = list(CDNow$ntileMonetary), 
                      FUN = mean)

plot(Monetary)

#why doesn't it work for frequency? 
Frequency <- aggregate(CDNow$retained, 
                       by = list(CDNow$ntileFrequency),
                       FUN = mean)
plot(Frequency)

#PART 2
#install.packages("lift")
library("lift")

plotLift(churn$predictions, churn$labels)
plotLift(CDNow$retained, CDNow$Frequency)
plotLift(CDNow$retained, CDNow$Monetary)
plotLift(CDNow$retained, CDNow$recency)


x#PART 3
#linear regression 
library(ggplot2)
linear_RFM <- lm(retained ~ recency + Frequency + Monetary, data = CDNow)
summary(linear_RFM)
CDNow$linear_RFM <- predict(linear_RFM, CDNow, type="response")
plot(CDNow$recency, CDNow$linear_RFM)
plot(CDNow$Frequency, CDNow$linear_RFM)
plot(CDNow$Monetary, CDNow$linear_RFM)

#logistic regression
logistic_RFM <- glm(retained ~ recency + Frequency + Monetary, data=CDNow, family="binomial")
summary(logistic_RFM)
CDNow$logistic_RFM <- predict(logistic_RFM, CDNow, type="response")
plot(CDNow$recency, CDNow$logistic_RFM)
plot(CDNow$Frequency, CDNow$logistic_RFM)
plot(CDNow$Monetary, CDNow$logistic_RFM)


#PART 4
#lift charts for logistic regression
plotLift(CDNow$retained, CDNow$logistic_RFM)

#PART 5
#Interaction Model and Cross-Validation 
#install.packages("visreg")
library("visreg")
interaction_RFM <- glm(retained ~ recency + Frequency + Monetary + recency*Frequency, data=CDNow, family = "binomial")
summary(interaction_RFM)

install.packages("cvTools")
library("cvTools")
cvFit(logistic_RFM, y=CDNow$retained, data=CDNow, K=5, R-5)
cvFit(interaction_RFM, y=CDNow$retained, data=CDNow, K=5, R=5)

