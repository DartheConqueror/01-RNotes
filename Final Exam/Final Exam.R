#FInal Exam
#May 2018

rm(list=ls())

library(visreg)
library(dplyr)

#Set Up ----
setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Final Exam")

order <- read.csv("orderquantity.csv")
donation <- read.csv("donation.csv")

#PART 2----

#5: linear regression, predict order qty 

ordqty <-lm(Order_Quantity ~ Loyalty , data=order)
summary(ordqty)

#9 Same regression but with income
ordqty2 <-lm(Order_Quantity ~ Loyalty + Income, data=order)
summary(ordqty2)
unique(order$Income)

#11 Same regression but with income as categorical
order$catInc <- factor(order$Income)

ordqty3 <-lm(Order_Quantity ~ Loyalty + catInc, data=order)
summary(ordqty3)

#13 regresssion but with missing dummmy approach

#subs missing values with 0
order$Income <- ifelse(is.na(order$Income), 0, order$Income)

#dummy column if value is 0, then 1
order$DummyIncome <- ifelse(order$Income == 0, 1, 0)

ordqty4 <-lm(Order_Quantity ~ Loyalty + Income + DummyIncome, data=order)
summary(ordqty4)

#16 regression with interaction
ordqty5 <-lm(Order_Quantity ~ Loyalty * Income, data=order)
summary(ordqty5)

visreg(ordqty5, "Loyalty", by = "Income", data = order, overlay = TRUE)
visreg(ordqty5, "Income", by = "Loyalty", data = order, overlay = TRUE)

#Bonus
order$linear_predictions <- predict(ordqty5, order, type = "response")
plot(y = order$linear_predictions, x = order$Income)

#PART3----

#18 bin all customers
donation$frequencydecile <- ntile(donation$frequency, 10)

freq.quantile.summary<-aggregate(donation$donate_11, by = list(donation$frequencydecile), FUN = mean)

#19 logit recency and freq to predict 2011 donations
donation.logisticfit <- glm(donate_11 ~ recency + frequency, data= donation, family = "binomial")
summary(donation.logisticfit)

#20 predictions using model
donation$logit_predict <- predict(donation.logisticfit, type = "response")

overall.response <- mean(donation$logit_predict)

#23 bin logit

donation$logitdecile <- ntile(donation$logit_predict, 10)

logit.quantile.summary <- aggregate(donation$donate_11, by = list(donation$logitdecile), FUN = mean)

#24 cum lift
colnames(logit.quantile.summary)<-c("quantile","retention.percentage")
logit.quantile.summary$quantile <- 11 - logit.quantile.summary$quantile
logit.quantile.summary<-logit.quantile.summary[order(logit.quantile.summary$quantile),]

logit.quantile.summary$n <- table(donation$logitdecile)

logit.quantile.summary$cum.n <- cumsum(logit.quantile.summary$n)

logit.quantile.summary$n.retained<-logit.quantile.summary$n*logit.quantile.summary$retention.percentage

logit.quantile.summary$cum.n.retained<-cumsum(logit.quantile.summary$n.retained)

logit.quantile.summary$cum.retention.rate <- logit.quantile.summary$cum.n.retained/logit.quantile.summary$cum.n

logit.quantile.summary$cumlift<-(logit.quantile.summary$cum.retention.rate)/overall.response

#26 logit with interaction
donation.logisticint <- glm(donate_11 ~ recency * frequency, data= donation, family = "binomial")
summary(donation.logisticint)

visreg(donation.logisticint, "frequency", by = "recency", data = donation, overlay = TRUE, scale = "response")

#27 cross validation
set.seed(123)

cvFit(donation.logisticfit, y = donation$donate_11, data=donation,K=5, R=5)

cvFit(donation.logisticint, y = donation$donate_11, data=donation,K=5, R=5)
