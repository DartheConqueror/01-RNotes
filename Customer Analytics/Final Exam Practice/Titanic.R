#Logistic Regression Practice
#Titanic Revisited

#Set up----

rm(list=ls())

library(dplyr)
library(ggplot2)
library(cvTools)
library(visreg)

#Set Working Directeory

setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Business Analytics/Projects/Kaggle 1/Given Data")

#Data

train <- read.csv("train.csv")

test <- read.csv("test.csv")

#Data Understanding----

#Ratio of Sex
plot(train$Sex)

#Average Age of Males and Females on Different Passenger Classes

#age by pclass

pclassage <- c(1:3)
pclassage <- as.data.frame(pclassage)
colnames(pclassage) <- c("Passenger_Class")


for (i in 1:unique(train$Pclass)) {
  
  pclassage$male_age_mean[i] <- mean(train[train$Pclass == i & train$Sex == "male", ]$Age, na.rm = TRUE)
  pclassage$female_age_mean[i] <- mean(train[train$Pclass == i & train$Sex == "female", ]$Age, na.rm = TRUE)
  
}  

#Check for Missing Data----

#Total Missing Values
sum(is.na(train))
sum(is.na(test))

#Columns with Missing Values
colnames(train)[colSums(is.na(train)) > 0]
colnames(train)[colSums(is.na(train)) > 0]

#Dummy Variable Approach to Filling in Missing Age

#if data is missing from column substitute a 1, else a 0
train$DummyAge <- ifelse(is.na(train$Age), 1, 0)
test$DummyAge <- ifelse(is.na(test$Age), 1, 0)

#fills missing data with a 0, if no missing values leaves original value
train$Age <- ifelse(is.na(train$Age), 0, train$Age)
test$Age <- ifelse(is.na(test$Age), 0, test$Age)

#Build a Logistic Model----

#Makes a logit and puts it into an object
Titanic_logit <- glm(Survived ~ Sex + Pclass + Age+ DummyAge, data=train, family = "binomial")


summary(Titanic_logit)

#makes predictions using induced model
train$logistic_predictions <- predict(Titanic_logit, train, type="response")

hist(train$logistic_predictions)

#Survivorship Predictions as A Function of Each Piece of the Logit
#Sex
plot(train$Sex, train$logistic_predictions)

#PClass
plot(train$Pclass, train$logistic_predictions)

#Age
plot(train$Age, train$logistic_predictions)

#DummyAge
plot(train$DummyAge, train$logistic_predictions)

#Visualize Logit with a Lift Chart----

#overal survive rate
overall.responserate <- mean(train$Survived)

train$Log_Predict_Deciles <- ntile(train$logistic_predictions, 10)
train$Log_Predict_Deciles <- 11 - train$Log_Predict_Deciles

#Finds Retention Rate in Each Decile 
logistic.quantile.summary<-aggregate(train$Survived, by = list(train$Log_Predict_Deciles), FUN = mean)


#Changes Column Names
colnames(logistic.quantile.summary)<-c("quantile","survived.percentage")


#finds n of each quantile
logistic.quantile.summary$n<-table(train$Log_Predict_Deciles)

#cum sum
logistic.quantile.summary$cum.n<-cumsum(logistic.quantile.summary$n)

#n survived = n * surived probability
logistic.quantile.summary$n.survived <- logistic.quantile.summary$n * logistic.quantile.summary$survived.percentage

#Cum n survived
logistic.quantile.summary$cum.n.survived <- cumsum(logistic.quantile.summary$n.survived)

#Cum Survive Rate
logistic.quantile.summary$cum.survive.rate <- logistic.quantile.summary$cum.n.survived/logistic.quantile.summary$cum.n

#Cum Lift
logistic.quantile.summary$cumlift <- logistic.quantile.summary$cum.survive.rate / overall.responserate

#Plot
plot(x = logistic.quantile.summary$quantile, y = logistic.quantile.summary$cumlift,
     type = "b",
     main = "Logistic Regression Cumulative Lift",
     ylab = "Cumulative Lift",
     xlab = "Decile"
     )
abline(1,0, col="red", lwd=5)

#Cross Validation----
cvFit(Titanic_logit, y = train$Survived, data = train, K = 5, R = 5)

#Make Prediction on Test Data----

test$logistic_predictions <- predict(Titanic_logit, test, type="response")

test$Survived <- ifelse(test$logistic_predictions > .5, 1, 0)

submission <- subset(test, select = c(1, 14))

write.csv(submission, file = "rpracticesubmission.csv")
