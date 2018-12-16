#Dareck Giuliani
#CD Now Case

rm(list=ls())
library(dplyr)
library(ggplot2)
library(cvTools)
library(visreg)

setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Cases/CD Now")

cd <- read.csv("CDNOW Restructured.csv")

#PART 1: TRADITIONAL RFM ANALYSIS ----

#dividing recency and monetary into deciles
#why would someone remove the reverse function of ntile?!
cd$recencyrank <- ntile(cd$recency, 10)
cd$monetaryrank <- ntile(cd$Monetary, 10)
cd$frequencyrank <- ntile(cd$Frequency, 10)

#retention rates of each decile in recency

recency <- c(1:10)
recency <- as.data.frame(recency)
colnames(recency) <- c("Decile")

#retention rates of each decile in monetary

monetary <- c(1:10)
monetary <- as.data.frame(monetary)
colnames(monetary) <- c("Decile")

#trying with frequency

frequency <- c(1:10)
frequency <- as.data.frame(frequency)
colnames(frequency) <- c("Decile")



for (i in 1:10) {

    recency$avg_retained[i] <- mean(cd[cd$recencyrank == i, ]$retained)
    monetary$avg_retained[i] <- mean(cd[cd$monetaryrank == i, ]$retained)
    frequency$avg_retained[i] <- mean(cd[cd$frequencyrank == i, ]$retained)
}


#plot recency retention rates

plot(recency, xlab = "Recency Rank", ylab = "Percent Retained")

#plot monetary retention rates

plot(monetary, xlab = "Monetary Rank", ylab = "Percent Retained")

#plot frequency

plot(frequency, xlab = "Frequency Rank", ylab = "Percent Retained")


#PART 2: VISUALIZATION WITH CUMULATIVE LIFT CHARTS----

#Calculating inputs of left chart


for (i in 1:10) {
  
  recency$n[i] <- length(cd[cd$recencyrank == i, ]$retained)
  
  recency$respondents_recency[i] <- length(cd[cd$retained == 1 & cd$recencyrank == i, ]$retained)
  
  recency$response_rate_percent_recency[i] <- recency$respondents_recency[i]/(length(cd[cd$recencyrank == i, ]$retained))
  
  monetary$n[i] <- length(cd[cd$monetaryrank == i, ]$retained)
  
  monetary$respondents_monetary[i] <- length(cd[cd$retained == 1 & cd$monetaryrank == i, ]$retained)
  
  monetary$response_rate_percent_monetary[i] <- monetary$respondents_monetary[i]/(length(cd[cd$monetaryrank == i, ]$retained))
  
}

avg_recency_response <- mean(recency$response_rate_percent_recency)
avg_monetary_response <- mean(monetary$response_rate_percent_monetary)

#lift

recency$lift_recency <- (recency$response_rate_percent_recency)/avg_recency_response

monetary$lift_monetary <- monetary$response_rate_percent_monetary/avg_monetary_response

#Something I am ashamed of, but I was having difficulty with rownames, so I took matters into my own hands
monetary$Decile <- (11- monetary$Decile)
monetary <- monetary[order(monetary$Decile), ]


#cumulative respondents

recency$cum_respondents <- cumsum(recency$respondents_recency)

monetary$cum_respondents <- cumsum(monetary$respondents_monetary)

#Cumulative N

recency$cum_n <- cumsum(recency$n)
monetary$cum_n <- cumsum(monetary$n)

#Cumulative response rate

recency$cum_response_rate <- recency$cum_respondents/recency$cum_n
monetary$cum_response_rate <- monetary$cum_respondents/monetary$cum_n

#Cumulative Lift

recency$cum_lift <- recency$cum_response_rate/avg_recency_response

monetary$cum_lift <- monetary$cum_response_rate/avg_monetary_response

#plot cum lift

plot(x = recency$Decile, y=recency$cum_lift,type = "b",
     main = "Recency Cumulative Lift",
     ylab = "Cumulative Lift",
     xlab = "Decile")



plot(x = monetary$Decile, y= monetary$cum_lift, type="b",
     main = "Monetary Cumulative Lift",
     ylab = "Cumulative Lift",
     xlab = "Decile")

#PART 3: PREDICTING RETENTION WITH REGRESSION----

#Linear Regression
#Predicting retained
summary(lm(cd$retained ~ cd$Monetary + cd$Frequency + cd$recency))
linear_predictions_retained <- lm(cd$retained ~ cd$Monetary + cd$Frequency + cd$recency)

#each piece of the RFM analysis has a significant main effect on retention

#predict retention from model
cd$linear_predictions_retained <- predict(linear_predictions_retained, cd, type="response")

#graph linear predictions as a function of recency
plot(cd$recency, cd$linear_predictions_retained)

#graph linear predictions as a function of frequency
plot(cd$Frequency, cd$linear_predictions_retained)

#graph linear predictions as a function of monetary
plot(cd$Monetary, cd$linear_predictions_retained)

#Logistic Regression
#Predicting Retained
summary(glm(cd$retained ~ cd$Monetary + cd$Frequency + cd$recency, family ="binomial"))
logistic_predictions_retained <- glm(cd$retained ~ cd$Monetary + cd$Frequency + cd$recency, family = "binomial")

#predict retention from logistic model
cd$logistic_predictions_retained <- predict(logistic_predictions_retained, cd, type = "response")

#graph logit predictions as a function of recency
plot(cd$recency, cd$logistic_predictions_retained)

#graph logit predictions as a function of frequency
plot(cd$Frequency, cd$logistic_predictions_retained)

#graph logit predictions as a function of monetary
plot(cd$Monetary, cd$logistic_predictions_retained)

#PART 4: VISUALIZIGNG LOGISTIC REGRESSION WITH A LIFT CHART ----

#Bucket predictions of customers based on log model into deciles

cd$Log_Prediction_Deciles <- ntile(cd$logistic_predictions_retained, 10) #reverse = TRUE)

log_prediction <- c(1:10)
log_prediction <- as.data.frame(log_prediction)
colnames(log_prediction) <- c("Decile")

#Finds Retention Rate in Each Decile 
for (i in 1:10) {
  
  #n
  log_prediction$n[i] <- length(cd[cd$Log_Prediction_Deciles == i, ]$retained)
  
  #retention rate for each decile
  log_prediction$Log_Predictions_Retention_Rate[i] <- mean(cd[cd$Log_Prediction_Deciles == i, ]$retained)
  
  #finds respondents in each decile
  log_prediction$respondents_log_predictions[i] <- length(cd[cd$retained == 1 & cd$Log_Prediction_Deciles == i, ]$retained)

  #finds response rate
  log_prediction$response_rate_percent_log[i] <- cd$respondents_log_predictions[i]/(length(cd[cd$Log_Prediction_Deciles == i, ]$retained))
  
}

#avg log response

avg_log_response <- mean(log_prediction$Log_Predictions_Retention_Rate)

#lift

log_prediction$Lift <- log_prediction$Log_Predictions_Retention_Rate/avg_log_response

#Not my proudest moment
log_prediction$Decile <- (11 - log_prediction$Decile)
log_prediction <- log_prediction[order(log_prediction$Decile), ]

#Cum Respondents

log_prediction$Cum_Respondents <- cumsum(log_prediction$respondents_log_predictions)

#Cum N

log_prediction$Cum_n <- cumsum(log_prediction$n)

#Cumulative response rate

log_prediction$cum_response_rate <- log_prediction$Cum_Respondents/log_prediction$Cum_n

#Cum Lift

log_prediction$Cum_Lift <- log_prediction$cum_response_rate/avg_log_response

#plot cum lift

plot(x = log_prediction$Decile, y=log_prediction$Cum_Lift,type = "b",
     main = "Logistic Regression Cumulative Lift",
     ylab = "Cumulative Lift",
     xlab = "Decile")

#PART 5: INTERACTION MODEL AND CROSS-VALIDATION ----

#logistic regression that includes rec, freq, and monetary value with interaction between rec and freq
summary(glm(cd$retained ~ cd$Monetary + cd$Frequency + cd$recency + cd$recency * cd$Frequency, family ="binomial"))
logistic_predictions_retained_interaction <- glm(cd$retained ~ cd$Monetary + cd$Frequency + cd$recency + 
                                                   (cd$recency * cd$Frequency), family ="binomial")

test <- glm(retained ~ Monetary + Frequency + recency + 
                                                   (recency * Frequency), data = cd)

visreg2d(test, xvar = "recency", yvar = "Frequency", scale = "response")
visreg(test, "Frequency", "recency", ylab= "retained", overlay = TRUE, scale = "response")


#Cross Validate
cvFit(logistic_predictions_retained_interaction, y = cd$retained, data = cd, K = 5, R = 5)

cvFit(logistic_predictions_retained, y = cd$retained, data = cd, K = 5, R = 5)
