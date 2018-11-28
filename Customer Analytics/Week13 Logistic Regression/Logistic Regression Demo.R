#Fernbach
#University of Colorado Leeds School of Business
#MKTG 3050: Customer Analytics
#Updated April 2018

rm(list = ls())

#SET WORKING DIRECTORY
setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Week13 Logistic Regression")

#IMPORT DATASET
transactions <- read.csv("transactions2.csv")

#VISUALIZE ORDER QUANTITY

#install.packages("ggplot2")
library(ggplot2)
ggplot(transactions, aes(x = Order_Quantity)) + geom_histogram(binwidth = 10, fill = "black") + labs(x="Order Quantity", y = "Frequency")

library(visreg)
#THE WRONG WAY

wrong_allpredictors <- lm(Order_Quantity ~ Gender + Married + Income + Loyalty + 
                             Lag_Purchase + Lag_Order_Quantity, data=transactions)
summary(wrong_allpredictors)
transactions$wrong_allpredictors <- predict(wrong_allpredictors, transactions, type="response")

#A BETTER WAY

#create new variable
transactions$Purchase <- ifelse(transactions$Order_Quantity == 0, 0, 1) 


#build response model using linear regression

linear_allpredictors <- lm(Purchase ~ Gender + Married + Income + Loyalty + 
                             Lag_Purchase + Lag_Order_Quantity, data=transactions)
summary(linear_allpredictors)
transactions$linear_allpredictors <- predict(linear_allpredictors, transactions, type="response")
plot(transactions$Lag_Purchase, transactions$linear_allpredictors)
plot(transactions$Lag_Order_Quantity, transactions$linear_allpredictors)




#build response model using logistic regression

logit_allpredictors <- glm(Purchase ~ Gender + Married + Income + Loyalty + 
                             Lag_Purchase + Lag_Order_Quantity, data=transactions, family="binomial")
summary(logit_allpredictors)
transactions$logit_allpredictors <- predict(logit_allpredictors, transactions, type="response")
plot(transactions$Lag_Purchase, transactions$logit_allpredictors)
plot(transactions$Lag_Order_Quantity, transactions$logit_allpredictors)



#build conditional-spend model using linear regression


linear_allpredictors_spend <- lm(Order_Quantity ~ Gender + Married + Income + Loyalty + 
                             Lag_Purchase + Lag_Order_Quantity, data=subset(transactions,Purchase==1))
summary(linear_allpredictors_spend)
transactions$linear_allpredictors_spend <- predict(linear_allpredictors_spend, transactions, type="response")

plot(transactions$Lag_Purchase, transactions$linear_allpredictors_spend)
plot(transactions$Lag_Order_Quantity, transactions$linear_allpredictors_spend)
plot(transactions$Income, transactions$linear_allpredictors_spend)
plot(transactions$Gender, transactions$linear_allpredictors_spend)

#combine models to predict future spend
transactions$future_spend_predict <- transactions$logit_allpredictors*transactions$linear_allpredictors_spend
plot(transactions$Lag_Purchase, transactions$future_spend_predict)
plot(transactions$Lag_Order_Quantity, transactions$future_spend_predict)

#COMPARISON OF THE WRONG AND THE BETTER WAY

plot(transactions$wrong_allpredictors, transactions$future_spend_predict, xlim=range(0:400), ylim=range(0:400))
+ abline (0,1)
 


