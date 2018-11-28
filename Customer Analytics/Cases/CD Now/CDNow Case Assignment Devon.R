# Devon Morgan
# CDNow Case Assignment
# Due Tuesday, May 1st


# Set working directory
setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Cases/CD Now")

# Import data set
CDNow.data <- read.csv("CDNow Restructured.csv")


# install.packages("ggplot2")
library(ggplot2)

# install.packages("dplyr")
library(dplyr)


# Part 1: Traditional RFM - Decile Analysis
# a.) For recency and monetary, create new columns that place each customer into a decile based on how they rank

CDNow.data$recency.decile <- ntile(CDNow.data$recency, 10)
CDNow.data$monetary.decile <- ntile(CDNow.data$Monetary, 10)



# b.) Calculate and plot the percent retention in each decile of recency and each decile of monetary.
#    Do these variables seem to many any difference to retention?

CDNow.data$recency.decile.percentage <- (CDNow.data$retained/CDNow.data$recency.decile)*100
ggplot(CDNow.data, aes(x = CDNow.data$recency.decile.percentage, y =)) + geom_histogram(binwidth = 10, fill = "black") + labs(x="% Retention in Each Decile of Recency", y = "# of Customers")



CDNow.data$monetary.decile.percentage <- (CDNow.data$retained/CDNow.data$monetary.decile)*100
ggplot(CDNow.data, aes(x = CDNow.data$monetary.decile.percentage)) + geom_histogram(binwidth = 10, fill = "black") + labs(x="% Retention of Each Decile of Monetary Value", y = "# of Customers")

library(visreg)


# c.) Now try to do the same analysis for frequency. What problems do you run into?

CDNow.data$frequency.decile <- ntile(CDNow.data$Frequency, 10)
CDNow.data$monetary.frequency.decile.percentage <- (CDNow.data$retained/CDNow.data$frequency.decile)*100
ggplot(CDNow.data, aes(x = CDNow.data$frequency.decile.percentage)) + geom_histogram(binwidth = 10, fill = "black") + labs(x="% Retention of Each Decile of Frequency", y = "# of Customers")

library(visreg)





# Part 2: Visualization with Cumulative Lift Charts

# a.) Create cumulative lift charts for recency and monetary.

install.packages("lift")
library(lift)

plotLift(CDNow.data$recency, CDNow.data$retained, cumulative = TRUE, n.buckets =10)
plotLift(CDNow.data$Monetary, CDNow.data$retained, cumulative = TRUE, n.buckets =10)






# Part 3: Predicting Retention with Regression

# a.) Use linear regression to predict retention using recency, monetary, and frequency as predictors & interpret the model
CDNow.linear.regression <-lm(retained ~ recency + Monetary + Frequency , data=CDNow.data)
summary(CDNow.linear.regression)


# b.) Generate a prediction for each customer. Plot the predictions as a function of recency, frequency, and
#     monetary value (thus create 3 plots, one for each predictor).

#create variable
CDNow.data$PredictionFunction <- ifelse(CDNow.data$retained == 0, 0, 1) 

#build response model using linear regression

linear_RMF <- lm(PredictionFunction ~ recency + Monetary + Frequency, data= CDNow.data)
summary(linear_RMF)
CDNow.data$linear_RMF <- predict(linear_RMF, CDNow.data, type="response")
plot(CDNow.data$recency, CDNow.data$linear_RMF)
plot(CDNow.data$Monetary, CDNow.data$linear_RMF)
plot(CDNow.data$Frequency, CDNow.data$linear_RMF)



# c.) Now do all the same steps using logistic regression

#build response model using logistic regression

logit_RMF <- glm(PredictionFunction ~ recency + Monetary + Frequency, data=CDNow.data, family="binomial")
summary(logit_RMF)
CDNow.data$logit_RMF <- predict(logit_RMF, CDNow.data, type="response")
plot(CDNow.data$recency, CDNow.data$logit_RMF)
plot(CDNow.data$Monetary, CDNow.data$logit_RMF)
plot(CDNow.data$Frequency, CDNow.data$logit_RMF)





# Part 4: Visualizing Logistic Regression with a Lift Chart
# a.) Create a cumulative lift chart based on the predictions of your logistic regression analysis. To do so, first bucket
#     customers into deciles based on the predictions of the logistic model. Use these deciles for your cumulative lift
#     chart. 

CDNow.data$logit_RMFdecile <- ntile(CDNow.data$logit_RMF, 10)
plotLift(CDNow.data$logit_RMFdecile, CDNow.data$retained, cumulative = TRUE, n.buckets =10)



#Part 5: Interaction Model & Cross-Validation

# a.) Run a new logistic regression that includes recency, frequency, monetary value, and in the interaction between recency 
#     and frequency


RMF.RF.int <-glm(retained ~ recency + Frequency + Monetary + recency*Frequency, data=CDNow.data)
summary(RMF.RF.int)
visreg(RMF.RF.int,"Frequency", by="recency", overlay = "TRUE")



# b.) Use the CcvTools package to cross-validate this model and the logistic model from part 3. 
#     Use K=5, R=5 for your parameters.

#install.packages("cvTools")
library(cvTools)


cvFit(RMF.RF.int, data=CDNow.data, y=CDNow.data$logit_RMF, K=5, R=5)
