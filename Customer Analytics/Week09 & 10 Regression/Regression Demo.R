#Fernbach
#University of Colorado Leeds School of Business
#MKTG 3050: Customer Analytics
#Updated February 2017

#SET WORKING DIRECTORY

setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Week09 & 10 Regression")

#IMPORT DATASET
regression.demo <- read.csv("Regression Demo.csv")

#Install and load visreg package for visualizing regression models. You only have to install once
#but need to reload the package every time you open R

#install.packages("visreg")
library(visreg)

#INTRODUCTORY T-TEST ANALYSIS
#t-test on male/female order quantity
t.test(regression.demo$Order_Quantity ~ regression.demo$Gender, var.equal=TRUE)



#INTERCEPT ONLY MODEL

interceptonly <-lm(Order_Quantity ~ 1 , data=regression.demo)
summary(interceptonly)


#ONE BINARY PREDICTOR
#Gender as a binary predictor, then create a scatterplot with prediction line

onebinary <-lm(Order_Quantity ~ Gender , data=regression.demo)
summary(onebinary)
visreg(onebinary,"Gender")


#ONE CONTINUOUS PREDICTOR

#Income as a continuous predictor, then create a scatterplot with prediction line

onecontinuous <-lm(Order_Quantity ~ Income, data=regression.demo)
summary(onecontinuous)
visreg(onecontinuous,"Income")



#A CATEGORICAL PREDICTOR WITH MULTIPLE LEVELS
#Income as a categorical predictor, then create a scatterplot with prediction line
regression.demo$income.cat<-as.factor(regression.demo$Income)
onecategorical <-lm(Order_Quantity ~ income.cat, data=regression.demo)
summary(onecategorical)
visreg(onecategorical,"income.cat")


#MULTIPLE PREDICTORS

#Order Quantity as a function of Income and Gender, then create a scatterplot with prediction line
income.gender <-lm(Order_Quantity ~ Income + Gender, data=regression.demo)
summary(income.gender)
visreg(income.gender,"Income", by="Gender", overlay="TRUE")



#DUMMY VARIABLE APPROACH TO MISSING DATA

regression.demo.missing <- read.csv("Regression Demo missing.csv")

#First use casewise deletion. R will automatically ignore NA values

casewise.deletion <-lm(Order_Quantity ~ Income, data=regression.demo.missing)
summary(casewise.deletion)
visreg(casewise.deletion,"Income")


#Next use zero imputation (substitute zero for missing values)
zero.impute <-lm(Order_Quantity ~ Income0, data=regression.demo.missing)
summary(zero.impute)
visreg(zero.impute,"Income0")


#Now use the dummy variable approach
dummy <-lm(Order_Quantity ~ Income0 + Dummy_Missing, data=regression.demo.missing)
summary(dummy)



#NON-LINEAR TRANSFORMATIONS


#Order Quantity as a function of log transformed Income, then create a scatterplot with prediction line

income.log <-lm(Order_Quantity ~ log(Income) , data=regression.demo)
summary(income.log)
visreg(income.log,"Income")

#Order Quantity as a function of linear and quadratic effects of income, then create a scatterplot with prediction line

income.quad <-lm(Order_Quantity ~ Income + I(Income^2) , data=regression.demo)
summary(income.quad)
visreg(income.quad,"Income")



#INTERACTION MODEL
#Order Quantity as a function of Income and Gender and the interaction between income and gender, then create a scatterplot with prediction line
income.gender.int <-lm(Order_Quantity ~ Income*Gender, data=regression.demo)
summary(income.gender.int)
visreg(income.gender.int,"Income", by="Gender", overlay="TRUE")





