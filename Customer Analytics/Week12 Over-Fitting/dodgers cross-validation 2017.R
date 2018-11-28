#Fernbach
#University of Colorado Leeds School of Business
#MKTG 3050: Customer Analytics
#Updated Feb 2017


rm(list=ls())
#install.packages("visreg")
#install.packages("cvTools")


setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Week12 Over-Fitting")

dodgers<-read.csv("dodgers.csv")

library(visreg)
library(cvTools)

plot(dodgers$temp,dodgers$attend)

#Linear model of temperature
linear.model <- lm(attend ~ temp, data=dodgers)
summary(linear.model)
visreg(linear.model,"temp")

#Quadratic model of temperature
quadratic.model <- lm(attend ~ temp + I(temp^2), data=dodgers)
summary(quadratic.model)
visreg(quadratic.model,"temp")

#Fifth-order model of temperature
fifthorder.model <- lm(attend ~ temp + I(temp^2)+ I(temp^3)+ I(temp^4) +I(temp^5), data=dodgers)
summary(fifthorder.model)
visreg(fifthorder.model,"temp")



#Use cvTools package to compare models

set.seed(1234)

cvFit(linear.model,y=dodgers$attend, data=dodgers, K=5, R=50)

cvFit(quadratic.model,y=dodgers$attend, data=dodgers, K=5, R=50)

cvFit(fifthorder.model,y=dodgers$attend, data=dodgers, K=5, R=50)

