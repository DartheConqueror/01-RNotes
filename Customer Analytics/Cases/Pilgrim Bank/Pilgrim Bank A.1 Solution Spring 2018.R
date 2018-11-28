#Fernbach
#University of Colorado Leeds School of Business
#MKTG 3050: Customer Analytics
#Updated April 2018

rm(list=ls())

#SET WORKING DIRECTORY



setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Cases/Pilgrim Bank")

#IMPORT DATASET
pilgrim <- read.csv("pilgrim A.1 data.csv")

#PART 1: CREATE PROFITABILITY SKEW (= WHALE PLOT)

pilgrim.ordered <- pilgrim[order(-pilgrim$Profit99),] #sort customers in increasing order of Profit99
pilgrim.ordered$CumProfit99 <- cumsum((pilgrim.ordered$Profit99)/sum(pilgrim.ordered$Profit99)) #create column with cumulative percent of total Profit99
pilgrim.ordered$customernumber <- seq(1:dim(pilgrim.ordered)[1])
pilgrim.ordered$cumpercentofcustomers <-pilgrim.ordered$customernumber/max(pilgrim.ordered$customernumber) #cumulative percent of customers

#whale plot
plot(x=pilgrim.ordered$cumpercentofcustomers, y=pilgrim.ordered$CumProfit99, type="l",
     xlab="Percent of Customers",
     ylab="Percent of Profit")
abline(h=1, col="blue")

#determine percentage of best customers needed to have same total profit
hundredprofit.customernumber<-min(pilgrim.ordered[pilgrim.ordered$CumProfit99>1,9])-1 #find customer number of first row where cum profit goes above 1 and subtract one
hundredprofit.percent <- pilgrim.ordered[hundredprofit.customernumber,10] #percent of customers responsible for 100% of profit

#percentage of best customers needed to maximize profit
maxprofit.percent<-pilgrim.ordered[match(max(pilgrim.ordered$CumProfit99), pilgrim.ordered$CumProfit99),10]


#PART 2: INITIAL ANALYSIS

t.test(pilgrim$Profit99 ~ pilgrim$Online99, var.equal=TRUE)


#PART 3: SIMPLE REGRESSION

#MODEL 1: INTERCEPT-ONLY MODEL
interceptOnly <-lm(Profit99 ~ 1 , data=pilgrim) #Estimate intercept-only regression model
summary(interceptOnly) #Analyze regression output
confint(interceptOnly, level=0.95) #Determine 95% confidence interval for parameters of regression model

#MODEL 2:ANALYZE EFFECT OF ONLINE VS. OFFLINE NOT CONTROLLING FOR CUSTOMER BACKGROUND CHARACTERISTICS
noBackground <-lm(Profit99 ~ Online99 , data=pilgrim)
summary(noBackground) #No significant effect of online vs. offline?

#MODEL 3:ANALYZE EFFECT OF CONTINUOUS AGE
age.cont <-lm(Profit99 ~ Age99 , data=pilgrim)
summary(age.cont) #No significant effect of online vs. offline?


#MODEL 4:ANALYZE EFFECT OF CATEGORICAL AGE
age.cat <-lm(Profit99 ~ as.factor(Age99) , data=pilgrim)
summary(age.cat) #No significant effect of online vs. offline?



#PART 4: MULTIPLE REGRESSION


#MODEL 5: ONLINE + CATEGORICAL AGE
online.agecat <-lm(Profit99 ~ Online99 + as.factor(Age99) , data=pilgrim)
summary(online.agecat)


#MODEL 6: ONLINE + CONTINUOUS AGE
online.agecont <-lm(Profit99 ~ Online99 + Age99 , data=pilgrim)
summary(online.agecont)


#MODEL 7: All DEMOGRAPHIC CONTROLS

allcontrols <-lm(Profit99 ~ Online99 + Age99 + Inc99 + Tenure99 + as.factor(District99) , data=pilgrim)
summary(allcontrols)


#PART 5: MISSING DATA

#MODEL 8: DUMMY VARIABLE APPROACH

#approach 1: dummy variable

pilgrim$Age99.dmiss <- ifelse(is.na(pilgrim$Age99), 0, pilgrim$Age99) #substitute missing age values with zeros
pilgrim$Inc99.dmiss <- ifelse(is.na(pilgrim$Inc99), 0, pilgrim$Inc99) #substitute missing income values with zeros
pilgrim$dummyAgeMiss <- ifelse(pilgrim$Age99.dmiss==0, 1,0) #dummy variable is 0 if age is observed, and 1 if missing
pilgrim$dummyIncMiss <- ifelse(pilgrim$Inc99.dmiss==0, 1,0) #dummy variable is 0 if income is observed, and 1 if missing

#are customers with missing values different from customers without missing values?
t.test(pilgrim$Profit99 ~ pilgrim$dummyAgeMiss)
t.test(pilgrim$Profit99 ~ pilgrim$dummyIncMiss)

t.test(pilgrim$Online99 ~ pilgrim$dummyAgeMiss)
t.test(pilgrim$Online99 ~ pilgrim$dummyIncMiss)

t.test(pilgrim$Tenure99 ~ pilgrim$dummyAgeMiss)
t.test(pilgrim$Tenure99 ~ pilgrim$dummyIncMiss)

#Modeling profit using all customers
dummyModel <-lm(Profit99 ~ Online99 + Age99.dmiss + dummyAgeMiss + Inc99.dmiss + dummyIncMiss 
                + Tenure99 + as.factor(District99), data=pilgrim)

summary(dummyModel)


#PART 6: INTERACTION MODEL

#install.packages("visreg")
library(visreg)

#MODEL 9: INTERACTION BETWEEN ONLINE AND CONTINUOUS AGE
online.income.int <-lm(Profit99 ~ Online99*Inc99, data=pilgrim)
summary(online.income.int)
visreg(online.income.int,"Inc99", by="Online99", overlay="TRUE")




#NONLINEAR TRANSFORMS (NOT IN ASSIGNMENT)


#age and income as continuous variables with quadratic effects

backgroundNoMissingContwithquad <-lm(Profit99 ~ Online99 + Tenure99 + Age99 + Inc99 + as.factor(District99) + I(Age99^2) + I(Inc99^2) , data=pilgrim)
summary(backgroundNoMissingContwithquad)


#age and income as continuous variables with log effects

backgroundNoMissingContwithlog <-lm(Profit99 ~ Online99 + Tenure99 + Age99 + Inc99 + as.factor(District99) + I(log(Age99)) + I(log(Inc99)) , data=pilgrim)
summary(backgroundNoMissingContwithlog)


