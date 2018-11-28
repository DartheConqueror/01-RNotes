# import data set
pilgrimData <- read.csv("Pilgrim Bank Case.csv")
View(pilgrimData)

# install visreg library
#install.packages("visreg")
library(visreg)

# Part 1
# 3527276
pilgrimDataSort <- pilgrimData[rev(order(pilgrimData$Profit99)),]


pilgrimDataSort$percentageProfit <-round(cumsum((pilgrimDataSort$Profit99/3527276.00*100)), 3)

View(pilgrimDataSort)

plot(x=cumCust, y=pilgrimDataSort$percentageProfit, type="b",
     main = "Profitability Curve",
     ylab = "Cumulative Profit",
     xlab = "Cumulative Number of Customers")

N <- 31634
pilgrimDataSort$cumCust <-c(1:N)/N

#row number
which(pilgrimDataSort$percentageProfit == 100)

# unprofitable customers
pilgrimDataSort$unprofit <- replace(pilgrimDataSort$Profit99, pilgrimDataSort$Profit99 < 0, 0)
unprofitCount <- sum(pilgrimDataSort$unprofit == "0")
unprofitCount/31634*100

profit <- sum(pilgrimDataSort$unprofit)
profit/3527276*100


View(pilgrimDataSort)
# t-test on offline/online
t.test(pilgrimData$Profit99~ pilgrimData$Online99, var.equal=TRUE)

# intercept only model
interceptOnly <-lm(Profit99 ~ 0 , data=pilgrimData)
summary(interceptOnly)

# one binary predictor (online/offline)


oneBinary <-lm(Profit99 ~ Online99 , data=pilgrimData)
summary(oneBinary)
visreg(oneBinary,"Online99")

?lm
  
# one continuous predictor 


oneCont <-lm(Profit99 ~ Age99, data=pilgrimData)
summary(oneCont)
visreg(oneCont,"Age99")


# categorical predictor
pilgrimData$ageCat<-as.factor(pilgrimData$Age99)

# intercept and age as categorical predictor
oneCat <-lm(Profit99 ~ ageCat, data=pilgrimData)
summary(oneCat)
visreg(oneCat,"ageCat")

#online/offline as predictor and age as a categorical predictor
onlineOffline <-lm(Profit99 ~ Online99 + Age99, data=pilgrimData)
summary(onlineOffline)
visreg(onlineOffline,"Age99", by="Online99", overlay="TRUE")

# creating online dataframe
OnlineSet <-pilgrimData[pilgrimData$Online99==1,]
OnlineSet <- na.omit(OnlineSet)

# column averages for online customers
colMeans(OnlineSet[sapply(OnlineSet, is.numeric)])

# CREATING OFFLINE DATAFRAME
OfflineSet <- pilgrimData[pilgrimData$Online99==0,]
OfflineSet <- na.omit(OfflineSet)

# column averages for offline customers
colMeans(OfflineSet[sapply(OfflineSet, is.numeric)])


# ESTIMADE A MODEL WITH AN INTERCEOT (ON/OFFLINE AS A PREDICOTR AND AGE AS A CAT. PREDICTOR
modelFive <- lm(Profit99 ~ Online99 + ageCat, data=pilgrimData)
summary(modelFive)
visreg(modelFive)

# model 6, same as model 5 but with age as a continuous predictor

modelSix <- lm(Profit99 ~ Online99 + Age99, data=pilgrimData)
visreg(modelSix)

# Model 7, off/online as a predictor and all demographic controls as predictors
# age, income, tenure as continuious rather than categorical
allDemographics <- lm(Profit99 ~ Online99 + Age99 + Tenure99 + Inc99 + District99, data=pilgrimData)
summary(allDemographics)
visreg(allDemographics, "Online99")




# Part 6

# Model 9
# on/offline as a preditor, income as continuous and the interaction between 
# on/offline  and income - ignore customers with missing data

IncomeInteraction <-lm(Profit99 ~ Online99*Age99, data=pilgrimData)
summary(IncomeInteraction)
visreg(IncomeInteraction,"Age99", by="Online99", overlay="TRUE")
