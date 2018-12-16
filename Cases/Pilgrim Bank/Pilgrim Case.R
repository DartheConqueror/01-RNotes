#Dareck Giuliani
#Pilgrim Bank

rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\014")

setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Cases/Pilgrim Bank")

pilgrim <- read.csv("pilgrim A.1 data.csv")

library(scales)
library(ggplot2)
library(visreg)
library(plyr)

#Part 1: Heterogenity in Profitbility----

#Recreate Whale Curve
sortedpilgrim <- pilgrim[order(-pilgrim$Profit99), ] #sorts customers from highest profitbaility to lowers

sortedpilgrim$CumProfit <- cumsum(sortedpilgrim$Profit99) #cumulative profit of each customer



#calculates cumulitivae profit as pa percentage of total profit
sortedpilgrim$CumProfitPercent <- percent((sortedpilgrim$CumProfit/tail(sortedpilgrim$CumProfit, n = 1)))

#Caulcuate customers as a percentage of total customers
sortedpilgrim$PercentCust <- percent((cumsum(sortedpilgrim$ID)/tail(cumsum(sortedpilgrim$ID), n = 1)))

write.csv(sortedpilgrim, file = "SortedPilgrim.csv")

#Plot Percentage of Cum Customers against Percentage of Cum Profit
ggplot(sortedpilgrim, aes(x = PercentCust, y = CumProfitPercent)) +
  geom_point(size = .1) +
  geom_smooth(method = 'lm', se = F, lwd = .25, col = 'grey')

#Minimum Percentage of Customers Needed to Realize 100% of Current Profit

for (i in 1:length(sortedpilgrim$CumProfit)) { 
  if (sortedpilgrim$CumProfitPercent[i] == '100%') {
  print(sortedpilgrim$PercentCust[i])
  } else {
  }
}

#Percentage of Unproftiable Customers

percent((length(pilgrim[pilgrim$Profit99 <= 0, ]$Profit99)/length(pilgrim$Profit99)))

#Profit if No Unprofitable Customers

percent((sum(pilgrim[pilgrim$Profit99 > 0, ]$Profit99)/sum(pilgrim$Profit99)))


#Part 2: Effect of online.offline - introudcotry analysis----

#Average Profitability: Offline/OnLine

mean(pilgrim[pilgrim$Online99 == 1 ,]$Profit99)

mean(pilgrim[pilgrim$Online99 == 0, ]$Profit99)

#Standard Error
seon <- sd(pilgrim[pilgrim$Online99 == 1 ,]$Profit99)/(sqrt(length(pilgrim$Online99))) #Standard Error of Online
seon

seoff <- sd(pilgrim[pilgrim$Online99 == 0 ,]$Profit99)/(sqrt(length(pilgrim$Online99))) #Standard Error of Offline
seoff

#95% Confidence Interval

#online
cion <- c(mean(pilgrim[pilgrim$Online99 == 1 ,]$Profit99) - 2 * seon, mean((pilgrim[pilgrim$Online99 == 1 ,]$Profit99) + 2 * seon))

#offline
cioff <- c(mean(pilgrim[pilgrim$Online99 == 0 ,]$Profit99) - 2 * seoff, mean((pilgrim[pilgrim$Online99 == 0 ,]$Profit99) + 2 * seoff))

#Graph Depicting Averages with Error Bars

category <- c("Online", "Offline")

value.of.point <- c((mean(pilgrim[pilgrim$Online99 == 1 ,]$Profit99)), (mean(pilgrim[pilgrim$Online99 == 0, ]$Profit99)))

error.bar.top <- c((value.of.point[1] + seon), (value.of.point[-1] + seoff))

error.bar.length <- error.bar.top - value.of.point

df <- data.frame(category, 
                 value.of.point,
                 error.bar.top,
                 error.bar.length)

errbar(x = c(1,2),
       
y = df$value.of.point,
yplus = df$value.of.point +
  df$error.bar.length,
ymin = df$value.of.point - 
  df$error.bar.length,
xaxt = "n", 
xlim = c(0.75, 4.25),
xlab = "Online/Offline", 
ylab = "Profitability")

#T-Test
#t-test on online/offline profitability
t.test(pilgrim$Profit99 ~ pilgrim$Online99, var.equal=TRUE)

#PART 3: SIMPLE REGRESSION----

#A Model 1

summary(lm(pilgrim$Profit99 ~ 1))

#B Model 2

summary(lm(pilgrim$Profit99 ~ pilgrim$Online99))

#C Model 3

summary(lm(pilgrim$Profit99 ~ pilgrim$Age99))
unique(pilgrim$Age99)

#D Model 4
pilgrim$catAge <- factor(pilgrim$Age99)
summary(lm(pilgrim$Profit99 ~ pilgrim$catAge))
unique(pilgrim$catAge)

#Charts for Ages

ggplot(pilgrim, aes( x = Age99, y = Profit99)) +
  geom_point() +
  stat_smooth(method = "lm", col ="red")

ggplot(pilgrim, aes( x = catAge, y = Profit99)) +
  geom_point() +
  stat_smooth(method = "lm", col ="red")

visreg((lm(Profit99 ~ Age99, data = pilgrim)), "Age99")
visreg((lm(Profit99 ~ catAge, data = pilgrim)), "catAge")

#PART 4: MULTIPLE REGRESSION----


#A

#mean Age

mean(pilgrim[pilgrim$Online99 == 1, ]$Age99, na.rm = TRUE) #mean age category for online customers
mean(pilgrim[pilgrim$Online99 == 0, ]$Age99, na.rm = TRUE) #mean age category for offline customers

#mean income

mean(pilgrim[pilgrim$Online99 == 1, ]$Inc99, na.rm = TRUE) 
mean(pilgrim[pilgrim$Online99 == 0, ]$Inc99, na.rm = TRUE) 

#mean tenure

mean(pilgrim[pilgrim$Online99 == 1, ]$Tenure99, na.rm = TRUE)
mean(pilgrim[pilgrim$Online99 == 0, ]$Tenure99, na.rm = TRUE) 

#geographic distribution

unique(pilgrim$District99)

length(pilgrim[pilgrim$Online99 == 1 & pilgrim$District99 == 1200, ]$District99)/length(pilgrim[pilgrim$Online99 == 1, ]$District99)
length(pilgrim[pilgrim$Online99 == 1 & pilgrim$District99 == 1100, ]$District99)/length(pilgrim[pilgrim$Online99 == 1, ]$District99)
length(pilgrim[pilgrim$Online99 == 1 & pilgrim$District99 == 1300, ]$District99)/length(pilgrim[pilgrim$Online99 == 1, ]$District99)

length(pilgrim[pilgrim$Online99 == 0 & pilgrim$District99 == 1200, ]$District99)/length(pilgrim[pilgrim$Online99 == 0, ]$District99)
length(pilgrim[pilgrim$Online99 == 0 & pilgrim$District99 == 1100, ]$District99)/length(pilgrim[pilgrim$Online99 == 0, ]$District99)
length(pilgrim[pilgrim$Online99 == 0 & pilgrim$District99 == 1300, ]$District99)/length(pilgrim[pilgrim$Online99 == 0, ]$District99)

#B

#Model 5

summary(lm(pilgrim$Profit99 ~ pilgrim$Online99 + pilgrim$catAge))

visreg(lm(Profit99 ~ Online99 + catAge, data = pilgrim))

# Model 6

summary(lm(pilgrim$Profit99 ~ pilgrim$Online99 + pilgrim$Age99))
     
# Model 7

summary(lm(pilgrim$Profit99 ~ pilgrim$Online99 + pilgrim$Age99 + pilgrim$Inc99 + pilgrim$Tenure99 + pilgrim$District99))

# PART 5: MISSING DATA----

# Model 8

#Dummy Code Columns

sortedpilgrim$dummyAge <- ifelse(is.na(sortedpilgrim$Age99), 1, 0)
sortedpilgrim$dummyInc <- ifelse(is.na(sortedpilgrim$Inc99), 1, 0)

sortedpilgrim$Age99 <- ifelse(is.na(sortedpilgrim$Age99), 0, sortedpilgrim$Age99)
sortedpilgrim$Inc99 <- ifelse(is.na(sortedpilgrim$Inc99), 0, sortedpilgrim$Inc99)

#Actual Model

summary(lm(sortedpilgrim$Profit99 ~ sortedpilgrim$Online99 + sortedpilgrim$Age99 + sortedpilgrim$Inc99 + sortedpilgrim$Tenure99 + sortedpilgrim$District99 + sortedpilgrim$dummyAge + sortedpilgrim$dummyInc))

#PART 6: INTERACTION MODELING----

#Model 9

summary(lm(pilgrim$Profit99 ~ pilgrim$Online99 + pilgrim$Inc99 + (pilgrim$Online99*pilgrim$Inc99)))
summary(lm(pilgrim$Profit99 ~ (pilgrim$Online99*pilgrim$Inc99)))

#Model 9 with Online as Categorical
sortedpilgrim$catOn <- factor(sortedpilgrim$Online99)

#Visuals

fit <- (lm(Profit99 ~ Online99 + Inc99 + (Online99*Inc99), data = pilgrim))
visreg(fit, "Online99", "Inc99", gg = TRUE, ylab= "Profit99", overlay = TRUE)
visreg(fit, "Online99", "Inc99", gg =TRUE, ylab= "Profit99", overlay = FALSE)

m9 <- (lm(Profit99 ~ catOn + Inc99 + (catOn * Inc99), data = sortedpilgrim))
visreg(m9, "catOn", "Inc99", gg= TRUE, ylab = "Profit99", overlay = TRUE)
visreg(m9, "catOn", "Inc99", ylab = "Profit99", overlay = FALSE)

unique(pilgrim$Inc99)
unique(sortedpilgrim$Inc99)

IncomeInteraction <-lm(Profit99 ~ Online99*Age99, data=pilgrim)
summary(IncomeInteraction)
visreg(IncomeInteraction,"Age99", by="Online99", overlay="TRUE")
