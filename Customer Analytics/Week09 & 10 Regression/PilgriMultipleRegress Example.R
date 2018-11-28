setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Cases/Pilgrim Bank")

pilgrim <- read.csv("pilgrim A.1 data.csv")

summary(lm(pilgrim$Online99 ~ pilgrim$Age99 + pilgrim$Inc99 + pilgrim$Tenure99))