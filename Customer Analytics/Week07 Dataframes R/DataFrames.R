#Data Frames

install.packages("ggplot2")
library(ggplot2)

?diamonds

head(diamonds) #first few rows

str(diamonds) #structure of data frame

summary(diamonds)

class(diamonds)
typeof(diamonds)

nrow(diamonds)
ncol(diamonds)
dim(diamonds) #rows then col

#col by name
head( diamonds$cut)
head( diamonds[ , 2])
head( diamonds[, "cut"])

head( diamonds[,c(2,3)])
head( diamonds[, c("cut", "color")])

#by row
diamonds[10,]
diamonds[25:30,]

#summary statistics
mean( diamonds$price) #a way to coerce as.numeric(c(diamonds[,"price]))
median( diamonds$price)
sd( diamonds$price)

#random sample of prces
diamonds$price[ sample (1:nrow(diamonds), size = 10, replace = FALSE)]

#Average Price of Ideal Cut Diamonds
mean( diamonds[diamonds$cut == "Ideal",]$price)

mean( subset(diamonds, cut == "Ideal")$price)

#Creates A Frequency Distribution
hist( diamonds$price)

#Create a New Variable
diamonds$boxSize <- diamonds$x * diamonds$y *diamonds$z
head(diamonds)

#Scatterplot
plot( x = diamonds$carat, y = diamonds$price)

table(diamonds$color, diamonds$cut)

aggregate(price ~ cut, FUN = mean, data = diamonds)
