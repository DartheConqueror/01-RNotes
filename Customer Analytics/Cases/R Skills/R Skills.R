#Case Assingment: R Skills
#Dareck Giuliani

#Part 1: Basic

#A
seq(from = 6, to = 72, by = 11)
seq(from = 6, by = 11, length.out = 7)
seq(from = 6, to = 72, length.out = 7)
seq(to = 72, by = 11, length.out = 7)

#B 
append(rep(seq(3, 7, 2), 2), 3) #consider lengthout funnction for rep rather than appeand


#C
a <- seq(from = 52, to = 284, by = 2)
mean(a)

#D
b <- seq(1,100,1)
c <-replace(b, rep(seq(from = 7, to = 100, by = 7)), 13) #replaces every 7th value with 13

c <- replace(c, 50, 0) 
c<- replace(c, 60, 0)

mean(c)
median(c)

#E
myvector <- sample(b, size =100, replace = TRUE)
greaterthan50 <- myvector[myvector > 50]

mean(myvector)
mean(greaterthan50)

#Part 2: Logicals and Loops

#A
#If the variable, myinput, is greater than 5 then reassign myinput with a value of 10. If it is greater than 0 then reassing myinput with the value of 5. ANd for any other case, give myinput a value of 0.

#B

myvecotr <- sample(100, 100) #creates a a vector of rendom variables fromm 1 to 100

min <- myvecotr[1] #this creates a variable of the first element in the vector

for ( i in myvecotr[-1] + 1) {
  if (myvecotr[i] < min) #if the element is less than the min then insert it into the min variable
    min <- myvecotr[i] #Although theere is an error message, I recieve the right answer
}
min # print min variable
print(min(myvecotr))

#C

NumGrades <- sample(100, 100)

for (i in NumGrades) {
  if (NumGrades[i] >= 90) {
  NumGrades[i] <- "A"
  } else if (NumGrades[i] >= 80){
    NumGrades[i] <- "B"
  } else if (NumGrades[i] >= 70){
    NumGrades[i] <- "C"
  } else if (NumGrades[i] >= 60){
    NumGrades[i] <- "D"
  } else{
    NumGrades[i] <- "F"
  }
}
NumGrades

#Part 3:Data Frames

library(ggplot2)
data("diamonds")

#A
diamonds[20:30,]

#B
diamonds$depth
diamonds[,"depth"]
diamonds[, c("depth")]

#C
subset(diamonds, price > 18800)

#D
mean(subset(diamonds[diamonds$carat < 1,])$price)

#E: Bonus Problem
diamonds$cut
?sample
?data.frame

#Takes a subset of each cut and samples out 100 values
cut.Ideal <- sample(subset(diamonds[diamonds$cut == "Ideal", ])$price, 100, replace = FALSE)
cut.Fair <- sample(subset(diamonds[diamonds$cut == "Fair", ])$price, 100, replace = FALSE)
cut.Good <- sample(subset(diamonds[diamonds$cut == "Good", ])$price, 100, replace = FALSE)
cut.VeryGood <- sample(subset(diamonds[diamonds$cut == "Very Good", ])$price, 100, replace = FALSE)
cut.Premium <- sample(subset(diamonds[diamonds$cut == "Premium", ])$price, 100, replace = FALSE)

#places each vector of each cut into a dataframe
cut.data <- data.frame(cut.Ideal, cut.Fair, cut.Good, cut.VeryGood, cut.Premium, stringsAsFactors = FALSE) 

#Sumamry Statistics for Each Cut
mean(cut.data$cut.Ideal)
median(cut.data$cut.Ideal)
sd(cut.data$cut.Ideal)

mean(cut.data$cut.Fair)
median(cut.data$cut.Fair)
sd(cut.data$cut.Fair)

mean(cut.data$cut.Good)
median(cut.data$cut.Good)
sd(cut.data$cut.Good)

mean(cut.data$cut.VeryGood)
median(cut.data$cut.VeryGood)
sd(cut.data$cut.VeryGood)

mean(cut.data$cut.Premium)
median(cut.data$cut.Premium)
sd(cut.data$cut.Premium)

#Histogram of Prices at Each Cut
ggplot(diamonds, aes(x = cut, y = price)) +
    geom_boxplot()

    