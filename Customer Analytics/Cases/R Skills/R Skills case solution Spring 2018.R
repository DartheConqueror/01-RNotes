#Fernbach
#University of Colorado Leeds School of Business
#MKTG 3050: Customer Analytics
#Updated Feb 2018

#set working directory
setwd("/Users/phfe9778/Dropbox/MKTG 3050/Spring 2018/Content/Week 5-7 Coding/R Skills Case") 


#Part 1

#a) Show four different ways (i.e., using different arguments) to create the following vector using the seq() function?

#[1]  6 17 28 39 50 61 72 
# you can use different combinations of
#  from, to, by, and length.out

seq( from=6, by=11, length.out=7 )
seq( from=6, to=72, length.out=7 )
seq( from=6, to=72, by=11 )
seq( to=72, by=11, length.out=7 )


#b) Create the following vector without using the c() function.

#[1] 3 5 7 3 5 7 3

# create vector by combining seq() and rep()
#  seq() produces [3,5,7]
#  rep() repeats [3,5,7] up to length.out=7

rep( seq( from=3, to=7, by=2 ), length.out=7 )

#c) Use R to find the mean value of all even numbers between 52 and 284 (inclusive of 52 and 248).

# seq() creates all even numbers between 52 and 283
# mean() finds the mean value of sequence

mean( seq( from=52, to=284, by=2 ) )

# you could do this in two steps:

evenNumbers <- seq( from=52, to=284, by=2 )
mean(evenNumbers)

mean(seq( from=52, to=284, by=2 ))

#d) Create a vector from 1 to 100. Replace every 7th element (i.e., 7th, 14th, etc.) in this vector with 13. Replace the 50th and 60th elements in the vector with 0. What are the mean and median of this new vector?

# create a vector (aVector) from 1-100
aVector <- 1:100

# replace every 7th element with 13
aVector[ seq(from=7, by=7, to=length(aVector)) ] <- 13

# replace 50th and 60th element with 0
aVector[ c(50, 60) ] <- 0

# find summary statistics
mean(aVector)
median(aVector)
summary(aVector)

#e) Create a vector (called “myVector”) that contains 100 random values between 0 and 100. 
#Next create a new vector (called “greaterThan50”) that contains all of the elements in “myVector” that are bigger than 50 (and none of the elements that are less than or equal to 50). What is the mean of "myvector" and "greaterThan50"? (Hint: to create myVector use wither the runif() function or the sample() function.)

# the key insight here is that a logical vector
#  can serve as a index

# create a random vector 
#  (this isn’t necessary but let’s you test your code)
myVector <- runif(n=100, min=0, max=100)

# here’s the solution
greaterThan50 <- myVector[ myVector>50 ]

#Part 2: Logicals and Looping

#a) Tell me, in words, what the following if logic does. In other words, what will this code return for different (numerical) values of “myInput”?

myInput<--100

if (myInput > 5) {
  myInput <- 10
} else if (myInput > 0) {
  myInput <- 5
} else {
  myInput <- 0
}




#b) Create a vector (called “myVector”) that contains 100 random values between 0 and 100. 
#Create a for loop that takesmyVector and finds its minimum value (i.e., the smallest element) using conditional logic (i.e., if statements) and comparison operators (i.e., >, <, ==, etc.). 
#(This means DO NOT use min(), max(), or similar functions to solve this, even inside a loop.)

# create a vector to test loop
myVector <- runif(n=100, min=0, max=100)
#or
myVector <- sample(0:100,size=100,replace = TRUE)


# for loop to find minimum
for (i in 1:length(myVector)) {
  if (i == 1) {
    minMyVector <- myVector[i]	
  } else if (myVector[i] < minMyVector) {
    minMyVector <- myVector[i]	
  }
}

# test it to see if it works
print( minMyVector )
print( min( myVector ) )



#c) Write a for loop that takes a vector of numerical grades between 0 and 100 (the loop should be able to handle vectors of any length) and returns a vector of corresponding letter grades (90+ = “A”, 80-90 = “B”, 70-80 = “C”, 60-70 = “D”, 0-60 = “F”). For example, if numGrades =  [85, 91, 67] the loop should return letterGrades = [“B”, “A”, “D”].

# create a vector to test loop


numGrades <- runif(n=100, min=50, max=100)

#or

numGrades <- sample(50:100, size=100, replace=TRUE)

# initialize letter grade vector
letterGrades <- rep(NA, length.out=length(numGrades))

# for loop to convert grades
for (i in 1:length(numGrades)) {
  if (numGrades[i] >= 90) {
    letterGrades[i] <- "A"
  } else if (numGrades[i] >= 80) {
    letterGrades[i] <- "B"
  } else if (numGrades[i] >= 70) {
    letterGrades[i] <- "C"
  } else if (numGrades[i] >= 60) {
    letterGrades[i] <- "D"
  } else {
    letterGrades[i] <- "F"
  }
}


#Part 3: Data Frames

library(ggplot2)
data("diamonds")

diamonds<-as.data.frame(diamonds)

summary(diamonds)


#a) How would you select only rows 20 through 30 (inclusive of 20 and 30)?
diamonds[20:30,]

#b) What are three different ways to select the depth column?
diamonds$depth
diamonds[,5]
diamonds[,"depth"]

#c) How would you select only the rows for which the price of the diamond was greater than 18,800?
diamonds[ diamonds$price > 18800, ]



#d) What is the average (mean) price for a diamond smaller than 1 carat?

mean( diamonds[ diamonds$carat < 1, "price" ] )

#e) Create a data frame that randomly draws 100 prices at each cut quality. This frame should have 500 data points.
#Please give the mean, median, and SD of price for each cut quality within this frame . 
# Please provide a boxplot showing  the price at each level of cut quality in this data frame. 

#I create a dataframe "Data" above not to mess with stuff , so here I am looking at it to make sure it's right
View(Data)

##One way to do it: I create a vector of "cut" for my loop
Cut<-as.factor(unique(Data$cut))
##creating an empty dataframe with the rows I need
subsetall <- Data[0,]
##filling the vector
for(i in 1:5){
  subsetcut<-subset(Data,Data$cut==Cut[i])
  subsetall<-rbind(subsetall,subsetcut[sample(nrow(subsetcut),100),])
}
##boxplot
boxplot( price ~ cut, data=subsetall )
#means, medians, and SD's
for(i in 1:5){
print(mean(subsetall$price[subsetall$cut==Cut[i]]))
}
for(i in 1:5){
  print(median(subsetall$price[subsetall$cut==Cut[i]]))
}
for(i in 1:5){
  print(sd(subsetall$price[subsetall$cut==Cut[i]]))
}

##boxplot
boxplot( price ~ cut, data=subsetall )






