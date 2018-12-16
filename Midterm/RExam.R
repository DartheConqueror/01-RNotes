#Dareck Giuliani
#Exam

#14

rep(seq(from = 4, by = 8, length.out = 3), length.out = 8)

#15

a <- sample(seq(from = 1, to = 10, by = 1), 50, replace = TRUE)

a[a > 7] <- 99

print(a)

#16

sd(sample(seq(from = 0, to = 10, by = 1), 1000, replace = TRUE))

#17

b <- 4

if (b > 5) {
  print("It's Larger Than 5")
  } else if (b < 5){
    print("It's Smaller Than 5")
  } else {
    (b <- 10)
}

#18

height <- sample(seq(from = 54, to = 78, by = 1), 25, replace = TRUE) #people can be the same height
catheight <- rep(NA, length.out=length(numGrades)) #a vector for categories of height: short, tall over average


for (i in 1:length(height)) {
 if (height[i] > 72) {
    catheight[i] <- "Tall"
  } else if (height[i] < 60) {
    catheight[i] <- "Short"
  } else {
    catheight[i] <- "Average"
  }
}


#Loading Diamonds

library(ggplot2)
diamonds <- as.data.frame(diamonds)

#19

df <- subset(diamonds, cut == "Ideal") #dataframe of only ideal cut diamonds

#20

nrow(df) #calculates number of diamonds in dataframe with only ideal cut diamonds
nrow(diamonds) #calculates number of diamonds in dataframe with all diamonds

#21

prem <- subset(diamonds, cut == "Premium") #subsets only oremmium diamonds
premE <- subset(prem, color == "E") #subsets premium diamonds with color E

median(premE$price) #calculates median price of premium E diamonds

#22

caratvec <- subset(diamonds, carat > 2)
caratvec <- subset(caratvec, select = -c(1:6, 8:10))

caratvec$price <- as.numeric(as.character(caratvec$price))

hist(caratvec$price)

#23

setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Week08 Midterm")

seg <- read.csv("TenSegments.csv")

#24

seg$Discount <- .1
seg$CLV <- ((seg$Margin*(1 + seg$Discount))/( 1 + seg$Discount - seg$RetentionRate)) - seg$AcquisitionCost #calc CLV

#25

negCLV <- rep(NA, length.out = 4) #blank vector

#places negative values in negCLV

for (i in 1:length(seg$CLV)) { 
  if (seg$CLV[i] < 0) {
    negCLV[i] <- seg$CLV[i]
  } else {
  }
}
print(negCLV)

#Bonus Point

mean(head(seg[order(seg$CLV),], n = 3)[,2])



