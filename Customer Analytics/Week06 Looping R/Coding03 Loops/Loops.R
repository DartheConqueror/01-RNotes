#Puzzles

avector <- seq(from = 0, by = 7, length.out = 20) #vector of multiples of 7

rev(avector) #reverse vector

avector[ seq(from = 2, to = 20, by = 2)] <- rep(13, times = 10) #replace everyother number with 13

mean(avector) #mean of vector

sd(avector) #standard devition of vector

newVector <- seq(from = 1, to = 10) #vector from 1 to 10 | c(1:10), 1:10

newVector[newVector > 5] <-13 #replace every element greater than 5 with 13
#newVector[newVector > 5] <- rep(13, length(newVector[ newVector > 5]) no hardcoding of length, more applicable

rm(list=ls())

#Conditional Logic

# if (conditional statement) {Do this if conditional statement is TRUE}

dVariable <- 10

if (dVariable == 10) {
  print("It's a 10")
}

eVariable <- 9

if (eVariable == 10) {
  print("It's a 10")
} else {
  print("It's not a 10")
}

fVariable <- 9

if (fVariable == 10) {
  print("It's a 10")
} else  if (fVariable == 9) {
  print("It's a 9")
} else {
  print("It's neither a 9 nor a 10")
}
 
gVariable <- 3

if (gVariable > 5) {
  gVariable <- 13
} else if (gVariable < 5) {
  gVariable <- 0
} else {
  print("It's a 5")
}

#Looping Structures

# for ( counter in vector) {Do this iteration}

for (i in c("apple", "orange", "bannana")) {
  print(i)
}

for (i in 1:10) {
  print("Banana")
}

for (i in 1:10)[seq(1:10) > 5] { #Does not work, need to fix issue
  print("Banana")
}

for (i in seq(from = 0, to = 30, by = 5)) {
  print(i)
}

aVariable <- 1

for (i in aVariable) {
  print(aVariable * 2^10) 
}

iVector <- c("one", "two", "three")

for (i in 1:length(iVector)) {
  print(iVector[i])
}

jVector <- 1:20

for (i in 1:length(jVector)) {
  if (jVector[i] > 9) {
    jVector[i] = 99
  } else if (jVector[i] == 6) {
    jVector[i] = 66
  } else {
    jVector[i] = (jVector[i]/2)
  }
}
jVector

#Challenge Problem Rolling a Die in Vegas

dieVector <- 1:6 #the die
numberofrolls <- 1:100 #number of rolls
rollsix <- 0 #inititate counter

for (i in 1:numberofrolls) {
  draw <- sample(dieVector, size = 1, replace = TRUE)
  if (draw == 6) {
    rollsix <- rollsix + 1
  }
}
print (rollsix) #not right

sum( sample(dieVector, size =100, replace = TRUE) == 6)

