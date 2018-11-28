#Basic Math
(4+6)/3

#Create a Sequence
seq(from =1, to = 202022, by = 2)

?seq

seq( from = 0, by = 2, length.out = 100 )
seq( from = 0, to = 10, length.out = 100 )
seq(0, 10, 2)
seq( 3, 75, length.out=10)
rep(4, times=19)
rep(seq(0, 30, 3), 2)

#Create a vector
avector <- 1:5
sample(avector, size = 5, replace = FALSE)
?sample()

vari <- seq(from = -4, to = -16,  by = -3)
sample(vari, size=4, replace=FALSE)
sample(avector, size=10, replace=TRUE)

#a <- 1
ls()

#Logicals
aLogical <- TRUE #logical recongized as 1 if true
notLogical.1 <- as.character(aLogical) #turns logical into a string
typeof(notLogical.1) #command telling us type of variable

notlogical.2 <- as.numeric(aLogical) #true = 1
typeof(notlogical.2)

TRUE + 1
c(TRUE, "Phil Fernbach") #will convert both to characters, c used to create vectors
1 == 1
2>1
1 !=1
(1 > 0) == 1
(1 == 1) & (2 == 1) #&- and function is it true that both of these statements are true
(1 == 1) | (2== 1) #| or
((1 == 1) & (2 == 1) | (1 == 1))

#Accessing Elements within a Vector
one2five <- 1:5
one2five == 4
one2five == c(1,4)
rep(c(1,4), length.out =5)
one2four <- c("one", "two", "three", "four")
one2four[1] #[] for index
one2four[1:2]
append(one2four, "five")
one2four[3] <- 3 #vectors can have only one data type
one2four[ one2four == "3"] <- "three"
one2four[ one2four == "three" | "four"] <- "3"
one2four == "three" | "four"
(one2four == "three" | one2four == "four")
one2four[ (one2four == "three" | one2four == "four")] <- "3"
one2four <- c("one", "two", "three", "four")
?which
which(one2four == "three")
one2four[ which(one2four == "three")] <- "3"

sevenvec <- seq(from = 0, by = 7, length.out = 20)
reverseseven <- rev(sevenvec)

mean(everyother)      
                