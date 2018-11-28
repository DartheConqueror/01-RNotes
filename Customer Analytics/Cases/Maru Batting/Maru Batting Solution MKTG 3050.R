#Fernbach and De Langhe
#University of Colorado Leeds School of Business
#Customer Analytics
#Updated January 2016

# Maru Part 1 case solution

#SET WORKING DIRECTORY

setwd("C:/Users/The Law/Downloads/Academic/Semester 2/Customer Analytics/Cases/Maru Batting")



#READ DATA

maru.data<-read.csv("maru data students final.csv")

#CALCULATE ANNUAL MARGIN

maru.data$total.cost.per.hr<-maru.data$instructor.labor.cost.per.hr* maru.data$instructors.needed + maru.data$worker.labor.cost.per.hr* maru.data$workers.needed
maru.data$margin.hr<-maru.data$price.per.hr-maru.data$total.cost.per.hr
maru.data$annual.margin<-maru.data$margin.hr*maru.data$annual.hours

# PROBLEM 1: CALCULATE ACQUISITION COST

maru.data$acquisition.cost<-maru.data$contact.cost/maru.data$response.rate

# PROBLEM 2: BREAKEVEN WITHOUT DISCOUNTING

## SOLUTION USING WHILE LOOP

cum.gainloss<--maru.data$acquisition.cost
breakeven.timeperiod<-rep(0,5)

  i<-1
q<-0
  while(q==0) {
 margin.timeperiod<- maru.data$annual.margin*maru.data$retention.rate^(i-1)

cum.gainloss<-cum.gainloss+margin.timeperiod

breakeven.timeperiod[breakeven.timeperiod==0]<-as.numeric(cum.gainloss>0)*i
q<-min(as.numeric(breakeven.timeperiod!=0))
i=i+1
}

breakeven.timeperiod

## ALTERNATIVE SOLUTION

cum.margin <- matrix(,nrow=5,ncol=6)
break.even<-c()
for (i in 1 :5){
  for (j in 2:6) {
    cum.margin[i,1] <- -maru.data$acquisition.cost[i] + maru.data$annual.margin[i]
    cum.margin[i,j] <- cum.margin[i,j-1] + maru.data$annual.margin[i] * maru.data$retention.rate[i]^(j-1)
  }
}

for (i in 1:5){
  break.even[i]<-which.max(cum.margin[i,]>=0)  
  }

break.even

# PROBLEM 3: COMPUTE CLV (ASSUMING INFINITE TIME HORIZON)
#CLV = (M * ( (1+i) / (1 + i - RR))) - AC Using formula #1

#Create a new column with CLV assuming numbers from case
maru.data$clv <- (maru.data$annual.margin* ((1+maru.data$interest.rate) / (1 + maru.data$interest.rate - maru.data$retention.rate))) - maru.data$acquisition.cost


# PROBLEM 5: CHIYODA WARD
clv.littleleaguers.now<-maru.data$clv[which(maru.data$X=="little leaguers")]
clv.littleleaguers.chiyoda<-5000*(1+.1)/(1+.1-.65)-(600/.08)

# PROBLEM 6: ELITE BALLPLAYERS DISCOUNT

clv.eliteballplayers.now<-maru.data$clv[which(maru.data$X=="elite ballplayers (party)")]
clv.eliteballplayers.discount<-((7000-6000)*20)*(1+.1)/(1+.1-.75)-(50000)+500*20


# PROBLEM 7: ELITE BALLPLAYERS BAT

clv.eliteballplayers.bat<-((7500-6000)*20)*(1+.1)/(1+.1-.6)-(12500/.29+10000)

# SENSITIVITY ANALYSIS

#creates scenario values
ac <- seq(from = 40000, to = 60000, by = 5000)
am <- seq(from = 20000, to = 40000, by = 5000)
rr <- seq(from = 0.30, to = 0.90, by = 0.12)

#Matrix of all pairwise comparisons

values <- expand.grid(ac=ac,am=am,rr=rr)

#adds constant interest rate column
values$ir <- 0.10

#computes CLV for all scenarios
values$clv <- values$am * ( (1+values$ir) / (1 + values$ir - values$rr)) - values$ac

#some descriptives
negativeclvpercent = sum(values$clv < 0)/ length(values$clv)
worsethanlittleleaguerspercent=sum(values$clv < maru.data$clv[1])/length(values$clv)

#visualization using scatter plot
scatter.smooth(x=values$ac, y=values$clv)
scatter.smooth(x=values$am, y=values$clv)
scatter.smooth(x=values$rr, y=values$clv)
