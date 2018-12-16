# Dareck Giuliani
# Thesis Analysis

#Preliminary Stuff----
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\014")



# Set Workign Directory
setwd("C:/Users/The Law/Downloads/Academic/Thesis/Data")

# read data <- <- 
dfa <- read.csv("06 - PartOverProd Songs LeafLab v1_March 22, 2018_17.03.csv")

#Code Book 
codeBook <- t(dfa[1,])

# write out and read in to auto-class variables
dfa <- dfa[-c(1, 2),]  # remove unnecessary lines

# write out df
write.csv(dfa, "C:/Users/The Law/Downloads/Academic/Thesis/Data/06 - PartOverProd Songs LeafLab v1_March 22, 2018_07.54.AUTOCLASS.csv")

# read in df
dfa <- read.csv("C:/Users/The Law/Downloads/Academic/Thesis/Data/06 - PartOverProd Songs LeafLab v1_March 22, 2018_07.54.AUTOCLASS.csv")

# remove if not finished
dfa <- subset(dfa, dfa$Finished == 1)

library(ggplot2)
library(psych)
# install.packages("tidyverse")
library(tidyr) 
library(visreg)

# Cleaning Data----

# Trim Features 
#dfb <- subset(dfa, select = -c(1:6, 8:20, 23:32, 35:114, 117:136, 139:216)) # only columns necessary for preliminary analysis are here

# Drop Rows if Artist Profile is Incorrent

dfa <- subset(dfa[dfa$othStuProf == 1832 | dfa$othStuProf == 2461, ]) #  JUSTIN: are we dropping real participants here? If so, we should manually check if their entry was close to one of the intended profiles.

#DropRow if Incorrect Student Profile Number

dfa <- subset(dfa, dfa$othStuProf %in% c('1832', '2461')) # 1832 = dem, 2461 = rep

# Coding IV----

# IV 1: Political Ideology/Affiliation of Participant (average of poli affiliation and ideologyy scores)

dfa$party <- as.numeric(as.character(dfa$party)) # convert column to numeric so math
dfa$ideology <- as.numeric(as.character((dfa$party))) # convert column to numeric so math

dfa$IV1.Participant.Politics <- rowMeans(dfa[,22:23]) # finds mean between ideaology and affiliation


# IV 2: Political Ideology/Affiliation of Other Student
# If 1832 Then -.5 -> Liberal
# If 2461 Then .5 -> Conservative

dfa$targCon <- ifelse(dfa$othStuProf == 1832, -.5, .5) # add new column with a value of -.5 if Other Student Profile = 1832, else add .5

#correlation between Ideology and Affiliation
cor.test(dfa$party, dfa$ideology) #wanted to ensure the two measures were highly correlated



# Coding DV1: Own PLaylist----



# DV 1: How many Liberal Songs Were Included in Playslit for Themselves? (Count & Average)




# Convert the Values of Columns Depending on Artist Profile
ArtProfNum3729 <- c(.5, -.5, .5, .5, -.5, -.5, .5, -.5, .5, -.5) #c(3, -3, 3, 3, -3, -3, 3, -2, 2, -2)  # Makes a vector of the partisan values for Artist Profile 3729
ArtProfNum1391 <- c(-.5, .5, -.5 ,-.5 ,.5 ,.5 ,-.5 ,.5 ,-.5 ,.5) #c(-3, 3, -3 ,-3 ,3 ,3 ,-2 ,3 ,-2 ,2)  # Makes a vector of the partisan values for Artist Profile 1391

# vector of conservative songs for artistProfNum 1391
setOneConsSongs <- c(1, 3, 4, 6, 10)
# setOneLibSongs <- c(2, 5, 7, 8, 9)

# vector of conservative songs for artistProfNum 3729
setTwoConsSongs <- c(2, 5, 7, 8, 9)
# setTwoLibSongs <- c(1, 3, 4, 6, 10)

# own playlist avg

# create numeric variable
dfa$selfPlaylist <- as.character(dfa$selfPlaylist_0_GROUP)

# create a vector to fill
averagesTemp <- rep(NA, nrow(dfa))

for (i in 1:nrow(dfa)) {
  
  # if they saw this set...
  if(dfa$artistProfNum[i] == '1391') {
    
    # create a list of the songs in their playlist
    vecTemp <- unlist(strsplit(dfa$selfPlaylist[i], ','))
    
    # store average of how many of those are in the liberal songs thing
    avg <- sum(vecTemp %in% setOneConsSongs) / length(vecTemp)
    
    averagesTemp[i] <- avg
    
  }
  
  # if they saw this set...
  if(dfa$artistProfNum[i] == '3729') {
    
    # create a list of the songs in their playlist
    vecTemp <- unlist(strsplit(dfa$selfPlaylist[i], ','))
    
    # store average of how many of those are in the liberal songs thing
    avg <- sum(vecTemp %in% setTwoConsSongs) / length(vecTemp)
    
    averagesTemp[i] <- avg
    
  }
  
  
}

# append averages into dataframe
dfa$ownPlaylistAvgCons <- averagesTemp

# target student's playlist

# create numeric variable
dfa$targetPlaylist <- as.character(dfa$othPlaylist_0_GROUP)

# create a vector to fill
averagesTemp <- rep(NA, nrow(df))


for (i in 1:nrow(dfa)) {
  
  
  if(dfa$artistProfNum[i] == '1391') {
    
    # create a list of the songs in their playlist
    vecTemp <- unlist(strsplit(dfa$targetPlaylist[i], ','))
    
    # store average of how many of those are in the liberal songs thing
    avg <- sum(vecTemp %in% setOneConsSongs) / length(vecTemp)
    
    averagesTemp[i] <- avg
    
  }
  
  if(dfa$artistProfNum[i] == '3729') {
    
    # create a list of the songs in their playlist
    vecTemp <- unlist(strsplit(dfa$targetPlaylist[i], ','))
    
    # store average of how many of those are in the liberal songs thing
    avg <- sum(vecTemp %in% setTwoConsSongs) / length(vecTemp)
    
    averagesTemp[i] <- avg
    
  }
  
  
}

# append averages into dataframe
dfa$targetPlaylistAvgCons <- averagesTemp

#Create a Match Varible 
dfa$match <- ifelse(dfa$IV1.Participant.Politics > 0 & dfa$targCon > 0 | dfa$IV1.Participant.Politics < 0 & dfa$targCon < 0, .5, -.5)

# New Dataframe with Only DVs and Ivs----

dfc <- subset(dfa, select = c(218:225))# JUSTIN: this is helpful
dfa$ownPlaylistAvgConsCons <- as.numeric(as.character(dfa$ownPlaylistAvgConsCons))
dfa$targetPlaylistAvgCons <- as.numeric(as.character(dfa$targetPlaylistAvgCons))

#Binned Participants and Correlations and Regressions on Groups----

#Binning Participants by Own Partisanship and Other Partisbaship
# visualize the collapsing

# generate correlations; firstWord = self, secondWord = target

libLib <-  cor(dfa[dfa$IV1.Participant.Politics < 0  & dfa$targCon < 0, c('ownPlaylistAvgCons', 'targetPlaylistAvgCons')])[2]
libCons <- cor(dfa[dfa$IV1.Participant.Politics < 0  & dfa$targCon > 0, c('ownPlaylistAvgCons', 'targetPlaylistAvgCons')])[2]

indLib <-  cor(dfa[dfa$IV1.Participant.Politics == 0 & dfa$targCon < 0, c('ownPlaylistAvgCons', 'targetPlaylistAvgCons')])[2]
indCons <- cor(dfa[dfa$IV1.Participant.Politics == 0 & dfa$targCon > 0, c('ownPlaylistAvgCons', 'targetPlaylistAvgCons')])[2]

consLib  <- cor(dfa[dfa$IV1.Participant.Politics > 0 & dfa$targCon < 0, c('ownPlaylistAvgCons', 'targetPlaylistAvgCons')])[2]
consCons <- cor(dfa[dfa$IV1.Participant.Politics > 0 & dfa$targCon > 0, c('ownPlaylistAvgCons', 'targetPlaylistAvgCons')])[2]

# create a table of them
correlations <- matrix(
  
  data = c(libLib, indLib, consLib,
           libCons, indCons, consCons),
  nrow = 2, byrow = T,
  dimnames = list(
    
    c('Target Liberal', 'Target Conservative'),
    c('Self Liberal', 'Self Independent', 'Self Conservative')
    
  ))

# chart them
barplot(correlations, beside = T, legend.text = T, args.legend = list(x = "bottomright"))

#EXPLORATORY ANALYSIS----
  # Factoring Politics INto Choices
    dfpoli <- subset(dfa, select= c(206:211))

#UNDERSTANDING SAMPLE----
dfsample <- subset(dfa, select = c(22:23, 213:215))
dfsample <- dfsample[-c(1:2), ] #eliminated text


#Distribution of Political Ideology of Students
ggplot(dfsample, aes(ideology)) + geom_histogram(col = 'black', fill = 'purple', alpha = .75, stat = "count")

#Sample Size
length(dfa$party )

#Sex of Sample
sum(dfa$male == .5)/length(dfa$male) #percent of males males

#Identification of Samples
sum(dfa$IV1.Participant.Politics < 0)/length(dfa$IV1.Participant.Politics) #% of liberals
sum(dfa$IV1.Participant.Politics > 0)/length(dfa$IV1.Participant.Politics) #% of cons
sum(dfa$IV1.Participant.Politics == 0)/length(dfa$IV1.Participant.Politics)#% of independents

#Summary of age
summary(dfa$age)
sd(dfa$age)


  # MANIPULATION AND ATTENTION CHECK----

dfmanip <- subset(dfa, select = -c(1:32, 35:114, 116:136, 138:161,164:185,196:217)) # makes a dataframe with the manipuation check variables

dfmanip <- subset(dfmanip[dfmanip$artistProfNum == 1319 | dfmanip$artistProfNum == 3729, ]) # drops rows with incorrect artist profile numbers

# if in condition 1832 then target student is Liberal, is assinged a 6, meaning very liberal
dfmanip$OtherLiberal <- ifelse(dfmanip$othStuProf == '1832', 6, 1)


# if in condition 2461 then target student is Conservative is assinged a 6, meaning very conservative
dfmanip$OtherCon  <- ifelse(dfmanip$othStuProf == '2461', 6, 1)

dfmanip$othStuChars_othStuChars_liberal <- as.numeric(as.character(dfmanip$othStuChars_othStuChars_liberal))
dfmanip$othStuChars_othStuChars_conservative <- as.numeric(as.character(dfmanip$othStuChars_othStuChars_conservative))

# takes the difference beteen student's guess on how liberal a student is and how liberal other student is on sheet
# closer to 0 is better
dfmanip$Diff.Guess.Profile.Lib <- dfmanip$othStuChars_othStuChars_liberal - dfmanip$OtherLiberal 

# again, closer to 0 is better
dfmanip$Diff.Guess.Profile.Con <- dfmanip$othStuChars_othStuChars_conservative - dfmanip$OtherCon 

#Measure Did Student's Try to Maximize the Enjoyment the Other Student ill get out of the Plyalist

#Did Student Perceieve Manipulated Politics in Artists (Depends on Condition Exposed to)

# Diff.Guess.Profile.Con and Diff.Guess.Profile.Lib should be perfectly correlated
# 0 on each column reflect accuracy
# That is, when other student is very liberal, they are not very conservative

#Compare Participant's Perception of Artists to Presentation of Artists in terms of politics
  
  #Renames columns for readability
  colnames(dfmanip)[7:16] <- c("Corey Smith", "Parmalee", "O'Death", "Rhiannon", "Crooked", "Amasa", "Deva", "Donna", "ModdJazz", "Anciients")

  #depending on condition assigns partisanship t artist

#Graphs of Data----


#Summary

# Summarizes IV 1: Participant Politics
describe(dfc$IV1.Participant.Politics)
ggplot(dfc, aes(IV1.Participant.Politics)) + geom_histogram(col = 'black', fill = 'blue', alpha = .75, binwidth = 1) # histogram

# Summarizes DV: Own Playlist
dfa$ownPlaylistAvgCons <- as.numeric(as.character(dfa$ownPlaylistAvgCons))
describe(dfa$ownPlaylistAvgCons)
ggplot(dfc, aes(OwnPlaylistAvg)) + geom_histogram(col = 'black', fill = 'green', alpha = .75, binwidth = .15) # histogram


# Summarizes DV: Other Playlist
dfa$targetPlaylistAvgCons <- as.numeric(as.character(dfa$targetPlaylistAvgCons))
describe(dfa$targetPlaylistAvgCons)
ggplot(dfc, aes(OtherPlaylistAvg)) + geom_histogram(col = 'black', fill = 'pink', alpha = .75, binwidth = .15) # histogram

#Overlapping Histograms
ggplot(dfc, aes(f0)) +
    geom_bar(data = dfa$ownPlaylistAvgCons, fill = "red", alpha = 0.2) +
    geom_bar(data = OtherPlaylistAvg, fill = "blue", alpha = 0.2)

# JUSTIN'S CODE

# other predicting other

      # histograms of average playlist composition given other student politics
      ggplot(subset(dfc, dfc$IV2.Oth.Stu.Con == .5), aes(x = OtherPlaylistAvg)) + geom_histogram(fill = 'red') + ggtitle('Other Student Conservative')
      ggplot(subset(dfc, dfc$IV2.Oth.Stu.Con == -.5), aes(x = OtherPlaylistAvg)) + geom_histogram(fill = 'blue') + ggtitle('Other Student Liberal')
      
            # overlapping density chart
            ggplot(dfc,aes(x = OtherPlaylistAvg)) + ggtitle('Playlist Avg Broken Out by Oth Student Politics') +
                geom_density(data=subset(dfc, dfc$IV2.Oth.Stu.Con ==  .5),fill = "red", alpha = 0.25, adjust = .8) +
                geom_density(data=subset(dfc, dfc$IV2.Oth.Stu.Con == -.5),fill = "blue", alpha = 0.25, adjust = .8)

# self predicting other
            
      # histograms of average playlist composition given OWN politics
      ggplot(subset(dfc, dfc$IV1.Participant.Politics > 0), aes(x = OtherPlaylistAvg)) + geom_histogram(fill = 'red') + ggtitle('Self Conservative; dichotomized for display purposes')
      ggplot(subset(dfc, dfc$IV1.Participant.Politics < 0), aes(x = OtherPlaylistAvg)) + geom_histogram(fill = 'blue') + ggtitle('Self Liberal; dichotomized for display purposes')
      
            # overlapping density chart
            ggplot(dfc,aes(x = OtherPlaylistAvg)) + ggtitle('Other Playlist Avg Broken Out by Own Politics; dichotomized for display purposes') +
                geom_density(data=subset(dfc, dfc$IV1.Participant.Politics > 0),fill = "red", alpha = 0.25, adjust = .8) +
                geom_density(data=subset(dfc, dfc$IV1.Participant.Politics < 0),fill = "blue", alpha = 0.25, adjust = .8)

# self predicting self
cor(dfa[,c('IV1.Participant.Politics', 'ownPlaylistAvgCons')])


      # histograms of average playlist composition given OWN politics
      ggplot(subset(dfa, dfa$IV1.Participant.Politics > 0), aes(x = ownPlaylistAvgCons)) + geom_histogram(fill = 'red') + ggtitle('Self Conservative; dichotomized for display purposes')
      ggplot(subset(dfa, dfa$IV1.Participant.Politics < 0), aes(x = ownPlaylistAvgCons)) + geom_histogram(fill = 'blue') + ggtitle('Self Liberal; dichotomized for display purposes')
      
      # overlapping density chart
      ggplot(dfc,aes(x = OwnPlaylistAvg)) + ggtitle('Own Playlist Avg Broken Out by Own Politics; dichotomized for display purposes') +
        geom_density(data=subset(dfc, dfc$IV1.Participant.Politics > 0),fill = "red", alpha = 0.25, adjust = .8) +
        geom_density(data=subset(dfc, dfc$IV1.Participant.Politics < 0),fill = "blue", alpha = 0.25, adjust = .8)

# Scatter Plots

#  own politics vs own playlist construction
ggplot(dfc, aes(x = IV1.Participant.Politics, y = OwnPlaylistAvg)) +
  geom_point(size = 4) +
  geom_smooth(method = 'lm', se = F, lwd = .25, col = 'grey')

# own politics vs other playlist construction
ggplot(dfc, aes(x = IV1.Participant.Politics, y = OtherPlaylistAvg)) +
  geom_point(size = 4) +
  geom_smooth(method = 'lm', se = F, lwd = .25, col = 'grey')

#Analysis----

summary(lm(dfa$ownPlaylistAvgCons ~ dfa$IV1.Participant.Politics + dfa$selfFirst))

summary(lm(dfa$targetPlaylistAvgCons ~ dfa$targCon +  dfa$selfFirst))

summary(lm(dfa$targetPlaylistAvgCons ~  (dfa$match * dfa$ownPlaylistAvgCons)))
summary(lm(targetPlaylistAvgCons ~ ownPlaylistAvgCons*match, data = dfa))

# match model
summary(lm(targetPlaylistAvgCons ~ ownPlaylistAvgCons*match, data = df))
        
summary(lm(dfa$targetPlaylistAvgCons ~ dfc$IV1.Participant.Politics + dfc$IV2.Oth.Stu.Con + dfa$ownPlaylistAvgCons))
summary(lm(dfa$targetPlaylistAvgCons ~ dfc$IV1.Participant.Politics + dfc$IV2.Oth.Stu.Con + dfa$ownPlaylistAvgCons + (dfc$IV1.Participant.Politics*dfc$IV2.Oth.Stu.Con*dfa$ownPlaylistAvgCons)))
 #if R dummy codes continuous variable -> df$variable <- as.numeric(as.character(df$variable))

)

visreg(lm(dfa$targetPlaylistAvgCons ~ dfc$match + dfa$ownPlaylistAvgCons + (dfc$match * dfa$ownPlaylistAvgCons)),"Match", by="OwnPlaylistAvg", overlay="TRUE")

       