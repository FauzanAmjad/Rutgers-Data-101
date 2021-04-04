library(readr)
trainData <- read_csv("Desktop/Data101 Assignment 8/M2021train.csv")
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

colors = c("red", "green", "blue")

## Summary
summary(trainData)
myprediction<-trainData
decision <- rep('Fail',nrow(myprediction))
decision[myprediction$Score>50] <- 'Pass'
decision[myprediction$Score<50] <- 'Fail'
myprediction$Grade <-decision
trainData$Projection <-decision
error <- mean(trainData$Grade != myprediction$Grade)
error

## Plots of Values Against Grade -> Rough Overview
boxplot(trainData$Attendance~trainData$Grade, main = "Attendance VS Grade", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(trainData$Major~trainData$Grade, main = "Major VS Grade", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(trainData$Questions~trainData$Grade, main = "Questions VS Grade", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(trainData$Score~trainData$Grade, main = "Score VS Grade", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(trainData$Seniority~trainData$Grade, main = "Seniority VS Grade", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(trainData$Texting~trainData$Grade, main = "Texting VS Grade", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))

## Subseting Data the First Time
subsetSmall <- subset(trainData, Score < 50)
subsetBig <- subset(trainData, Score >= 50)

## Analyzing SubsetSmall
boxplot(subsetSmall$Attendance~subsetSmall$Grade, main = "Attendance VS Grade for Score < 50", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(subsetSmall$Major~subsetSmall$Grade, main = "Major VS Grade for Score < 50", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetSmall$Questions~subsetSmall$Grade, main = "Questions VS Grade for Score < 50", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetSmall$Score~subsetSmall$Grade, main = "Score VS Grade for Score < 50", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetSmall$Seniority~subsetSmall$Grade, main = "Seniority VS Grade for Score < 50", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetSmall$Texting~subsetSmall$Grade, main = "Texting VS Grade for Score < 50", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))

## Subsetting SubsetSmall between CS and other majors
subsetCSsmall <- subset(subsetSmall, Major == "Cs")
subsetMajorSmall <- subset(subsetSmall, Major != "Cs")

## CS and Score < 50
boxplot(subsetCSsmall$Attendance~subsetCSsmall$Grade, main = "Attendance VS Grade for CS Students w/ Score < 50", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(ssubsetCSsmall$Major~subsetCSsmall$Grade, main = "Major VS Grade for CS Students w/ Score < 50", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetCSsmall$Questions~subsetCSsmall$Grade, main = "Questions VS Grade for CS Students w/ Score < 50", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetCSsmall$Score~subsetCSsmall$Grade, main = "Score VS Grade for CS Students w/ Score < 50", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetCSsmall$Seniority~subsetCSsmall$Grade, main = "Seniority VS Grade for CS Students w/ Score < 50", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetCSsmall$Texting~subsetCSsmall$Grade, main = "Texting VS Grade for CS Students w/ Score < 50", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))

## Other Majors and Score < 50
boxplot(subsetMajorSmall$Attendance~subsetMajorSmall$Grade, main = "Attendance VS Grade for Non-CS Students w/ Score < 50", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmall$Major~subsetMajorSmall$Grade, main = "Major VS Grade for Non-CS Students w/ Score < 50", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(ssubsetMajorSmall$Questions~subsetMajorSmall$Grade, main = "Questions VS Grade for Non-CS Students w/ Score < 50", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetMajorSmall$Score~subsetMajorSmall$Grade, main = "Score VS Grade for Non-CS Students w/ Score < 50", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmall$Seniority~subsetMajorSmall$Grade, main = "Seniority VS Grade for Non-CS Students w/ Score < 50", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmall$Texting~subsetMajorSmall$Grade, main = "Texting VS Grade for Non-CS Students w/ Score < 50", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))

## Analyzing SubsetBig
boxplot(subsetBig$Attendance~subsetBig$Grade, main = "Attendance VS Grade for Score >= 50", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(subsetBig$Major~subsetBig$Grade, main = "Major VS Grade for Score  >= 50", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetBig$Questions~subsetBig$Grade, main = "Questions VS Grade for Score  >= 50", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetBig$Score~subsetBig$Grade, main = "Score VS Grade for Score  >= 50", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetBig$Seniority~subsetBig$Grade, main = "Seniority VS Grade for Score  >= 50", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetBig$Texting~subsetBig$Grade, main = "Texting VS Grade for Score  >= 50", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))