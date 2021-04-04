library(readr)
trainData <- read_csv("Desktop/Data101 Assignment 8/M2021train.csv")
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
M2021test_students <- read_csv("Desktop/Data101 Assignment 8/M2021test-students.csv")
M2021test_submission_file <- read_csv("Desktop/Data101 Assignment 8/M2021test-submission-file.csv")

colors = c("red", "green", "blue")

## Summary
summary(trainData)
myprediction <- M2021test_students
decision <- rep('Fail',nrow(myprediction))
decision[myprediction$Score>=50] <- 'Pass'
decision[myprediction$Score<50 ] <- 'Fail'
## First Half
decision[myprediction$Score<50 & (myprediction$Seniority == "Freshman" | myprediction$Seniority == "Junior") & myprediction$Score > 39.5 & myprediction$Attendance >= 59.5] <- 'Pass' ##  0.1781488
decision[myprediction$Score<50 & (myprediction$Major != "Cs") & myprediction$Questions == "Always" & myprediction$Major == "Polsci"] <- 'Pass' ## error -> 0.1843829
decision[myprediction$Score<50 & (myprediction$Major != "Cs") & myprediction$Questions == "Always" & myprediction$Major != "Polsci" & myprediction$Score > 40.5] <- 'Pass' ## 0.1749789
decision[myprediction$Score<50 & (myprediction$Major != "Cs") & myprediction$Questions == "Rarely" & myprediction$Score >= 38.8] <- 'Pass' ## 0.1785714
## Second Half
decision[myprediction$Score >= 50 & myprediction$Score < 69.5 & myprediction$Major == "Cs" & (myprediction$Seniority == "Senior" | myprediction$Seniority == "Junior")] <- 'Fail'
## input here
myprediction$Grade <-decision
M2021test_students$Projection <-decision


## Running on Test File
Studentid <- c(M2021test_students$Studentid)
Studentid
Grade<- c(M2021test_students$Projection)
Grade
df <- data.frame(Studentid,Grade)
df
write.csv(df, "Desktop/Fauzan_Real_Submission.csv", row.names = FALSE)

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
mosaicplot(subsetCSsmall$Major~subsetCSsmall$Grade, main = "Major VS Grade for CS Students w/ Score < 50", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetCSsmall$Questions~subsetCSsmall$Grade, main = "Questions VS Grade for CS Students w/ Score < 50", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetCSsmall$Score~subsetCSsmall$Grade, main = "Score VS Grade for CS Students w/ Score < 50", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetCSsmall$Seniority~subsetCSsmall$Grade, main = "Seniority VS Grade for CS Students w/ Score < 50", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetCSsmall$Texting~subsetCSsmall$Grade, main = "Texting VS Grade for CS Students w/ Score < 50", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))

## Other Majors and Score < 50
boxplot(subsetMajorSmall$Attendance~subsetMajorSmall$Grade, main = "Attendance VS Grade for Non-CS Students w/ Score < 50", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmall$Major~subsetMajorSmall$Grade, main = "Major VS Grade for Non-CS Students w/ Score < 50", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmall$Questions~subsetMajorSmall$Grade, main = "Questions VS Grade for Non-CS Students w/ Score < 50", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetMajorSmall$Score~subsetMajorSmall$Grade, main = "Score VS Grade for Non-CS Students w/ Score < 50", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmall$Seniority~subsetMajorSmall$Grade, main = "Seniority VS Grade for Non-CS Students w/ Score < 50", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmall$Texting~subsetMajorSmall$Grade, main = "Texting VS Grade for Non-CS Students w/ Score < 50", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))

## Splitting Non Cs student w/ Score < 50 by Questions
subsetMajorSmallAlways <- subset(subsetMajorSmall, Questions == "Always")
subsetMajorSmallRarely <- subset(subsetMajorSmall, Questions == "Rarely")

## Non Cs Students Score < 50 Always Ask Questions Plots
boxplot(subsetMajorSmallAlways$Attendance~subsetMajorSmallAlways$Grade, main = "Attendance VS Grade for Non-CS Always Question-Asking Students w/ Score < 50", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmallAlways$Major~subsetMajorSmallAlways$Grade, main = "Major VS Grade for Non-CS Always Question-Asking Students w/ Score < 50", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmallAlways$Questions~subsetMajorSmallAlways$Grade, main = "Questions VS Grade for Non-CS Always Question-Asking Students w/ Score < 50", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetMajorSmallAlways$Score~subsetMajorSmallAlways$Grade, main = "Score VS Grade for Non-CS Always Question-Asking Students w/ Score < 50", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmallAlways$Seniority~subsetMajorSmallAlways$Grade, main = "Seniority VS Grade for Non-CS Always Question-Asking Students w/ Score < 50", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmallAlways$Texting~subsetMajorSmallAlways$Grade, main = "Texting VS Grade for Non-CS Always Question-Asking Students w/ Score < 50", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))

## Non Cs Students Score < 50 Rarely Ask Questions
boxplot(subsetMajorSmallRarely$Attendance~subsetMajorSmallRarely$Grade, main = "Attendance VS Grade for Non-CS Rarely Question-Asking Students w/ Score < 50", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmallRarely$Major~subsetMajorSmallRarely$Grade, main = "Major VS Grade for Non-CS Rarely Question-Asking Students w/ Score < 50", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmallRarely$Questions~subsetMajorSmallRarely$Grade, main = "Questions VS Grade for Non-CS Rarely Question-Asking Students w/ Score < 50", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetMajorSmallRarely$Score~subsetMajorSmallRarely$Grade, main = "Score VS Grade for Non-CS Rarely Question-Asking Students w/ Score < 50", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmallRarely$Seniority~subsetMajorSmallRarely$Grade, main = "Seniority VS Grade for Non-CS Rarely Question-Asking Students w/ Score < 50", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetMajorSmallRarely$Texting~subsetMajorSmallRarely$Grade, main = "Texting VS Grade for Non-CS Rarely Question-Asking Students w/ Score < 50", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))

## Analyzing SubsetBig
boxplot(subsetBig$Attendance~subsetBig$Grade, main = "Attendance VS Grade for Score >= 50", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(subsetBig$Major~subsetBig$Grade, main = "Major VS Grade for Score  >= 50", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetBig$Questions~subsetBig$Grade, main = "Questions VS Grade for Score  >= 50", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetBig$Score~subsetBig$Grade, main = "Score VS Grade for Score  >= 50", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetBig$Seniority~subsetBig$Grade, main = "Seniority VS Grade for Score  >= 50", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetBig$Texting~subsetBig$Grade, main = "Texting VS Grade for Score  >= 50", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))

## 69 Big
subsetBigSmall <- subset(subsetBig,Score < 69 )
subsetBigBig <- subset(subsetBig,Score >= 69 )

## Analyzing SubsetBigSmall
boxplot(subsetBigSmall$Attendance~subsetBigSmall$Grade, main = "Attendance VS Grade for Score >= 50 and < 69", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(subsetBigSmall$Major~subsetBigSmall$Grade, main = "Major VS Grade for Score  >= 50 and < 69", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetBigSmall$Questions~subsetBigSmall$Grade, main = "Questions VS Grade for Score  >= 50 and < 69", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetBigSmall$Score~subsetBigSmall$Grade, main = "Score VS Grade for Score  >= 50 and < 69", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetBigSmall$Seniority~subsetBigSmall$Grade, main = "Seniority VS Grade for Score  >= 50 and < 69", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetBigSmall$Texting~subsetBigSmall$Grade, main = "Texting VS Grade for Score  >= 50 and < 69", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))

## SubsetBigSmall CS
subsetBigSmallCS <- subset(subsetBigSmall,Major == "Cs" )
boxplot(subsetBigSmallCS$Attendance~subsetBigSmallCS$Grade, main = "Attendance VS Grade for CS Student who Score >= 50 and < 69", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(subsetBigSmallCS$Major~subsetBigSmallCS$Grade, main = "Major VS Grade for CS Student who Score  >= 50 and < 69", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetBigSmallCS$Questions~subsetBigSmallCS$Grade, main = "Questions VS Grade forCS Student who Score  >= 50 and < 69", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetBigSmallCS$Score~subsetBigSmallCS$Grade, main = "Score VS Grade for for CS Student who Score  >= 50 and < 69", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetBigSmallCS$Seniority~subsetBigSmallCS$Grade, main = "Seniority VS Grade for CS Student who Score  >= 50 and < 69", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetBigSmallCS$Texting~subsetBigSmallCS$Grade, main = "Texting VS Grade for CS Student who Score  >= 50 and < 69", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))

## Analyzing SubsetBigBig
boxplot(subsetBigBig$Attendance~subsetBigBig$Grade, main = "Attendance VS Grade for Score >= 69", xlab = "Grade", ylab = "Attendance", col = c("red", "green", "blue"))
mosaicplot(subsetBigBig$Major~subsetBigBig$Grade, main = "Major VS Grade for Score  >= 69", xlab = "Major", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetBigBig$Questions~subsetBigBig$Grade, main = "Questions VS Grade for Score  >= 69", xlab = "Questions", ylab = "Grade", col = c("red", "green", "blue"))
boxplot(subsetBigBig$Score~subsetBigBig$Grade, main = "Score VS Grade for Score  >= 69", xlab = "Grade", ylab = "Score", col = c("red", "green", "blue"))
mosaicplot(subsetBigBig$Seniority~subsetBigBig$Grade, main = "Seniority VS Grade for Score  >= 69", xlab = "Seniority", ylab = "Grade", col = c("red", "green", "blue"))
mosaicplot(subsetBigBig$Texting~subsetBigBig$Grade, main = "Texting VS Grade for Score  >= 69", xlab = "Texting", ylab = "Grade", col = c("red", "green", "blue"))




