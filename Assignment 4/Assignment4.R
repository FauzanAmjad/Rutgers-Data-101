##Fauzan Amjad

## Dataset Import
setwd("~/Desktop")
moody <- read.csv("~/Desktop/moody2020b.csv")

## Colors
colors <- c("red", "blue" , "green")

## Main Boxplot
boxplot(moody$score ~ moody$texting, main = "Boxplot of Score VS Frequency of Texting", xlab = "Texting", ylab = "Score", col = colors)

## Subset our data
alwaysTexting.data <- subset(moody, moody$texting == "always")
somestimesTexting.data <- subset(moody, moody$texting == "sometimes")

## Get our Scores
alwaysTexting.scores <- alwaysTexting.data$score
sometimesTexting.scores <- somestimesTexting.data$score

## Get our Mean Values
mean.alwaysTexting <- mean(alwaysTexting.scores)
mean.sometimesTexting <- mean(sometimesTexting.scores)

## Get our Standard Deviations 
sd.alwaysTexting <- sd(alwaysTexting.scores)
sd.sometimesTexting <- sd(sometimesTexting.scores)

## Get our Lengths 
len_alwaysTexting <- length(alwaysTexting.scores) 
len_sometimesTexting <- length(sometimesTexting.scores)

## Get Combined Standard Deviation
sd.al.st <- sqrt(sd.alwaysTexting^2/len_alwaysTexting + sd.sometimesTexting^2/len_sometimesTexting)

## Calculate our Z-value
zeta.al.st <- (mean.alwaysTexting - mean.sometimesTexting)/sd.al.st
zeta.al.st

##Get our P-value
p.al.st = 1 - pnorm(zeta.al.st)
p.al.st


