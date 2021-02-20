## Dataset Import
setwd("~/Desktop")
moody <- read.csv("~/Desktop/moody2020b.csv")

colors <- c("red", "blue" , "green")

## Colors
tapply(moody$score, moody$grade, min)
tapply(moody$score, moody$grade, max)
tapply(moody$score, moody$grade, median)

## Score Distribution
boxplot(moody$score ~ moody$grade, col = colors, main = "Score Distribution", xlab = "Grade", ylab = "Score")

## Subset by Letter Grade
subsetA <- subset(moody, grade == "A")
subsetB <- subset(moody, grade == "B")
subsetC <- subset(moody, grade == "C")
subsetD <- subset(moody, grade == "D")
subsetF <- subset(moody, grade == "F")

## Subset Based on Numeric Grade
subsetG <- subset(moody, moody$score <= 99.44 & moody$score >=73.40) ## A range
subsetH <- subset(moody, moody$score <= 81.93 & moody$score >= 58.57) ## B range
subsetI <- subset(moody, moody$score <= 69.86 & moody$score >= 42.60) ## C range
subsetJ <- subset(moody, moody$score <= 51.40 & moody$score >= 22.15) ## D range
subsetK <- subset(moody, moody$score <= 34.65 & moody$score >= 3.62)  ## F range

## Grade VS Texting 
mosaicplot(subsetG$grade ~ subsetG$texting, main = "99.4 to 73.40 Range Grade VS Texting", xlab = "Grade", ylab = "texting", col = colors) 
mosaicplot(subsetH$grade ~ subsetH$texting, main = "81.93 to 58.57 Range Grade VS Texting", xlab = "Grade", ylab = "texting", col = colors)
mosaicplot(subsetI$grade ~ subsetI$texting, main = "69.86 to 42.60 Range Grade VS Texting", xlab = "Grade", ylab = "texting", col = colors)
mosaicplot(subsetJ$grade ~ subsetJ$texting, main = "51.40 to 22.15 Range Grade VS Texting", xlab = "Grade", ylab = "texting", col = colors)
mosaicplot(subsetK$grade ~ subsetK$texting, main = "34.65 to 3.62 Range Grade VS Texting", xlab = "Grade", ylab = "texting", col = colors)

## Grade VS Questions
mosaicplot(subsetG$grade ~ subsetG$questions, main = "99.4 to 73.40 Range Grade VS Questions", xlab = "Grade", ylab = "Questions", col = colors)
mosaicplot(subsetH$grade ~ subsetH$questions, main = "81.93 to 58.57 Range Grade VS Questions", xlab = "Grade", ylab = "Questions", col = colors)
mosaicplot(subsetI$grade ~ subsetI$questions, main = "69.86 to 42.60 Range Grade VS Questions", xlab = "Grade", ylab = "Questions", col = colors)
mosaicplot(subsetJ$grade ~ subsetJ$questions, main = "51.40 to 22.15 Range Grade VS Questions", xlab = "Grade", ylab = "Questions", col = colors)
mosaicplot(subsetK$grade ~ subsetK$questions, main = "34.65 to 3.62 Range Grade VS Questions", xlab = "Grade", ylab = "Questions", col = colors)

## Grade VS Participation
boxplot(subsetG$participation ~ subsetG$grade, main = "99.4 to 73.40 Range Grade VS Participation", xlab = "Grade", ylab = "Participation", col = colors)
boxplot(subsetH$participation ~ subsetH$grade, main = "81.93 to 58.57 Range Grade VS Participation", xlab = "Grade", ylab = "Participation", col = colors)
boxplot(subsetI$participation ~ subsetI$grade, main = "69.86 to 42.60 Range Grade VS Participation", xlab = "Grade", ylab = "Participation", col = colors)
boxplot(subsetJ$participation ~ subsetJ$grade, main = "51.40 to 22.15 Range Grade VS Participation", xlab = "Grade", ylab = "Participation", col = colors)
boxplot(subsetK$participation ~ subsetK$grade, main = "34.65 to 3.62 Range Grade VS Participation", xlab = "Grade", ylab = "Participation", col = colors)

