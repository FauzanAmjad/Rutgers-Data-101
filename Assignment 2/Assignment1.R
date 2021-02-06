## Fauzan

getwd()
setwd("/Users/..")
moody <- read.csv("~/Desktop/MOODY-2019.csv")

colors <- c("red", "blue" , "green")

## General Test of Histogram
hist(moody$SCORE, main = "Score histogram", 
     xlab = "ranges",ylab="number of students with score", col = colors,
     breaks = 10, ylim = c(0,250))


## Plot 1: Score distribution for each letter grade (in one plot) DONE
boxplot(MOODY_2019$SCORE ~ MOODY_2019$GRADE, main = 'Score distribution for each letter grade', xlab = 'Letter Grade', ylab = 'Numeric Score', col = colors)

## Plot 2: Letter grade frequency distribution depending on values of  the attribute "Ask questions"
mosaicplot(moody$GRADE~moody$ASKS_QUESTIONS, col = colors, main = 'Letter grade frequency distribution VS ASK_QUESTION', xlab = 'Grade', ylab = 'Asks Questions' )

## Plot 3: Frequency distribution of attribute  "On Smartphone" for students who scored more than 80 points DONE
moodyTwo <- subset(MOODY_2019, SCORE > 80)
barplot(table(moodyTwo$ON_SMARTPHONE), col = colors, main = 'Frequency distribution of "On Smartphone" for Above 80 Students', xlab = 'On Smartphone', ylab = 'Frequency of Students')
boxplot(moodyTwo$SCORE ~ moodyTwo$ON_SMARTPHONE, col = colors, main = 'Frequency distribution of "On Smartphone" for Above 80 Students', xlab = 'On Smartphone', ylab = 'Numeric Score')

## Plot 4:  Average score for each value of attribute Late_in_class DONE
tapply(moody$SCORE, moody$LATE_IN_CLASS,mean)
boxplot(MOODY_2019$SCORE ~ moody$LATE_IN_CLASS, main = 'Score Distribution for Each Late in Class Attribute', xlab = 'Late in Class', ylab = 'Numeric Score', col = colors)

## Plot 5: Plot of your choice which shows something interesing. 
mosaicplot(MOODY_2019$ON_SMARTPHONE ~ MOODY_2019$ASKS_QUESTIONS, las = 2, col = colors, main = 'Frequency of Asking Questions VS Frequency to Be on Smartphone', xlab = 'On Smartphone', ylab = 'Asks Questions')
mosaicplot(moodyTwo$ON_SMARTPHONE ~ moodyTwo$ASKS_QUESTIONS, las = 2, col = colors, main = 'Frequency of Asking Questions VS Frequency to Be on Smartphone', xlab = 'On Smartphone', ylab = 'Asks Questions')
