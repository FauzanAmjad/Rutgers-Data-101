
## Import Libraries
library(readr)
library(lmvar)
library(e1071)
library(randomForest)
library(MASS)
library(data.table)
library(mltools)

## Import Dataset
Earnings_Train2021 <- read_csv("Desktop/Data 101 Assignment 11/Earnings_Train2021.csv")
Earnings_Test_Students <- read_csv("Desktop/Data 101 Assignment 11/Earnings_Test_Students.csv")
earning_submission <- read_csv("Desktop/Data 101 Assignment 11/earning_submission.csv")

## Subset Via Major
majorBusiness <- subset(Earnings_Train2021, Earnings_Train2021$Major == "Buisness")
majorHumanities <- subset(Earnings_Train2021, Earnings_Train2021$Major == "Humanities")
majorOther <- subset(Earnings_Train2021, Earnings_Train2021$Major == "Other")
majorProfessional <- subset(Earnings_Train2021, Earnings_Train2021$Major == "Professional")
majorSTEM <- subset(Earnings_Train2021, Earnings_Train2021$Major == "STEM")
majorVocational <- subset(Earnings_Train2021, Earnings_Train2021$Major == "Vocational")

## Scatterplot for Business and Other 

## Analyze Every Variable Against Earnings

## GPA VS Earning
GPA <- Earnings_Train2021$GPA
Earnings <- Earnings_Train2021$Earnings
plot(GPA, Earnings, main="Earnings VS GPA")
lines(GPA, Earnings , type="h", pch=22, col="red")

## Professional Connections vs Earnings
Professional_Connections <- Earnings_Train2021$Number_Of_Professional_Connections
plot(Professional_Connections, Earnings, main="Earnings VS Professional Connections")
lines(Professional_Connections, Earnings , type="h", pch=22, col="yellow")

Professional_Connections <- majorBusiness$Number_Of_Professional_Connections
tempEarnings <- majorBusiness$Earnings
plot(Professional_Connections, tempEarnings, main="Earnings VS Professional Connections for Business Majors")
lines(Professional_Connections, tempEarnings , type="h", pch=22, col="red")

Professional_Connections <- majorOther$Number_Of_Professional_Connections
tempEarnings1 <- majorOther$Earnings
plot(Professional_Connections, tempEarnings1, main="Earnings VS Professional Connections for Other Majors")
lines(Professional_Connections, tempEarnings1 , type="h", pch=22, col="blue")

Professional_Connections <- majorHumanities$Number_Of_Professional_Connections
tempEarnings2 <- majorHumanities$Earnings
plot(Professional_Connections, tempEarnings2, main="Earnings VS Professional Connections for Humanities Majors")
lines(Professional_Connections, tempEarnings2 , type="h", pch=22, col="blue")

Professional_Connections <- majorProfessional$Number_Of_Professional_Connections
tempEarnings3 <- majorProfessional$Earnings
plot(Professional_Connections, tempEarnings3, main="Earnings VS Professional Connections for Professional Majors")
lines(Professional_Connections, tempEarnings3 , type="h", pch=22, col="blue")

Professional_Connections <- majorSTEM$Number_Of_Professional_Connections
tempEarnings4 <- majorSTEM$Earnings
plot(Professional_Connections, tempEarnings4, main="Earnings VS Professional Connections for STEM Majors")
lines(Professional_Connections, tempEarnings4, type="h", pch=22, col="blue")

Professional_Connections <- majorVocational$Number_Of_Professional_Connections
tempEarnings5 <- majorVocational$Earnings
plot(Professional_Connections, tempEarnings5, main="Earnings VS Professional Connections for Voc. Majors")
lines(Professional_Connections, tempEarnings5 , type="h", pch=22, col="blue")

## Graudation Year VS Earnings
Graduation_Year <- Earnings_Train2021$Graduation_Year
plot(Graduation_Year, Earnings, main="Earnings VS Graduation Year")
lines(Graduation_Year, Earnings , type="h", pch=22, col="blue")

## Major Vs Earning
boxplot(Earnings_Train2021$Earnings ~ Earnings_Train2021$Major,outline=FALSE, col = c("red","blue","green"), main = "Major VS Earnings", xlab = "Major", ylab = "Earnings")

## Height VS Earning
Height <- Earnings_Train2021$Height
plot(Height, Earnings, main="Earnings VS Height")
lines(Height, Earnings , type="h", pch=22, col="yellow")

## Credits VS Earning
Credits <- Earnings_Train2021$Number_Of_Credits
plot(Credits, Earnings, main="Earnings VS Credits")
lines(Credits, Earnings , type="h", pch=22, col="red")

## Linear Regression
linear_regression_model <- lm(Earnings~Number_Of_Professional_Connections+Major, data = Earnings_Train2021, x = TRUE, y = TRUE)
lm_predictions <- predict(linear_regression_model, newdata = Earnings_Train2021)
mse(lm_predictions, Earnings_Train2021$Earnings)
cv.lm(linear_regression_model, m = 3)

## Random Forest
random_forest <- randomForest::randomForest(Earnings~Number_Of_Professional_Connections+Major, data = Earnings_Train2021)
training_predictions <- predict(random_forest, newdata = Earnings_Train2021)
mse(training_predictions, Earnings_Train2021$Earnings)

## Predict Random Forest on Test Dataset
testing_predictions <- predict(random_forest, newdata = Earnings_Test_Students)
earning_submission$Earnings <- testing_predictions
write_csv(earning_submission, file = "Earning_Submission0.csv")

## Linear Discriminant
lda_fit <- lda(Earnings~Number_Of_Professional_Connections+Major+Graduation_Year+Height+Number_Of_Credits+Number_Of_Parking_Tickets, data = Earnings_Train2021)

## SVM
svm_fit <- svm(Earnings~Number_Of_Professional_Connections+Major, data = Earnings_Train2021)
svm_training_predictions <- predict(svm_fit, newdata = Earnings_Train2021)
mse(svm_training_predictions, Earnings_Train2021$Earnings)

##Final Model
lda_fit <- lda(Earnings~Number_Of_Professional_Connections, data = majorOther)
lda_training_predictions <- predict(lda_fit, newdata = majorOther)$class
lda_training_predictions <- as.numeric(as.character(lda_training_predictions)) 
mse(lda_training_predictions, majorOther$Earnings)

myprediction<-Earnings_Train2021
decision <- rep(0,nrow(myprediction))
decision@Earnings <- 0
decision[Major == "Other"]$Earnings <- predict(lda_fit, newdata = majorOther)$class
decision[Major == "Other"]$Earnings <- as.numeric(as.character(lda_training_predictions))
decision[Major == "Other"]$Earnings
View(decision)
