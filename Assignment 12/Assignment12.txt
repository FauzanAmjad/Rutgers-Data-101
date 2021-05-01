library(readr)
data <- read_csv("Desktop/Data101 Assignment 12/BlackBoxtrainApril22.csv")
dataTest <- read_csv("Desktop/Data101 Assignment 12/BlackBoxTestApril22-students.csv")
submission <- read_csv("Desktop/Data101 Assignment 12/BlackBoxTestApril22-submission.csv")


##Exploring the Basics of the Data

subsetHigh <- subset(data, data$SWITCH == "High")
subsetLow <- subset(data, data$SWITCH == "Low")
subsetMax <- subset(data, data$SWITCH == "Maximum")
subsetMed <- subset(data, data$SWITCH == "Medium")
subsetMin <- subset(data, data$SWITCH == "Minimum")

##subset high
boxplot(subsetHigh$INPUT1 ~ subsetHigh$SOUND, col = c("red","blue","green","purple"), main = "Input 1 VS Sound")
boxplot(subsetHigh$INPUT2 ~ subsetHigh$SOUND, col = c("red","blue","green","purple"), main = "Input 2 VS Sound")
boxplot(subsetHigh$INPUT3 ~ subsetHigh$SOUND, col = c("red","blue","green","purple"), main = "Input 3 VS Sound")
boxplot(subsetHigh$INPUT4 ~ subsetHigh$SOUND, col = c("red","blue","green","purple"), main = "Input 4 VS Sound")

##subset low
boxplot(subsetLow$INPUT1 ~ subsetLow$SOUND, col = c("red","blue","green","purple"), main = "Input 1 VS Sound")
boxplot(subsetLow$INPUT2 ~ subsetLow$SOUND, col = c("red","blue","green","purple"), main = "Input 2 VS Sound")
boxplot(subsetLow$INPUT3 ~ subsetLow$SOUND, col = c("red","blue","green","purple"), main = "Input 3 VS Sound")
boxplot(subsetLow$INPUT4 ~ subsetLow$SOUND, col = c("red","blue","green","purple"), main = "Input 4 VS Sound")

## subset max
boxplot(subsetMax$INPUT1 ~ subsetMax$SOUND, col = c("red","blue","green","purple"), main = "Input 1 VS Sound")
boxplot(subsetMax$INPUT2 ~ subsetMax$SOUND, col = c("red","blue","green","purple"), main = "Input 2 VS Sound")
boxplot(subsetMax$INPUT3 ~ subsetMax$SOUND, col = c("red","blue","green","purple"), main = "Input 3 VS Sound")
boxplot(subsetMax$INPUT4 ~ subsetMax$SOUND, col = c("red","blue","green","purple"), main = "Input 4 VS Sound")

# subset med
boxplot(subsetMed$INPUT1 ~ subsetMed$SOUND, col = c("red","blue","green","purple"), main = "Input 1 VS Sound")
boxplot(subsetMed$INPUT2 ~ subsetMed$SOUND, col = c("red","blue","green","purple"), main = "Input 2 VS Sound")
boxplot(subsetMed$INPUT3 ~ subsetMed$SOUND, col = c("red","blue","green","purple"), main = "Input 3 VS Sound")
boxplot(subsetMed$INPUT4 ~ subsetMed$SOUND, col = c("red","blue","green","purple"), main = "Input 4 VS Sound")

## subset min
boxplot(subsetMin$INPUT1 ~ subsetMin$SOUND, col = c("red","blue","green","purple"), main = "Input 1 VS Sound")
boxplot(subsetMin$INPUT2 ~ subsetMin$SOUND, col = c("red","blue","green","purple"), main = "Input 2 VS Sound")
boxplot(subsetMin$INPUT3 ~ subsetMin$SOUND, col = c("red","blue","green","purple"), main = "Input 3 VS Sound")
boxplot(subsetMin$INPUT4 ~ subsetMin$SOUND, col = c("red","blue","green","purple"), main = "Input 4 VS Sound")


data$add <- data$INPUT1  + data$INPUT4 
data$add

boxplot(data$add ~ data$SOUND )

## Rpart 65%
library(rpart)
tree <- rpart(SOUND ~ SWITCH+INPUT1+INPUT2+INPUT3+INPUT4,control = rpart.control(minsplit = 1), data=data)
library(rpart.plot)
rpart.plot(tree)
CrossValidation::cross_validate(data, tree, 2, 0.8)
pred = predict(tree, newdata = dataTest, type = "class")
pred
submission$SOUND <- pred
submission$SOUND
write.csv(submission, "Fauzan_14_Submission.csv", row.names = FALSE)


## Forest
library(randomForest)
random_forest <- randomForest::randomForest(SOUND ~ SWITCH+INPUT1+INPUT2+INPUT3+INPUT4, data=data)
training_predictions <- predict(random_forest, newdata = data)
mse(training_predictions, data$SOUND)

## Neural Network
library(neuralnet)
perf.nnet = neuralnet(SOUND ~ SWITCH+INPUT1+INPUT2+INPUT3+INPUT4, data = data, hidden = 3, linear.output = FALSE)
