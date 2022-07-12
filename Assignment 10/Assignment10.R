library(readr)
data <- read_csv("Desktop/HomeworkMarket.csv")


data$bar <- 0
data$slice <- 0

data[data$Snacks == "Popcorn",]$bar <- 1
data[data$Location == "Princeton" & data$SoftDrinks == "Cola",]$slice <- 1

PermutationTestSecond::Permutation(data,"slice", "bar", 10000, "0", "1" )

data$bar <- 0
data$slice <- 0

data[data$Snacks == "Popcorn",]$bar <- 1
data[data$Location == "Princeton",]$slice <- 1

PermutationTestSecond::Permutation(data,"slice", "bar", 10000, "0", "1" )

data$bar <- 0
data$slice <- 0

data[data$Snacks == "Popcorn",]$bar <- 1
data[data$SoftDrinks == "Cola",]$slice <- 1

PermutationTestSecond::Permutation(data,"slice", "bar", 10000, "0", "1" )