##Fauzan Amjad

## Data Set
library(readr)
Gun_Control <- read_csv("Desktop/Gun_Control.csv")


## Colors
colors <- c("red", "blue" , "green") 

## Null Hypothesis: The Average monetary damage of strict gun laws is the same the average monetary damage of loose gun laws

## Subset Data via Gun Laws 
strict.data <- subset(Gun_Control, Gun_Control$Gun_Laws == "Strict_Gun_Laws")
loose.data <- subset(Gun_Control, Gun_Control$Gun_Laws == "Loose_Gun_Laws")

## Collect Damage Data Per Gun Law
strict.damage <- strict.data$Monetary_Damage
loose.damage <- loose.data$Monetary_Damage

## Collect Mean data for Each
mean.strict <- mean(strict.damage)
mean.loose <- mean(loose.damage)
mean.strict
mean.loose

## Permutation Test
PermutationTestSecond::Permutation(Gun_Control, "Gun_Laws", "Monetary_Damage",100000,"Strict_Gun_Laws", "Loose_Gun_Laws")
PermutationTestSecond::Permutation(Gun_Control, "Gun_Laws", "Monetary_Damage",100000,"Medium_Gun_Laws", "Loose_Gun_Laws")
PermutationTestSecond::Permutation(Gun_Control, "Gun_Laws", "Monetary_Damage",100000,"Strict_Gun_Laws", "Medium_Gun_Laws")

## Exhibit 2
medium.data <- subset(Gun_Control, Gun_Control$Gun_Laws == "Medium_Gun_Laws")

medium.gun <- subset(medium.data, medium.data$Weapon_Used == "Gun")
loose.gun <- subset(loose.data, loose.data$Weapon_Used == "Gun")

medium.gun.damage <- medium.gun$Monetary_Damage
loose.gun.damage <- loose.gun$Monetary_Damage

mean.medium <- mean(medium.gun.damage)
mean.loose.new <- mean(loose.gun.damage)

sd.mean.medium <- sd(medium.gun.damage)
sd.mean.loose.new <- sd(loose.gun.damage)

len1 <- length(medium.gun.damage)
len2 <- length(loose.gun.damage)

sd.all <- sqrt(sd.mean.medium^2/len1 + sd.mean.loose.new^2/len2)

zeta.al.set <- (mean.medium - mean.loose.new)/sd.all

pNew <- 1 - pnorm(zeta.al.set)
pNew
