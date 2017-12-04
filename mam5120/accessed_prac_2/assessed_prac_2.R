# sap21 _ MAM5120 _ Assessed Practical 2 _ 6th November 2017

#################
#Question 1
#relationship between height (cm) & weight (kg) - 100 people

#a - produce plot
attach(weight_height)
plot(height, weight, main="Scatter Plot of Weight against Height", ylab="Weight (kg)", xlab="Height (cm)")

#b - linear model to regress y on x
#reproduce & interpret summary table of linear model in R
#including the equation of the fitted line and proportion of variability explained by model
hw.lm <-lm(height~weight) 
summary(hw.lm)
abline(lm(weight~height), col="red") # regression line (y~x) 

#c - what is meaning of Estimate in row labeled "height" in table of (b)

#################
#Question 2

#################
#Question 3

#################
#Question 4