# sap21 _ MAM5120 _ Assessed Practical 2 _ 6th November 2017

#Question 1 - relationship between height (cm) & weight (kg) - 100 people

#a - produce plot
#library(readr)
weight_height <- read.csv("~/git/R/R/mam5120/assessed_prac_2/weight_height.csv")
#View(weight_height)
attach(weight_height)
plot(height, weight, main="Scatter Plot of Weight against Height of 100 People", ylab="Weight (kg)", xlab="Height (cm)")

#b - linear model to regress y on x
#reproduce & interpret summary table of linear model in R
abline(lm(weight~height), col="red") #regression line (y~x) 
#including the equation of the fitted line and proportion of variability explained by model
hw.lm <-lm(weight~height) 
summary(hw.lm)
a <- -45.73068
b <- 0.67113
equation <- "y <- a + bx"
ans1b <- "y <- -45.73068 + 0.67113(x)"

#c - what is meaning of Estimate in row labeled "height" in table of (b)
ans1c <- "slope of the graph"

###################################################
#Question 2 - effect of changes in pH on efficiency of an industrial process
ph <- c(5.3, 8.0, 6.0, 6.2, 6.7, 7.4, 7.7, 7.5, 6.6, 5.1, 7.0, 7.7)
eff <- c(30.2, 59, 37.3, 38, 42, 52, 56.3, 55.4, 43.3, 27.8, 44.8, 55.1)

#a - best fitting models
pe.lm <- lm(eff~ph)
summary(pe.lm)
model1 <- "y <- -28.3889 + 10.8604(x)"
pe.lm2 <- lm(eff~ph+(I(ph^2)))
summary(pe.lm2)
model2 <- "y <- 16.0767 - 2.9898(x) + 1.0562(x)"
ans2a <- "model 2 better fit as R-squared is higher"

#b - proportion of variability is explained by each model
m1.r2 <- 0.9808
m2.r2 <- 0.987

#c - anova
#model 2: whether inclusion of quadratic term significantly improves the fit of the model
#be explicit about exactly which null hypothesis the F-test is testing
h0 <- "no significant difference"
ha <- "some significant difference"
anova(pe.lm, pe.lm2)
p-value <- 0.06905
ans2c <- "no need to reject null hypothesis as p-value (at 10%) is higher than 5%"

###################################################
#Question 3
Question3_Week9 <- read.csv("~/git/R/R/mam5120/assessed_prac_2/Question3_Week9.csv")
#View(Question3_Week9)
attach(Question3_Week9)
A.df <- read.csv("Question3_Week9.csv")

#a - fit linear model on y against x - generate standard diagnostic plots
a.lm <- lm(y~x, data = A.df)
summary(a.lm)
plot(a.lm) #residuals vs fitted, normal Q-Q

#b - log diagnostic plots
alog.lm <- lm(log(y)~x, data = A.df)
plot(alog.lm) #residuals vs fitted, normal Q-Q

#c - fitted model in log form
ans3c <- "log(y) <- a + bx"

#d - what is corresponding interpretation of e
ans3d <- "each time x is increased by 1, y is multiplied by e(^^bx)"

###################################################
#Question 4 - ANCOVA (analysis of covariance): one continious explanatory variable and one categorical explanatory variable (factor)
#fit separate regression line for each value of the factor: may or may not be parallel
#a
#b

#c - fit linear model regressing call rate (y) on temp (x)
BirdCalls <- read.csv("BirdCalls.csv")
plot(BirdCalls$Temp, BirdCalls$CallRate)

bc.lm <- lm(CallRate~Temp, data = BirdCalls)
summary(bc.lm)
plot(bc.lm)
