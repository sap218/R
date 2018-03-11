# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 2 - Time Series
# Samantha Pendleton - sap21

# install.packages("curl") # for unix: sudo apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev
install.packages("TSA")
library("TSA")
install.packages("forecast")
library("forecast")

##############################################################

# Exercise 1. Autocorrelation and Stationarity
data("oil.price")

# a)
par(mfrow=c(2,1))
plot(oil.price, type='o', main="(i) Time Series Plot of Oil Prices from 1986-2006", ylab="Price", xlab="Year") # (i) time series plot
#mdl <- lm(oil.price~time(oil.price))
#summary(mdl)
#abline(mdl)
acf(oil.price, main="(ii) Sample Autocorrelation of Residuals Series")
# b)
par(mfrow=c(2,1))
plot(diff(log(oil.price)), type='l', main="(i) Time Series Plot of 1st differences of log Oil Prices from 1986-2006", ylab="Change in log(Price)") 
acf(diff(log(oil.price)), main="(ii) Sample Autocorrelation of Residuals Serie at log 1st differences")

##############################################################

# Exercise 2. ARMA and ARIMA models
# 2.1
data("ima22.s")
# a)
par(mfrow=c(1,1))
plot(ima22.s, main="Time Series Plot of Simulated IMA(2,2) Series")

# c)
par(mfrow=c(2,2))
plot(ima22.s, type='o', main="Simulation of an IMA(2,2) Series", ylab="Simulation")
plot(diff(ima22.s), type='o', main="First Difference of the IMA(2,2) Simulation", ylab="Differenced Once")
plot(diff(ima22.s, differences = 2), type='o', main="Second Difference of the IMA(2,2) Simulation", ylab="Differenced Twice")
plot(diff(ima22.s, differences = 3), type='o', main="Third Difference of the IMA(2,2) Simulation", ylab="Differenced Thrice")

##############################

# 2.2
data("airpass")
# a)
par(mfrow=c(1,1))
plot(airpass, type='l', ylab="Count",
     main="Time Series plot of monthly totals of US air passenger miles flown between\nJanuary 1949 and December 1960")
points(y=airpass, x=time(airpass), cex=.9, col="blue", pch=(as.vector(season(airpass))))

# b)
layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))
plot(log(airpass), main="Log time Series plot of monthly totals of US air passenger miles flown between January 1949 and December 1960", ylab="Value")
plot(diff(log(airpass), lag = 1), main="First Difference", ylab="Value")
acf(diff(log(airpass), lag = 1), main="Autocorrelation of Residuals\nSeries at log 1st Differences")
plot(diff(log(airpass), lag = 12), main="Twelve Difference", ylab="Value")
acf(diff(log(airpass), lag = 12), main="Autocorrelation of Residuals\nSeries at log 12th Differences")

# c)
layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))
plot(log(airpass), main="Log time Series plot of monthly totals of US air passenger miles flown between January 1949 and December 1960", ylab="Value")
plot(diff(log(airpass), differences = 1), main="First Difference", ylab="Value")
acf(diff(log(airpass), differences = 1), main="Autocorrelation of Residuals\nSeries at log 1st Differences")
plot(diff(log(airpass), differences = 12), main="Twelve Difference", ylab="Value")
acf(diff(log(airpass), differences = 12), main="Autocorrelation of Residuals\nSeries at log 12th Differences")

##############################################################

# Exercise 3. Hares and Roots!

##############################################################

# Exercise 4. Forecasting
# a)
# b)
# c)
# d)