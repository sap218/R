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

par(mfrow=c(4,2))
plot(ima22.s, type='o', main="Simulation of an IMA(2,2) Series", ylab="Simulation")
acf(ima22.s, main="ACF Simulation of an IMA(2,2) Series")
plot(diff(ima22.s), type='o', main="First Difference of the IMA(2,2) Simulation", ylab="Differenced Once")
acf(diff(ima22.s), main="ACF First Difference Simulation of the IMA(2,2) Series")
plot(diff(ima22.s, differences = 2), type='o', main="Second Difference of the IMA(2,2) Simulation", ylab="Differenced Twice")
acf(diff(ima22.s, differences = 2), main="ACF Second Difference Simulation of an IMA(2,2) Series")
plot(diff(ima22.s, differences = 3), type='o', main="Third Difference of the IMA(2,2) Simulation", ylab="Differenced Thrice")
acf(diff(ima22.s, differences = 3), main="ACF Third Differences Simulation of an IMA(2,2) Series")

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
data(hare)
arima(hare, order=c(1,0,0))
arima(hare, order=c(2,0,0))
arima(hare, order=c(3,0,0))
auto.arima(hare)

par(mfrow=c(2,2))
pacf(residuals(arima(hare, order = c(1,0,0))), main="PACF Series ARMA(1,0,0) of Hare")
pacf(residuals(arima(hare, order = c(2,0,0))), main="PACF Series ARMA(2,0,0) of Hare")
pacf(residuals(arima(hare, order = c(3,0,0))), main="PACF Series ARMA(3,0,0) of Hare")
pacf(residuals(auto.arima(hare)), main="PACF Series Automatic ARMA Model of Hare")
par(mfrow=c(1,1))
pacf(residuals(arima(hare, order = c(2,0,4))), main="PACF Series ARMA(2,0,4)")

#

sqrt.hare <- sqrt(hare)
arima(sqrt.hare, order=c(1,0,0))
arima(sqrt.hare, order=c(2,0,0))
arima(sqrt.hare, order=c(3,0,0))
auto.arima(sqrt.hare)

par(mfrow=c(2,2))
pacf(residuals(arima(sqrt.hare, order = c(1,0,0))), main="PACF Series ARMA(1,0,0) of Hare (square root)")
pacf(residuals(arima(sqrt.hare, order = c(2,0,0))), main="PACF Series ARMA(2,0,0) of Hare (square root)")
pacf(residuals(arima(sqrt.hare, order = c(3,0,0))), main="PACF Series ARMA(3,0,0) of Hare (square root)")
pacf(residuals(auto.arima(sqrt.hare)), main="PACF Series Automatic ARMA Model of\nare (square root)")
par(mfrow=c(1,1))
pacf(residuals(arima(sqrt.hare, order = c(3,0,5))), main="PACF Series ARMA(3,0,5) of Hare (square root)")

##############################################################

# Exercise 4. Forecasting
# Simulate 50 values but set aside the last 10 values to compare forecasts with actual values
phi <- 0.7
mu <- 100

data(hare)
sim <- (arima.sim(list(order=c(1,0,0), ar=phi), n=50) +mu)
sim.10 <- tail(sim, n=10)

# a)
arima((sim[1:40]), order=c(1,0,0))


fit <- auto.arima(sim[1:40])
fit
plot(forecast(fit,h=40))




# b)
# c)

# d)
ml.hare <- arima(sqrt(hare),order=c(3,0,0))
ml.hare
plot(ml.hare,n.ahead=25,type='b',xlab='Year',ylab='Sqrt(hare)')
abline(h=coef(ml.hare)[names(coef(ml.hare))=='intercept'])