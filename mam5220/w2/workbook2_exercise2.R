# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 2 - Time Series
# Samantha Pendleton - sap21

# install.packages("curl") # for unix: sudo apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev
install.packages("TSA")
library("TSA")
install.packages("forecast")
library("forecast")

##############################################################

# Exercise 2 - ARMA and ARIMA models for Time Series

# MA(1) - illustrating how correlation arises in model is Moving Average, where values are combinations of current & previous white noise
# ma1.1.s was simulated with parameter θ = 0.9
# ma1.2.s had θ = -0.9 
# MA(q) - the  current  value  depends directly  on  the  previous  one (such as in epidemics)

# AR(1) - e = white noise
# ar1.s has φ = 0.9
# AR(p) - in the autoregressive process of order p, the current value is a linear combination of p previous series values plus noise
# ar2.s is  an  example  of  an  AR(2)  with φ[1] = 1.5  and φ[2] = -0.75
# AR(p) and the Partial Autocorrelation Function (PACF)

# ARMA models
# autoregressive AR(p) but noise is correlated, MA(q) instead of white noise - gives us ARMA(p, q) class which general satisfies equation
# acf of the ARMA(1, 1) series arma11.s

# ARIMA models and Non-stationarity
# non-stationary  random  walk  but  with  correlated  MA(1)  noise instead  of  white  noise - this amounts to ARMA(1,1) with φ = 1
# ∇ is the symbol used for the 1st difference
# This model is called an integrated moving 
# Average, IMA(1,1).  When 1st differencing leads to an ARMA(p,q) for ∇Y[t]
# The model for Y[t] is called an autoregressive integrated moving average or ARIMA(p, 1, q) 
# Such series tend to show random changes of level
# If d successive  differences  yield  a  stationary  ARMA(p,q)  we  have  an  ARIMA(p,d,q)  
# Usually in practice d = 0, 1 or 2 with, of course, d = 0 meaning a stationary series
# E.g. if 2nd differences follow an MA(2), then ARIMA models with d=2 are characterised by random changes of both level and slope

data(co2)
arima(co2)
plot(diff(co2),ylab='First difference of CO2')
y <- diff(diff(co2), lag=12)
plot(y, xlab= 'Time')
acf(y)
acf(y,lag.max=36)

###########################

# Exercise

# 2.1
# a)
data("ima22.s")
plot(ima22.s, main="A simulated IMA(2,2) series with theta1=1 and theta2=-0.6")


# 2.2
# a)
data("airpass")
plot(airpass, ylab="Count", main="Monthly totals of US air passenger miles flown between January 1949 and December 1960    \nanalysed by Box and Jenkins (1976)")

# b)
log.air <- log(airpass)
par(mfrow=c(4,2))
plot(airpass)
plot(log.air)

plot(diff(log.air, differences = 1))
plot(diff(log.air, differences = 12))

plot(diff(log.air, lag = 1))
plot(diff(log.air, lag = 12))

acf(airpass)
acf(log.air)
