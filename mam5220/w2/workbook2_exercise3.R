# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 2 - Time Series
# Samantha Pendleton - sap21

# install.packages("curl") # for unix: sudo apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev
install.packages("TSA")
library("TSA")
install.packages("forecast")
library("forecast")

##############################################################

# Exercise 3 - Hares and Roots!

# Estimating ARMA models
data(hare)
AR1.hare <- arima(hare, order=c(1,0,0)) # ARIMA
AR1.hare

# Choosing the best model
AIC1.hare <- aic(hare, pen=2)
AIC1.hare

# Checking Model Adequacy
par(mfrow=c(1,2))
plot(rstandard(AR1.hare), ylab='standardised residuals', type='o')
abline(h=0)
acf(residuals(arima(hare, order=c(3,0,0))))
#abline(h=0)

ARMA.hare <- arima(hare,order=c(3,0,1)) # overfitting


# Exercise

par(mfrow=c(1,2))
plot(hare, ylab='Abundance', xlab='Year', type='o') # comparative smoothness  of  this  plot  reflects  correlation  between  observations in successive years, ie autocorrelation
sqrt.hare <- sqrt(hare)
plot(sqrt.hare, ylab='Abundance', xlab='Year', type='o')

ARI.sqrt.hare <- auto.arima(sqrt.hare)
ARI.sqrt.hare
ARI.hare <- auto.arima(hare)
ARI.hare

# Model Residuals
tsdisplay(residuals(ARI.hare), lag.max=45, main="Model Residuals")
tsdisplay(residuals(ARI.sqrt.hare), lag.max=45, main="Square Root Model Residuals")

# Ljung-Box test tries to reject the independence of some values
Box.test(residuals(ARI.hare), lag=24, fitdf=4, type="Ljung")
Box.test(residuals(ARI.sqrt.hare), lag=24, fitdf=4, type="Ljung")
# If p-value < 0.05: You can reject the null hypothesis assuming a 5% chance of making a mistake,
  # so you can assume that your values are showing dependence on each other.
# If p-value > 0.05: You don't have enough statistical evidence to reject the null hypothesis,
  # so you can not assume that your values are dependent,
  # this could mean that your values are dependent anyway or it can mean that your values are independent.

# Forecasts for the seasonally adjusted electrical orders index.
par(mfrow=c(1,2))
plot(forecast(ARI.hare))
plot(forecast(ARI.sqrt.hare))