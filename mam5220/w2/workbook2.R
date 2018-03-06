# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 2 - Time Series
# Samantha Pendleton - sap21

# install.packages("curl") # for unix: sudo apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev
install.packages("TSA")
library("TSA")

##############################################################

# Exercise 1 - Autocorrelation and Stationarity

data(hare)
par(mfrow=c(1,1))
plot(hare, ylab='Abundance', xlab='Year', type='o')
# comparative smoothness  of  this  plot  reflects  correlation  between  observations in successive years, ie autocorrelation

par(mfrow=c(2,1))
plot(y=hare, x=zlag(hare), ylab='Abundance', xlab='Previous Year Abundance')
# observation at time t, Y[t] against its predecessor, Y[t-1]
plot(y=hare, x=zlag(hare, d=2))
cor(hare,zlag(hare), use="pairwise.complete.obs")

# The Autocorrelation Function
par(mfrow=c(1,1))
acf(hare)
# well-defined, constant mean level μ
# constant finite variance γ[0]

# White Noise
# uncorrelated errors having mean zero and constant variance σ^2
# for a white noise series the true autocorrelations are zero
sim.white <- ts(rnorm(100), freq=1, start=1)
par(mfrow=c(2,1))
plot(sim.white, type='o', ylab='White Noise')
acf(sim.white)
# afc plot has two lines parallel to x-axis, these can be used to test whether  a  given  series  of  length n could  be  white  noise
# however note this involves a multiple test of hypotheses so no surprise if occasionally 1 or 2 autocorrelations stray outside lines

# A Non-stationary Series
par(mfrow=c(2,1))
data(rwalk)
plot(rwalk,type='o',ylab='Random Walk')
acf(rwalk)
########
par(mfrow=c(4,1)) 
sim.rwalk <- ts(cumsum(rnorm(100)), freq=1, start=1)
plot(sim.rwalk,type='o',ylab='A Random Walk')
acf(sim.rwalk) 
# Series like this have no defined mean level and an infinite variance and are therefore non-stationary
# acf is typically slow to die away and decreases in a linear fashion
# However if we calculate the series of 1st differences of the random walk, we find these are stationary
plot(diff(sim.rwalk), type='o') 
acf(diff(sim.rwalk))
# Differencing is often a convenient way to reduce non-stationary series to stationarity

# Percentage Changes
# Sometimes the effect of noise is proportional to the level of a time series
# We can spot this effect when the series is more volatile at high values and less so for low
# For series whose variance increases w/ their level we hope that taking logs followed by differencing would produce a stationary series

##############################################################

# Exercise 1: Application to oil prices
data("oil.price")

# a) Is the series stationary? Explain evidence
par(mfrow=c(1,2))
# (i) time series plot
plot(oil.price, type='o', main="(i) Time Series of Oil Prices from 1986-2006", ylab="Price")
# (ii) sample afc
acf(oil.price, main="(ii) Sample afc Series")

# b) Construct the series of 1st differences of the logs of the oil price series
# Examine the time series plot and the acf and comment on stationarity
# Include your plots with discussion in your answers
par(mfrow=c(1,3))
plot(diff(oil.price), type='o') 
acf(diff(oil.price))
plot(y=oil.price, x=zlag(oil.price, d=1))

##############################################################
##############################################################
##############################################################

par(mfrow=c(1,1))