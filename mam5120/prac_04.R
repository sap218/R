# sap21 _ MAM5120 _ week 4 practical

seq(0,8,by=2) # generates vector (0,2,4,6,8)
seq(0,1,length=101) # == (0, 0.01, 0.02, ..., 0.99, 1)

# binomial - different number of heads in 10 coin tosses
k.vector <- seq(0,10,by=1) # possible outcomes
y.binom <- dbinom(k.vector,10,prob=0.5) # fair coin
plot(k.vector,y.binom,type="h") # h for bars

# normal - density of a standard (0,1) Normal variable on interval [-3,3]
x.vector <- seq(-3,3,length=1001)
y.norm <- dnorm(x.vector,mean=0,sd=1)
plot(x.vector,y.norm,type="l") # l for lines

# plot standard (0,1) Normal density and also a N(0,4) density on same plot - range on y-axis, when overlay a second plot
x.vector <- seq(-4,4,length=1001)
y1 <- dnorm(x.vector,mean=0,sd=1)
y2 <- dnorm(x.vector,mean=0,sd=2) # i.e. sigma^2=4
ylim <- range(y1, y2)
plot(x.vector,y1,type="l",ylim=ylim) # ylim i.e. range of y-axis is given
points(x.vector,y2,type="l",col="red")

######################

# Question 1
# bayes: p(theta | x) &< L(theta | x) x p(theta)

# (a) - # vector length = 1000 on [0,1]
b.vector <- seq(0,1,length=1000)
# (i) - # alpha = 1, beta = 1
y.binom <- dbeta(b.vector,1,1,ncp=0)
plot(b.vector,y.binom, type="l")
# (ii) - # alpha = 4, beta = 2
y.binom <- dbeta(b.vector,4,2,ncp=0)
plot(b.vector,y.binom, type="l")
# (iii) - # alpha = 3, beta = 0.5
y.binom <- dbeta(b.vector,3,0.5,ncp=0)
plot(b.vector,y.binom, type="l")

# (b) - # on [1,1] - # coin 25, w/ 6 heads
b.vector <- seq(0,1,length=1000)
y.bin <- dbeta(b.vector,6,25,ncp=0)
y.bin2 <- dbeta(b.vector,7,20,ncp=0)
plot(b.vector,y.bin,type="l") 
points(b.vector,y.bin2,type="l",col="red")

# (c) - # on (b) but w/ beta(2,2)
b.vector <- seq(0,1,length=1000)
y.bin <- dbeta(b.vector,6,25)
y.bin2 <- dbeta(b.vector,8,21)
plot(b.vector,y.bin,type="l") 
points(b.vector,y.bin2,type="l",col="red")

######################

# Question 2
# poisson: gamma

# (a) - # alpha is shape, beta is rate
g.vector <- seq(0,10,length=1000)
# (i) - # a=1, b=1
y.poisson <- dgamma(g.vector,1,rate=1)
plot(g.vector,y.poisson,type="l")
# (ii) - # a=3, b=1
y.poisson <- dgamma(g.vector,3,rate=1)
plot(g.vector,y.poisson,type="l")
# (iii) - # a=10, b=5
y.poisson <- dgamma(g.vector,10,rate=5)
plot(g.vector,y.poisson,type="l")

# (b) - gamma(3,1)
x <- c(1,4,2,5,3,2,4,0,1)
sum(x)
g.vector <- seq(0,10,length=1000)
y.pois <- dgamma(g.vector,3,rate=1)
y.pois2 <- dgamma(g.vector,25,rate=11)
ylim <- range(y.pois,y.pois2)
plot(g.vector,y.pois,type="l",ylim=ylim) 
points(g.vector,y.pois2,type="l",col="red")

# (c) - # calculate posterior probability that population mean is less than 3
mean(x)
g.vector <- seq(0,4,length=1000)
mean(g.vector)
y.pois <- pgamma(g.vector,3,rate=1)
y.pois2 <- pgamma(g.vector,25,rate=5)
ylim <- range(y.pois,y.pois2)
plot(g.vector,y.pois,type="l",ylim=ylim) 
points(g.vector,y.pois2,type="l",col="red")
var <- pgamma(3,25,rate=5)

######################

# Question 3
# normal
# population variance sigma^2 == 9
# N(20,10), mu=20, (sigma^2)0 == 10
x = c(16.4, 10.8, 16.1, 17.3, 13.8, 22.5, 15.9, 14.8, 19.2, 22.7)
sum(x)
mean(x)
# (a)
post.mean = ((sum(x)/9) + (20/10)) / ((10/9) + (1/10))
variance = ((10/9) + (1/10))^-1

# (b)