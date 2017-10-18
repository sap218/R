# sap21 _ MAM5120 _ week 3 practical

x <- c(0,1,2,3) 
freqs <- c(5,4,2,6)
N <- sum(freqs) # counting total number of data points
mean.x <- sum(x*freqs)/N
var.x <- var(rep(x,freqs))

#### Question 1
nom <- c(0,1,2,3,4) # ..5
freq <- c(0,5,14,12,8) # ..0
NN <- sum(freq)
mean.nom <- sum(nom*freq)/NN
dpois(3,mean.nom)
ppois((1-sum(nom)),mean.nom) 
var.nom <- var(rep(nom,freq))

#### Question 2
# ETC[].


#### Question 3
#(a)
n = 30 #eggs
k = 10 #male
curve(dbinom(k,n,x),from=0,to=1,main="A likelihood plot for p: 10 males from 30 crocodile eggs",xlab="p",ylab="Likelihood")

#(b)
#Calculate an approximate 95% confidence interval for p
p = k/n
sqroot = sqrt((p*(1-p))/n)
twosqrt = 2*sqroot
pa = p + twosqrt
ps = p - twosqrt

#(c)
par(mfrow=c(1,2))
hight <- curve(dbinom(10,25,x),from=0,to=1,main="10M, 25 eggs, high temp",xlab="p",ylab="Likelihood",col="red")
lowt <- curve(dbinom(22,42,x),from=0,to=1,main="22M, 43 eggs, low temp",xlab="p",ylab="Likelihood",col="green")
par(mfrow=c(1,1))
plot(hight,type="l",col="red",main="gender of crocodile depends on eggs temperature",xlab="p",ylab="Likelihood")
lines(lowt,col="green")


#### Question 4
# ETC[].