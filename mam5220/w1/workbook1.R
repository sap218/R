# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 1 - Epidemiology
# Samantha Pendleton - sap21

library(ggplot2)
require(scales)
library(gridExtra)
library(readr)
# https://stackoverflow.com/questions/5293715/how-to-use-greek-symbols-in-ggplot2

# Question 2
# a)
epid <- read.csv("~/git/R/R/mam5220/w1/epidemiology.data.csv") # for laptop
epid <- read_csv("/aber/sap21/git/R/R/mam5220/w1/epidemiology.data.csv") # for computer
qplot(epid$Week, epid$No.infected, xlab="Week", ylab="Infected", ylim=c(0,100000), main="Number of Infected over 30 weeks \nΝ=1000000, R0=10, γ=1/14, Δt=0.1", geom = c("point","line")) 

###########################
###########################

# b)
theta <- c(0.25, 0.5, 0.75, 1)

I0 <- 61 
N <- 1000000 
gamma <- 1/14 
beta <- (10*gamma) / N # R0=10 basic reproduction number
delta.t <- 0.1 
N.time.steps <- (70*30)  

S0 <- theta[1]*N
S <- numeric(N.time.steps+1)  
I <- numeric(N.time.steps+1) 
S[1] <- S0  
I[1] <- I0
for (i in 1:N.time.steps){  
  S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
  I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
} 
#time.vector <- seq(0,N.time.steps)
I.1 <- I
#S0.1 <- qplot(time.vector,I.1,xlab="Time",ylab="I", main="S0=theta*N (theta=0.25)", geom=c("point","line"), col="red") 

S0 <- theta[2]*N
S <- numeric(N.time.steps+1)  
I <- numeric(N.time.steps+1) 
S[1] <- S0  
I[1] <- I0
for (i in 1:N.time.steps){  
  S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
  I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
} 
I.2 <- I
#S0.2 <- qplot(time.vector,I.2,xlab="Time",ylab="I", main="S0=theta*N (theta=0.5)", geom=c("point","line"), col="blue") 

S0 <- theta[3]*N
S <- numeric(N.time.steps+1)  
I <- numeric(N.time.steps+1) 
S[1] <- S0  
I[1] <- I0
for (i in 1:N.time.steps){  
  S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
  I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
} 
I.3 <- I
#S0.3 <- qplot(time.vector,I.3,xlab="Time",ylab="I", main="S0=theta*N (theta=0.75)", geom=c("point","line"), colour="green") 

S0 <- theta[4]*N
S <- numeric(N.time.steps+1)  
I <- numeric(N.time.steps+1) 
S[1] <- S0  
I[1] <- I0
for (i in 1:N.time.steps){  
  S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
  I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
} 
I.4 <- I
#S0.4 <- qplot(time.vector,I.4,xlab="Time",ylab="I", main="S0=theta*N (theta=1.0)", geom=c("point","line"), col="yellow") 

#time.vector <- seq(0,N.time.steps) # time index
time.vector <- seq(0,30,by=30/N.time.steps) # days

ggplot() + geom_line(aes(time.vector,I.1, color="0.25")) + geom_line(aes(time.vector,I.2, color="0.5")) + 
  geom_line(aes(time.vector,I.3, color="0.75")) + geom_line(aes(time.vector,I.4, color="1")) +
  scale_y_continuous(labels=comma, name="Infected", breaks=seq(0,700000,by=50000)) + labs(color="theta θ") + xlab("Week") + 
  labs(title = "Number of Infected over 30 Weeks with SIR Model", subtitle = "Ν=1000000, R0=10, γ=1/14, Δt=0.1, S0=θΝ") +
  scale_x_continuous(breaks=seq(0,30,by=1))

###########################
###########################

# c)
epid <- read.csv("~/git/R/R/mam5220/w1/epidemiology.data.csv") # for laptop
epid <- read_csv("/aber/sap21/git/R/R/mam5220/w1/epidemiology.data.csv") # for computer

theta <- c(0.25, 0.5, 0.75, 1)
errorSS <- function(epid, N=1000000, R0=10, D=14, theta, N.time.steps=2100, delta.t=0.1) {
  gamma <- 1/D
  beta <- R0*gamma/N
  S0 <- theta*N
  I0 <- epid$No.infected[1]
  sample.index <- epid$Time.index
  S <- numeric(N.time.steps+1)
  I <- numeric(N.time.steps+1)
  S[1] <- S0
  I[1] <- I0
  for (i in 1:N.time.steps) {
    S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
    I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
  }
  errorSS <- sum(epid$No.infected-I[sample.index])^2
  return(errorSS)
}

log(errorSS(epid = epid, theta = theta[1]))
log(errorSS(epid = epid, theta = theta[2]))
log(errorSS(epid = epid, theta = theta[3]))
log(errorSS(epid = epid, theta = theta[4]))

###########################

# d)
theta <- seq(0,1.00, by=0.01)
theta.x <- theta
for (i in 1:length(theta)) {
  theta[i] <- errorSS(epid = epid, theta = theta[i])
}
errorss <- qplot(theta.x, theta, geom=c("point","line"), xlab="theta", ylab="error sum of squares")
logerrorss <- qplot(theta.x, log(theta), geom=c("point","line"), xlab="theta", ylab="log error sum of squares")
grid.arrange(errorss, logerrorss, ncol=2, top="ErrorSS of SIR Model when varying θ seq(0,1, by=0.01) \nΝ=1000000, R0=10, γ=1/14, Δt=0.1, S0=θΝ")

###########################

# e)
m <- which.min(theta)
best.theta <- (theta.x[m])
lm <- log(errorSS(epid = epid, theta = best.theta))

par(mfrow=c(1,2))
plot(theta.x,theta, type="l", xlab="theta", ylab="error sum of squares") 
abline(h=m, v=0.3, col="blue")
plot(theta.x,log(theta), type="l", xlab="theta", ylab="log error sum of squares") 
abline(h=lm, v=0.3, col="red")

###########################

# f)
# 0.3 theta
epid <- read.csv("~/git/R/R/mam5220/w1/epidemiology.data.csv") # for laptop
epid <- read_csv("/aber/sap21/git/R/R/mam5220/w1/epidemiology.data.csv") # for computer

theta <- 0.3
I0 <- 61 
N <- 1000000 
gamma <- 1/14 
beta <- (10*gamma) / N 
delta.t <- 0.1 
N.time.steps <- (70*30)  
S0 <- theta[1]*N
S <- numeric(N.time.steps+1)  
I <- numeric(N.time.steps+1) 
S[1] <- S0  
I[1] <- I0
for (i in 1:N.time.steps){  
  S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
  I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
} 
time.vector <- seq(0,30,by=30/N.time.steps)

ggplot() + geom_line(aes(epid$Week,epid$No.infected, color="real")) + geom_line(aes(time.vector,I, color="simulation")) + 
  scale_y_continuous(labels=comma, name="Infected", breaks=seq(0,100000,by=5000)) + labs(color="Model") + xlab("Week") + 
  labs(title = "Number of Infected over 30 Weeks, SIR Model Vs. Real", subtitle = "Ν=1000000, R0=10, γ=1/14, Δt=0.1, S0=0.3*Ν") +
  scale_x_continuous(breaks=seq(0,30,by=1))


#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################

# Question 3

# a)
N <- 5000000 
R0 <- 20
gamma <- 0.1
delta.t <- 0.1
beta <- R0*gamma/N

###########################

# b) 

theta <- seq(0,1.00, by=0.01)
S0 <- theta*N
#S0 <- S0[2]
I0 <- 1
N.time.steps <- 3000 # ??

##

#theta.x <- theta
for (i in 1:length(theta)) {
  S <- numeric(N.time.steps+1) 
  I <- numeric(N.time.steps+1) 
  S[1] <- S0
  I[1] <- I0 
  for (i in 1:N.time.steps){ 
    S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
    I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
  } 
  time.vector[i] <- seq(0,N.time.steps*delta.t,by=delta.t) 
}
par(mfrow=c(1,1))
plot((time.vector/10),I, main="insert title", type="l") 

##

S <- numeric(N.time.steps+1) 
I <- numeric(N.time.steps+1) 
S[1] <- S0
I[1] <- I0 
for (i in 1:N.time.steps){ 
  S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
  I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
} 
time.vector <- seq(0,N.time.steps*delta.t,by=delta.t) 
par(mfrow=c(1,1))
plot(time.vector,I,type="l",xlab="Time",ylab="I")

#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################

# Question 4

# a)