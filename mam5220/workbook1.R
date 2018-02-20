# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 1 - Epidemiology
# Samantha Pendleton - sap21

library(ggplot2)
library(readr)

# Question 2

# a)
# Describe the epidemic curve

#epid <- read.csv("git/R/R/mam5220/epidemiology.data.csv") # for laptop
epid <- read_csv("/aber/sap21/git/R/R/mam5220/epidemiology.data.csv") # for computer
qplot(epid$Week, epid$No.infected, xlab="Week", ylab="Infected", main="Infection rate over 30 weeks", geom = c("point","line")) 

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
require(scales)
ggplot() + geom_line(aes(time.vector,I.1, color="0.25")) + geom_line(aes(time.vector,I.2, color="0.5")) + 
  geom_line(aes(time.vector,I.3, color="0.75")) + geom_line(aes(time.vector,I.4, color="1")) +
  scale_y_continuous(labels=comma, name="number of infected") + labs(color="theta") + xlab("week") + ggtitle("insert title")

###########################

# c)  

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
    I[i+1] <- I[i]-beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
  }
  errorSS <- sum(epid$No.infected-I[sample.index])^2
  return(errorSS)
}

log(errorSS(epid = epid, theta = theta[1]))
log(errorSS(epid = epid, theta = theta[2]))
log(errorSS(epid = epid, theta = theta[3]))
log(errorSS(epid = epid, theta = theta[4]))

###########################
###########################

# d) ##### FIX THESE BELOW 
# vector of theta values [0, 0.01, 0.02, ..., 0.98, 0.99, 1.00]
# Produce plots of (error sum of squares against theta) and (log error sum of squares against theta)

#theta <- seq(0.01:1.00, by=0.01)
#for (i in 1:100) {
#  errorSS(epid = epid, theta = theta[1])
#  theta[1] +1
#}
theta <- c(0.25, 0.5, 0.75, 1)

et1 <- errorSS(epid = epid, theta = theta[1])
et2 <- errorSS(epid = epid, theta = theta[2])
et3 <- errorSS(epid = epid, theta = theta[3])
et4 <- errorSS(epid = epid, theta = theta[4])

et11 <- log(errorSS(epid = epid, theta = theta[1]))
et22 <- log(errorSS(epid = epid, theta = theta[2]))
et33 <- log(errorSS(epid = epid, theta = theta[3]))
et44 <- log(errorSS(epid = epid, theta = theta[4]))

par(mfrow=c(1,2)) 
plot(x=c(theta[1], theta[2], theta[3], theta[4]), y=c(et1, et2, et3, et4),
     xlab="theta", ylab="error", main="insert title", type = ("l")) 
plot(x=c(theta[1], theta[2], theta[3], theta[4]), y=c(et11, et22, et33, et44),
     xlab="theta", ylab="error (log)", main="insert title", type = ("l")) 

#qplot(x=c(theta[1], theta[2], theta[3], theta[4]), y=c(et1, et2, et3, et4),
#      xlab="theta", ylab="error", main="insert title", geom = c("point","line")) 
     
###########################

# e) 
# f)


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

theta <- c(0.001, 0.2, 0.25, 0.5, 0.55, 0.6, 0.65, 0.75, 0.85, 0.95, 1)
S0 <- theta*N
I0 <- c(0.001, 0.2, 0.25, 0.5, 0.55, 0.6, 0.65, 0.75, 0.85, 0.95, 1) #  consider a range of possible values

N.time.steps <- 3000 # ??

S <- numeric(N.time.steps+1) 
I <- numeric(N.time.steps+1) 
S[1] <- S0[5] 
I[1] <- I0[7] 
for (i in 1:N.time.steps){ 
  S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
  I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
} 
time.vector <- seq(0,N.time.steps*delta.t,by=delta.t) 
plot(time.vector,I,type="l",xlab="Time",ylab="I")
