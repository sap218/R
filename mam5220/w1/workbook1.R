# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 1 - Epidemiology
# Samantha Pendleton - sap21

library(ggplot2)
library(readr)
# https://stackoverflow.com/questions/5293715/how-to-use-greek-symbols-in-ggplot2

# Question 2
# a)
epid <- read.csv("~/git/R/R/mam5220/w1/epidemiology.data.csv") # for laptop
epid <- read_csv("/aber/sap21/git/R/R/mam5220/w1/epidemiology.data.csv") # for computer
qplot(epid$Week, epid$No.infected, xlab="Week", ylab="Infected", ylim=c(0,100000), main="Number of Infected over 30 weeks \nΝ=1000000, R0=10, γ=1/14, Δt=0.1", geom = c("point","line")) 

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
  scale_y_continuous(labels=comma, name="Infected", breaks=seq(0,700000,by=50000)) + labs(color="theta θ") + xlab("Week") + 
  labs(title = "Number of Infected over 30 Weeks with SIR Model", subtitle = "Ν=1000000, R0=10, γ=1/14, Δt=0.1, S0=θΝ") +
  scale_x_continuous(breaks=seq(0,30,by=1))

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

# d)
theta <- seq(0,1.00, by=0.01)
theta.x <- theta
for (i in 1:length(theta)) {
  theta[i] <- errorSS(epid = epid, theta = theta[i])
}
#qplot(theta.x, theta)
par(mfrow=c(1,2))
plot(theta.x,theta, main="insert title", type="l") # error sum of squares against theta 
plot(theta.x,log(theta), main="insert title (log)") # log esos against theta
     
###########################

# e)  DO THESE
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
