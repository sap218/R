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
epid <- read.csv("~/git/R/R/mam5220/w1/epidemiology.data.csv") # for laptop directory
epid <- read_csv("/aber/sap21/git/R/R/mam5220/w1/epidemiology.data.csv") # for university computers
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

# Question 3

# a)
N <- 5000000 
R0 <- 20
gamma <- 0.1
delta.t <- 0.1
beta <- R0*gamma/N

############

# b) 
theta <- c(0.01, 0.1, 0.2, 0.25)
I0 <- c(1,100,10000,1000000)
N.time.steps <- (70*30)  
time.vector <- seq(0,30,by=30/N.time.steps) # days | time index

infecteds <- list()

for (k in 1:length(I0)) {
  I0.current <- I0[k]

  infecteds[[k]] <- list()  
  for (j in 1:length(theta)) {
    theta.current <- theta[j]
    
    S0 <- theta.current*N
    S <- numeric(N.time.steps+1)  
    I <- numeric(N.time.steps+1) 
    S[1] <- S0  
    I[1] <- I0.current
    for (i in 1:N.time.steps){  
      S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
      I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
    } 
    infecteds[[k]][[j]] <- I
  }
}

p1 <- ggplot() + geom_line(aes(time.vector,infecteds[[1]][[1]], color="0.01")) + geom_line(aes(time.vector,infecteds[[1]][[2]], color="0.1")) + 
  geom_line(aes(time.vector,infecteds[[1]][[3]], color="0.2")) + geom_line(aes(time.vector,infecteds[[1]][[4]], color="0.25")) +
  scale_y_continuous(labels=comma, name="Infected", limits=c(0,1750000)) + labs(color="theta θ") + xlab("Week") + 
  labs(subtitle = "I0 = 1") + scale_x_continuous(breaks=seq(0,30,by=1))

p2 <- ggplot() + geom_line(aes(time.vector,infecteds[[2]][[1]], color="0.01")) + geom_line(aes(time.vector,infecteds[[2]][[2]], color="0.1")) + 
  geom_line(aes(time.vector,infecteds[[2]][[3]], color="0.2")) + geom_line(aes(time.vector,infecteds[[2]][[4]], color="0.25")) +
  scale_y_continuous(labels=comma, name="Infected", limits=c(0,1750000)) + labs(color="theta θ") + xlab("Week") + 
  labs(subtitle = "I0 = 1000") +
  scale_x_continuous(breaks=seq(0,30,by=1))

p3 <- ggplot() + geom_line(aes(time.vector,infecteds[[3]][[1]], color="0.01")) + geom_line(aes(time.vector,infecteds[[3]][[2]], color="0.1")) + 
  geom_line(aes(time.vector,infecteds[[3]][[3]], color="0.2")) + geom_line(aes(time.vector,infecteds[[3]][[4]], color="0.25")) +
  scale_y_continuous(labels=comma, name="Infected", limits=c(0,1750000)) + labs(color="theta θ") + xlab("Week") + 
  labs(subtitle = "I0 = 10,000") +
  scale_x_continuous(breaks=seq(0,30,by=1))

p4 <- ggplot() + geom_line(aes(time.vector,infecteds[[4]][[1]], color="0.01")) + geom_line(aes(time.vector,infecteds[[4]][[2]], color="0.1")) + 
  geom_line(aes(time.vector,infecteds[[4]][[3]], color="0.2")) + geom_line(aes(time.vector,infecteds[[4]][[4]], color="0.25")) +
  scale_y_continuous(labels=comma, name="Infected", limits=c(0,1750000)) + labs(color="theta θ") + xlab("Week") + 
  labs(subtitle = "I0 = 1,000,000") +
  scale_x_continuous(breaks=seq(0,30,by=1))

grid.arrange(p1, p2, p3, p4, ncol=1, top="Number of Infected over 30 Weeks with SIR Model \nΝ=5000000, R0=20, γ=0.1, Δt=0.1, S0=θΝ")

#################################################################################
#################################################################################
#################################################################################

# Question 4

# b) - diphtheria

generate.S.I.by.time.vital.dynamics <- function(N.time.steps, delta.t=0.1, S0, I0, R0, gamma, mu, N) {
  S <- numeric(N.time.steps+1)
  I <- numeric(N.time.steps+1)
  S[1] <- S0
  I[1] <- I0
  beta <- R0*gamma/N
  for (i in 1:N.time.steps) {
    S[i+1] <- S[i]+mu*N*delta.t-beta*S[i]*I[i]*delta.t-mu*S[i]*delta.t
    I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t-mu*I[i]*delta.t
  }
  time.vector <- seq(0, N.time.steps*delta.t, by=delta.t)
  out <- list(S=S, I=I, time.vector=time.vector)
  return(out)
}

N <- 800000
S0 <- 0.02*N
I0 <- 50
gamma <- 0.1
delta.t <- 0.1

N.time.steps <- 20000

R0 <- 6
#B <- (30*(N/1000)/365) # daily birth number
B.rate <- ((30/1000)/365)*10 # daily birth rate, 10 daily timesteps
# B = mu*N | mu = B/N | mu=(B/N*10)
data <- generate.S.I.by.time.vital.dynamics(N.time.steps=N.time.steps, S0=S0, I0=I0, R0=R0, gamma=gamma, mu=(B.rate), N=N)
qplot(data$time.vector, data$I, xlab="Time Steps", ylab="Infected Count", 
     main=("Extending SIR model to include birth and death rates \nΝ=800000, R0=6, γ=0.1, Δt=0.1, S0=0.02*Ν, I0=50, birth rate is 30 per 1000"),
     geom = c("point","line"))

#############

# c)
R0 <- c(6, 8, 10, 12)
N.time.steps <- 20000
B.rate <- ((30/1000)/365)*10 
data.r01 <- generate.S.I.by.time.vital.dynamics(N.time.steps=N.time.steps, S0=S0, I0=I0, R0=R0[1], gamma=gamma, mu=(B.rate), N=N)
data.r02 <- generate.S.I.by.time.vital.dynamics(N.time.steps=N.time.steps, S0=S0, I0=I0, R0=R0[2], gamma=gamma, mu=(B.rate), N=N)
data.r03 <- generate.S.I.by.time.vital.dynamics(N.time.steps=N.time.steps, S0=S0, I0=I0, R0=R0[3], gamma=gamma, mu=(B.rate), N=N)
data.r04 <- generate.S.I.by.time.vital.dynamics(N.time.steps=N.time.steps, S0=S0, I0=I0, R0=R0[4], gamma=gamma, mu=(B.rate), N=N)

p1 <- ggplot() + geom_line(aes(data.r01$time.vector, data.r01$I)) +
  scale_y_continuous(labels=comma, name="Infected") + xlab(" ") + labs(subtitle = "R0 = 6") 
p2 <- ggplot() + geom_line(aes(data.r02$time.vector, data.r02$I)) +
  scale_y_continuous(labels=comma, name="Infected") + xlab(" ") + labs(subtitle = "R0 = 8")
p3 <- ggplot() + geom_line(aes(data.r03$time.vector, data.r03$I)) +
  scale_y_continuous(labels=comma, name="Infected") + xlab(" ") + labs(subtitle = "R0 = 10")
p4 <- ggplot() + geom_line(aes(data.r04$time.vector, data.r04$I)) +
  scale_y_continuous(labels=comma, name="Infected") + xlab("Time Steps") + labs(subtitle = "R0 = 12")
grid.arrange(p1, p2, p3, p4, ncol=1, top="Number of Infected with SIR Model extended by including birth/death rates \nΝ=800000, γ=0.1, Δt=0.1, S0=0.02*Ν, I0=50, time steps=20000, birth rate is 30 per 1000")

ggplot() + geom_line(aes(data.r01$time.vector,data.r01$I, color="a) 6")) + geom_line(aes(data.r02$time.vector,data.r02$I, color="b) 8")) + 
  geom_line(aes(data.r03$time.vector,data.r03$I, color="c) 10")) + geom_line(aes(data.r04$time.vector,data.r04$I, color="d) 12")) +
  scale_y_continuous(labels=comma, name="Infected") + labs(color="R0") + xlab("Time Steps") + 
  labs(title = "Number of Infected with SIR Model extended by including birth/death rates", subtitle = "Ν=800000, γ=0.1, Δt=0.1, S0=0.02*Ν, birth rate is 30 per 1000")

#############

# d)
R0 <- 10
B <- c(20,60,100,140)
B.rate <- ((B/1000)/365)*10
data.b1 <- generate.S.I.by.time.vital.dynamics(N.time.steps=N.time.steps, S0=S0, I0=I0, R0=R0, gamma=gamma, mu=(B.rate[1]), N=N)
data.b2 <- generate.S.I.by.time.vital.dynamics(N.time.steps=N.time.steps, S0=S0, I0=I0, R0=R0, gamma=gamma, mu=(B.rate[2]), N=N)
data.b3 <- generate.S.I.by.time.vital.dynamics(N.time.steps=N.time.steps, S0=S0, I0=I0, R0=R0, gamma=gamma, mu=(B.rate[3]), N=N)
data.b4 <- generate.S.I.by.time.vital.dynamics(N.time.steps=N.time.steps, S0=S0, I0=I0, R0=R0, gamma=gamma, mu=(B.rate[4]), N=N)

p1 <- ggplot() + geom_line(aes(data.b1$time.vector, data.b1$I)) +
  scale_y_continuous(labels=comma, name="Infected", limits=c(0,85000)) + xlab(" ") + labs(subtitle = "B = 20")
p2 <- ggplot() + geom_line(aes(data.b2$time.vector, data.b2$I)) +
  scale_y_continuous(labels=comma, name="Infected", limits=c(0,85000)) + xlab(" ") + labs(subtitle = "B = 60")
p3 <- ggplot() + geom_line(aes(data.b3$time.vector, data.b3$I)) +
  scale_y_continuous(labels=comma, name="Infected", limits=c(0,85000)) + xlab(" ") + labs(subtitle = "B = 100")
p4 <- ggplot() + geom_line(aes(data.b4$time.vector, data.b4$I)) +
  scale_y_continuous(labels=comma, name="Infected", limits=c(0,85000)) + xlab("Time Steps") + labs(subtitle = "B = 140")
grid.arrange(p1, p2, p3, p4, ncol=2, top="Number of Infected with SIR Model extended by including birth/death rates \nΝ=800000, R0=10, γ=0.1, Δt=0.1, S0=0.02*Ν, I0=50")

#######################################

# e)
R0 <- 10
B <- seq(20, 140, by=10)
B.rate <- ((B/1000)/365)*10

times <- list()
peaks <- list()
equ <- list()

for (i in 1:length(B)) {
  data <- generate.S.I.by.time.vital.dynamics(N.time.steps=N.time.steps, S0=S0, I0=I0, R0=R0, gamma=gamma, mu=(B.rate[i]), N=N)
  times[[i]] <- head((which(diff(sign(diff(data$I)))==-2)+1)/10, 4) # where peaks are on x-axis
  peaks[[i]] <- head(data$I[which(diff(sign(diff(data$I)))==-2)+1], 4) # values of peaks
  equ[[i]] <- mean(tail(data$I,100))
}

par(mfrow=c(3,1))
plot(B[1], equ[[1]], xlim=c(20,140), ylim=c(0,30000), xlab=" ", ylab="Equilibrium Approximation",
     main="Approximate equilibrium, first four infection and time peaks for varying birth rate; time steps=20000 \nΝ=800000, R0=10, γ=0.1, Δt=0.1, S0=0.02*Ν, I0=50")
for (i in 2:length(B)) {
  points(B[i], equ[[i]])
}
plot(rep(B[1], 4), peaks[[1]], xlim=c(20,140), ylim=c(0,max(peaks[[13]])), xlab=" ", ylab="Infection Peaks")
for (i in 2:length(B)) {
  points(rep(B[i], 4), peaks[[i]])
}
plot(rep(B[1], 4), times[[1]], xlim=c(20,140), ylim=c(0,max(times[[1]])), xlab="B (births per 1000)", ylab="Time Peaks")
for (i in 2:length(B)) {
  points(rep(B[i], 4), times[[i]])
}