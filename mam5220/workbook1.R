# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 1 - Epidemiology
# Samantha Pendleton - sap21

library(ggplot2)

# Question 2
# a)
# Create a plot of number infected against week index with appropriate titles and axis labels
# Describe the epidemic curve
s <- read.csv("git/R/R/mam5220/epidemiology.data.csv")
df <- data.frame(s$Week, s$Time.index, s$No.infected)
qplot(df$s.Week, df$s.No.infected,type="l",xlab="week",ylab="Infected", main="Infection rate over 30 weeks", geom = c("point","line")) 


###########################

# b)

#par(mfrow=c(2,2))
theta <- c(0.25, 0.5, 0.75, 1)

I0 <- 61 # infectious 
N <- 1000000 # population
S0 <- theta[1]*N
gamma <- 1/14 # recovery date
beta <- (10*gamma) / N # infection rate - R0=10 basic reproduction number
delta.t <- 0.1 # small time increment 
N.time.steps <- (70*30) # Number of time steps 
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
#grid.arrange(S0.1, S0.2, S0.3, S0.4) # ncol=1
require(scales)
ggplot(subtitle="time") + geom_line(aes(time.vector,I.1, color="0.25")) + geom_line(aes(time.vector,I.2, color="0.5")) + 
  geom_line(aes(time.vector,I.3, color="0.75")) + geom_line(aes(time.vector,I.4, color="1")) +
  scale_y_continuous(labels=comma, name="num of infected") + labs(color="theta") + xlab("week")

