# modelling epidemics

# R code for Euler's method

S0 <- 9000 # Starting value for susceptibles
I0 <- 50 # Starting value for infectious 
N <- 10000 # Total population size 
beta <- 0.0001 # infection rate 
gamma <- 1/50 # recovery date 
delta.t <- 0.1 # Small time increment 
N.time.steps <- 3000 # Number of time steps 
S <- numeric(N.time.steps+1) # Blank vector to receive S values 
I <- numeric(N.time.steps+1) # Blank vector to receive I values 
S[1] <- S0 # Initial value for S 
I[1] <- I0 # Initial value for I 
for (i in 1:N.time.steps){ # Iterative loop to define the next value of S and I 
  S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
  I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
  } 
time.vector <- seq(0,N.time.steps*delta.t,by=delta.t) # The time points
plot(time.vector,I,type="l",xlab="Time",ylab="I")

############################

# I against S plots 

N <- 1000 
S <- 1:N 
S0 <- N-1 
I0 <- 1 
gamma <- 0.2 
beta.vector <- c(0.0004,0.0008,0.0016,0.0032) 
#gamma/beta.vector = 500.0 250.0 125.0  62.5 # Four values of gamma/beta
#I.matrix <- matrix(0,length(beta.vector),N) # Empty matrix for I values
#for (i in 1:length(beta.vector)){ 
#  I.matrix[i,]<-I0+S0-S+gamma/beta.vector[i]*log(S/S0) # Calculation
#  } 
#ylim <-range(I.matrix) 

#plot(S,I.matrix[1,],type="l",col=1,ylim=ylim,xlim=c(0,N),xlab="S",ylab="I") 
#for (i in 2:length(beta.vector)){ 
#  points(S,I.matrix[i,],type="l",col=i) 
#  } 
#title("I against S, first attempt") 
#legend(x="bottomright",legend=c("g/b=500","g/b=250","g/b=125","g/b=62.5"),lwd=2,lty=1,col=1:4) 

# Eliminating negative values in I values 
index <- which(I.matrix[1,]>=0) # Only focusing on positive values 
ylim <- c(0,max(I.matrix)) 
plot(S[index],I.matrix[1,index],type="l",lwd=2,xlab="S",ylab="I",ylim=ylim,xlim=c(0,N)) 
for (i in 2:length(beta.vector)){ 
  index<-which(I.matrix[i,]>=0) 
  points(S[index],I.matrix[i,index],type="l",lwd=2,col=i) 
} 
title("I against S, improved") 
legend(x="topright",legend=c("g/b=500","g/b=250","g/b=125","g/b=62.5"),col=1:4,lty=1,lwd=2) 
