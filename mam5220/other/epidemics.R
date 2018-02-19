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


###############################################
###############################################


# Workbook Question 1


# Question 1
# a)
# Modify the above code to find the number of susceptibles and infectious
# (which will not necessarily be whole numbers), after 10 time units
# if we start off with 5000 susceptibles, 100 infectious and 900 removed (i.e. N=6000),
# using the following parameter values beta = 0.00008, gamma = 0.02
S0 <- 5000 # Starting value for susceptibles
I0 <- 100 # Starting value for infectious 
N <- 6000 # Total population size 
beta <- 0.00008 # infection rate 
gamma <- 0.02 # recovery date 
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

# b)
# Produce side by side plots, with suitable labeling,
# of the number of susceptibles against time
# and the number of infectious against t over the time interval [0,200] 
N.time.steps <- 2000
S <- numeric(N.time.steps+1)  
I <- numeric(N.time.steps+1)  
S[1] <- S0 
I[1] <- I0 
for (i in 1:N.time.steps){ 
  S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
  I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
} 
time.vector <- seq(0,N.time.steps*delta.t,by=delta.t) 
time.point <- min(which(S<S0/2))*delta.t # calculate the time point at which the number of susceptibles
# first drops below S0/2 (i.e. half of the initial number of susceptibles)
par(mfrow=c(1,2)) 
plot(time.vector,S,type="l",xlab="Time",ylab="S", ylim=c(0, 6000))
points(time.point, 200)
plot(time.vector,I,type="l",xlab="Time",ylab="I", ylim=c(0, 6000)) # ylim=c(0,6000)
points(time.point, 200)

# c)
# To see how varying the infection rate, β, affects the time for the number of susceptibles to drop
# below half of its initial value, produce a plot of time.point (on the y-axis) against β (x-axis)
# for 100 equally spaced values of β between 0.00005 and 0.0003 (with γ held constant)
# (Set up a vector of βs and calculate time.point for each element of the vector using a for loop.)
beta <- seq(from=0.00005, to=0.0003, by=0.00005)
time.point <- min(which(S<S0/2))*delta.t
par(mfrow=c(1,1)) 
#plot(beta,time.vector,type="o",xlab="beta",ylab="time") # doesn't work
plot(0.00008,time.point,type="o",xlab="beta",ylab="time", xlim=c(0.00005,0.0003))

# d)
# For what value of β does the time for the number of susceptibles to drop
# below half of its initial value first fall below 100 time units?
time.point <- min(which(S<S0/100))*delta.t