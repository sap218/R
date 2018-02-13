# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 1 - Epidemiology
# Samantha Pendleton - sap21

# Example
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

# Question 1
# a)
# Modify the above code to find the number of susceptibles and infectious
# (which will not necessarily be whole numbers), after 10 time units
# if we start off with 5000 susceptibles, 100 infectious and 900 removed (i.e. N=6000),
# using the following parameter values beta = 0.00008, gamma = 0.02
S0 <- 5000
I0 <- 100  
N <- 6000  
beta <- 0.00008 
gamma <- 0.02 
delta.t <- 0.1 
N.time.steps <- 300 
S <- numeric(N.time.steps+1) 
I <- numeric(N.time.steps+1) 
S[1] <- S0 
I[1] <- I0 
for (i in 1:N.time.steps){  
  S[i+1] <- S[i]-beta*S[i]*I[i]*delta.t
  I[i+1] <- I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
} 
time.vector <- seq(0,N.time.steps*delta.t,by=delta.t) 
plot(time.vector,I,type="l",xlab="Time",ylab="I")

# b)
# Produce side by side plots, with suitable labeling,
# of the number of susceptibles against time
# and the number of infectious against t over the time interval [0,200] 
S0 <- 5000
I0 <- 100
N <- 6000
beta <- 0.00008  
gamma <- 0.02 
delta.t <- 0.1 
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
plot(time.vector,I,type="l",xlab="Time",ylab="I", ylim=c(0, 4500)) # ylim=c(0,6000)
points(time.point, 200)
plot(time.vector,S,type="l",xlab="Time",ylab="S", ylim=c(0, 6000))
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


##############################################
##############################################

# Question 2
# a)
I0 <- 61 # infectious 
N <- 1000000 # population
S0 <- (N-I0) # susceptibles
gamma <- 1/14 # recovery date
beta <- 10/(N/gamma) # infection rate - R0=10 basic reproduction number
delta.t <- 0.1 # small time increment 
N.time.steps <- (7*300) # Number of time steps 
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
plot(time.vector,I,type="l",xlab="Time",ylab="I", ylim=c(0,1000000))

