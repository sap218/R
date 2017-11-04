# sap21 _ MAM5120 _ Assessed Practical 1 Practice _ 1st November 2017

# Question 1
# length of leg bones in chimpanzees - standard deviation is sqrt(variance)
m = 28 # average normal distribution
v = 16 # variance
# I - probability > 31.5cm
pnorm(31.5, mean = m, sd = sqrt(v), lower.tail = FALSE)
# II - probability < 26.1cm
pnorm(26.1, mean = m, sd = sqrt(v), lower.tail = TRUE)
# III - probability between 25 and 34
pnorm(25, 34, mean = 28, sd = (16/sqrt(100)))
pnorm(25, 34, mean = 28, sd = 4)
# IV - sample size N=15, find distribution of sample mean
N <- 15
pnorm(N,m,(v/sqrt(N))) 
# V - prob sample mean w/ N < 27cm
N <- 27
pnorm(N, mean = m, sd = sqrt(v), lower.tail = TRUE)
# VI - what is D, such prob sample mean is 15 is less than D is 95%
N <- 15
D <- m * v