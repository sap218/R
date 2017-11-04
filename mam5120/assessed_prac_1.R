# sap21 _ MAM5120 _ Assessed Practical 1 _ 1st November 2017

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

#################
#################

# Question 2
# wingspan of adult robins
N <- 38 # sample population of birds
x <- 10.3 # sample mean
s <- 4.1^2 # variance

# I - construct a 95% confidence interval for population mean
sigma <- 4.1^2 # true population variance using z-test
sd <- sqrt(sigma)
zval <- 1.96

x - zval*(sd/sqrt(N))
x + zval*(sd/sqrt(N))

# II - p value for null hypothesis against alternative
# H0: mu = 10 # population mean
# Ha: mu > 10
z <- (x - 10)/(sd/sqrt(N)) # 10 is H0 || z-test
p <- pnorm(z, lower.tail = FALSE)

# III 
# H0: mu = 10.7
# Ha: mu =/ 10.7
z <- (x - 10.7)/(sd/sqrt(N))
p <- 2*pnorm(z)

# IV

###############
###############
###############

# Question 3
N = 37
x = 7.42
S = 0.49^2

# I - standard error
se <- sqrt(S)

# II - distribution is t-test

# III
x - zval*(sqrt(S)/sqrt(N))
x + zval*(sqrt(S)/sqrt(N))







