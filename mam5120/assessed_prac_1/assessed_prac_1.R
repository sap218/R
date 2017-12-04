# sap21 _ MAM5120 _ Assessed Practical 1 _ 1st November 2017

#################
# Question 2
# wingspan of adult robins
N <- 38 # sample population of birds
x <- 10.3 # sample mean
s <- 4.1^2 # variance

# I - construct a 95% confidence interval for population mean
sigma <- 4.1^2 # true population variance using z-test
sd <- sqrt(sigma)
zval <- 1.96 # 95%
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
zval <- 2.59 # 99 %
x - zval*(sd/sqrt(N))
x + zval*(sd/sqrt(N))

#################
#################
# Question 3
N = 37
x = 7.42
S = 0.49^2

# I - standard error
se <- sqrt(S)

# II = distribution is t-test

# III
x - qt(0.975, (N-1))
x + qt(0.975, (N-1))

# IV 
x - qt(0.995, (N-1))
x + qt(0.995, (N-1))

# V
# H0: mu = 7.7
# Ha: mu < 7.7
zval <- 2.59 # 99 %
x - zval*(se/sqrt(N))
x + zval*(se/sqrt(N))

# VI
z <- (x - 7.7)/(se/sqrt(N))
p <- 2*pnorm(z)

#################
#################
# Question 4
irish <- 120
scottish <- 130
# I = X~Bin(120,p)

# II
# H0: p = 0.57
# Ha: p > 0.57
p <- 0.57
z <- 1.64
pp <- p + z*sqrt(((1/120)*p)*(1-p))
pp * irish

# III
pbinom(72, irish, p, lower.tail = FALSE)

# IV
o <- 71
mu <- o/scottish
s <- sqrt((mu*(1-mu))/scottish)
zval <- 1.96
mu - zval*(s/sqrt(scottish))
mu + zval*(s/sqrt(scottish))