# sap21 _ MAM5120 _ week 6 practical

# question 1
men <- c(173.2, 176.5, 179.3, 172.2, 179.0, 178.2, 178.4, 183.6) # 8 
mean <- (sum(men))/8
t.test(men, mu = mean, alternative = "greater")

# question 2
N <- 20
ctrl <- c(16.54, 18.35, 11.94, 21.95, 19.72, 19.6, 18.69, 16.76, 17.65, 21.71, 20.33, 16.35, 14.12, 12.19, 15.32, 15.22, 11.28, 21.26, 10.97, 17.59)
treat <- c(22.26, 19.37, 16.04, 17.06, 18.93, 19.44, 20.25, 20.52, 23.28, 26.04, 24.09, 19.81, 16.7, 19.53, 23.6, 26.35, 25.15, 18.91, 20.1, 29.34)
t.test(ctrl, treat, alternative = "two.sided")
t.test(ctrl, treat, alternative = "two.sided", paired = TRUE)

# question 3
mu1 <- 100
mu2 <- 110
sigma1 <- 5
sigma2 <- 13
N1 <- 13
N2 <- 18
x1 <- rnorm(N1,mu1,sigma1)
x2 <- rnorm(N2,mu2,sigma2)
var.test(x1,x2)
t.test(x1, x2, alternative = "two.sided")

# question 4
bdrug <- c(83.7, 90.4, 80.6, 88.7, 75.1, 89, 96.6, 99.5, 81.3, 87, 82.5, 90.3)
adrug <- c(82, 93.6, 78.1, 84.9, 74, 87.3, 94.2, 99.1, 81.1, 85.5, 80.1, 85.3)
t.test(bdrug, adrug, alternative = "two.sided")
t.test(bdrug, adrug, alternative = "two.sided", paired = TRUE)

# question 5
adhd <- 26/374
x1 <- 26
n1 <- 374
placebo <- 8/210
x2 <- 8
n2 <- 210
n <- n1 + n2
mu <- n/n
p.hat <- (x1+x2)/(n1+n2)
z <- ((adhd)-(placebo))/(sqrt(((p.hat)*(1-(p.hat)))*((1/n1)+(1/n2))))
pnorm(z, 0, 1, lower.tail = FALSE)

# question 6
stand <- c(3.05, 2.29, 3.54, 4.14, 2.71, 3.11, 3.02, 4.22, 3.48, 2.13)
suppl <- c(2.67, 2.42, 4.49, 4.79, 1.91, 4.11, 4.01, 5.05, 1.82, 3.19)
t.test(stand, suppl, alternative = "two.sided", paired = TRUE)
