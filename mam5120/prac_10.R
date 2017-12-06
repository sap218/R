# sap21 _ MAM5120 _ week 10 practical

ct <- 2*2
x2 <- 3.1
df <- (2-1)*(2-1) # want to compare x2 with a chi-squared on 2 degree of freedom
p <- pchisq(3.1,1,lower.tail = FALSE) # insufficient evidence at 5% level to reject null hypothesis that distribution across columns is independant of the category

# Question 1
ct <- 2*2
t.clay <- c(12,18)
o1 <- 12
o2 <- 18
t.sandy <- c(48,9)
o3 <- 48
o4 <- 9
t.sycamore <- c(12,48)
t.oak <- c(18,9)
t1 <- sum(t.clay)
t2 <- sum(t.sandy)
t3 <- sum(t.sycamore)
t4 <- sum(t.oak)
n <- sum(sum(t.clay)+sum(t.sandy)) #sum(sum(t.sycamore)+sum(t.oak))
# a)
H0 <- "distributions across columns is independant of the category"
# b)
e1 <- (t1*t3)/n
e2 <- (t1*t4)/n
e3 <- (t2*t3)/n
e4 <- (t2*t4)/n
# c)
x2S.one <- ((o1-e1)^2)/e1
x2S.two <- ((o2-e2)^2)/e2
x2S.three <- ((o3-e3)^2)/e3
x2S.four <- ((o4-e4)^2)/e4
x2S <- sum(x2S.one, x2S.two, x2S.three, x2S.four)
# d) chi-squared distribution on (r-1)*(c-1)
df <- (2-1)*(2-1)
# e) 
p <- pchisq(x2S,df,lower.tail = FALSE)
# f) is sufficient evidence to reject the null hypothesis

##################################################################

# Question 2
# a)
s.thigh <- c(4788, 30)
o1 <- 4788
o2 <- 30
s.arm <- c(8916, 76)
o3 <- 8916
o4 <- 76
s.nsr <- c(4788, 8916)
s.sr <- c(30, 76)
t1 <- sum(s.thigh)
t2 <- sum(s.arm)
t3 <- sum(s.nsr)
t4 <- sum(s.sr)
n <- sum(t1+t2)
e1 <- (t1*t3)/n
e2 <- (t1*t4)/n
e3 <- (t2*t3)/n
e4 <- (t2*t4)/n
g2.one <- (o1*(log(o1/e1)))
g2.two <- (o2*(log(o2/e2)))
g2.three <- (o3*(log(o3/e3)))
g2.four <- (o4*(log(o4/e4)))
g2 <- 2*sum(g2.one, g2.two, g2.three, g2.four)
# b) 
p <- pchisq(g2, 1, lower.tail = FALSE)

##################################################################

# Question 3
# a)
ct <- 3*2 # 3 rows, 2 columns
o1 <- 43
o2 <- 237
o3 <- 52
o4 <- 198
o5 <- 25
o6 <- 245
t1 <- 280
t2 <- 250
t3 <- 270
t4 <- 120
t5 <- 680
n <- 800
e1 <- (t1*t4)/n
e2 <- (t1*t5)/n
e3 <- (t2*t4)/n
e4 <- (t2*t5)/n
e5 <- (t3*t4)/n
e6 <- (t3*t5)/n
# a) null hypothesis is that distributions across columns is independant of the category
# b)
Flu <- matrix(c(43,52,25,237,198,245), ncol=2)
chisq.test(Flu) # X-squared=13.603, df=2, p-value=0.001112
# c) 'ncol' used in matrix to specify column (nrow)
# d) conclusion: sufficient evidence to reject null hypothesis

##################################################################

# Question 4
# a)
gc <- matrix(c(23,25,27,15,18,10,9,9,10), ncol = 3, nrow = 3)
chisq.test(gc) # X-squared=2.3387, df=4, p-value=0.6737
df <- 4
# b)
fisher.test(gc) # p-value=0.6674, Ha <- "alternative hypothesis: two.sided"
# c)
o1 <- 23
o2 <- 15
o3 <- 9
o4 <- 25
o5 <- 18
o6 <- 9
o7 <- 27
o8 <- 10
o9 <- 10
t1 <- 47
t2 <- 52
t3 <- 47
t4 <- 75
t5 <- 43
t6 <- 28
n <- 146
e1 <- (t1*t4)/n
e2 <- (t1*t5)/n
e3 <- (t1*t6)/n
e4 <- (t2*t4)/n
e5 <- (t2*t5)/n
e6 <- (t2*t6)/n
e7 <- (t3*t4)/n
e8 <- (t3*t5)/n
e9 <- (t3*t6)/n
g2.one <- (o1*(log(o1/e1)))
g2.two <- (o2*(log(o2/e2)))
g2.three <- (o3*(log(o3/e3)))
g2.four <- (o4*(log(o4/e4)))
g2.five <- (o5*(log(o5/e5)))
g2.six <- (o6*(log(o6/e6)))
g2.seven <- (o7*(log(o7/e7)))
g2.eight <- (o8*(log(o8/e8)))
g2.nine <- (o9*(log(o9/e9)))
g2 <- 2*sum(g2.one, g2.two, g2.three, g2.four, g2.five, g2.six, g2.seven, g2.eight, g2.nine)
# d)
p <- pchisq(g2, df, lower.tail = FALSE)