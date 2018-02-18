# @18.02.18

sum(1, 3, 5)
rep(3, times=2)
example(qt) # shows an example - examples are quite random

list.files()
source("file.R") # runs a script

c(3, 4, 7) # vectors cannot hold different value types, they convert otherwise
9:5 # 9,8,7,6,5
seq(5, 9, 0.5)

r <- c(3, 5, NA, 3, 9) # NA values
sum(r, na.rm=TRUE)

################## VARIABLES

var <- c('a', 'b', 'c')
var[2]
var[4:6] <- c('d', 'e', 'f')
var[c(1,3)]
var[c(2:4)] # var[2:4]

a <- 1:3
names(a) <- c("first", "second", "third")
a
a["first"]
a["third"] <- 5
a["third"]
a

#################### VECTOR MATH

z <- c(3, 7, 12)
z + 1 # 4, 8, 13
z / 2 # 1.5, 3.5, 6.0
z * 3 # 9, 21, 36
z # 3,7,12
v # 6,9,2
z + x # 9, 16, 14

z == c(3, 8, 10) # TRUE FALSE FALSE
z < c(5, 10, 11) # TRUE TRUE FALSE

sin(z)
sqrt(z)


####################
#################### PLOTTING
####################


v <- c(6,9,2)
v
barplot(v)
names(v) <- c("england", "wales", "norway") # vessels from
v
barplot(v)

#################### SCATTER

x <- seq(1, 20, 0.1)
y <- sin(x)
plot(x, y)

x <- -10:10 # values
y <- abs(x) # absolutes
plot(x, y)


####################
#################### MATRICES
####################


matrix(0,3,4)
matrix(1,5,5)
matrix(1:12, 3, 4) # column-based

m <- 1:8
dim(m) <- c(2, 4) # 2 rows, 4 columns
m
m[2,3] # row 2, column 3
m[1,4] <- 0
m
m[1,] # all of row 1
m[,3] # all of column 3
m[, 2:4] # columns 2:4
m


e <- matrix(1, 10, 10)
e
e[4,6] <- 0
#e[,3] <- 2 # change column 3 values
#e[9,] <- 3 
e
contour(e)
persp(e)
persp(e, expand=0.25)


contour(volcano) # terrain flat
persp(volcano, expand = 0.75) # terrain 3D
image(volcano) #heatmap


####################
#################### SUMMARY STATS
####################


l <- c(4, 3, 4, 3, 7, 2, 2.5, 3.5) # length of bees
names(l) <- c("barry", "bonny", "brenda", "beatrice", "ben", "bob", "barbara", "becky")
l
barplot(l)
abline(h = mean(l), col="blue") # needs to be 'h' horizontal | v = vertical
abline(h = median(l), col="red")

p <- c(45000, 50000, 35000, 40000, 35000, 45000, 10000, 15000) # treasure loot
m <- mean(p)
d <- sd(p) # standard deviation: from mean to describe range of typical values showing how much they vary from average value
barplot(p)
abline(h = m, col="red")
text(m, "mean", col="red") 
abline(h = d, col="blue")
text(d, "sd", col="blue")
abline(h = m + d, col="purple")
text((m+d), "mean+sd", col="purple")
abline(h = m - d, col="orange")
text((m-d), "mean-sd", col="orange")


####################
#################### FACTORS
####################


s <- c('meerkat', 'lizard', 'owl', 'meerkat', 'owl')
t <- factor(s)
as.integer(t) # assigns strings to an integer based on alphabetically
#levels(t)
w <- c(110, 175, 100, 250, 55) # weight
p <- c(2000, 1000, 1200, 3200, 800) # price

plot(w, p, pch=as.integer(t))
# hint: see how plots first before applying legend
legend("bottomright", c("lizard", "meerkat", "owl"), pch=1:3)
legend("topleft", levels(t), pch=1:length(levels(t)))

# help(package = "ggplot2")
# install.packages("ggplot2")
library(ggplot2)
qplot(w, p, color=t) +ggtitle("pet shop")

####################
#################### DATA FRAMES
####################

df <- data.frame(w, p, t)
df[[2]] # checks column 2
df[["w"]]
df$t


###################


s <- read.csv("git/R/R/002/ships.csv")
#df.s <- data.frame(s[1], s[2], s[3]) 
#plot(df.s[,1], df.s[,2])
p <- read.table("git/R/R/002/ports.txt", sep="\t", header=TRUE) # if header
#df.p <- data.frame(p[1], p[2])
#plot(df.p[,1], df.p[,2])

m <- merge(x = s, y = p)
plot(m$port, m$population)
legend("topleft", c("aber", "liv", "mad", "nor"))
plot(m$port, m$income)
plot(m$port, m$ships)

plot(m$population, m$income)
#legend("topleft", c("aber", "liv", "mad", "nor"), pch=1:4)
cor.test(m$population, m$income) # if less than 0.05 then results are considered significant
l <- lm(m$income ~ m$population)
abline(l)