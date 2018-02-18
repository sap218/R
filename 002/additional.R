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

#################### PLOTTING

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
