# sap21 _ MAM5120 _ week 1 practical

(5-12)*3
log(exp(2))
sin(2*pi)
sqrt(16)

x <- 3*x-1
y <- 10
z <- c(1:5) #concatenate
rm(x,y,z)

x <- c(1,3,5,8,12)
y <- seq(3,27,by=3)
abs(length(x) - length(y))
boxplot(x,y)

######################## Whales

whales <- c(74, 122, 235, 111, 292, 111, 211, 133, 156, 79) #lengths

whales[3:7]
length(whales)

median(whales)
sum(whales)
sum(whales)/length(whales) #mean(whales)
sd(whales) #standard deviation

w_len <- c(69, 141, 231, 99, 301, 115, 209, 134, 153, 81)
w_ages <- c(1, 30, 55, 29, 61, 33, 22, 25, 26, 3)
plot(w_ages,w_len, abline(v=3))

boxplot(whales, w_len, w_ages, main="num, length, and ages", ylab="count", col.main="darkblue", col.lab="darkred")

hist(w_ages)

######################## Function

function1 <- function(arg1,arg2) {
  out <- arg1 * arg2
  return(out)
}

######################## Trees exercise

# import dataset Trees
attach(Trees) # to avoid excessive use of $
summary(Volume)
#summary(log(Trees$Volume)) # if no attachment
boxplot(Volume,Diameter,Height)
plot(Diameter,Volume)
##
treeFormula <- pi*((Diameter/24)^2)*Height #surface area
plot(Volume,treeFormula, main="Calculated volume against measured volume for 31 trees", ylab="Surface Area")
detach(Trees) 

######################## Iris exercise

attach(iris)
summary(iris$Sepal.Length)
plot(Sepal.Width, Sepal.Length, abline(lm(Sepal.Length ~ Sepal.Width)), col=rgb(0,100,0,50,maxColorValue=255), pch=19, cex=1.3)
# abline adds regression, colours idk?; pch makes solid, cex makes bigger
boxplot(Sepal.Width~Species,notch=TRUE)
# if notch TRUE a notch is drawn in side of boxes, if notches of two plots don't overlap is ‘strong evidence’ that two medians differ 
detach(iris)