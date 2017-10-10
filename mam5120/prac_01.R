#sap21 @ Saturday 7th October 18:19

(5-12)*-3
5^4
(7-3)^-2

sqrt(2)
sin(2*pi)
log(exp(2))

x <- 2
y <- 3*x - 1
x <- c(1:5)
rm(x,y) #removes variables

#########################

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

######

x <- c(1,3,5,8,12)
y <- seq(3,27,by=3)
abs(length(x) - length(y))
boxplot(x,y)

#########################
######################### function

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