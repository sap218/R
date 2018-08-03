# manova

# R code for carrying out 1-way MANOVA on anteater data set 

logx1 <- c(2.068,2.068,2.090,2.097,2.117,2.140,2.045,2.076,2.090,2.111,2.093,2.100,2.104) 
logx2 <- c(2.070,2.074,2.090,2.093,2.125,2.146,2.054,2.088,2.093,2.114,2.098,2.106,2.101) 
logx3 <- c(1.580,1.602,1.613,1.613,1.663,1.681,1.580,1.602,1.643,1.643,1.653,1.623,1.653) 
logx <- cbind(logx1,logx2,logx3)
# Putting the columns together
location<-rep(c("MinasGraes","MattoGrosso","SantaCruz"),c(6,4,3)) 
anteater.man <- manova(logx~location) 
summary(anteater.man) 

#############################

# IRIS

head(iris)
iris.man <- manova(cbind(iris$Sepal.Length,iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width)~iris$Species) 
summary(iris.man) 
summary(iris.man,test="Wilks") 
iris.man$coef 
#summary.aov(manova(as.matrix(iris.response)~iris$Species)) 