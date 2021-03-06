# sap21 _ MAM5120 _ week 2 practical

dbinom(2,10,1/6) # probability of rolling 2 sixes when dice rolled 10 times
pbinom(4,20,0.5, lower.tail=FALSE) # chance of getting 4 of fewer heads in 20 tosses P(X<=4)
# lower.tail if FALSE P(X>4)

dpois(4,7) # gives P(X=4) if X~Po(7)
ppois(4,7) # gives P(X<=4) if X~Po(7)
ppois(15,50,lower.tail = FALSE) # gives P(X>15) if X~Po(50)

x <- seq(0,25,1)
plot(dbinom(x,25,0.5),type="h") # type h makes makes not plots
plot(ppois(x,40),type="s") # type s makes line graph

###################################
###################################

attach(iris)

plot1 <- ggplot(iris, aes(x=Petal.Width,y=Petal.Length))
plot1 <- plot1+geom_point()
plot1 <- plot1 + ggtitle("Petal Dimensions for 3 Species of Iris")+labs(x="Petal Width", y="Petal Length")
plot2 <- plot1 + geom_point(aes(colour=Species)) # adds colour
plot3 <- plot2 + facet_grid(~Species) # separates them
plot4 <- plot3
plot4 <- plot1 + geom_point(aes(colour=Species,size=Sepal.Length)) # thicker plots

plot5 <- ggplot(iris, aes(x=Species,y=Petal.Width))
plot5 <- plot5+geom_point()
plot5 <- plot5 + ggtitle("Petal Width for 3 Species of Iris")+labs(x="Species", y="Petal Width")
plot5 <- plot5 + geom_point(aes(colour=Petal.Length)) 

##################

par(mfrow=c(1,1))
with(iris, {
  plot(Petal.Width,Petal.Length,col=Species)
  legend(x="bottomright", legend=unique(Species),col=1:3,pch=1)
})

par(mfrow=c(1,1))
with(iris,
     plot(Petal.Width,Petal.Length,col=Species,cex=Sepal.Length/min(Sepal.Length),pch=19)
) ## thicker plots but graphs together

####### three graphs of each species petal width on length

xlim<-range(iris$Petal.Width)
ylim<-range(iris$Petal.Length)

par(mfrow=c(1,3)) 
with(subset(iris,Species=="setosa"),
     {
       plot(Petal.Width,Petal.Length,xlim=xlim,ylim=ylim,col=1,main="setosa") 
     })
with(subset(iris,Species=="versicolor"),
     {
       plot(Petal.Width,Petal.Length,xlim=xlim,ylim=ylim,col=2,main="versicolor")
     })
with(subset(iris,Species=="virginica"),
     {
       plot(Petal.Width,Petal.Length,xlim=xlim,ylim=ylim,col=3,main="virginica")
     })

###########

par(mfrow=c(1,2)) 
with(iris, {
  plot(Petal.Width,Sepal.Width,col=Species)
  legend(x="topright", legend=unique(Species),col=1:3,pch=1)
})
with(iris, {
  plot(Petal.Length,Sepal.Length,col=Species, abline(lm(Sepal.Length~Petal.Length), col="blue"))
  legend(x="topleft", legend=unique(Species),col=1:3,pch=1)
})

###################################

#data(malaria/tartar/etc)

malariaHist <- ggplot(malaria,aes(log(parasites)))+ geom_histogram(binwidth = 0.5)
malariaHist <- malariaHist + labs(x="number of parasites",y="frequency")
malariaHist

with(malaria,
     {
       hist(parasites,breaks=10,xlab="num of parasites",ylab="freq") #change breaks num
     })

tarplot <- ggplot(tartar, aes(x=treat,y=index))+geom_boxplot()
chickplot <- ggplot(chickwts,aes(x=feed,y=weight))+geom_point(shape=2,size=3,colour="red")

###################################
################################### EXERCISES
###################################

#exercise 1
attach(dogwhelks)
plot1 <- ggplot(dogwhelks, aes(x=Width,y=Length))+geom_point()
plot2 <- ggplot(dogwhelks, aes(x=Width,y=Weight))+geom_point()
plot3 <- ggplot(dogwhelks, aes(x=Location,y=Length))+geom_point()
plot4 <- ggplot(dogwhelks, aes(x=Colour,y=Weight))+geom_point()

#exercise 2
#(a)
par(mfrow=c(1,1)) 
d <- dbinom(x,100,0.1)
p <- ppois(x,10)
plot(d,type="l",col="red")
#par(new=TRUE) #may or may not be needed?
lines(p,col="green")

# par(mfrow=c(1,2)) make both side by side
# plot(dbinom(x,100,0.1), col="red")
# plot(ppois(x,10), col="green")

#(b)
#--

#exercise 3
#(a)
par(mfrow=c(1,1)) 
numheads <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
freq <- c(3, 5, 11, 11, 6, 3, 1, 0, 0)
hist(numheads,freq,ylim=c(0,0.5))

df <- data.frame(numheads = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
                 freq = c(3, 5, 11, 11, 6, 3, 1, 0, 0))
hist(df$numheads,freq)
with(df, hist(freq))
