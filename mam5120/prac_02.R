#sap21 - wednesday 11th october @ 08:58

dbinom(2,10,1/6) # probability of rolling 2 sixes when dice rolled 10 times
pbinom(4,20,0.5, lower.tail=FALSE) # chance of getting 4 of fewer heads in 20 tosses P(X<=4)
# lower.tail if FALSE P(X>4)

dpois(4,7) # gives P(X=4) if X~Po(7)
ppois(4,7) # gives P(X<=4) if X~Po(7)
ppois(15,50,lower.tail = FALSE) # gives P(X>15) if X~Po(50)

x <- seq(0,25,1)
plot(dbinom(x,25,0.5),type="h") # type h makes makes not plots
plot(ppois(x,40),type="s") # type s makes line graph

#############################################

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
###################################
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
