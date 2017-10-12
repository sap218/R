#sap21 thurs 12 october @ 14:25

#exercise 1
attach(dogwhelks)
plot1 <- ggplot(dogwhelks, aes(x=Width,y=Length))+geom_point()
plot2 <- ggplot(dogwhelks, aes(x=Width,y=Weight))+geom_point()
plot3 <- ggplot(dogwhelks, aes(x=Location,y=Length))+geom_point()
plot4 <- ggplot(dogwhelks, aes(x=Colour,y=Weight))+geom_point()

#exercise 2
plot(dbinom(x,100,0.1))
plot(ppois(x,10))
