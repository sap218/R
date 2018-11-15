library(class)
library(readr)

file <- readLines("stdin", n=1)
df <- read.csv(file)
#df <- read_csv("database.csv")

#PART A
##compute the distance to each row to 0,0,0. For each row, print using: cat(distance_i, "\n")

df$distancefrom0 <- sqrt(((df$X1-0)^2)+((df$X2-0)^2)+((df$X3-0)^2))
### cat(df$distancefrom0, sep=" \n")

for (distance in c(df$distancefrom0)){
  cat(distance, "\n")
}

#PART B
##give the prediction (Y) for K = 1
##print using: cat(as.character(prediction), "\n")

k.1 <- knn(train=cbind(df$X1,df$X2,df$X3), test=c(0,0,0), cl=df$Y, k=1)
cat(as.character(k.1), "\n")

#PART C
##give the prediction (Y) for K = 3
##print using: cat(as.character(prediction), "\n")

k.3 <- knn(train=cbind(df$X1,df$X2,df$X3), test=c(0,0,0), cl=df$Y, k=3)
cat(as.character(k.3), "\n")
