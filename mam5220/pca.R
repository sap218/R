# principle component analysis
library(MASS) # library for mvrnorm

# R code for PCA (2D)

# Step 1 generating and plotting data matrix 
N <- 50 # Number of observations 
mu <- c(1,3) # Centre of simulated data 
Sigma <- matrix(c(1,0.9,0.9,1),ncol=2) # Sigma is the population covariance matrix 
X.2d <- mvrnorm(N,mu,Sigma) # Simulating data from bivariate Normal distribution 
plot(X.2d,cex=2,xlab="X1",ylab="X2",main="X2 against X1") # Simulating data and plotting it 

# Step 2 applying PCA 
PCA.2d <- prcomp(X.2d) # prcomp does PCA on columns of X.2d 
ylim <- range(PCA.2d$x) 
xlim <- ylim # Define limits for axes so plot is square 
plot(PCA.2d$x,cex=2,xlab="PC1",xlim=xlim,ylim=ylim,ylab="PC2",main="PC2 against PC1") # PCA.2d$x contains the coordinates of the points with respect to the principal components  

# Step 3 Proportions of variability explained - percentages of variability explained by PCs 
PCA.2d <- prcomp(X.2d) # PCA as before 
proportions<-PCA.2d$sdev^2/sum(PCA.2d$sdev^2) # Calculate proportion of total variability explained by each principal component 

# Step 4 principal component loadings 
PCA.2d <- prcomp(X.2d) # PCA as before 
PCA.2d$rotation  

# R code for PCA, with and without scaling -- unsure if this part works
X.diff.var <- data.frame(X1,X2) 
PCA.diff.var.unscaled <- prcomp(X.diff.var) 
PCA.diff.var.scaled <- prcomp(X.diff.var,scale=TRUE) 
PCA.diff.var.unscaled$rotation
PCA.diff.var.scaled$rotation

################################################

# IRIS

X1= "Sepal length" 
X2= "Sepal width" 
X3= "Petal length" 
X4= "Petal width"
N=50 # for each species (i.e. 150 plants in total) 

iris.pca <- prcomp(iris[,1:4]) 
plot(iris.pca$x[,1],iris.pca$x[,2],col=as.numeric(iris$Species)) 
proportions <- iris.pca$sdev^2/sum(iris.pca$sdev^2) # Working out the proportions of variability explained  
round(iris.pca$rotation,3) # The form of the principal components
