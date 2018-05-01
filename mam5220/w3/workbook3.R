# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 3 - Transcriptomics
# Samantha Pendleton - sap21

library(readr)

install.packages("rootSolve")
library(rootSolve)

##################################################################################

# Part 1
# Question 1
crab <- read_csv("~/git/R/R/mam5220/w3/Crab_transcriptomics.csv")

dim(crab) # finding out size of dataframe - 8236 rows, 3 columns
head(crab) # first six rows

# a) # https://www.r-bloggers.com/which-function-in-r/
# i)
length(which(crab$Condition_A == 0))
# ii)
length(which(crab$Condition_B == 0))

par(mfrow=c(1,1))
# b) - condition A values under B == 0
hist(log(crab[which(crab$Condition_B == 0),]$Condition_A), col=rgb(0,0,1,1/4),
     main="Histogram of log values of the transcript counts under condition A
     for the transcripts that show a value of 0 under condition B",
     xlab="Value") # normal because curve, but because bigger sample, bell-shaped

# c) - condition B values under A == 0
hist(log(crab[which(crab$Condition_A == 0),]$Condition_B), col=rgb(1,0,1,1/4),
     main="Histogram of log values of the transcript counts under condition B
     for the transcripts that show a value of 0 under condition A",
     xlab="Value") # not as normal

# d)
# Compare the means and variances in the two cases and comment on the Q-Q plots - qqnorm() function
par(mfrow=c(2,2))
hist(log(crab[which(crab$Condition_B == 0),]$Condition_A), col=rgb(0,0,1,1/4),
     main="Log of transcript counts under condition A
     for transcript values of 0 under condition B", xlab="Value")
hist(log(crab[which(crab$Condition_A == 0),]$Condition_B), col=rgb(1,0,1,1/4),
     main="Log of transcript counts under condition B
     for transcript values of 0 under condition A", xlab="Value")
qqnorm(log(crab[which(crab$Condition_B == 0),]$Condition_A)); qqline(log(crab[which(crab$Condition_B == 0),]$Condition_A), col=2, lwd=2, lty=2)
qqnorm(log(crab[which(crab$Condition_A == 0),]$Condition_B)); qqline(log(crab[which(crab$Condition_A == 0),]$Condition_B), col=2, lwd=2, lty=2)

# e)
crab_subset = data.frame(crab[which(crab$Condition_A != 0 & crab$Condition_B != 0),])
crab_subset$fold = crab_subset$Condition_B / crab_subset$Condition_A

par(mfrow=c(1,1))
hist(log(crab_subset$fold), col=rgb(1,0,0,1/4),
     main="Histogram of log values of the transcript fold counts under FC = log(B/A)", xlab="Value")

# f)
sort.p <- sort(crab_subset$fold, decreasing=TRUE, index.return=TRUE)
sort.p.i <- sort.p[["ix"]][1:5] # top five index
sort.p.top <- sort.p[["x"]][1:5] # top five values
sort.n <- sort(crab_subset$fold, decreasing=FALSE, index.return=TRUE)
sort.n.i <- sort.n[["ix"]][1:5]
sort.n.top <- sort.n[["x"]][1:5]

sorted.i <- vector() # index in vector
sorted.i <- append(sorted.i, sort.p.i)
sorted.i <- append(sorted.i, sort.n.i)
sorted.v <- vector() # index in vector
sorted.v <- append(sorted.v, sort.p.top)
sorted.v <- append(sorted.v, sort.n.top)

fold.change <- data.frame(sorted.i, sorted.v)
colnames(fold.change) <- c("Transcript ID", "Fold-change")
write.csv(fold.change, "~/git/R/R/mam5220/w3/crab_subset_fold_change.csv")

##################################################################################
##################################################################################

# Part 2
LR <- read_csv("~/git/R/R/mam5220/w3/LR.transcriptomics.csv")
dim(LR) # finding out size of dataframe - 5000 rows, 73 columns
head(LR) # first six rows

##################################################################################

# Question 2
nrep <- rep(4,18)
times <- c(0,seq(6,54,by=3))
times.vector <- rep(times,nrep)

# a)
par(mfrow=c(2,2))
which(LR$X1 == "AT1G47830")
AT1G47830 <- LR[2986,2:73]
which(LR$X1 == "AT1G08940")
AT1G08940 <- LR[662,2:73]
which(LR$X1 == "AT1G01120")
AT1G01120 <- LR[11,2:73]
which(LR$X1 == "AT1G59840")
AT1G59840 <- LR[3768,2:73]
plot(times.vector, AT1G47830, xlab="Time", ylab="Log mRNA Levels", main="AT1G47830")
plot(times.vector, AT1G08940, xlab="Time", ylab="Log mRNA Levels", main="AT1G08940")
plot(times.vector, AT1G01120, xlab="Time", ylab="Log mRNA Levels", main="AT1G01120")
plot(times.vector, AT1G59840, xlab="Time", ylab="Log mRNA Levels", main="AT1G59840")

# b)
oneway.ANOVA.func <- function(transcriptomics.data, times.vector, rowID){
  data.vector <- as.numeric(transcriptomics.data[rowID,])
  temp.aov <- aov(lm(data.vector~as.factor(times.vector)))
  p.value <- summary(temp.aov)[[1]][1,5]
  return(p.value)
}

p.values <- vector()
for (p in 1:(dim(LR)[1])) {
  p.values[p] <- oneway.ANOVA.func(LR[2:73], times.vector, p)
}
sorting.p.values <- sort(p.values, decreasing=FALSE, index.return=TRUE)
sorting.p.values[["ix"]][1:6]

par(mfrow=c(2,3))
###
g.one <- LR$X1[1438]
gene.one <- LR[1438,2:73]
plot(times.vector, gene.one, xlab="Time", ylab="Log mRNA Levels", main=g.one)
g.two <- LR$X1[2146]
gene.two <- LR[2146,2:73]
plot(times.vector, gene.two, xlab="Time", ylab="Log mRNA Levels", main=g.two)
g.three <- LR$X1[841]
gene.three <- LR[841,2:73]
plot(times.vector, gene.three, xlab="Time", ylab="Log mRNA Levels", main=g.three)
g.four <- LR$X1[4712]
gene.four <- LR[4712,2:73]
plot(times.vector, gene.four, xlab="Time", ylab="Log mRNA Levels", main=g.four)
g.five <- LR$X1[4882]
gene.five <- LR[4882,2:73]
plot(times.vector, gene.five, xlab="Time", ylab="Log mRNA Levels", main=g.five)
g.six <- LR$X1[2262]
gene.six <- LR[2262,2:73]
plot(times.vector, gene.six, xlab="Time", ylab="Log mRNA Levels", main=g.six)

# c)
p.v.adj <- p.adjust(p.values, method="bonferroni")
(sum(p.v.adj <= 0.05))/5000
(sum(p.v.adj <= 0.025))/5000
(sum(p.v.adj <= 0.01))/5000
(sum(p.v.adj <= 0.005))/5000
(sum(p.v.adj <= 0.001))/5000

# d)
p.v.fdr <- p.adjust(p.values, method="fdr")
# i)
(sum(p.v.fdr <= 0.01))/5000
(sum(p.v.fdr <= 0.05))/5000
(sum(p.v.fdr <= 0.1))/5000
(sum(p.v.fdr <= 0.2))/5000
(sum(p.v.fdr <= 0.25))/5000
# ii)
sum(p.v.fdr <= 0.6)/5000
sum(p.v.fdr <= 0.7)/5000
sum(p.v.fdr <= 0.8)/5000

# e)
# i)
which(LR$X1 == "AT1G64000") # member of the WRKY transcription family of genes
AT1G64000 <- LR[4082,2:73]
plot(times.vector, AT1G64000, xlab="Time", ylab="Log mRNA Levels", main="AT1G64000")

# ii)
cor.func <- function(transcriptomics.data, rowID1, rowID2) {
  out <- cor(as.numeric(transcriptomics.data[rowID1,]),as.numeric(transcriptomics.data[rowID2,]))
  return(out)
}
AT1G64000 <- as.numeric(AT1G64000)

common <- vector()
for (p in 1:(dim(LR)[1])) {
  common[p] <- cor.func(LR[2:73], 4082, p)
}
sorting.common <- sort(common, decreasing=TRUE, index.return=TRUE)
sorting.common[["ix"]][2:6]

par(mfrow=c(2,3)) # AT1G64000 in the top left
plot(times.vector, AT1G64000, xlab="Time", ylab="Log mRNA Levels", main="AT1G64000")
g.two <- LR$X1[3276]
gene.two <- LR[3276,2:73]
plot(times.vector, gene.two, xlab="Time", ylab="Log mRNA Levels", main=g.two)
g.three <- LR$X1[1417]
gene.three <- LR[1417,2:73]
plot(times.vector, gene.three, xlab="Time", ylab="Log mRNA Levels", main=g.three)
g.four <- LR$X1[3653]
gene.four <- LR[3653,2:73]
plot(times.vector, gene.four, xlab="Time", ylab="Log mRNA Levels", main=g.four)
g.five <- LR$X1[1859]
gene.five <- LR[1859,2:73]
plot(times.vector, gene.five, xlab="Time", ylab="Log mRNA Levels", main=g.five)
g.six <- LR$X1[271]
gene.six <- LR[271,2:73]
plot(times.vector, gene.six, xlab="Time", ylab="Log mRNA Levels", main=g.six)

# iii)
sorting.common.negative <- sort(common, decreasing=FALSE, index.return=TRUE)
sorting.common.negative[["ix"]][1]

g.negative <- LR$X1[4818]
gene.negative <- LR[4818,2:73]

par(mfrow=c(1,1))
plot(times.vector, AT1G64000, xlab="Time", ylab="Log mRNA Levels", ylim=c(3,8), col="aquamarine4",
     main="Time profile for the gene, AT1G64000, plotted with the strongest negative correlation gene, AT1G72850") # AT1G64000
par(new=TRUE)
plot(times.vector, gene.negative, xlab="Time", ylab="Log mRNA Levels", main=" ", ylim=c(3,8), col="lightpink3") # AT1G72850
legend(3, 8, legend=c("AT1G64000", "AT1G72850"),
       col=c("aquamarine4", "lightpink3"), pch="o", cex=0.8)

##################################################################################

# Question 3

# a)
par(mfrow=c(1,2))
which(LR$X1 == "AT1G01060")
LHY <- LR$X1[5]
AT1G01060 <- LR[2,2:73]
plot(times.vector, AT1G01060, xlab="Time", ylab="Log mRNA Levels", main=LHY)
which(LR$X1 == "AT1G22770")
GI <- LR$X1[1806]
AT1G22770 <- LR[1806,2:73]
plot(times.vector, AT1G22770, xlab="Time", ylab="Log mRNA Levels", main=GI)

# b)
AT1G01060 <- as.numeric(AT1G01060)
common.g1 <- vector()
for (p in 1:(dim(LR)[1])) {
  common.g1[p] <- cor.func(LR[2:73], 2, p)
}
sorting.common.g1 <- sort(common.g1, decreasing=TRUE, index.return=TRUE) # positive
sorting.common.g1[["ix"]][2:6] # index
LR$X1[(sorting.common.g1[["ix"]][2:6])] # Gene
round((sorting.common.g1[["x"]][2:6]),2) # Value
sorting.common.g1.neg <- sort(common.g1, decreasing=FALSE, index.return=TRUE) # negative
sorting.common.g1.neg[["ix"]][2:6] # index
LR$X1[(sorting.common.g1.neg[["ix"]][2:6])] # Gene
round((sorting.common.g1.neg[["x"]][2:6]),2) # Value

AT1G22770 <- as.numeric(AT1G22770)
common.g2 <- vector()
for (p in 1:(dim(LR)[1])) {
  common.g2[p] <- cor.func(LR[2:73], 1806, p)
}
sorting.common.g2 <- sort(common.g2, decreasing=TRUE, index.return=TRUE) # positive
sorting.common.g2[["ix"]][2:6] # index
LR$X1[(sorting.common.g2[["ix"]][2:6])] # Gene
round((sorting.common.g2[["x"]][2:6]),2) # Value
sorting.common.g2.neg <- sort(common.g2, decreasing=FALSE, index.return=TRUE) # negative
sorting.common.g2.neg[["ix"]][2:6] # index
LR$X1[(sorting.common.g2.neg[["ix"]][2:6])] # Gene
round((sorting.common.g2.neg[["x"]][2:6]),2) # Value

# c) # gives the differences between consecutive turning points
par(mfrow=c(1,2))

which(LR$X1 == "AT1G01060")
LHY <- LR[2,2:73]
LHY.splinefunc <- splinefun(times.vector, LHY)
x <- seq(0,54,by=0.1)
y <- LHY.splinefunc(x,deriv=0) # gives spline
z <- LHY.splinefunc(x,deriv=1) # first derivative of spline
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main="LHY spline")
plot(x,z,type="l",xlab="Time",ylab="log2 mRNA", main="LHY spline (1st derivative)")

which(LR$X1 == "AT1G22770")
GI <- LR[1806,2:73]
GI.splinefunc <- splinefun(times.vector, GI)
x <- seq(0,54,by=0.1)
y <- GI.splinefunc(x,deriv=0) # gives spline
z <- GI.splinefunc(x,deriv=1) # first derivative of spline
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main="GI spline")
plot(x,z,type="l",xlab="Time",ylab="log2 mRNA", main="GI spline (1st derivative)")

turning.points.GI <- uniroot.all(GI.splinefunc,interval=c(0,54),deriv=1)
diff.g <- diff(turning.points.GI) 

# d)
gene.spline <- function(transcriptomics.data, rowID) {
  gene.splinefunc <- splinefun(times.vector, transcriptomics.data[rowID,])
  turning.points <- uniroot.all(gene.splinefunc, interval=c(0,54), deriv=1)
  out <- c(mean(diff(turning.points)), sd(diff(turning.points)))
  return(out)
}

g.matrix <- matrix(0, nrow=5000, ncol=2) 
for (g in 1:(dim(LR)[1])) {
  temp <- gene.spline(LR[2:73], g)
  g.matrix[g,1] <- temp[1]
  g.matrix[g,2] <- temp[2]
}

means <- vector()
for (m in 1:(dim(g.matrix)[1])) {
  means[m] <- mean(g.matrix[m,])
}
s.d <- vector()
for (d in 1:(dim(g.matrix)[1])) {
  s.d[d] <- sd(g.matrix[d,])
}

genes <- LR[which(means < 13 & means > 11 & s.d <= 3),]$X1

# e)
par(mfrow=c(4,5))
x <- seq(0,54,by=0.1)

gene <- LR[(which(LR$X1 == genes[1])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[1])

gene <- LR[(which(LR$X1 == genes[2])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[2])

gene <- LR[(which(LR$X1 == genes[3])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[3])

gene <- LR[(which(LR$X1 == genes[4])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[4])

gene <- LR[(which(LR$X1 == genes[5])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[5])

gene <- LR[(which(LR$X1 == genes[6])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[6])

gene <- LR[(which(LR$X1 == genes[7])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[7])

gene <- LR[(which(LR$X1 == genes[8])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[8])

gene <- LR[(which(LR$X1 == genes[9])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[9])

gene <- LR[(which(LR$X1 == genes[10])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[10])

gene <- LR[(which(LR$X1 == genes[11])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[11])

gene <- LR[(which(LR$X1 == genes[12])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[12])

gene <- LR[(which(LR$X1 == genes[13])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[13])

gene <- LR[(which(LR$X1 == genes[14])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[14])

gene <- LR[(which(LR$X1 == genes[15])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[15])

gene <- LR[(which(LR$X1 == genes[16])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[16])

gene <- LR[(which(LR$X1 == genes[17])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[17])

gene <- LR[(which(LR$X1 == genes[18])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[18])

gene <- LR[(which(LR$X1 == genes[19])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[19])

gene <- LR[(which(LR$X1 == genes[20])),2:73]
gene.splinefunc <- splinefun(times.vector, gene)
y <- gene.splinefunc(x,deriv=0) 
plot(x,y,type="l",xlab="Time",ylab="log2 mRNA", main=genes[20])
