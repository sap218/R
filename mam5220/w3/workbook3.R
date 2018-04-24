# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 3 - Transcriptomics
# Samantha Pendleton - sap21

library(readr)
install.packages("ggplot2")
library(ggplot2)
require(graphics)

# Question 1

crab <- read_csv("M:/MAM5220 - CB/Crab_transcriptomics.csv")

dim(crab) # finding out size of dataframe - 8236 rows, 3 columns
head(crab) # first six rows

# a) # https://www.r-bloggers.com/which-function-in-r/
# i)
length(which(crab$Condition_A == 0))
# ii)
length(which(crab$Condition_B == 0))

# b)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) 
par(mfrow=c(3,1))

p1.loga <- hist(log(crab$Condition_A), col=rgb(0,0,1,1/4), xlim=c(-6,12))
p2.logwhichb <- hist(log(which(crab$Condition_B == 0)), col=rgb(1,0,0,1/4), xlim=c(-6,12))

plot( p1.loga, col=rgb(0,0,1,1/4), xlim=c(-6,12), ylim=c(0,2000))
plot( p2.logwhichb, col=rgb(1,0,0,1/4), xlim=c(-6,12), add=T)
legend('topright',c('Condition B','Condition A'),
       fill = rgb(1:0,0,0:1,0.4), bty = 'n',
       border = NA)

# c)
p1.logb <- hist(log(crab$Condition_B))
p2.logwhicha <- hist(log(which(crab$Condition_A == 0)))

plot( p1.logb, col=rgb(0,0,1,1/4), xlim=c(-6,12), ylim=c(0,1000))
plot( p2.logwhicha, col=rgb(1,0,0,1/4), xlim=c(-6,12), add=T)
legend('topright',c('Condition A','Condition B'),
       fill = rgb(1:0,0,0:1,0.4), bty = 'n',
       border = NA)

# d)
# Comment on the form of the histograms produced in parts b) and c)
mean(log(crab$Condition_A)) # doesn't work?
mean(log(crab$Condition_B)) # doesn't work?
mean(log(which(crab$Condition_A == 0)))
mean(log(which(crab$Condition_B == 0)))

# Compare the means and variances in the two cases and comment on the Q-Q plots - qqnorm() function
par(mfrow=c(1,2))
qqnorm(crab$Condition_A); qqline(crab$Condition_A, col=2, lwd=2, lty=2) # results mean not normal?
qqnorm(crab$Condition_B); qqline(crab$Condition_B, col=2, lwd=2, lty=2) 

par(mfrow=c(1,1))
qqplot(crab$Condition_A, crab$Condition_B)

# e)
# f)

##################################################################################
##################################################################################

LR <- read_csv("M:/MAM5220 - CB/LR.transcriptomics.csv")
dim(LR) # finding out size of dataframe - 5000 rows, 73 columns
head(LR) # first six rows

# Question 2

# Question 3