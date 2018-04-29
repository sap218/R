# MAM5220 - Statistical Techniques for Computational Biology (2017-18)
# Workbook 3 - Transcriptomics
# Samantha Pendleton - sap21

library(readr)

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

LR <- read_csv("~/git/R/R/mam5220/w3/LR.transcriptomics.csv")
dim(LR) # finding out size of dataframe - 5000 rows, 73 columns
head(LR) # first six rows

# Question 2

# Question 3