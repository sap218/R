# Software Carpentries Workshop (2019) - R

cwd <- getwd() # getting current working directory
setwd(cwd) # setting

# packages ----------------------------------------------------------------

installed.packages() # lists all packages
update.packages() # updates packges
install.packages('gapminder')
library(gapminder)

# variables, listing, removing --------------------------------------------

x <- 1:5
y <- 2
x^y

ls() # lists all variables
.var <- 10
ls(all.names=TRUE)

rm(.var)
rm(list = ls()) # removes all variables

# vectors -----------------------------------------------------------------

seq(1,10, by=0.1)

x <- 1:26
x <- x*2
paste(names(x) <- LETTERS)

# help --------------------------------------------------------------------

help(c)
help(paste)

# paste: sep & collaspe ---------------------------------------------------

paste(c("a","b"), "c", sep = ",")
paste(c("a","b"), "c", collapse = "|")
paste(c("a","b"), "c", sep = ",", collapse = "|")

# data frame & exporting to csv -------------------------------------------

cats <- data.frame(coat = c("calico", "black", "tabby"), 
                   weight = c(2.1, 5.0, 3.2), 
                   likes_string = c(1, 0, 1))
write.csv(x = cats, file = "data/feline-data.csv", row.names = FALSE)

library(readr)
cats <- read.csv(file = "data/feline-data.csv")
cats$weight
paste("My cat is", cats$coat)

typeof(cats$likes_string)
typeof(cats$weight)
typeof(cats$coat)


# experimenting with data frames ------------------------------------------

age <- c(2, 3, 5)
cbind(cats,age)










