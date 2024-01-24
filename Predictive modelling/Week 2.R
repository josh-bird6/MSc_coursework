
library(tidyverse)

##Week 2 example 3

crime <- read_csv("Datasets/data/crime.csv")

X <- cbind(1,crime$Dropout) #design matrix

y <- crime$Crime

crime.beta <- solve(t(X)%*%X) %*% (t(X) %*% y) ## least squares estimate

#Week2 task8

xray <- read_csv("Datasets/data/XRAY.csv")

X <- (xray$t)

y <- log(xray$`n(t)`/40000) #40000 is from Week1

xray.beta <- solve(t(X)%*%X) %*% (t(X)%*%y)

#########################

#Task 9

a <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)
b <- seq(1,14,1)
c <- seq(110.07,1430,100)

test <- cbind(a,b,c)

#first part of least squares est
t(test)%*%test

#second part
t(test)%*%(matrix(seq(1,14,1)))





