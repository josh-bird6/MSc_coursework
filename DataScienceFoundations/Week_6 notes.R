library(tidyverse)

##EXPONENTIAL
theta <- seq(0.01,.2,length=50)

hittime <- c(1,5,15,2,3,45,13,3,3,16,23,42,4,7,4)

number <- length(hittime)

thetahate <- 1/mean(hittime)

loglike <- number*log(theta)-theta*sum(hittime)

plot(theta,loglike, lwd=3, type = "l", xlab=expression(theta)) 
  abline(v=thetahate, lwd=3)

############################################################
############################################################
############################################################
############################################################
############################################################
############################################################

  #EXPONENTIAL 
sequence <-  seq(.001, .03, length=50)

theta2 <- c(50,44,102,72,22,39,3,15,197,188,79,88,46,5,5,36,22,139,210,97,30,23,13,14)

thetahat <- 1/mean(theta2)

loglik2 <- length(theta2)*log(sequence)-sequence*sum(theta2)

plot(sequence,loglik2, type = "l", lwd=3, xlab=expression(theta2))  
abline(v=thetahat, lwd=3)

##OPTIMISATION

AIRCON <- function(sequence,y,n){
  length(theta2)*log(sequence)-sequence*sum(theta2)
}

#can toggle the initial par value
optim(par=.05, fn=AIRCON, method="BFGS", control=list(fnscale=-1),y=theta2, n=length(theta2))
