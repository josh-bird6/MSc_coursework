#Poisson

#creating dataset
month <- seq(1:20)

accidents <- c(2,2,1,1,0,4,2,1,2,1,1,1,3,1,2,2,3,2,3,4)

poisson <- data.frame(month,accidents)

#plausible sequence of values:
lambda = seq(1,3, length=1000)

#MLE
MLE <- mean(accidents)


##likelihood function:
likelihood_poisson <- (lambda^sum(poisson$accidents))*exp(-length(poisson$month)*lambda)/prod(factorial(length(poisson$accidents)))

plot(lambda, likelihood_poisson, ylab="likelihood", xlab="lambda", type="l", lwd=3)
abline(v=MLE, lwd=3, col="green")

##log likelihood

loglikehood_poisson <- sum(poisson$accidents)*log(lambda) - length(poisson$accidents)*lambda

plot(lambda,loglikehood_poisson, ylab="log-likelihood", type="l", lwd="")
abline(v=MLE, col="green")
#################################################################

#relative log-likelihood



relativeloglike_poisson <- 
  sum(poisson$accidents)*log(lambda) - length(poisson$accidents)*lambda - 
  (sum(poisson$accidents)*log(MLE)-length(poisson$accidents)*MLE)

plot(lambda, relativeloglike_poisson, ylab='relative log-likelihood', type = 'l')
abline(v=MLE, lwd=3, col="green")
#Wilks interval
abline(h=-1.92, lwd=3, col="green", lty=2)

#how to find the min and max #WILKS INTERVAL
min(lambda[(relativeloglike_poisson>=-1.92)])
max(lambda[(relativeloglike_poisson>=-1.92)])

#WALD INTERVAL

#MLE
MLE

#variance: 1/(sum(x)/MLE^2) = SECOND DERIVATIVE OF POISSON DIST
1/(sum(accidents)/(MLE^2))

#Standard error
sqrt(1/(sum(accidents)/(MLE^2)))

#Confidence interval

MLE+(1.96*sqrt(1/(sum(accidents)/(MLE^2))))
MLE-(1.96*sqrt(1/(sum(accidents)/(MLE^2))))


#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################

poisson2 <- poidata

lambda2 <- seq(.5,1.5, length=100)

MLE2 <- mean(poisson2)

##RELATIVE LOG LIKELIHOOD
relativeloglike_poisson2 <- 
  (sum(poisson2)*log(lambda2) - length(poisson2)*lambda2 - 
  (sum(poisson2)*log(MLE2)-length(poisson2)*MLE2))

# relativeloglike_poisson2 <- 
#   (-length(poisson2)*lambda2) + (sum(poisson2)*log(lambda2))-
#   ((-length(poisson2)*MLE2)+(sum(poisson2)*log(MLE2)))

plot(lambda2, relativeloglike_poisson2, ylab='relative log-likelihood', type = 'l')
abline(v=MLE2, lwd=3, col="green")
#Wilks interval
abline(h=-1.92, lwd=3, col="green", lty=2)

#how to find the min and max #WILKS INTERVAL
min(lambda2[(relativeloglike_poisson2>=-1.92)])
max(lambda2[(relativeloglike_poisson2>=-1.92)])

# MLE is  .9 (.73-1.1)

#optimisation
n <- length(poisson2)

myfunction <- function(lambda2, y,n){
  (-n*lambda2)+(sum(y)*log(lambda2))
}

optim(par=.9, fn=myfunction,method="BFGS",control=list(fnscale=-1),y=poisson2, n=100)


#WALD INTERVAL

#variance: 1/(sum(x)/MLE^2) = SECOND DERIVATIVE OF EXPONENTIAL DIST
1/(sum(poisson2)/(MLE2^2))

#Standard error
sqrt(1/(sum(poisson2)/(MLE2^2)))

#Confidence interval

MLE2+(1.96*sqrt(1/(sum(poisson2)/(MLE2^2))))
MLE2-(1.96*sqrt(1/(sum(poisson2)/(MLE2^2))))
