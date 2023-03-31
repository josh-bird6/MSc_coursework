#week 7 notes

#EXPONENTIAL
#EXTENSION from WEEK 6

#values for x-axis, may require experimentation
theta <- seq(0.01,.2,length=50)

#data
hittime <- c(1,5,15,2,3,45,13,3,3,16,23,42,4,7,4)

#n
number <- length(hittime)

#MAXIMUM LIKELHOOD ESTIMATOR
thetahate <- 1/mean(hittime)

#Log likelihood
loglike <- number*log(theta)-theta*sum(hittime)

##relative log-like
relativeloglike <- number*log(theta)-theta*sum(hittime)-(number*log(thetahate)-thetahate*sum(hittime))

plot(theta,relativeloglike, type = 'l', lwd=3, ylab="relative log_likelihood")
abline(v=thetahate, lwd=3)
axis(3,at=c(thetahate))

#plot Wilks confidence interval
# abline(h=log(.5), lwd=3, col="green", lty=2)

#95% confidence interval from chi-square table (ALSO KNOWN AS WILKS INTERVAL):
#3.8415/-2
abline(h=-1.92, lwd=3, col="green", lty=2)

#FIND MIN MAX
min(theta[(relativeloglike>=-1.92)])
max(theta[(relativeloglike>=-1.92)])


###########################
#WALD INTERVAL

#MLE
thetahate

#variance: 1/(n/MLE^2) = SECOND DERIVATIVE OF EXPONENTIAL DIST
1/(length(hittime)/(thetahate^2))

#Standard error
sqrt(1/(length(hittime)/(thetahate^2)))

#Confidence interval

thetahate+(1.96*sqrt(1/(length(hittime)/(thetahate^2))))
thetahate-(1.96*sqrt(1/(length(hittime)/(thetahate^2))))
