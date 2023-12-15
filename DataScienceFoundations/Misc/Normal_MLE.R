#Normal

Normal <- FTSE

##Standard deviation: sum(x^2/n)
SDnormal<- sum(Normal^2/length(Normal))

##Relative likelihood

phi <- seq(.05,.3, length= 100)

relativeloglike_normal <- (-length(Normal)/2)*log(phi)-((sum(Normal^2))/(2*phi))-
  (-length(Normal)/2*log(SDnormal)-sum((Normal^2))/(2*SDnormal))


plot(phi, relativeloglike_normal, ylab='relative log-likelihood', type = 'l')
abline(v=SDnormal, lwd=3, col="green")

#Wilks interval
abline(h=-1.92, lwd=3, col="green", lty=2)
min(phi[(relativeloglike_normal>=-1.92)])
max(phi[(relativeloglike_normal>=-1.92)])

#Wald Interval: N/2MLE^2

#variance: 
1/(length(Normal)/(2*SDnormal^2))

# ((2*(SDnormal^2))/length(Normal))

#Standard error
sqrt(1/(length(Normal)/(2*SDnormal^2)))

#Confidence interval

SDnormal+(1.96*sqrt(1/(length(Normal)/(2*SDnormal^2))))
SDnormal-(1.96*sqrt(1/(length(Normal)/(2*SDnormal^2))))
