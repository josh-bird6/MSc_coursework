library(tidyverse)

airtemp <- c (49.9, 52.3, 49.4, 51.1, 49.4, 47.9, 49.8, 50.9, 49.3, 51.9, 50.8, 49.6, 49.3, 50.6,48.4, 50.7, 50.9, 50.6, 51.5, 52.8, 51.8, 51.1, 49.8, 50.2, 50.4, 51.6,
51.8, 50.9, 48.8, 51.7, 51.0, 50.6, 51.7, 51.5, 52.1, 51.3, 51.0, 54.0, 51.4, 52.7,53.1, 54.6,52.0, 52.0, 50.9, 52.6, 50.2, 52.6, 51.6, 51.9, 50.5, 50.9, 51.7, 51.4,
51.7, 50.8, 51.9, 51.8, 51.9, 53.0) %>% 
  data.frame()

ggplot(data= airtemp, aes(x=.))+
  geom_histogram(bins=30)

#MEAN
Week8mean<- mean(airtemp$.)

#VARIANCE
Week8variance <- (1/length(airtemp$.))*sum((airtemp$.-mean(airtemp$.))^2)

#sd
sqrt(Week8variance)

#OPTIMISATION

# stupidexample <- function(x,y, n){
#   -(n/2)*log(2*pi)-n*log(x)-(sum((y-x)^2)/(2*x^2))
# }

ltemp <- function(x,y,n){
  -((n/2)*log(2*pi))-(n*log(x[2]))-(sum((y-x[1])^2)/(2*((x[2])^2)))
}

optim(par=c(51.16,1.255017), 
      fn=ltemp, 
      method = "BFGS", 
      control=list(fnscale=-1), 
      n=60,
      y=airtemp$.
      )

#######################################################################

# x <- c(37,35,31,36,25,43,41,33,25,37,27,30,32,35,38)
# y <- c(4,4,3,3,3,5,3,4,5,4,4,3,4,4,3)
# 
# mean(x)
# mean(y)

#FROM TASK 2 in WEEK 8

#FIRST DERIVATIVE x/theta - ((n-x)/(1-theta))
(15/.3)-(35/(1-.30))

(26/.52)-(24/(1-.52))

#FIRST DERIVATIVE: SECOND METHOD?? 
#Binomial (1/theta)*sum(x) - (1/(1-theta))*(n-sum(x))
(1/.3)*15-((1/(1-.3))*(50-15))

##WHY IS THIS BEING FUCKY?
((1/.52)*26)-((1/(1-.52))*(50-26))

test1 <- ((1/.52)*26)

test2 <- ((1/(1-.52))*(50-26))


###SECOND DERIVATIVE (second method)
#Number 1
-(15/.3^2) - ((50-15)/(1-.3)^2)
#OR
-(1/.3^2)*15 - (1/(1-.3)^2)*(50-15)

#Number 2
-(26/.52^2) - ((50-26)/(1-.52)^2)



########################################################
n=20
a=15

20/(15+1)^2

1/(20/(15+1)^2)

(15+1)^2/20
