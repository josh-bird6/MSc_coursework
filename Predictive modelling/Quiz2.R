#Quiz 2

#Q3

102/sqrt(244*43.25)

#Q4

.183/(25.48/27)

(qt(p=.975, df=27))

#df=27
#part b

X_new <- matrix(c(7.4733028, -.86706392, -1.00902982, -.8670639, .20383591, .05880065, -1.0090298, .05880065, .17047641), ncol=3)

b <- matrix(c(0,1,0),ncol=1)

matrixtrans <- t(b)%*%X_new%*%b

#solving

1.7851 + (qt(p=.975, df=27)) * sqrt((25.480)/27*matrixtrans)
1.7851 - (qt(p=.975, df=27)) * sqrt((25.480)/27*matrixtrans)

##########################
#prediction interval


b_new <- matrix(c(1,4,6))

matrixtrans_new <- t(b_new)%*%X_new%*%b_new

(8.4151 + 4*1.7851 + 6*4.6432) + (qt(p=.975, df=27)) * sqrt((25.480)/27*(1+matrixtrans_new))
(8.4151 + 4*1.7851 + 6*4.6432) - (qt(p=.975, df=27)) * sqrt((25.480)/27*(1+matrixtrans_new))

###############################


#Q5
0.6696

#calculating RSE to equal .6696
RSE <- sqrt(17.038/(40-2))


1-(17.038/246.8831)


#prediction int

(2.02486 + .76374*log(.92)) 

exp(1.961178)


