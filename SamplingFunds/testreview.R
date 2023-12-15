
#From week 8 practice exam https://moodle.gla.ac.uk/mod/quiz/review.php?attempt=3896838&cmid=3755870

means <- matrix(c(-.5,1),ncol=1)

multp <- matrix(c(1,-1), nrow=1)

matrix2 <- matrix(c(5,2,2,8),nrow=2)

##mean
multp%*%means

#sd
multp%*%matrix2%*%t(multp)

##REMEMBER TO SQUARE ROOT WHEN PUTTING IN THE EQUATION
###############################################################

##Example 11 - week 8
pnorm(-5, 0, sqrt(1500/12))

##Task 5, week 8

1-pnorm(8500, (100*90), sqrt(100*90^2))
###############################################################
#week 8 pracice
1000*.08
1000*.08*(1-.08)

#P(x>63)
#with continuity correction

1-pnorm(62.5, 80, sqrt(73.6))

###############################################################
#weeek 8 example 13 (POISSON)

#P(x<60)

pnorm(59.5, 50, sqrt(50))

#P(50=<x=60)

pnorm(59.5,50,sqrt(50))-(pnorm(49.5, 50, sqrt(50)))

###############################################################
#week 8 review exercise

pnorm(65.5, 70, sqrt(70))
