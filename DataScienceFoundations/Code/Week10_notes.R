library(tidyverse)


##TASK 2

#datsets
task2_x <- c(1164,1119,1415,964,711,773,579,977,792,1244,1115,1186)
task2_y <- c(1382,883,1040,729,651,1227,1195,1370,1079,1089,1175,900,821,1075,1256,1134,918)

#MLEs
mle_x <- sum(task2_x)/length(task2_x)
mle_y <- sum(task2_y)/length(task2_y)


#H1:
(sum(task2_x)*log(mle_x)-length(task2_x)*mle_x)+(sum(task2_y)*log(mle_y)-length(task2_y)*mle_y)


#H0:
thetahatMLE <- (sum(task2_x)+sum(task2_y))/(length(task2_x)+length(task2_y))

(sum(task2_x)+sum(task2_y))*log(thetahatMLE)-(length(task2_x)+length(task2_y))*thetahatMLE

#Chi-square

2*(
  ((sum(task2_x)*log(mle_x)-length(task2_x)*mle_x)+(sum(task2_y)*log(mle_y)-length(task2_y)*mle_y))-
    ((sum(task2_x)+sum(task2_y))*log(thetahatMLE)-(length(task2_x)+length(task2_y))*thetahatMLE))

#reject h0 as 17.8>3.84 under x^2 distribution

########################
#CONFIDENCE INTERVAL:

#SAMPLE INFO:
(sum(task2_x))/(mle_x^2))
(sum(task2_y))/(mle_y^2))

#Variance
1/((sum(task2_x))/(mle_x^2))
1/((sum(task2_y))/(mle_y^2))

#confint

(mle_x-mle_y)+1.96*sqrt(
  (1/((sum(task2_x))/(mle_x^2))+1/((sum(task2_y))/(mle_y^2))))
(mle_x-mle_y)-1.96*sqrt(
  (1/((sum(task2_x))/(mle_x^2))+1/((sum(task2_y))/(mle_y^2))))

 #################################

Task5_Dose5 <- 21/99
Task5_Dose15 <- 31/100
Task5_Dose25 <- 42/99



#H1
log(Task5_Dose5)*21+log(1-Task5_Dose5)*78+
  log(Task5_Dose15)*31+log(1-Task5_Dose15)*69+
  log(Task5_Dose25)*42+log(1-Task5_Dose25)*57

#H0
log((94/298))*94+log(1-(94/298))*204

#observed value of test statistic:
2*((log(Task5_Dose5)*21+log(1-Task5_Dose5)*78+
      log(Task5_Dose15)*31+log(1-Task5_Dose15)*69+
      log(Task5_Dose25)*42+log(1-Task5_Dose25)*57)-
    (log((94/298))*94+log(1-(94/298))*204))

2*(-180.5495-(-185.7677))


1-pchisq((2*((log(Task5_Dose5)*21+log(1-Task5_Dose5)*78+
                log(Task5_Dose15)*31+log(1-Task5_Dose15)*69+
                log(Task5_Dose25)*42+log(1-Task5_Dose25)*57)-
               (log((94/298))*94+log(1-(94/298))*204))), df=2)

###############################
#Task 6

2*(18*log(18/20)+14*log(14/20)+24*log(24/20)+29*log(29/20)+15*log(15/20))

1-pchisq((2*(18*log(18/20)+14*log(14/20)+24*log(24/20)+29*log(29/20)+15*log(15/20))),df=4)

############################################
#Quiz3
2*(-6.148-(-9.721))

1-pchisq(7.146, df=1)

2*((12*log(12/20))+(25*log(25/20))+(23*log(23/20)))
1-pchisq((2*(12*log(12/20)+25*log(25/20)+23*log(23/20))), df=2)
1-pchisq(5.326412, df=2)
