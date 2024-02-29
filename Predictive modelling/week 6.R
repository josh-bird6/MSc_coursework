
#week 6

#trees dataset - coefficients and so forth
summary(lm(log(Volume)~log(Height)+log(Diameter), data = trees))

#generating confidence interval
confint(lm(log(Volume)~log(Height)+log(Diameter), data = trees))


#To verify our initial analysis we should look at the smaller models (i.e. those with only one variable).
#For the models with log (diameter) alone and log (height) alone the R^2 values are 95.4% and 42.1% respectively. 
#Although we know that log (height) is related to log (volume), it does not give muchpractical assistance in terms of additional predictive power. 
#It may be that we want to fit a modelwith only log(diameter) alone, which might have better predictive power, but based on the aboveanalysis we will keep both variables as predictor.

summary(lm(log(Volume)~log(Height), data = trees))
summary(lm(log(Volume)~log(Diameter), data = trees))

################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################

#church dataset

cofe <- read_csv("Datasets/data/cofe.csv")

GGally::ggpairs(cofe) 

summary(lm(formula = giving ~employ + attend, data = cofe))

#employment by itself is probably not a good predictor
anova(lm(formula = giving ~employ + attend, data = cofe))

confint(lm(formula = giving ~employ + attend, data = cofe))

#alternative way to calculate confint: 
#coefficient +/- tvalue * std. error

1.8375+(qt(p=.975, df=17))*.6552
1.8375-(qt(p=.975, df=17))*.6552

#going with smaller models

#attendance is still a significant (negative) predictor
summary(lm(formula = giving ~ attend, data = cofe))

confint(lm(formula = giving ~ attend, data = cofe))

#whereas employment is no longer significant by itself
summary(lm(formula = giving ~ employ, data = cofe))

confint(lm(formula = giving ~ employ, data = cofe))

#This demonstrates that starting wiith lower dimensional models would have led us to exclude the variable that was not significant by itself, but became significant in a larger model.
#Has to do with interplay with variables - one has negative and other has positive correlation. It is better to start with a larger model and work your way down

################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################

#task 1

#n=25

X <- matrix(c(.26943, -.02131, -.02302, -.02131, .00520, -.00143, -.02302, -.00143, .00625), ncol=3)

#first coefficient
b <- matrix(c(0,1,0), ncol=1)

matrixtrans <- t(b)%*%X%*%b

.704 + (qt(p=.975, df=22)) * sqrt((68.713)/22*matrixtrans)
.704 - (qt(p=.975, df=22)) * sqrt((68.713)/22*matrixtrans)

#estimate is .704 (.4397, .9683) so it does not include 0 so it is significant so it should be included

#second coefficient
b_2 <- matrix(c(0,0,1),ncol=1)

matrixtrans_new <- t(b_2)%*%X%*%b_2

.939 + (qt(p=.975, df=22)) * sqrt((68.713)/22*matrixtrans_new)
.939 - (qt(p=.975, df=22)) * sqrt((68.713)/22*matrixtrans_new)

#estimate is .939 (.6492, 1.2288) so it does not include 0 so it is significant so it should be retained

################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################

#task 2

-.03100 + (qt(p=.975, df=23)) * .01721
-.03100 - (qt(p=.975, df=23)) * .01721

#This interval includes 0, which means that squared soil cores are not significantly associated with terrestrial radioactivity. 
#This is evidenced by the p-value; under the null hypothesis there would be no association (or H=0), and the p-value of .085 does not give us sufficient evidence to reject the null.
#Residual standard error is sqrt(rss/(n-2), is the estimate of the standard deviation of the residuals, and is on the same scale as the dependent variable.

