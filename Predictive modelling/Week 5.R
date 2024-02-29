

library(readr)

pregnancy <- read_csv("Datasets/data/PROTEIN.csv")

###
##EXAMPLE 4

summary(lm(Protein~Gestation, data=pregnancy))

#n=19
#Ex=456
#Ex^2=12164

#design matrix

X <- solve(matrix(c(19,456,456,12164),nrow=2))

b <- matrix(c(0,1),ncol=1)

matrixtrans <- t(b)%*%X%*%b

RSS <- sum(resid(summary(lm(Protein~Gestation, data=pregnancy)))^2)

#two tailed t test table: https://www.scribbr.com/wp-content/uploads/2022/06/Critical-values-of-t-for-two-tailed-tests-l.webp

.022844 + (qt(p=.975, df=17)) * sqrt((RSS/17)*matrixtrans)
.022844 - (qt(p=.975, df=17)) * sqrt((RSS/17)*matrixtrans)

#PREDICTION INTERVAL: trying to figure out if x=27

newb <- matrix(c(1,27),ncol=1)

new_matrixtrans <- t(newb)%*%X%*%newb

(0.201738 + 27 * 0.022844) + (qt(p=.975, df=17)) * sqrt((RSS/17)*(1+new_matrixtrans))

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

trees <- read_csv("Datasets/data/TREES.csv")

#grabbing coefficients
summary(lm(log(Volume)~log(Height)+log(Diameter), data = trees))

#grabbing RSS
RSS <- sum(resid(summary(lm(log(Volume)~log(Height)+log(Diameter), data = trees)))^2)

#grabbing design matrix
X <- cbind(1, log(trees$Diameter), log(trees$Height))

X_new <- solve(t(X)%*%X) 

#b for the FIRST coefficient
b <- matrix(c(0,1,0),ncol=1)

matrixtrans <- t(b)%*%X_new%*%b

##solving for the FIRST coefficient
1.98265 + (qt(p=.975, df=28)) * sqrt((RSS)/28*matrixtrans)
1.98265 - (qt(p=.975, df=28)) * sqrt((RSS)/28*matrixtrans)

#b for the SECOND coefficient
b_2 <- matrix(c(0,0,1),ncol=1)

matrixtrans_2 <- t(b_2)%*%X_new%*%b_2

#Solving for the SECOND coefficient
1.11712 + (qt(p=.975, df=28)) * sqrt((RSS)/28*matrixtrans_2)
1.11712 - (qt(p=.975, df=28)) * sqrt((RSS)/28*matrixtrans_2)

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

#Task 1

auto <- read_csv("Datasets/data/autoanalyser.csv")


#first constructing design matrix
X <- cbind(1, auto$true)

X_new <- solve(t(X)%*%X) 

b <- matrix(c(1,6),ncol=1)

matrixtrans <- t(b)%*%X_new%*%b

RSS <-  sum(resid(summary(lm(autoanalyser~true,data=auto)))^2)

summary(lm(autoanalyser~true,data=auto))



#confidence interval
(0.68333 + 6 * 0.85000) + (qt(p=.975, df=10)) * sqrt((RSS/10)*matrixtrans)
(0.68333 + 6 * 0.85000) - (qt(p=.975, df=10)) * sqrt((RSS/10)*matrixtrans)

#task 2: prediction interval when x=6
(0.68333 + 6 * 0.85000) + (qt(p=.975, df=10)) * sqrt((RSS/10)*(1+matrixtrans))
(0.68333 + 6 * 0.85000) - (qt(p=.975, df=10)) * sqrt((RSS/10)*(1+matrixtrans))


#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

#Finding R^2 from pregnancy data

anova(lm(Protein~Gestation, data=pregnancy))

#find RSS
RSS <- sum(resid(summary(lm(Protein~Gestation, data=pregnancy)))^2)

#OR try
sum((fitted(lm(Protein~Gestation, data=pregnancy))-pregnancy$Protein)^2)

#MSS
sum((fitted(lm(Protein~Gestation, data=pregnancy))-mean(pregnancy$Protein))^2)

#R^2 is 1 - RSS/TSS 
# or    1 - RSS/(RSS+MSS)

1-(sum((fitted(lm(Protein~Gestation, data=pregnancy))-pregnancy$Protein)^2))/
  ((sum((fitted(lm(Protein~Gestation, data=pregnancy))-pregnancy$Protein)^2))+sum((fitted(lm(Protein~Gestation, data=pregnancy))-mean(pregnancy$Protein))^2))

#calculate F value
((sum((fitted(lm(Protein~Gestation, data=pregnancy))-mean(pregnancy$Protein))^2))/1)/
 ((sum((fitted(lm(Protein~Gestation, data=pregnancy))-pregnancy$Protein)^2))/17)

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

#FROM WEEK 3

summary(df$response)

#means for each
df %>% 
  group_by(month) %>% 
  summarise(avg = mean(response))

summary(lm(response ~factor(month), data=df))

#The intercept term is the average values of our baseline category January. The estimate 1.802 corresponds to the difference between Month 2, February, and January.
#therefore, avg value is coefficient of response-coefficient of intercept
#P values suggest neither Feb nor Mar is significantly different from Jan

anova(lm(response ~factor(month), data=df))

#The null hypothesis of this anova is that all regression coefficients in our fitted model are equal to 0. The alternative hypothesis is that at least one is not equal to zero. 
#Given a p-value of 0.2, this suggests we cannot reject the null hypothesis. This means that all regression coefficients are zero. 
#In particular, this implies that there is no signficantdifference between the three months. 
#In other words, in this case with one categoricalvariable, the anova is testing for a difference between categories. 
#The anova reports one single p-value which is correspondig to the test that at least oneof the coefficient in the regression is significantly different from zero.

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

#Task 4

#n=34

#design matrix
X_new <- matrix(c(1.686, -.0235, -.197, -.0235, .00197, .00017, -.197, .00017, .00025), ncol=3)

#first coefficient
b <- matrix(c(0,1,0),ncol=1)

matrixtrans <- t(b)%*%X_new%*%b

#CI - significant positive association with cognitive test
.0191 + (qt(p=.975, df=31)) * sqrt((.2099)/31*matrixtrans)
.0191 - (qt(p=.975, df=31)) * sqrt((.2099)/31*matrixtrans)


#second coefficient

b_2 <- matrix(c(0,0,1),ncol=1)

matrixtrans_2 <- t(b_2)%*%X_new%*%b_2

# again, both significant so probably shouldn't drop terms from model
.00639 + (qt(p=.975, df=31)) * sqrt((.2099)/31*matrixtrans_2)
.00639 - (qt(p=.975, df=31)) * sqrt((.2099)/31*matrixtrans_2)

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

#Task 5

#n=15

#Part A

X_new <- matrix(c(5.94801, -.0052074, -.169733, -.0052074, .0034387, -.0041331, -.169733, -.0041331, .010241),ncol=3)

#first coefficient
b <- matrix(c(0,1,0),ncol=1)

matrixtrans <- t(b)%*%X_new%*%b

-.096 + (qt(p=.975, df=12)) * sqrt((69.52)/12*matrixtrans)
-.096 - (qt(p=.975, df=12)) * sqrt((69.52)/12*matrixtrans)

#interval for fetal scalp level includes 0, so not a significant predictor of umbilical vein blood oxygen

#second coefficient
b_2 <- matrix(c(0,0,1),ncol=1)

matrixtrans_2 <- t(b_2)%*%X_new%*%b_2

#maternal oxygen IS significant
.760 + (qt(p=.975, df=12)) * sqrt((69.52)/12*matrixtrans_2)
.760 - (qt(p=.975, df=12)) * sqrt((69.52)/12*matrixtrans_2)

###########
#Part B
###########
#first: CI

X_1 <- matrix(c(5.940120948, -.175992437, -.175992437, .005273445),ncol=2)

b_1 <- matrix(c(1,30), ncol=1)

matrix_new <- t(b_1)%*%X_1%*%b_1

#CI
(-.537 + 30 * .647) + (qt(p=.975, df=13)) * sqrt((56.61)/13*matrix_new)
(-.537 + 30 * .647) - (qt(p=.975, df=13)) * sqrt((56.61)/13*matrix_new)

#PI
(-.537 + 30 * .647) + (qt(p=.975, df=13)) * sqrt((56.61)/13*(1+matrix_new))
(-.537 + 30 * .647) - (qt(p=.975, df=13)) * sqrt((56.61)/13*(1+matrix_new))

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

#Task 6

#n=32

#1)TSS
#=MSS+RSS

60714+71014

#calculate 95% confidence interval

X_new <- matrix(c(.703264, .027865, .027865, .001155), ncol=2)

b <- matrix(c(0,1),ncol=1)

matrixtrans <- t(b)%*%X_new%*%b

#2) 95% CI
8.375+ (qt(p=.975, df=30)) *sqrt((71014/30)*matrixtrans)
8.375- (qt(p=.975, df=30)) *sqrt((71014/30)*matrixtrans)

#null hypothesis is that there is no relationship between the degrees of fractionation and the age of materials found in earth
#P value suggests there is a significant negative relationship between age and fractionation (one unit increase in fractionation means decrease of 8.375 years, CI 2.55-14.20)

#3) prediction interval when x=-23

b_new <- matrix(c(1,-23), ncol=1)

matrix_new <- t(b_new)%*%X_new%*%b_new

4719.31+-23*8.375 + (qt(p=.975, df=30)) * sqrt((71014)/30*(1+matrix_new))
4719.31+-23*8.375 - (qt(p=.975, df=30)) * sqrt((71014)/30*(1+matrix_new))

#A new piece of material with a fractionation factor of -23 is highly likely to have an agebetween 4426 years and 4628 years.

#F value
(60714/1)/(71014/30)


