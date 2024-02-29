#Week 4

#task 1

#Sxx

Sxx <- 8066-((272^2)/10)
Syy <- 7072-((250^2)/10)

Sxy <- 7396-((272*250)/10)

Sxy/sqrt(Sxx*Syy)
######################################

library(MASS)
data(hills)

#summary statistics
summary(hills)

#plotting correlations
GGally::ggpairs(hills) 
GGally::ggpairs(log(hills))

#model with residuals plots

hills_model <- lm(formula = time~climb+dist, data = hills)
hills_model_log <- lm(formula = log(time)~log(climb)+log(dist), data = hills)

summary(hills_model)
summary(hills_model_log)


par(mfrow=c(2,2))

#Knock Hill is very influential in the model
plot(hills_model)
plot(hills_model_log)

#This is confirmed when checking for outliers
car::outlierTest(hills_model_log)
car::outlierTest(hills_model)

#consider removing that observation which will improve model fit.

##########################

#task 5
1-(.2099/.5993)


#task 6

matrix1 <- matrix(c(5.94801, -.0052074, -.169733, -.0052074, .0034387, -.0041331, -.169733, -.0041331, .010241), nrow=3)

matrix2 <- matrix(c(315.6, 13256.996, 10655.224))

#least squares
coeff <- matrix1 %*% matrix2

#RSS
ymatrix <- matrix(c(315.6,13256.996,10655.224), nrow=1)                  

6775.93 - ymatrix %*% coeff

#calulating R^2
#first, TSS 
6775.83-15*(315.6/15)^2

#R^2
1-((6775.93 - ymatrix %*% coeff)/(6775.83-15*(315.6/15)^2))

#Task 7

#R^2
1-(68.713/382.731)

#with the additional variable added
1-(50.699/382.731)


#Task 8
1-(301.9066/882.643)

#Task 10

#TSS
3286+4377

#R^2
1-(3286/(3286+4377))

