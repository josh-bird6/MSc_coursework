#Week 7

#using the mtcars dataset, we want to predict the variable mpg (miles per gallon) from four possible predictors: disp, hp, wt, qsec


#ll subsets regression
library(olsrr)

#Does what it says on the tin
ols_step_all_possible(lm(mpg~disp+hp+wt+qsec, data=mtcars))
#best models are: 1/1; 2/5; 11/6 and 15/15


#Now that we have identified the best models for each size, we use only these models and compare
ols_step_best_subset(lm(mpg~disp+hp+wt+qsec, data=mtcars))
#different criteria chooses different models as best. adj R^2 likes model 3, whereas AIC choses model 2

#########################

#Stepwise regression

#backward regression: start with full model and at each step fit all possible models with one variable removed. The model with best value of criterion is chosen as current model for the next step. If the current model scores best then the stepwise selection ends

lakes <- readr::read_csv("Datasets/data/lakes.csv")

#exploratory
GGally::ggpairs(lakes) 
#no obvious multicollinearity (i.e. linear relationships between predictors) issues

#create model, derive CI
summary(lm(ltotalbio ~ ., data=lakes))
confint(lm(ltotalbio ~ ., data=lakes))

#Many estimates of regression coefficients have p>.05 and conf.int. includes 0. So we want to reduce the model
#Use step function, which defeaults to AIC

step(lm(ltotalbio ~ ., data=lakes))

#By removing one covariate at a time, we end of with a model with three variables. In this final step we see the row with the lowest AIC is <none>, which means we would not gain anything dropping this variable
#NOTE: a different criteorion may have chosen a different model

#When running the model with just these variables, lColour is still not significant
summary(lm(ltotalbio ~ lColour + lTotalP + lAlkalinity, data = lakes))

#so we drop that:
summary(lm(ltotalbio ~ lTotalP + lAlkalinity, data = lakes))
#however this results in a minor reduction in adjusted R^2, so keep lColour in - this is interesting! 
#Don't just look at statistical significance, also assess model fit

#################################################################

#olsrr package provides functions for best subset selection

#Forward selection using p-value criteria
ols_step_forward_p(lm(ltotalbio ~ ., data=lakes))
#this one retains the three variables above, but also includes altitude


#Backward selection uses pvalue criteria to remove variables from motel starting with full model
ols_step_backward_p(lm(ltotalbio ~ ., data=lakes))
#NOTE - this one says to which ones to remove. i.e. the inverse of the forward selection method


#now we'll do forward-backward selection using AIC
ols_step_both_aic(lm(ltotalbio ~ ., data=lakes))
#NOTE THIS GIVES THE SAME RESULT AS step() ABOVE, WHICH DEFAULTS TO AIC

#########################################################

#Task 1
#perform an analysis to find signficiant predictors of Fertility

data(swiss)

#FIRST: exploratory
GGally::ggpairs(swiss)
#appears to be some evidence of multicolinearity?

#Does what it says on the tin
ols_step_all_possible(lm(Fertility ~ ., data = swiss))
#best models are: 1/1; 2/6; 3/16, 4/26 and and 5/31



#SECOND: create a model and generate CI
summary(lm(Fertility ~ ., data = swiss))
confint(lm(Fertility ~ ., data = swiss))
#looks like education and agriculture are negatively associated with fertility score, catholic and infant mortality is positively associated
#no significant association with examination

#lets first reduce the model using step()
step(lm(Fertility ~ ., data=swiss))
#best model with AIC is one where examination is removed. So create the summary with just these

summary(lm(Fertility ~ Agriculture + Infant.Mortality + Catholic + Education, data = swiss))
#all coefficients are now significantly associated with, fertility, but slightly lower  R^2 than full model
#duplicates the results above
ols_step_both_aic(lm(Fertility ~ ., data=swiss))

#now let's try using p-value instead of AIC
ols_step_forward_p(lm(Fertility ~ ., data=swiss))
#this keeps the same four as above


#The same predictors were retained regardless of AIC or p value criteria
#so significant predictors of fertility are going to be agriculture, infant mortality, catholic and education

