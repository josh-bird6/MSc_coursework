
library(tidyverse)

quiz1 <- read_csv("Datasets/data/data.csv")


X <- cbind(quiz1$x1, quiz1$x2)

y <- quiz1$y

#computing XtX
t(X)%*%X

#What dimensions to XtY have
t(X) %*% y

#beta coefficients

beta <- solve(t(X)%*%X) %*% (t(X) %*% y)

#RSS
rss <- (t(y)%*%y) - (t(y)%*%X%*%beta)

#error variance

rss/(10-2)

model <- lm(y ~ x1 + x2 , data= quiz1)

summary(model)$coefficients

##############################################
##############################################
##############################################
##############################################
##############################################
##############################################

quiz2 <- read_csv("Datasets/data/oxygen.csv")


X <- cbind(1, quiz2$RunTime, quiz2$RunPulse, quiz2$MaxPulse)

y <- quiz2$Oxygen

#least squared estimates
solve(t(X)%*%X) %*% (t(X) %*% y)

model2 <- lm(Oxygen ~ RunTime+RunPulse+MaxPulse, data=quiz2)

summary(model2)

summary(quiz2)

#predicting
80.900 + (-2.9701867*11.12) + (-0.3751142* 164) + (0.3542189*176)


