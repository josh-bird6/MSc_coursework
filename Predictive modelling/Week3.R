
obs <- c(1,2,3,4,5,6,7,8,9,10)
response <- c(.69,1.028,.507,1.689,-1.8,2.966,.653,3.423,1.476,.540)
month <- c(1,3,2,3,1,2,1,3,2,3)

df <- data.frame(obs,response,month)

df <- df %>% 
  mutate(month_feb = case_when(month==2~1,
                               T~0),
         month_mar = case_when(month ==3~1,
                               T~0))


##vector matrix notation

X <- cbind(1, df$month_feb, df$month_mar)

y <- df$response

solve(t(X)%*%X) %*% (t(X) %*% y)


##or alternatively with the summary call
df$month <- as.factor(df$month)

modelling <- lm(response~month, data = df)

summary(modelling)


###################################

#task4

#this doesn't work
X <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol = 2)


#normally you would do all the y values and multiply them by the X matrix, but this question has provided the answer
y <- matrix(c(196.7, 271.9),ncol=1)

#Least squares estimates 
solve(t(X)%*%X) %*% y

#RSS
5816.2 - y %*% (solve(t(X)%*%X) %*%  y)


