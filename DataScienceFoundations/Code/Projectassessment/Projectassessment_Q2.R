library(tidyverse)

summary(paris$price)
sd(paris$price)

summary(copenhagen$price)
sd(copenhagen$price)

##NOT SYMMETRIC
ggplot(paris, aes(x=price))+
  geom_histogram(bins=30)

ggplot(copenhagen, aes(x=price))+
  geom_histogram(bins=50)

ggplot(paris, aes(x=log(price)))+
  geom_histogram(bins=8)

ggplot(copenhagen, aes(x=log(price)))+
  geom_histogram(bins=8)

ggplot(paris, aes(x=price))+
  geom_boxplot()

ggplot(copenhagen, aes(x=price))+
  geom_boxplot()


#######
mean(paris$price) + qt(.975, df=70158)*(sd(paris$price)/sqrt(nrow(paris)))
mean(paris$price) - qt(.975, df=70158)*(sd(paris$price)/sqrt(nrow(paris)))

confint(lm(price~1, paris))

confint(lm(price~1, copenhagen))

mean(log(paris$price))

mean(log(copenhagen$price))

confint(lm(log(price)~1, paris))

confint(lm(log(price)~1, copenhagen))

##BOOTSTRAP

paris_boot<- sample(paris$price, size = nrow(paris), replace = T)

paris_boot_output <- boot::boot(paris_boot, function(u,i) mean(u[i]), R=100)

boot::boot.ci(paris_boot_output, type = c("norm", "basic", "perc"))

# 
# I might also consider employ a non-parametric method for deriving confidence intervals. One such approach could be nonparametric bootstrapping, which creates a new distribution "...from the observed data already collected if the samples are independent" (see e.g. Johnston and Faulkner, 2020). In other words, it is a particularly useful technique for analysing data such as Airbnb prices for Paris and Copenhagen, which do not follow a normal distribution. 
# 
# 100 random samples each the size of the original datasets (70,158 and 21,301 for Paris and Copenhagen, respectively) were drawn with replacement for both cities. The distributions of these bootstrapped samples was used to calculate the mean and confidence intervals, which are shown in Output 2. For both cities, the upper and lower bounds of the confidence interval are slightly higher than the traditional confidence interval method noted above.
###########

# mean(copenhagen$price) - mean(paris$price) + (1.95999* sqrt((sd(copenhagen$price)/sqrt(nrow(copenhagen)))^2+(sd(paris$price)/sqrt(nrow(paris)))^2))
# mean(copenhagen$price) - mean(paris$price) - (1.95999* sqrt((sd(copenhagen$price)/sqrt(nrow(copenhagen)))^2+(sd(paris$price)/sqrt(nrow(paris)))^2))


paristtest <- paris %>% 
  select(price) %>% 
  mutate(city="Paris",
         price2=log(price))

copenhagentest <- copenhagen %>% 
  select(price) %>% 
  mutate(city="Copenhagen",
         price2=log(price))

ttest <- rbind(paristtest,copenhagentest)

t.test(ttest$price~ttest$city, var.equal=T)
t.test(ttest$price2~ttest$city, var.equal=T)

wilcox.test(ttest$price~ttest$city)

###########
###########
###########
###########
###########
###########
###########
###########
###########
###########

Q3copenhagen <- copenhagen %>% 
  mutate(private = case_when(room_type == "Private room"~"1. Yes",
                             T~"2. No"))

prop.table(table(Q3copenhagen$private))

((3564/nrow(Q3copenhagen)) + 1.96*sqrt(((3564/nrow(Q3copenhagen))*(1-(3564/nrow(Q3copenhagen))))/nrow(Q3copenhagen)))*100
((3564/nrow(Q3copenhagen)) - 1.96*sqrt(((3564/nrow(Q3copenhagen))*(1-(3564/nrow(Q3copenhagen))))/nrow(Q3copenhagen)))*100

prop.test(table(Q3copenhagen$private))

##################################

Q3paris <- paris %>% 
  mutate(private = case_when(room_type == "Private room"~"1. Yes",
                             T~"2. No"))

prop.table(table(Q3paris$private))

((7880/nrow(Q3paris)) + 1.96*sqrt(((7880/nrow(Q3paris))*(1-(7880/nrow(Q3paris))))/nrow(Q3paris)))*100
((7880/nrow(Q3paris)) - 1.96*sqrt(((7880/nrow(Q3paris))*(1-(7880/nrow(Q3paris))))/nrow(Q3paris)))*100

prop.test(table(Q3paris$private))
###################################



matrix(data = c(3564,7880,17737,62278), nrow=2, ncol=2)

prop.test(c(3564,7880),c(nrow(Q3copenhagen), nrow(Q3paris)))
                             
