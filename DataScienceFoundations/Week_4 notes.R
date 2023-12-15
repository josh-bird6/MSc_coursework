
summary(IQ)

sd(IQ$iq)^2

mean(IQ$iq)

qt(.975, df=38)*((sd(IQ$iq))/(sqrt(39)))

2.024394*(5.538724/(sqrt(39)))

mean(IQ$iq)

92.17949+1.795446
92.17949-1.795446
##############################

price <-c(120, 110, 108, 100, 150, 106, 100, 100, 114, 130, 122, 100, 120, 130, 115, 112, 126,
          110, 120, 128) %>% 
  data.frame()

ggplot(data=price, aes(x=.))+
  geom_boxplot()
ggplot(data=price, aes(x=.))+
  geom_histogram(bins=10)
#Not really that normal
#outlier
#independent? Probably

mean(price$.) + qt(.975, 19)*((sd(price$.)/sqrt(count(price))))
mean(price$.) - qt(.975, 19)*((sd(price$.)/sqrt(count(price))))

#Price is 116.05 (110.02 - 122.08)

t.test(price$., mu=118)
#Not significant to reject null hypothesis that mean house price is different than £118,000

##############################

#TWO SAMPLE T-TEST
qt(.975, 18)

women <- c(75,77,78,79,77,73,78,79,78,80) 
men <- c(74,72,77,76,76,73,75,73,74,75)

sex <- data.frame(men,women) 

sex2 <- sex %>% 
  pivot_longer(1:2, names_to="boo", values_to="tits") %>% 
  arrange(desc(boo))

t.test(sex2$tits~sex2$boo, var.equal=T) ##be sure to add var.equal=T

(mean(women)-mean(men))/
sqrt((sd(sex$men)/sqrt(count(sex)))^2 + (sd(sex$women)/sqrt(count(sex)))^2) # this gives difference in standard error

#CONF INTERVAL
(mean(women)-mean(men)) + qt(.975, 18)*sqrt((sd(sex$men)/sqrt(count(sex)))^2 + (sd(sex$women)/sqrt(count(sex)))^2)
(mean(women)-mean(men)) - qt(.975, 18)*sqrt((sd(sex$men)/sqrt(count(sex)))^2 + (sd(sex$women)/sqrt(count(sex)))^2)

##################################
#TASK 2
#TWO-SAMPLE T TEST
Salary <- c(17.7, 17.2, 20.2, 34.0, 36.4, 11.3, 24.0, 17.6, 26.0, 25.7, 17.2, 14.1, 22.0, 17.2, 20.9,
  16.8, 19.3, 15.8, 27.0, 20.4, 25.5, 30.1, 28.3, 29.5, 31.6, 18.9, 10.5, 17.5, 13.1, 13.0, 18.2, 22.0,
  13.0, 25.0, 12.2, 10.3, 15.5, 24.4, 11.8, 15.0, 25.6, 11.8, 22.8, 19.4, 12.3, 22.7, 27.3, 16.0, 11.0,
  12.6) %>% 
  data.frame() %>% 
  mutate(row=row_number(),
         Sector = case_when(row<26~"private", 
                            T~"public")) %>% 
  select(1,3)

tapply(Salary$., Salary$Sector, summary)
tapply(Salary$., Salary$Sector, sd)
tapply(Salary$., Salary$Sector, mean)

ggplot(Salary, aes(x=.))+
  geom_histogram(binwidth = 3)+
  facet_wrap(~Sector)

ggplot(Salary, aes(x=.))+
  geom_boxplot()+
  facet_wrap(~Sector,ncol=1)

t.test(Salary$.~Salary$Sector, var.equal=T)

##CONFIDENCE INTERVAL MANUALLY
(22.632-16.876) + qt(.975,48)*sqrt((6.550046/sqrt(25))^2+(5.388203/sqrt(25))^2)
(22.632-16.876) - qt(.975,48)*sqrt((6.550046/sqrt(25))^2+(5.388203/sqrt(25))^2)



######################################
#EXAMPLE 4
##BINOMIAL APPXOIMATION
#DIFFERENCES IN PROPORTIONS
summary(blooddata)

prop.table(table(blooddata$Hemisphere, blooddata$Taster), margin = 1)

prop.test(table(blooddata$Hemisphere, blooddata$Taster))

#conf interval -TASTERS
156/213 + 1.96*sqrt(((156/213)*(1-(156/213)))/213)
156/213 - 1.96*sqrt(((156/213)*(1-(156/213)))/213)

#conf interval - NON TASTERS
57/213 - 1.96*sqrt(((57/213)*(1-(57/213)))/213)
57/213 + 1.96*sqrt(((57/213)*(1-(57/213)))/213)

######################################
##TASK 4
#raw counts
table(blooddata$BloodgroupO)

#prop
prop.table(table(blooddata$BloodgroupO))

#TEST FOR BLOOD TYPE 0
prop.test(77,213)
#conf interval - BLOOD TYPE 0
77/213 + 1.96*sqrt(((77/213)*(1-(77/213)))/213)
77/213 - 1.96*sqrt(((77/213)*(1-(7/213)))/213)

#we reject the null hypothesis and conclude that there is evidence that. In other words, the proportion of living humans with Blood Group O is highly likely not to equal 50%.


#NOW BLOOD TYPE BY MALE/FEMALE
#FIRST TABLE IS COLUMNS, SECOND IS ROWS
prop.table(table(blooddata$Sex, blooddata$BloodgroupO), margin = 1)

prop.test(table(blooddata$Sex, blooddata$BloodgroupO))
# The p-value is 0.73 which is and so we cannot reject the null hypothesis, that the population proportion of females with Blood Group O is equal to the population proportion of males with Blood Group O. An approximate 95% confidence interval for the difference in humans with Blood Group O between the sexes is (-0.17, 0.11). This interval includes zero, and so we can conclude that there is not enough evidence to suggest that there is a difference between genders.

######################################
#TASK 5

#Mean:
average <- 346.2/43

#Variance
(1/42)*(3066.94-(43*(346.2/43)^2))

#Standard deviation
sd <- sqrt((1/42)*(3066.94-(43*(346.2/43)^2)))

#confidence interval
average + qt(.975, 42)*(sd/sqrt(43))
average - qt(.975, 42)*(sd/sqrt(43))

#Mean tricep thickness in Glasgow is 8.05 (7.25-8.85)
#Since the confidence interval does not contain 9.3 we can safely assume that mean tricep size is significantly *less* than in SE England

######################################
##TASK 6


colours %>% 
  pivot_longer(2:3, names_to="cat", values_to="time") %>% 
  ggplot(aes(x=time))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~cat)

colours %>% 
  pivot_longer(2:3, names_to="cat", values_to="time") %>% 
  ggplot(aes(x=time))+
  geom_boxplot()+
  facet_wrap(~cat,ncol=1)

summary(colours)

##Trying to determine whether there is a difference in time between black and coloured text
##in other words, H0=0, H1!=0

#creating a new variable of the difference
#DO NOT APPLY INDEPENDENT SAMPLES T-TEST FOR PAIRED DATA
colours <-  
  colours %>% 
  mutate(diff = DiffCol-Black)

sd(colours$diff)
mean(colours$diff)

#ONE SAMPLE T-TEST AS IT IS ONLY ONE GROUP (SAME AS BASELINE + FOLLOW-UP)
t.test(colours$diff, mu=0)
#The time required to read coloured text was significantly higher than for black text (P<.05)
#Mean time: 2.3 (.44 to 4.2 minutes extra)


######################################
##TASK 7

summary(ICU)
sd(ICU$R)
sd(ICU$NR)

#Equation
(mean(ICU$NR)-mean(ICU$R))/ #difference in mean
  sqrt((sd(ICU$NR)/sqrt(count(ICU))^2) + (sd(ICU$R)/sqrt(count(ICU))^2)) ##difference in standard error

(mean(ICU$NR)-mean(ICU$R)) + qt(.975, 38) * sqrt((sd(ICU$NR)/sqrt(count(ICU)))^2 + (sd(ICU$R)/sqrt(count(ICU)))^2)
(mean(ICU$NR)-mean(ICU$R)) - qt(.975, 38) * sqrt((sd(ICU$NR)/sqrt(count(ICU)))^2 + (sd(ICU$R)/sqrt(count(ICU)))^2)

#Mean difference of 7.9 (4.75 - 11.05)


#T-Test
ICU2<- ICU %>% 
  pivot_longer(1:2, names_to = "cat", values_to="total")

t.test(ICU2$total~ICU2$cat, var.equal = T)
#Reject H0, there is significant difference.

######################################
##TASK 8
summary(Physiology)

tapply(Physiology$Haemo, Physiology$Sex, summary)

tapply(Physiology$Haemo, Physiology$Sex, mean)
tapply(Physiology$Haemo, Physiology$Sex, sd)

Physiology %>% 
  ggplot(aes(x=Haemo))+
  geom_histogram(bins=5)+
  facet_wrap(~Sex)

Physiology %>% 
  ggplot(aes(x=Haemo))+
  geom_boxplot()+
  facet_wrap(~Sex,ncol=1)

#Is this a normal distribution?

t.test(Physiology$Haemo~Physiology$Sex, var.equal=T)

#Probably not reject null hypothesis that there is evidence of difference between M/F haemoglobin. However the p-value suggests looking at this further



