####Assessed project exercise

##Reading in project data: https://moodle.gla.ac.uk/mod/resource/view.php?id=3155640
##Description and source file: https://moodle.gla.ac.uk/pluginfile.php/5664272/mod_resource/content/2/Assessed_Project_Data_Description.pdf

#Paris and Copenhagen
View(paris)
View(copenhagen)


#QUESTION 5

#5a): number of reviews each property receives

summary(copenhagen$reviews)

summary(paris$reviews)



#Airbnb appears to be more embedded in Paris with this dataset comprising 70,158 properties compared to 21,301 for Copenhagen.

#Both cities have a median of four reviews per property, and also have a similar inter-quartile range: 14 for Paris and 11 for Copenhagen. 
#However the mean number of reviews is notably higher for Paris, with a mean (rounded) total of 15 reviews per property compared to 10 for Copenhagen
#This suggests the data is not symmertrically distributed, which is born out when summarising the data. Table 1 shows that the vast majority of properties have less than 10 reviews for both cities
#This is reaffirmed when visualising the data. Boxplots and histograms show that review data for both cities are skewed heavily right - that is, many properties have few reviews while only a few properties have many reviews. Paris also has a notable outlier, a single room in the Sainte-Marguerite neighbourhood with 529 reviews (the next closest is 472). This suggests that data transformation is necessary to conduct further analysis
#Another consideration is missing data; over one in five properties in Copenhagen (21%) and one in four properties in Paris (27%) have no reviews at all. This may be in part due to the fact that (as per the data dictionary) "reviews occasionally vanish from the site", so further exploration might be required to determine how much this factor impacts data quality.
#However, even when ommitting missing data, log transformation of datasets for reviews still produces positively-skewed distributions for both cities. It may therefore be the case that a different distribution should be considered, or a more robust form of inference may be required to evaluate reviews

##############################
copenhagen %>% 
  mutate(`Number of reviews`=case_when(reviews==0~"a. 0",
                               reviews>0 & reviews<11~"b. 1 to 10",
                               reviews>10 & reviews<51~"c. 11 to 50",
                               reviews>50 & reviews<101~"d. 51 to 100",
                               reviews>100 & reviews<201~"e. 101 to 200",
                               reviews>200 & reviews<301~"f. 201 to 300",
                               reviews>300 & reviews<401~"g. 301 to 400",
                               T~"h. over 400")) %>% 
  group_by(`Number of reviews`) %>% 
  summarise(total = n()) %>% 
  mutate(sums = sum(total),
         prop = (total/sums)*100) 


paris %>% 
  mutate(`Number of reviews`=case_when(reviews==0~"a. 0",
                                       reviews>0 & reviews<11~"b. 1 to 10",
                                       reviews>10 & reviews<51~"c. 11 to 50",
                                       reviews>50 & reviews<101~"d. 51 to 100",
                                       reviews>100 & reviews<201~"e. 101 to 200",
                                       reviews>200 & reviews<301~"f. 201 to 300",
                                       reviews>300 & reviews<401~"g. 301 to 400",
                                       reviews>400 & reviews<501~"h. 401 to 500",
                                       T~"i. over 500")) %>% 
  group_by(`Number of reviews`) %>% 
  summarise(total = n()) %>% 
  mutate(sums = sum(total),
         prop = (total/sums)*100)

# paris %>% 
#   group_by(reviews) %>% 
#   summarise(total = n()) %>% 
#   mutate(sums = sum(total),
#          prop = (total/sums)*100)

#######################################

copenhagen %>% 
  mutate(logreviews = log(reviews)) %>% 
  filter(reviews>0) %>%
  ggplot(aes(x=logreviews))+
  geom_histogram(bins = 13) +
  theme_bw() +
  labs(x= "Number of reviews (log-transformed)",
       y="Count",
       title="Copenhagen") +
  scale_y_continuous(labels=scales::comma)

copenhagen %>% 
  mutate(logreviews = log(reviews)) %>% 
  filter(reviews>0) %>%
  ggplot(aes(x=logreviews))+
  geom_boxplot()+
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Copenhagen",
       x= "Number of reviews (log-transformed)")

paris %>% 
  mutate(logreviews = log(reviews)) %>% 
  filter(reviews>0) %>%
  ggplot(aes(x=logreviews))+
  geom_histogram(bins = 13) +
  theme_bw() +
  labs(x= "Number of reviews (log-transformed)",
       y="Count",
       title="Paris") +
  scale_y_continuous(labels=scales::comma) 

paris %>% 
  mutate(logreviews = log(reviews)) %>% 
  filter(reviews>0) %>%
  ggplot(aes(x=logreviews))+
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Paris",
       x= "Number of reviews (log-transformed)")



#5b): Room type in each property
#DO NOT FORGET TO ADD THE N

paris %>% 
  group_by(room_type) %>% 
  summarise(total = n()) %>% 
  mutate(tottt = sum(total),
         prop = round((total/tottt)*100,1),
         city = "Paris \n(n=70,158)")

copenhagen %>% 
  group_by(room_type) %>% 
  summarise(total = n()) %>% 
  mutate(tottt = sum(total),
         prop = round((total/tottt)*100,1),
         city = "Copenhagen \n(N=21,301)")

#5c): Satisfaction

summary(paris$overall_satisfaction)

summary(copenhagen$overall_satisfaction)

paris %>% 
  mutate(`Overall Satisfaction` = overall_satisfaction) %>% 
  ggplot(aes(x=`Overall Satisfaction`))+
  geom_histogram(bins=10) +
  geom_vline(xintercept = mean(paris$overall_satisfaction)) +
  geom_vline(xintercept = median(paris$overall_satisfaction), linetype = "dashed") +
  theme_bw()+
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0,10, by=1))+
  labs(title = "Paris")

copenhagen %>% 
  mutate(`Overall Satisfaction` = overall_satisfaction) %>% 
  ggplot(aes(x=`Overall Satisfaction`))+
  geom_histogram(bins=10) +
  geom_vline(xintercept = mean(copenhagen$overall_satisfaction)) +
  geom_vline(xintercept = median(copenhagen$overall_satisfaction), linetype = "dashed") +
  theme_bw()+
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0,10, by=1))+
  labs(title = "Copenhagen")

paris %>% 
  group_by(reviews, overall_satisfaction) %>% 
  summarise(total = n()) %>% 
  mutate(totals = 70158,
         prop = (total/totals)*100) %>% 
  arrange(overall_satisfaction)

copenhagen %>% 
  group_by(reviews, overall_satisfaction) %>% 
  summarise(total = n()) %>% 
  mutate(totals = 21301,
         prop = (total/totals)*100) %>% 
  arrange(overall_satisfaction)

copenhagen %>% 
  group_by(overall_satisfaction) %>% 
  summarise(total = n()) %>% 
  mutate(totals = sum(total),
         prop = (total/totals)*100)


.6455573+qt(.975, df=12)*(sqrt(2.2890526)/sqrt(13))

.6455573-qt(.975, df=12)*(sqrt(2.2890526)/sqrt(13))
