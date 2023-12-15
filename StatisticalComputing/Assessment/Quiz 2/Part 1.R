#loading libraries
library(tidyverse)

#Number of characters from the house 'Lannister'
characters %>% 
  group_by(houseName) %>% 
  summarise(total = n()) %>% 
  filter(houseName == "Lannister")

#Locations of scenes in S2E3

scenes %>% 
  filter(episode == "S2E03") %>% 
  select(episode, number, location)


#Add column Northerner based on family name
characters %>% 
  mutate(northerner = case_when(houseName %in% c("Mormont", "Bolton", "Stark")~TRUE,
                                T~FALSE))

#Number of kills

#First grabbing Starks
Starks <- characters %>% 
  filter(houseName=="Stark") %>% 
  mutate(killedBy = characterName) 

#Joining Starks dataset onto kills dataset and visualising
kills %>% 
  left_join(Starks, by="killedBy") %>% 
  filter(houseName == "Stark") %>% 
  group_by(characterName) %>% 
  summarise(`Number of kills` = n()) %>% 
  ggplot(aes(x=characterName, y = `Number of kills`))+
  geom_col() +
  theme_bw()+
  labs(x="Character")

##Episode with the most scenes
scenes %>% 
  group_by(episode) %>% 
  summarise(numberofscenes = n()) %>% 
  arrange(desc(numberofscenes))

#numbrer of scenes in each location

scenes %>% 
  filter(location %in% c("North of the Wall", "The North", "The Wall", "The Crownlands")) %>% 
  mutate(Season = str_sub(episode, 2, 2)) %>% 
  group_by(location, Season) %>% 
  summarise(`Number of scenes`=n()) %>% 
  ggplot(aes(x=Season, y=`Number of scenes`, group = location))+
  geom_line(aes(group = location, colour = location))+
  theme_bw()

#######################################################
#first transforming to long format

rainfall_transformed <-
  rainfall%>% 
  pivot_longer(names_to = "region", values_to = "totalrain", cols = 3:17)

##Average rainfaill for each region
rainfall_transformed %>% 
  group_by(region) %>% 
  summarise(averagerainfall = mean(totalrain),
            medianrainfall = median(totalrain)) %>% 
  arrange(desc(averagerainfall))

#Average rainfall for each month across all years and regions
rainfall_transformed %>% 
  group_by(Month) %>% 
  summarise(averagerainfall = mean(totalrain))%>% 
  arrange(desc(averagerainfall))

#Average rainfall for each year across all months and regions
rainfall_transformed %>% 
  group_by(Year) %>% 
  summarise(averagerainfall = mean(totalrain)) %>% 
  ggplot(aes(x=Year, y=averagerainfall)) +
  geom_line() +
  geom_smooth()
