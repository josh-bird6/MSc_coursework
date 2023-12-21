###################
###################
###################
###Set-up script###
###################
###################
###################

#####################
##Reading in datasets
#####################

library(tidyverse)
library(DT)
library(shiny)
library(shinyWidgets)
library(shinymanager)
library(shinyBS)
library(bsplus)
library(plotly)

################
##Importing data
################

#first creating single data frame with filepaths for all the datasets to import
datasets <- list.files('Data') %>% 
  data.frame() %>% 
  slice(-1:-2) %>% 
  mutate(filepath = paste0("Data/",.))

#concatenating all datsets into a base dataset  
basedata <- datasets %>% 
  rowwise() %>% 
  do(., read_csv(file = .$filepath))

#grabbing locations dataset for info about each individual monitoring station
locations <- read_csv(file = "Data/__Stations.csv") %>% 
  mutate(AirQualityStationEoICode = EoICode) %>% 
  select(6:9)

#Joining onto base dataset
basedata_final <- 
  basedata %>% 
  left_join(locations, by="AirQualityStationEoICode") 

#formatting date column
basedata_final$yearmon <- lubridate::myd(paste(basedata_final$Month, basedata_final$Year, basedata_final$Day))

#################################
##creating drop down menu options
#################################

station_name <- unique(basedata_final$StationName) %>% sort()
station_code <- unique(basedata_final$AirQualityStationEoICode)
pollutant_name <- unique(basedata_final$AirPollutant)

categories <- unique(dailyaverage$category)


#############################################################

dailyaverage <- basedata_final %>% 
  group_by(yearmon, StationName, AirPollutant) %>% 
  summarise(`Daily average` = mean(Concentration),
            `Daily max` = max(Concentration)) %>% 
  pivot_longer(4:5, names_to = "category", values_to="total")


dailyaverage %>% 
  filter(AirPollutant == "PM10" & 
           StationName %in% c("Brno-Kroftova", "Brno-Lisen", "Brno-Sobesice" )&
           category=="dailyaverage") %>% 
  ggplot(aes(x=yearmon, y=total, group = StationName, color=StationName)) +
  geom_line(aes(group = StationName, color=StationName)) 


  
#############################################################

plottheme <- 
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20),
        legend.position = "bottom") 
