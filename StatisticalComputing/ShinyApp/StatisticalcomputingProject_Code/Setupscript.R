###################
###################
###################
###Set-up script###
###################
###################
###################


##First, reading in datasets
library(tidyverse)
library(shiny)




##Next, importing data

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
