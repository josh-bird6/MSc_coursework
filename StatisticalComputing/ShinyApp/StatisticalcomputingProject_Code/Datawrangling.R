########################
########################
########################
###Data wrangling tab###
########################
########################
########################

#####################################
##Creating plot theme for first tab##
#####################################

plottheme <- 
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20),
        legend.position = "bottom",
        plot.caption = element_text(size=15)) 


###################################
##Creating dataset for second tab##
###################################

Tab2_Dataset <- basedata_final %>% 
  group_by(yearmon, StationName, AirPollutant) %>% 
  summarise(`Daily average` = mean(Concentration),
            `Daily max` = max(Concentration)) %>% 
  pivot_longer(4:5, names_to = "category", values_to="total") %>% 
  mutate(total = round(total, 1))

#################################
##creating drop down menu options
#################################

#Selection for year
yearselect <- unique(basedata_final$Year) %>% sort()

#Selction for pollutant name
pollutant_name <- unique(basedata_final$AirPollutant)

#Creating station options for individual pollutants, as some stations don't record everything - in other words, users can only select one pollutant at a time

#first the cleaning function
stationname_function <- function(pollutant) {
  basedata_final %>%
    filter(AirPollutant == pollutant) %>%
    select(StationName) %>%
    unique() %>%
    arrange(StationName)
}
#Now creating a dataframe for each pollutant with respective station names

NO2_stations <- stationname_function("NO2")
PM10_stations <- stationname_function("PM10")
PM2.5_stations <- stationname_function("PM2.5")
SO2_stations <- stationname_function("SO2")

#Metrics
categories <- unique(Tab2_Dataset$category)

