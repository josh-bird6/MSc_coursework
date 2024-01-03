####################
####################
####################
###Server###########
####################
####################
####################


shinyServer(function(input, output, session)
    {
    observeEvent(
        input$link_to_home,
        {
            updateTabsetPanel(session, "Panels",
                              selected = "Introduction")
        })
    
    ################
    ##Raw data tab##
    ################
    
    #We need to include the input for station and pollutant type in the server section rather than the UI section because station is dependent on pollutant input
    
    output$Pollutant_rawdata <- renderUI({
        pickerInput(
            inputId = "Pollutant_rawdata",
            label = "Pollutant",
            selected = NULL,
            multiple = TRUE,
            choices = pollutant_name,
            options = list(size = 10,
                           "max-options" = 1))
    })
    
    output$Station_Name_rawdata <- renderUI({
        pickerInput(
            inputId="Station_Name_rawdata",
            label = "Station Name (select up to 3)",
            choices = (if(input$Pollutant_rawdata == "NO2")
                NO2_stations
                else if(input$Pollutant_rawdata == "PM10")
                    PM10_stations
                else if(input$Pollutant_rawdata == "PM2.5")
                    PM2.5_stations
                else SO2_stations),
            multiple = TRUE,
            selected = NULL,
            options = list(
                size = 10,
                `live-search` = TRUE,
                `selected-text-format` = "count > 1",
                `count-selected-text` = "{0} locations chosen",
                "max-options" = 3,
                "max-options-text" = "Only 3 options can be chosen"
            )
        )
    })

    #Plot graph based on the user input.
    #First we create a subset based on user input

    tab1_dataoutput <- reactive({
        basedata_final %>%
            filter(Year %in% input$Year
                   & StationName %in% input$Station_Name_rawdata
                   & AirPollutant %in% input$Pollutant_rawdata 
            ) %>% 
            select(Year, StationName, Pollutant, Concentration, yearmon)
    })
    
    #############
    #Plot output#
    #############
    
    output$rawdata_plot <- renderPlot({

        ## Default output is message in a blank ggplot object telling users to make proper selections

        if (is.null(input$Station_Name_rawdata) |
            is.null(input$Pollutant_rawdata)
        )

        {
            set.seed(20)
            x <- rnorm(10)
            y <- rnorm(10,1,.5)

            data.frame(x,y) %>%
                ggplot(aes(x,y))+
                labs(x="",
                     y="")+
                theme(panel.grid = element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())+
                annotate("text", x=5,5, label = "Please select a pollutant and a station from the drop down menus above", size=11)
        }

        # If conditions are met, chart is plotted

        else {

            ggplot(tab1_dataoutput(), aes(x=yearmon, y = Concentration, group = StationName, color = StationName))+
                geom_point(aes(group = StationName, color = StationName))+
                labs(title = paste0(input$Pollutant_rawdata),
                     y= paste0(input$Pollutant_rawdata, " concentration (µg/m3) recorded daily"),
                     x="Date",
                     caption = "Source: European Environmental Agency",
                     color = "Station Name") +
                plottheme +
                scale_y_continuous(limits = c(0,max(tab1_dataoutput()$Concentration)))

        }

    })

    ##############
    #Table output#
    ##############

    output$rawdata_table <- renderDataTable({
        tab1_dataoutput()

    })

    #################
    #Download button#
    #################

    output$download_rawdata <- downloadHandler(
        filename = "pollutant-rawdata-subset.csv",
        content = function(file) {
            write.csv(tab1_dataoutput(), file, row.names=FALSE)
        }
    )
    
    ##############################
    ##Aggregated time series tab##
    ##############################

    #We need to include the input for station and pollutant type in the server section rather than the UI section because station is dependent on pollutant input
    
    output$Pollutant <- renderUI({
        pickerInput(
            inputId = "Pollutant",
            label = "Pollutant",
            selected = NULL,
            multiple = TRUE,
            choices = pollutant_name,
            options = list(size = 10,
                           "max-options" = 1))
    })
    
    output$Station_Name <- renderUI({
        pickerInput(
            inputId="Station_Name",
            label = "Station Name (select up to 3)",
            choices = (if(input$Pollutant == "NO2")
                NO2_stations
                else if(input$Pollutant == "PM10")
                    PM10_stations
                else if(input$Pollutant == "PM2.5")
                    PM2.5_stations
                else SO2_stations),
            multiple = TRUE,
            selected = NULL,
            options = list(
                size = 10,
                `live-search` = TRUE,
                `selected-text-format` = "count > 1",
                `count-selected-text` = "{0} locations chosen",
                "max-options" = 3,
                "max-options-text" = "Only 3 options can be chosen"
            )
        )
    })
    
    #Plot graph based on the user input.
    #First we create a subset based on user input
    
    tab2_dataoutput <- reactive({
        Tab2_Dataset %>%
            filter(StationName %in% input$Station_Name 
                   & AirPollutant %in% input$Pollutant 
                   & categories %in% input$Category
            )
        })
    
    #############
    #Plot output#
    #############
    
    output$timeseries_plot <- renderPlot({
        
        ## Default output is message in a blank ggplot object telling users to make proper selections
        
        if (is.null(input$Station_Name) |
             is.null(input$Pollutant) |
             is.null(input$Category)
        )
            
        { 
            set.seed(20)
            x <- rnorm(10)
            y <- rnorm(10,1,.5)
            
            data.frame(x,y) %>% 
                ggplot(aes(x,y))+
                labs(x="",
                       y="")+
                theme(panel.grid = element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())+
                annotate("text", x=5,5, label = "Please select a pollutant and a station and a metric \nfrom the drop down menus above", size=11)
           }
        
        # If conditions are met, chart is plotted
        
        else {

            ggplot(tab2_dataoutput(), aes(x=yearmon, y = total, group = StationName, color = StationName))+
            geom_line(aes(group = StationName, color = StationName))+
            labs(title = paste0(input$Pollutant, " ",input$Category),
                 y= paste0(input$Category, " ", input$Pollutant, " concentration (µg/m3)"),
                 x="Date",
                 caption = "Source: European Environmental Agency",
                 color = "Station Name") +
            plottheme +
            scale_y_continuous(limits = c(0,max(tab2_dataoutput()$total)))
    
        }
            
    })
    
    ##############
    #Table output#
    ##############
    
    output$timeseries_table <- renderDataTable({
        tab2_dataoutput()
        
    })
    
    #################
    #Download button#
    #################
    
    output$download_timeseries <- downloadHandler(
                filename = "pollutant-aggregated-subset.csv",
                content = function(file) {
                    write.csv(tab2_dataoutput(), file, row.names=FALSE)
                }
            )
    
    
})
