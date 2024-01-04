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
                annotate("text", x=5,5, label = "Please select a pollutant and a station and a metric \nfrom the drop down menus above \n\n(Note that the chart may take some time to \nrender due to the size of the underlying dataset)", size=11)
           }
        
        # If conditions are met, chart is plotted
        
        else {

           vis_tab2 <- ggplot(tab2_dataoutput(), aes(x=`Date`, y = total, group = StationName, color = StationName))+
            geom_line(aes(group = StationName, color = StationName))+
            labs(title = paste0(input$Pollutant, " ",input$Category),
                 y= paste0(input$Category, " ", input$Pollutant, " concentration (µg/m3)"),
                 x="Date",
                 caption = "Source: European Environmental Agency",
                 color = "Station Name") +
            plottheme +
            scale_y_continuous(limits = c(0,max(tab2_dataoutput()$total))) +
            scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y")
    
            if(input$Category == "Daily average" 
               & input$Pollutant == "PM10")
            {
                vis_tab2 + geom_hline(yintercept = 50, linetype = "dashed") +
                    labs(subtitle = "Dashed line corresponds to daily average limit for PM10 (see Introduction page for more information)")
                
            }
            else {
                
                vis_tab2
                
            }
            
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
    
    #####################################
    ##End of aggregated time series tab##
    #####################################
    
    ##############################
    ##Yearly time series tab######
    ##############################

    #We need to include the input for station and pollutant type in the server section rather than the UI section because station is dependent on pollutant input

    output$Pollutant_yearly <- renderUI({
        pickerInput(
            inputId = "Pollutant_yearly",
            label = "Pollutant",
            selected = NULL,
            multiple = TRUE,
            choices = pollutant_name,
            options = list(size = 10,
                           "max-options" = 1))
    })

    output$Station_Name_yearly <- renderUI({
        pickerInput(
            inputId="Station_Name_yearly",
            label = "Station Name (select up to 3)",
            choices = (if(input$Pollutant_yearly == "NO2")
                NO2_stations
                else if(input$Pollutant_yearly == "PM10")
                    PM10_stations
                else if(input$Pollutant_yearly == "PM2.5")
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

    tab3_dataoutput <- reactive({
        Tab3_Dataset %>%
            filter(StationName %in% input$Station_Name_yearly
                   & AirPollutant %in% input$Pollutant_yearly 
                   & categories %in% input$Category_yearly
            )
    })

    #############
    #Plot output#
    #############

    output$yearly_plot <- renderPlot({

        ## Default output is message in a blank ggplot object telling users to make proper selections

        if (is.null(input$Station_Name_yearly) |
            is.null(input$Pollutant_yearly) |
            is.null(input$Category_yearly)
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

           vis_tab3 <-  ggplot(tab3_dataoutput(), aes(x=`Date`, y = total, group = StationName, color = StationName))+
                geom_point(aes(group = StationName, color = StationName))+
                labs(title = paste0(input$Pollutant_yearly, " ",input$Category_yearly, " by month, 2013-2018"),
                     y= paste0(input$Category_yearly, " ", input$Pollutant_yearly, " concentration (µg/m3)"),
                     x="Month",
                     caption = "Source: European Environmental Agency",
                     color = "Station Name") +
                plottheme +
                theme(axis.text.x = element_text(angle = 0, hjust=.5, vjust=.5))+
                scale_y_continuous(limits = c(0,max(tab3_dataoutput()$total))) +
                scale_x_date(date_breaks = "month", date_labels = "%B")
           
           #Adding horizontal lines for daily limits
           
         if(input$Category_yearly == "Daily average" 
              & input$Pollutant_yearly == "PM10")
           {
               vis_tab3 + geom_hline(yintercept = 50, linetype = "dashed") +
                 labs(subtitle = "Dashed line corresponds to daily average limit for PM10 (see Introduction page for more information)")
               
         }
           
           # else if(input$Category_yearly == "Daily average"
           #         & input$Pollutant_yearly == "SO2")
           # {
           #     vis_tab3 + geom_hline(yintercept = 125, linetype = "dashed") +
           #         labs(subtitle = "Dashed line corresponds to daily average limit for SO2 (see Introduction page for more information")
           # }
           
           else {
               vis_tab3
           }

        }

    })

    ##############
    #Table output#
    ##############

    output$yearlydata_table <- renderDataTable({
        tab3_dataoutput() %>% 
            select(-`Date`)

    })

    #################
    #Download button#
    #################

    output$download_yearly <- downloadHandler(
        filename = "pollutant-aggregated-subset.csv",
        content = function(file) {
            write.csv(tab3_dataoutput(), file, row.names=FALSE)
        }
    )

    #################################
    ##End of yearly time series tab##
    #################################
    
    # ##############################
    # ##Hourly time series tab######
    # ##############################

    #We need to include the input for station and pollutant type in the server section rather than the UI section because station is dependent on pollutant input

    output$Pollutant_hourly <- renderUI({
        pickerInput(
            inputId = "Pollutant_hourly",
            label = "Pollutant",
            selected = NULL,
            multiple = TRUE,
            choices = pollutant_name,
            options = list(size = 10,
                           "max-options" = 1))
    })

    output$Station_Name_hourly <- renderUI({
        pickerInput(
            inputId="Station_Name_hourly",
            label = "Station Name (select up to 3)",
            choices = (if(input$Pollutant_hourly == "NO2")
                NO2_stations
                else if(input$Pollutant_hourly == "PM10")
                    PM10_stations
                else if(input$Pollutant_hourly == "PM2.5")
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

    tab4_dataoutput <- reactive({
        Tab4_Dataset %>%
            filter(StationName %in% input$Station_Name_hourly
                   & AirPollutant %in% input$Pollutant_hourly
                   & categories_hourly %in% input$Category_hourly
            )
    })

    #############
    #Plot output#
    #############

    output$hourly_plot <- renderPlot({

        ## Default output is message in a blank ggplot object telling users to make proper selections

        if (is.null(input$Station_Name_hourly) |
            is.null(input$Pollutant_hourly) |
            is.null(input$Category_hourly)
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

            vis_tab4 <-  ggplot(tab4_dataoutput(), aes(x=Hour, y = total, group = StationName, color = StationName))+
                geom_point(aes(group = StationName, color = StationName))+
                labs(title = paste0(input$Pollutant_hourly, " ",input$Category_hourly, " by hour, 2013-2018"),
                     y= paste0(input$Category_hourly, " ", input$Pollutant_hourly, " concentration (µg/m3)"),
                     x="Hour of the day",
                     caption = "Source: European Environmental Agency",
                     color = "Station Name") +
                plottheme +
                scale_y_continuous(limits = c(0,max(tab4_dataoutput()$total)))+
                scale_x_continuous(breaks = seq(0,23,1))
            
            vis_tab4

            #Adding a line for daily limit
# 
#             if(input$Category_hourly == "Daily average"
#                & input$Pollutant_hourly == "PM10")
#             {
#                 vis_tab4 + geom_hline(yintercept = 50, linetype = "dashed") +
#                     labs(subtitle = "Dashed line corresponds to daily average limit for PM10 (see Introduction page for more information)")
# 
#             }
#             else {
#                 vis_tab4
#             }

        }

    })

    ##############
    #Table output#
    ##############

    output$hourlydata_table <- renderDataTable({
        tab4_dataoutput()

    })

    #################
    #Download button#
    #################

    output$download_hourly <- downloadHandler(
        filename = "pollutant-aggregated-subset.csv",
        content = function(file) {
            write.csv(tab4_dataoutput(), file, row.names=FALSE)
        }
    )

    #################################
    ##End of hourly time series tab##
    #################################
    
})
