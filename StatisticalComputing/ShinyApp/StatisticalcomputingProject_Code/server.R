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
    
    ###############
    ##Geography tab
    ###############

    
    #we can then plot the graph based on the user input.
    #First we create a subset based on user input
    
    geography_new <- reactive({
        dailyaverage %>%
            filter(StationName %in% input$Station_Name 
                   & AirPollutant %in% input$Pollutant 
                   & categories %in% input$Category
            )
        })
    
    #############
    #Plot output#
    #############
    
    output$geography_plot <- renderPlot({
        
        ## Default output is message in a blank ggplot objecttelling users to make proper selections
        
        if (is.null(input$Station_Name) |
             is.null(input$Pollutant)
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
                annotate("text", x=5,5, label = "Please select a station and a pollutant from the drop down menus above", size=11)
           }
        
        # If conditions are met, chart is plotted
        
        else {

            ggplot(geography_new(), aes(x=yearmon, y = total, group = StationName, color = StationName))+
            geom_line(aes(group = StationName, color = StationName))+
            labs(title = paste0(input$Pollutant, " ",input$Category),
                 y= paste0(input$Category, " ", input$Pollutant, " concentration (µg/m3)"),
                 x="Date",
                 caption = "Source: European Environmental Agency",
                 color = "Station Name") +
            plottheme +
            scale_y_continuous(limits = c(0,max(geography_new()$total)))
    
        }
            
    })
    
    ##############
    #Table output#
    ##############
    
    output$geography_table <- renderDataTable({
        geography_new()
        
    })
    
    #################
    #Download button#
    #################
    
    output$download_timetrend <- downloadHandler(
                filename = "pollutant-subset.csv",
                content = function(file) {
                    write.csv(geography_new(), file, row.names=FALSE)
                    
                }
            )
    
    
})












# 
# 
# 
# shinyServer(function(input, output, session) {
#     mcycle2 <- reactive( { # Define reactive object mcycle2
#         subset(mcycle, times>=input$range[1] & times<=input$range[2])
#         # Label from ui.R ˆˆˆˆˆ ˆˆˆˆˆ
#     } )
#     
#     updateSelectizeInput(session, 'selectize1',
#                          choices = unique(basedata_final$StationName),
#                          server = TRUE
#                          )
#     
#     output$plot1 <- renderPlot( {
#         ggplot(mcycle2(), aes(times, accel))+
#             geom_point()+
#             geom_smooth(method = "lm")+
#             scale_x_continuous(limits = range(mcycle$times))+
#             scale_y_continuous(limits = range(mcycle$accel))
#     })
#     
#     output$text1 <- renderPrint( {
#         summary(mcycle2())
#         # ˆˆˆˆˆˆˆˆˆ Get the reactive object
#     } )
#     output$table1 <- renderDataTable( {
#         mcycle2()
#         # ˆˆˆˆˆˆˆˆˆ Get the reactive object
#     } )
#     output$download1 <- downloadHandler(
#         filename = "mcycle-subset.csv",
#         content = function(file) {
#             write.csv(mcycle2(), file, row.names=FALSE)
#             # ˆˆˆˆˆˆˆˆˆ Get the reactive object
#         }
#     )
# })
