




shinyServer(function(input, output, session) {
    mcycle2 <- reactive( { # Define reactive object mcycle2
        subset(mcycle, times>=input$range[1] & times<=input$range[2])
        # Label from ui.R ˆˆˆˆˆ ˆˆˆˆˆ
    } )
    
    updateSelectizeInput(session, 'selectize1',
                         choices = unique(basedata_final$StationName),
                         server = TRUE
                         )
    
    output$plot1 <- renderPlot( {
        ggplot(mcycle2(), aes(times, accel))+
            geom_point()+
            geom_smooth(method = "lm")+
            scale_x_continuous(limits = range(mcycle$times))+
            scale_y_continuous(limits = range(mcycle$accel))
    })
    
    output$text1 <- renderPrint( {
        summary(mcycle2())
        # ˆˆˆˆˆˆˆˆˆ Get the reactive object
    } )
    output$table1 <- renderDataTable( {
        mcycle2()
        # ˆˆˆˆˆˆˆˆˆ Get the reactive object
    } )
    output$download1 <- downloadHandler(
        filename = "mcycle-subset.csv",
        content = function(file) {
            write.csv(mcycle2(), file, row.names=FALSE)
            # ˆˆˆˆˆˆˆˆˆ Get the reactive object
        }
    )
})
