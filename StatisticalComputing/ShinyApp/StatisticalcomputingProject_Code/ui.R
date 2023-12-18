
#Load source file
source('StatisticalcomputingProject_Code/Setupscript.R')

##########################################################################

fluidPage(
    titlePanel("Subsetting the motorcycle data"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput("selectize1", "Station", 
                           selected = NULL,
                           choices = NULL,
                           multiple = F,
                           options = list(maxItems=3,
                                          placeholder = "Select a station (up to three max)")),
            sliderInput("range", "Subset range", min=0, max=60, value=c(0,60)),
            # ˆˆˆˆˆ Label used in server.R when querying slider
            downloadButton("download1", "Download subset", shiny::icon("download"))
            # ˆˆˆˆˆˆˆˆˆˆˆ Label used in server.R for download
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Introduction",
                         p("here is where the introduction text of the APP will go - description, methods, data sources and so on"),
                ),
                tabPanel("Plot",
                         plotOutput("plot1")
                         ),
                tabPanel("Text",
                         verbatimTextOutput("text1")
                ),
                tabPanel("Table",
                         dataTableOutput("table1")
                         )
                         
            )
        )
    )
)
        # mainPanel(
        #     tabsetPanel(
        #         tabPanel("Introduction",
        #                  p("Here is where the text for the introduction of the APP is going to go - description, methods, data sources and so on")
        #                  ),
        #         tabPanel("Output",
        #                  uiOutput("ui") # Put adaptive UI component here
        #                  ),
        #         tabPanel("Other output",
        #                  p("Another plot or some output...")

