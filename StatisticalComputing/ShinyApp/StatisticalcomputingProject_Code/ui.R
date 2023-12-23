####################
####################
####################
###User interface###
####################
####################
####################


######################
##Loading source files
######################

##First the data import file
#NOTE: THIS TAKES A WHILE TO IMPORT EVERYTHING SO JUST RUN FOR THE ONE TIME
#source(here::here('StatisticalcomputingProject_Code/Setupscript.R'))

##An extra file for some wrangling for each of the tabs



#################
##Beginning of UI
#################

fluidPage(
    style = "width: 100%; height: 100%; max-width: 1200px;",
    tags$head(
        tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
        ),
        tags$head(
            tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                        type="text/javascript")
        ),
        
       #The following chunk of code does two things:
       # 1. Paints the ribbon that contains the tab headers white
       # 2. Highlights the header of the active tab in blue
       
       tags$style(
           HTML("
                .tabbable > .nav > li > a {background-color: #def0ff; color: #000000;}
                .tabbable > .nav > li[class = active] > a {background-color: #0072B2;color: #FFFFFF;}
                ")
       )
        
    ),
    
    #Adding title panel title
    titlePanel(title=div(h1("Air pollution in Czechia"),
                         style = "height:96px;"),
               windowTitle="Air pollution in Czechia"),
    
    
##Separating into discrete sections

tabsetPanel(
    id = "Panels",
    
    ##########
    ##Home tab
    ##########
    
    tabPanel(
        "Introduction",
        icon = shiny::icon("info-circle"),
        style = "float: top; height: 95%; width: 95%;
        background-color: #FFFFFF; border: 0px solid #FFFFFF;",
        
        column(2),
        column(8,
               p(
                   br(),
                   "This dashboard provides a detailed breakdown of air pollution recorded in Czechia between 2013 and 2019. You can visualise these data using the following pages:"
                   ),
               tags$ul(tags$li("Time series data",
                   icon("line-chart"),
                   " - compare data by location and pollutant over time."
               )), 
               bs_accordion(id = "drhs_introduction_text") %>%
                   bs_set_opts(panel_type = "primary") %>%
                   bs_append(
                       title = tags$u("Technical information"),
                       content =
                           p(
                               "This dashboard provides information on air pollution recorded in Air Quality Stations across Czechia between 2013 and 2019.",
                               br(),
                               br(),
                               "Data is sourced from the",
                               tags$a(href = "https://www.eea.europa.eu/en/topics/in-depth/air-pollution", "European Environmental Agency,"),
                               " and information on individual pollutants, as well as the legal limits under EU law, can be found below:",
                               tags$ul(
                                   tags$li(HTML(
                                       paste0("<b>Fine particulates (PM2.5)</b>")
                                   ), "Yearly average of at most 20", HTML(paste0(
                                       "<em>µg/m", tags$sup("3"), ".</em>"
                                   ))),
                                   tags$li(
                                       HTML(paste0("<b>Particulates (PM10)</b>")),
                                       "Daily average exceeding 50",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), "</em>")),
                                       "observed on at most 35 days a year, and yearly average of at most 40",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), ".</em>"))
                                   ),
                                   tags$li(
                                       HTML(paste0(
                                           "<b>Sulphur dioxide (SO", tags$sub("2"), ")</b>"
                                       )),
                                       "Hourly concentration exceeding 350",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), "</em>")),
                                       "for at most 24 hours per year, and average daily concentration exceeding 125",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), "</em>")),
                                       "on at most 3 days per year."
                                   ),
                                   tags$li(
                                       HTML(paste0(
                                           "<b>Nitrogen dioxide (NO", tags$sub("2"), ")</b>"
                                       )),
                                       "Hourly concentration exceeding 200",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), "</em>")),
                                       "for at most 18 hours per year, and average yearly concentration of at most 40",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), ".</em>"))
                                   )
                               ),
                               br(),
                               
                               HTML(paste0("<b>NOTE</b>", ": Data is not available for every single pollutant at every single station on every single date.")),
                               br(),
                               br(),
                               "For more information on EU air quality standards, plesae visit the relevant ",
                               tags$a(href = "https://environment.ec.europa.eu/topics/air/air-quality_en", "European Commission "),
                               "webpage"
                           )
                   ),
               p(
                   "If you experience any problems using this dashboard, please contact me at:",
                   HTML(
                       paste0(
                           '<b> <a href="mailto:Joshua.Bird@gov.scot">Joshua.Bird@gov.scot</a></b>.'
                       )
                   )
               )
        )
    ), 
    
    ###############
    ##Geography tab
    ###############
    
        tabPanel(
        "Time series data",
        icon = icon("line-chart"),
        style = "height: 95%; width: 95%; background-color: #FFFFFF;
        border: 0px solid #FFFFFF;",
        
        h3("Time series data"),
        
        p(
            h4("Visualise air pollution data over time and make comparisons between stations. ")
                
            ),
        bs_accordion(id = "drhs_location_comparison_text") %>% 
            bs_set_opts(panel_type = "primary") %>%
            bs_append(title = tags$u("Data selection"), 
                      content = p(
                          "The chart can be modified using the drop down boxes:", 
                          tags$ul(
                              tags$li("Station name: the name of the station recording data (3 selections max)"),
                              tags$li(HTML(paste0("Pollutant: Fine particulates (PM2.5); Particulates (PM10); Sulphur dioxide (SO", tags$sub("2"), "); Nitrogen dioxide (NO", tags$sub("2"),")"))),
                              tags$li(HTML(paste0("Metric: Daily average or daily max concentration (measured in µg/m", tags$sup("3"), "."))),
                          ), 
                          "To download your data selection as a CSV file, use the
                  'Download data' button under the drop down boxes.", 
                          br(),br(),
                          "For technical information, please see the",
                          actionLink(
                              "link_to_home", "introduction"
                          ), " page."
                      ))%>%
            bs_append(title = tags$u("Table functions"), 
                      content = p(HTML("To view 
        your data selection in a table, use the
                            'Show/hide table'  button at the
                            bottom of the page."),
                                  tags$ul(
                                      tags$li(tags$b("Show entries"), " - change the number of rows shown
                            in the table using the drop-down box."),
                                      tags$li(tags$b("Search"), " - enter text to search data for a specific word or
                            numerical value."),
                                      tags$li(icon("sort", lib = "glyphicon"),
                                              tags$b("Sort"), " - click to sort the table in ascending or 
                            descending order based on the values in a column."),
                                      tags$li(tags$b("Page controls"), " - switch to specific page of data 
                            within the table.")
                                  )
                      )),
        p(""),
        
        wellPanel(
            tags$style(
                ".well { background-color: #FFFFFF;
        border: 0px solid #336699; }"
            ),

            #Insert the reactive filters.
            #We have three filters at this point
            # 1 - Station name
            # 2 - Pollutant
            # 3 - Metric (daily avg or daily max)

            column(
                4,
                shinyWidgets::pickerInput(
                    inputId = "Station_Name",
                    label = "Station Name (select up to 3)",
                    choices = station_name,
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
            ),
            
            column(
                4,
                shinyWidgets::pickerInput(
                    inputId = "Pollutant",
                    label = "Pollutant",
                    choices = pollutant_name,
                    selected = NULL,
                    options = list("max-options" = 1)
                )
            ),

         
            column(
                4,
                shinyWidgets::pickerInput(
                    inputId = "Category",
                    label = "Metric",
                    choices = categories
                )
            )
            
            
        ), 
        
        
        downloadButton(outputId = "download_timetrend", 
                       label = "Download data", 
                       class = "geographybutton"),
        
        tags$head(
            tags$style(".geographybutton { background-color: 
                   #0072B2; } 
                   .geographybutton { color: #FFFFFF; }")
        ),
        
        #In the main panel of the tab, insert the geography plot
        
        mainPanel(
            width = 12,
            plotOutput("geography_plot",
                         width = "1090px",
                         height = "500px") %>% 
                shinycssloaders::withSpinner(),
            br(),
            HTML("<button data-toggle = 'collapse' href = '#substances'
                   class = 'btn btn-primary' id = 'substances_link'> 
                   <strong> Show/hide table </strong></button>"),
            HTML("<div id = 'substances' class = 'collapse'>"),
            br(),
            dataTableOutput("geography_table"),
            HTML("</div>"),
            br(),
            br()
        )
        
        #End of first tab panel
        )
        )
)









# fluidPage(
#     titlePanel("Subsetting the motorcycle data"),
#     sidebarLayout(
#         sidebarPanel(
#             selectizeInput("selectize1", "Station", 
#                            selected = NULL,
#                            choices = NULL,
#                            multiple = F,
#                            options = list(maxItems=3,
#                                           placeholder = "Select a station (up to three max)")),
#             sliderInput("range", "Subset range", min=0, max=60, value=c(0,60)),
#             # ˆˆˆˆˆ Label used in server.R when querying slider
#             downloadButton("download1", "Download subset", shiny::icon("download"))
#             # ˆˆˆˆˆˆˆˆˆˆˆ Label used in server.R for download
#         ),
#         mainPanel(
#             tabsetPanel(
#                 tabPanel("Introduction",
#                          p("here is where the introduction text of the APP will go - description, methods, data sources and so on"),
#                 ),
#                 tabPanel("Plot",
#                          plotOutput("plot1")
#                          ),
#                 tabPanel("Text",
#                          verbatimTextOutput("text1")
#                 ),
#                 tabPanel("Table",
#                          dataTableOutput("table1")
#                          )
#                          
#             )
#         )
#     )
# )
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

