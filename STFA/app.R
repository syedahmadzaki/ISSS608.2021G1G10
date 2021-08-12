# Load key R libraries
library(shiny)
library(shinydashboard)
library(readr)
library(tidyverse)
#library(shinythemes)
library(readxl)
library(rsconnect)
library(sf)
library(tmap)
library(formattable)
library(DT)
library(colourpicker)
library(fresh)


# Define UI for application that draws a histogram
ui <-  navbarPage(title = "VAST Mini Challenge 2", selected = "Introduction", collapsible = TRUE, inverse = FALSE,
                  header = use_theme(
                    create_theme(
                      theme = "default",
                      bs_vars_navbar(
                        default_bg = "darkslategrey",
                        default_color = "#FFFFFF",
                        default_border = "darkslategrey",
                        default_link_color = "#FFFFFF",
                        default_link_active_color = "darkslategrey",
                        default_link_active_bg = "#FFFFFF",
                        default_link_hover_color = "#FFFFFF",
                        default_link_hover_bg = "gray"
                      ),        output_file = NULL
                    )),
                  #theme = shinytheme("flatly"),
                  
                  # Intro Tab - Mainly a write-up of our project.
                  
                  tabPanel("Introduction",icon = icon("chalkboard-teacher"),
                           fluidRow(
                             column(7,
                                    h2(HTML("<b>A Brief Introduction</b>")),
                                    tags$div(
                                      tags$p("This application explores the movement and tracking issues raised from VAST Challenge 2021: Mini Challenge 2."), 
                                      tags$p("With the aid of curated visualisations, we aim to forensically understand and investigate any suspicious activities, leading up to the disappearance of several employees."),
                                      tags$p("This application is broken down into the sections as per the image on the right.")),
                                    tags$div("Click here to find out more information about the ",
                                             tags$a(href="https://vast-challenge.github.io/2021/MC2.html", "VAST Challenge 2021"), ", as well as explore its accompanying dataset."),
                                    br(),
                                    tags$div("Alternatively, for more information on this project, do visit our ",
                                             tags$a(href="https://isss608-2021g1g10.netlify.app/proposal.html", "Group Project site"),
                                             "and our ",
                                             tags$a(href="https://github.com/syedahmadzaki/ISSS608.2021G1G10", "GitHub page.")),
                                    br(),
                                    tags$p("Lastly, we would like to thank Prof Kam for his invaluable guidance and saintly patience in making this application a reality.")
                             ), 
                             column(5,img(src = "overview.png", width = "100%")
                             ))),
                  
                  # ANOVA Tab - Explains how to use our Shiny app.
                  
                  tabPanel("ANOVA",icon = icon("tint"),
                           
                           # Application title
                           titlePanel("One-Way ANOVA - Difference in Spends"),
                           
                           sidebarLayout(
                             
                             # Sidebar for basic and advanced settings for ANOVA Plot
                             sidebarPanel(width = 2,
                                          h4(HTML("ANOVA Plot Settings")),
                                          
                                          # Users select plot type
                                          # p for parametric, np for non-parametric, r for robust and bayes for Bayes Factor
                                          selectInput(
                                            "type",
                                            label = ("Select test statistics"),
                                            choices = list("Parametric" = "p",
                                                           "Non-Parametric" = "np",
                                                           "Robust" = "r",
                                                           "Bayes Factor" = "bf"),
                                            selected = "np"),
                                          
                                          # Users confidence level for one-way ANOVA
                                          #selectInput(
                                          #"cf",
                                          #label = ("Select confidence level"),
                                          #choices = list("0.5" = 0.5,
                                          #"0.9" = 0.9,
                                          #"0.95" = 0.95,
                                          #"0.99" = 0.99),
                                          #selected = 0.95),
                                          
                                          # Users no. of bins for histogram
                                          sliderInput(
                                            "bins",
                                            label = ("No. of bins"),
                                            value = 5,
                                            min = 3,
                                            max = 30),
                                          
                                          # Users to submit 
                                          actionButton("submit","Submit")
                                          
                             ),
                             
                             # Show tab panels of file import, data cleaning and plot 
                             mainPanel(
                               tabsetPanel(
                                 
                                 # Instructions
                                 tabPanel("Guide",
                                          br(),
                                          fluidRow(
                                            column(6,
                                                   h4(HTML("<b>Understanding One-Way Analysis of Variance Test</b>")),
                                                   tags$div(
                                                     tags$p("Analysis of variance (ANOVA) test is conducted to check if the means of two or more groups are significantly different from each other. 
                                  For example, we could use one-way ANOVA to understand if the test scores between 5 different classes are significantly different."), 
                                                     tags$p("It is important to realize that one-way ANOVA is an omnibus test statistic which is unable to tell us which specific groups were statsitically significantly different from each other.
                                   Thus, we can make use of post-hoc test that can help us determine which of the groups differ from each other."), 
                                                     tags$p("By using the ggbetweenstats function from ggstatplot package, we can easily visualize the results of one-way ANOVA. 
                                    The visualization is built upon boxviolin plot to show how the dependent variable distribute for each group. 
                                    The function offers users the flexibility to select the type of hypothesis testing, including parametric, non-parametric, robust or Bayes Factor.
                                    It also automatically selects the appropriate post-hoc test and displays the results of the pairwise comparisons on the chart."),
                                                     tags$p("In our use case, we made use of credit card transaction data to showcase how such a visualization 
                            can help us understand the difference in spend amount across locations and credit card users.
                          The visualization aims to answer questions like 'Is the spending of user A significantly from user B?' and 'Is the mean spend at this location significantly different from other cafes?'"), 
                                                     tags$a(href="https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggbetweenstats.html", "Click here to find out more about ggstatplot R package.")),
                                                   br(),
                                                   h4(HTML("<b>Steps</b>")),
                                                   tags$ol(
                                                     tags$li("Begin by exploring the credit card transaction dataset under the 'EDA tab."),
                                                     tags$li("Select the ANOVA tab based on the type of groups you are interested in comparing - 'ANOVA - Between Locations' or 'ANOVA - Between Credit Cards'."),
                                                     tags$li("Select the locations or credit card numbers you would like to zoom in on within each tab."), 
                                                     tags$li("You can play with the number of bins to understand the nature of the data to select the right test statistics."), 
                                                     tags$li("For example, if the dataset is not normally distributed, you can select a non-parametric test."),
                                                     tags$li("Click on 'Submit' button and the plot will be updated with the one-way ANOVA and pairwise comparison results."),
                                                   ),
                                                   br(),
                                                   br(),
                                                   br()
                                            ), 
                                            column(6,tags$img(src="https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggbetweenstats_files/figure-html/ggbetweenstats1-1.png", width = "100%"),
                                                   tags$h6("Source: vignettes/web_only/ggbetweenstats.Rmd")
                                            ))
                                 ), 
                                 
                                 # EDA plots
                                 tabPanel("EDA",
                                          br(),
                                          fluidRow(h4(HTML("<b>Exploratory Data Analysis</b>"))),
                                          fluidRow(column(6,plotlyOutput("EDA_1A", height = 700)),
                                                   column(6,plotlyOutput("EDA_2A", height = 700)))
                                 ), 
                                 
                                 
                                 # Default chart 1
                                 tabPanel("ANOVA - Between Locations",
                                          br(),
                                          
                                          # Select location 
                                          fluidRow(selectInput(
                                            "locations",
                                            label = ("Select locations"),
                                            choices = location_list,
                                            selected = c("Kalami Kafenion", "Guy's Gyros","Brew've Been Served","Jack's Magical Beans"),
                                            multiple=TRUE)),
                                          
                                          
                                          fluidRow(column(8,plotOutput("ANOVAL", height = 600)),
                                                   column(4,plotlyOutput("histL", height = 600))),
                                          br(),
                                          br()
                                 ),
                                 
                                 
                                 # Default chart 2
                                 tabPanel("ANOVA - Between Credit Cards",
                                          br(),
                                          
                                          # Select credit card number 
                                          fluidRow(selectInput(
                                            "ccnums",
                                            label = ("Select credit card numbers"),
                                            choices = cc_list,
                                            selected = c("4795","5368","8129","2142"),
                                            multiple=TRUE)),
                                          
                                          
                                          fluidRow(column(8,plotOutput("ANOVAC", height = 600)),
                                                   column(4,plotlyOutput("histC", height = 600))),
                                          br(),
                                          br()
                                 )#,
                                 
                                 # User to upload data and show ANOVA chart
                                 #                                 tabPanel("Import Own Data",
                                 #                                          fluidRow(
                                 #                                            column(6,
                                 #                                                   AcsvFileUI("datafile", "User data (.csv format)"),
                                 #                                                   dataTableOutput("table")
                                 #                                            ),
                                 
                                 # To add plot here
                                 #                                            column(6,)
                                 #                                          )
                                 #                                 )
                                 
                                 
                                 
                               )
                             )
                           )
                           
                  ),
                  
                  # UpSet Tab - Explains how to use our Shiny app.
                  
                  tabPanel("Upset Chart",icon = icon("chart-bar")
                           
                           
                           
                  ),
                  
                  # Exploring GPS Tab - Explores the GPS data and locations on a map.
                  
                  tabPanel("Exploring GPS",icon = icon("map-marked-alt"),
                           #                           titlePanel("Using A Customised Map With Interactive Control"),
                           sidebarLayout(position = "left", fluid = TRUE,
                                         sidebarPanel(tags$style(".well {background-color:#e7e7e7;}"),
                                                      width = 3, fluid = TRUE,
                                                      h4("SELECT DATE", align = "center"),
                                                      dateInput(inputId = "MapDate",
                                                                min = ("2014-01-06"),
                                                                max = ("2014-01-19"),
                                                                #                               sliderInput(inputId = "MapDate",
                                                                #                                           min = as.Date("2014-01-06","%Y-%m-%d"),
                                                                #                                           max = as.Date("2014-01-19","%Y-%m-%d"),
                                                                value=("2014-01-06"),
                                                                format = "d M",
                                                                #                                           timeFormat="%e-%b",
                                                                #                                           step = 1,
                                                                label = NULL),
                                                      hr(style = "border-top: 3px solid #FFFFFF;"),
                                                      h4("COMPARE MAX THREE STAFF", align = "center"),
                                                      
                                                      # Select Staff ID 1
                                                      
                                                      selectInput(inputId="id1",
                                                                  label="Select Staff ID 1",
                                                                  choices = stafflist1,
                                                                  selected="IT Helpdesk Nils Calixto"),
                                                      fluidRow(
                                                        column(6,
                                                               colourInput(
                                                                 inputId = "id1colour",
                                                                 label = "Select Line Colour 1",
                                                                 palette = "limited",
                                                                 showColour = "background",
                                                                 "#000000"
                                                               )),
                                                        column(6,
                                                               selectInput(inputId = "id1linetype",
                                                                           label = "Select Line Type 1",
                                                                           choices = linetypelist,
                                                                           selected = "1")
                                                        )
                                                      ),
                                                      
                                                      # Select Staff ID 2
                                                      
                                                      selectInput(inputId = "id2",
                                                                  label = "Select Staff ID 2",
                                                                  choices = stafflist1,
                                                                  selected = "Engineer Lars Azada"),
                                                      fluidRow(
                                                        column(6,
                                                               colourInput(
                                                                 inputId = "id2colour",
                                                                 label = "Select Line Colour 2",
                                                                 palette = "limited",
                                                                 showColour = "background",
                                                                 "#0000FF"
                                                               )
                                                               #                                        selectInput(inputId = "id2colour",
                                                               #                                                    label = "Select Line Colour 2",
                                                               #                                                    choices = colourlist,
                                                               #                                                    selected = "blue")
                                                        ),
                                                        column(6,
                                                               selectInput(inputId = "id2linetype",
                                                                           label = "Select Line Type 2",
                                                                           choices = linetypelist,
                                                                           selected = "2")
                                                        )),
                                                      
                                                      # Select Staff ID 3
                                                      
                                                      selectInput(inputId="id3",
                                                                  label="Select Staff ID 3",
                                                                  choices = stafflist1,
                                                                  selected="Engineer Felix Balas"),
                                                      fluidRow(
                                                        column(6,
                                                               colourInput(
                                                                 inputId = "id3colour",
                                                                 label = "Select Line Colour 3",
                                                                 palette = "limited",
                                                                 showColour = "background",
                                                                 "#FF0000"
                                                               )
                                                               #                                          selectInput(inputId = "id3colour",
                                                               #                                                      label = "Select Line Colour 3",
                                                               #                                                      choices = colourlist,
                                                               #                                                      selected = "red")
                                                        ),
                                                        column(6,
                                                               selectInput(inputId = "id3linetype",
                                                                           label = "Select Line Type 3",
                                                                           choices = linetypelist,
                                                                           selected = "3")
                                                        )
                                                      ),
                                                      submitButton("Apply Changes", icon("refresh"))
                                         ),
                                         mainPanel(width = 9, fluid = TRUE,
                                                   tabsetPanel(
                                                     tabPanel("Guide",
                                                              br(),
                                                              fluidRow(
                                                                column(6,
                                                                       h4(HTML("<b>Understanding Map Exploration Feature</b>")),
                                                                       tags$div(
                                                                         tags$p("An interactive map visualisation is provided, in the following tab, to allow you to explore the day-by-day movements of three chosen staff, as they traverse the city."), 
                                                                         tags$p("This map visualisation comes in two parts: the map itself and the table below it. The map allows you to zoom into the map, at your discretion, and understand the movements made by the chosen staff. 
                                                                                At the same time, the table below it shows the locations and duration of visit."), 
                                                                         tags$p("Using this map visualisation, we aim to answer questions like 'On a certain day, which locations were visited by Staff 1 and for how long? What was the route taken by Staff 1?"),
                                                                         tags$p("This visualisation takes advantage of the unique properties of the tmap package, where it allows layer by layer customisation."), 
                                                                         tags$a(href="https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html", "Click here to find out more about tmap R package.")),
                                                                       br(),
                                                                       h4(HTML("<b>Steps</b>")),
                                                                       tags$ol(
                                                                         tags$li("Begin by exploring the map, along with its related paths and locations under the 'Map tab."),
                                                                         tags$li("Click on the lines and locations to reveal more information within a tooltip."),
                                                                         tags$li("Select the date of interest, followed by the three staff you would like to focus on."), 
                                                                         tags$li("Customise and differentiate each staff's path by selecting its respective line colour and line type."), 
                                                                         tags$li("Click on 'Apply Changes' button and the map will be updated with the respective paths."),
                                                                         tags$li("View the table below the map to see the locations visited by each staff, as well as its start time and duration of visit."),
                                                                       ),
                                                                       br(),
                                                                       br(),
                                                                       br()
                                                                ), 
                                                                column(6,img(src = "guide_map.png", width = "100%")
                                                                ))
                                                     ),
                                                     tabPanel("Map",
                                                              tmapOutput("map", width = "100%", height = 600),
                                                              tags$div(tags$br()),
                                                              fluidRow(width = 12,
                                                                       column(4, align = "center",
                                                                              #                                           h4(textOutput("selected_id1"), align = "center"),
                                                                              #                                           h4("STAFF ID 1's MOVEMENTS", align = "center"),
                                                                              strong(textOutput("selected_id1")),
                                                                              formattableOutput("id1table", width = "100%", height = 400),
                                                                              style='border-right: 1px solid black'
                                                                       ),
                                                                       column(4, align = "center",
                                                                              #                                          h4("STAFF ID 2's MOVEMENTS", align = "center"),
                                                                              strong(textOutput("selected_id2")),
                                                                              #                                         textOutput("selected_id2"),
                                                                              formattableOutput("id2table", width = "100%", height = 400),
                                                                              style='border-right: 1px solid black'
                                                                       ),
                                                                       column(4, align = "center",
                                                                              strong(textOutput("selected_id3")),
                                                                              #                                           h4("STAFF ID 3's MOVEMENTS", align = "center"),
                                                                              #                                          textOutput("selected_id3"),
                                                                              formattableOutput("id3table", width = "100%", height = 400)
                                                                       )
                                                              ))),
                                                   
                                                   
                                                   #                                 column(4,
                                                   #                                        box(title = "Super Duper",
                                                   #                                            solidHeader = TRUE,
                                                   #                                            collapsible = TRUE,
                                                   #                                            background = "teal",
                                                   #                                          formattableOutput("id1table", width = "100%", height = 400))
                                                   #                                        ),
                                                   #                                 column(4,
                                                   #                                        box(formattableOutput("id2table", width = "100%", height = 400))
                                                   #                                        ),
                                                   #                                 column(4,
                                                   #                                        box(formattableOutput("id3table", width = "100%", height = 400))
                                                   #                                        )
                                                   #                               ),
                                                   #                                fluidRow(
                                                   #                                  column(DT::dataTableOutput("rawtable"), width = 12)),
                                         )
                           )
                  ),                           
                  
                  
                  # Parallel Coords Tab - Exploring data using a parallel coords tab.
                  
                  tabPanel("Parallel Coords",icon = icon("wave-square")),
                  
                  # Event Timeline Tab - Exploring happenings via an event timeline.
                  
                  tabPanel("Event Timeline",icon = icon("stream")),
                  
                  # Network Graph Tab - Understanding the linkages between locations at certain junctures and people.
                  
                  tabPanel("Network Graph",icon = icon("project-diagram"))
)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  #------------------------------ANOVA tab
  
  # Ingest uploaded dataset
  datafile <- AcsvFileServer("datafile", stringsAsFactors = FALSE)
  
  # Preview imported data set
  output$table <- renderDataTable({
    datafile()
  })
  
  # Assign uploaded dataset
  #eventReactive({
  #input$confirm
  #isolate(ANOVA_userdata <- datafile())
  #})
  
  ### uploading data from external source
  #ANOVA_userdata <- reactive({
  #if(input$file == 0){return()}
  #inFile <- input$file1
  #if (is.null(inFile)){return(NULL)}
  
  #isolate({ 
  #input$Load
  #my_data <- read.csv(inFile$datapath, header = input$header, sep = input$sep, stringsAsFactors = FALSE, dec = input$dec)
  #})
  #my_data
  #})
  
  # Assign default datasets
  ANOVA_data <- cc_data
  
  # Create location and credit card list for selection
  location_list <- cc_data %>%
    distinct(location) %>%  
    as.list(location)
  
  cc_list <- cc_data %>%
    distinct(last4ccnum) %>%  
    as.list(last4ccnum)
  
  
  # Plot EDA charts
  output$EDA_1A <- renderPlotly({
    EDA_1 <- ggplot(EDA_location, 
                    aes(y=reorder(location, spend), x=spend,
                        text = paste('Spend: $', spend,
                                     '<br>Location: ', location))) +
      geom_bar(stat = "identity") +
      labs(title= "Mean spend per location",
           x = "Spend ($)", y = "Locations") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            title = element_text(size=10),
            axis.title.y = element_text(size=10),
            axis.title.x = element_text(size=10),
            axis.text = element_text(size=8))
    
    ggplotly(EDA_1, tooltip = c("text"))
  })  
  
  output$EDA_2A <- renderPlotly({
    EDA_2 <- ggplot(EDA_cc, 
                    aes(y=reorder(last4ccnum, spend), x=spend,
                        text = paste('Spend: $', spend,
                                     '<br>Credit Card Number: ', last4ccnum))) +
      geom_bar(stat = "identity") +
      labs(title= "Mean spend per credit card no.",
           x = "Spend ($)", y = "Credit Card Number") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            title = element_text(size=10),
            axis.title.y = element_text(size=10),
            axis.title.x = element_text(size=10),
            axis.text = element_text(size=8))
    
    ggplotly(EDA_2, tooltip = c("text"))
  }) 
  
  
  # Plot ANOVAs
  # Plot ANOVA - by location
  output$ANOVAL <- renderPlot({
    
    # Plot graph after users click on Plot button
    input$submit
    
    isolate(ggbetweenstats(
      data = (ANOVA_data %>% 
                filter(location %in% input$locations)),
      x = location,
      y = price,
      type = input$type,
      #conf.level = input$cf,
      title = "Difference in mean spend across locations",
      results.subtitle = TRUE,
      xlab = "Location",
      ylab = "Price ($)",
      mean.label.size = 5,
      mean.size = 7
    ) +
      theme(text = element_text(size=13), 
            axis.text.x = element_text(size = 12), 
            axis.text = element_text(size = 13), 
            axis.text.y = element_text(size = 12), 
            plot.title = element_text(size = 13),
            plot.caption = element_text(size = 13), 
            plot.subtitle = element_text(size = 13))
    )
  })
  
  # Plot ANOVA - by credit card
  output$ANOVAC <- renderPlot({
    
    # Update graph after users click on Submit button
    input$submit
    
    isolate(ggbetweenstats(
      data = (ANOVA_data %>% 
                filter(last4ccnum %in% input$ccnums)),
      x = last4ccnum,
      y = price,
      type = input$type,
      #conf.level = input$cf,
      title = "Difference in mean spend across credit card numbers",
      results.subtitle = TRUE,
      xlab = "Credit Card Numbers",
      ylab = "Price ($)"
    ) +
      theme(text = element_text(size=13), 
            axis.text.x = element_text(size = 12), 
            axis.text = element_text(size = 13), 
            axis.text.y = element_text(size = 12), 
            plot.title = element_text(size = 13),
            plot.caption = element_text(size = 13), 
            plot.subtitle = element_text(size = 13))
    )
  })
  
  
  # Plot histograms
  output$histL <- renderPlotly({
    
    # Plot graph after users click on Plot button
    input$submit
    
    isolate(hist <- ggplot(ANOVA_data %>% 
                             filter(location %in% input$locations), aes(x=price)) +
              geom_histogram(bins = input$bins) +
              facet_wrap(~location) +
              labs(title = "Distribution of spend amount by location",
                   y = "Frequency", x = "Price ($)") +
              theme_minimal() +
              theme(#panel.grid = element_blank(),
                #panel.grid.major = element_blank(),
                panel.background = element_blank(),
                title = element_text(size=10, face = "bold", margin = margin(t = 40, r = 0, b = 100, l = 0)),
                axis.title.y = element_text(size=10, margin = margin(t = 0, r = 50, b = 0, l = 0)),
                axis.title.x = element_text(size=10),
                axis.text = element_text(size=8))
    )
    
    isolate(ggplotly(hist))
  })  
  
  output$histC <- renderPlotly({
    
    # Plot graph after users click on Plot button
    input$submit
    
    isolate(hist <- ggplot(ANOVA_data %>% 
                             filter(last4ccnum %in% input$ccnums), aes(x=price)) +
              geom_histogram(bins = input$bins) +
              facet_wrap(~last4ccnum) +
              labs(title = "Distribution of spend amount by location",
                   y = "Frequency", x = "Price ($)") +
              theme_minimal() +
              theme(#panel.grid = element_blank(),
                #panel.grid.major = element_blank(),
                panel.background = element_blank(),
                title = element_text(size=10, face = "bold", margin = margin(t = 40, r = 0, b = 100, l = 0)),
                axis.title.y = element_text(size=10, margin = margin(t = 0, r = 50, b = 0, l = 0)),
                axis.title.x = element_text(size=10),
                axis.text = element_text(size=8))
    )
    
    isolate(ggplotly(hist))
  })    
  
  #------------------------------Upset tab  
  
  
  
  #------------------------------Map tab  
  
  
  #------------ Limit selectInput Staff ID options within Map tab
  
  observe({
    if(!is.null(c(input$id1, input$id2)))
      updateSelectInput(session, "id3", 
                        choices = stafflist1[!(stafflist1 %in% c(input$id1, input$id2))], 
                        selected = isolate(input$id3) )
  })
  
  observe({
    if(!is.null(c(input$id2, input$id3)))
      updateSelectInput(session, "id1", 
                        choices = stafflist1[!(stafflist1 %in% c(input$id2, input$id3))], 
                        selected = isolate(input$id1) )
  })
  
  observe({
    if(!is.null(c(input$id3, input$id1)))
      updateSelectInput(session, "id2", 
                        choices = stafflist1[!(stafflist1 %in% c(input$id3, input$id1))], 
                        selected = isolate(input$id2) )
  })
  
  #------------ Limit selectInput Line Colour options within Map tab
  
  #  observe({
  #    if(!is.null(c(input$id1colour, input$id2colour)))
  #      updateSelectInput(session, "id3colour", 
  #                        choices = colourlist[!(colourlist %in% c(input$id1colour, input$id2colour))], 
  #                        selected = isolate(input$id3colour) )
  #  })
  
  #  observe({
  #    if(!is.null(c(input$id2colour, input$id3colour)))
  #      updateSelectInput(session, "id1colour", 
  #                        choices = colourlist[!(colourlist %in% c(input$id2colour, input$id3colour))], 
  #                        selected = isolate(input$id1colour) )
  #  })
  
  #  observe({
  #    if(!is.null(c(input$id3colour, input$id1colour)))
  #      updateSelectInput(session, "id2colour", 
  #                        choices = colourlist[!(colourlist %in% c(input$id3colour, input$id1colour))], 
  #                        selected = isolate(input$id2colour) )
  #  })
  
  #------------ Limit selectInput Line Type options within Map tab
  
  observe({
    if(!is.null(c(input$id1linetype, input$id2linetype)))
      updateSelectInput(session, "id3linetype", 
                        choices = linetypelist[!(linetypelist %in% c(input$id1linetype, input$id2linetype))], 
                        selected = isolate(input$id3linetype) )
  })
  
  observe({
    if(!is.null(c(input$id2linetype, input$id3linetype)))
      updateSelectInput(session, "id1linetype", 
                        choices = linetypelist[!(linetypelist %in% c(input$id2linetype, input$id3linetype))], 
                        selected = isolate(input$id1linetype) )
  })
  
  observe({
    if(!is.null(c(input$id3linetype, input$id1linetype)))
      updateSelectInput(session, "id2linetype", 
                        choices = linetypelist[!(linetypelist %in% c(input$id3linetype, input$id1linetype))], 
                        selected = isolate(input$id2linetype) )
  })
  
  #------------ Map output  
  
  output$map <- renderTmap({
    tm_shape(sea_poly) +
      tm_polygons(col = "#d0cfd4") +
      tm_shape(Kronos_sf_small) +
      tm_polygons(col = "#e7e7e7") +
      tm_shape(Abila_st_buffer) +
      tm_polygons(col = "white") +
      tm_shape(
        
        st_sfc(st_linestring(st_coordinates(gps_sf %>% filter(date == input$MapDate & Name==input$id1))), crs = 4326)
        
        #        gps_path %>% filter(date == input$MapDate & Name==input$id1)
        
      ) +
      tm_lines(col = input$id1colour,
               lwd = 2,
               lty = input$id1linetype,
               id = "RoleNName") +
      tm_shape(
        
        st_sfc(st_linestring(st_coordinates(gps_sf %>%filter(date == input$MapDate & Name==input$id2))), crs = 4326)
        
        #        gps_path %>% filter(date == input$MapDate & Name==input$id2)
        
      ) +
      tm_lines(col = input$id2colour,
               lwd = 2,
               lty = input$id2linetype,
               id = "RoleNName") +  
      tm_shape(
        
        st_sfc(st_linestring(st_coordinates(gps_sf %>%filter(date == input$MapDate & Name==input$id3))), crs = 4326)
        
        #        gps_path %>% filter(date == input$MapDate & Name==input$id3)
        
      ) +
      tm_lines(col = input$id3colour,
               lwd = 2,
               lty = input$id3linetype,
               id = "RoleNName") +  
      tm_shape(spots_median_sf %>%
                 filter(Location.Type == "Residential")) +
      tm_symbols(shape = tmap_icons("pic/house.png"),
                 size = 0.3,
                 popup.vars = "Location.Type") +
      tm_shape(spots_median_sf %>%
                 filter(Location.Type == "Unknown")) +
      tm_symbols(shape = tmap_icons("pic/unknown.png"),
                 size = 0.3,
                 popup.vars = "Location.Type") +
      tm_shape(spots_median_sf %>%
                 filter(Location.Type == "Coffee Cafe")) +
      tm_symbols(shape = tmap_icons("pic/coffee.png"),
                 size = 0.3,
                 popup.vars = "Location.Type") +
      tm_shape(spots_median_sf %>%
                 filter(Location.Type == "Food Joints")) +
      tm_symbols(shape = tmap_icons("pic/food.png"),
                 size = 0.3,
                 popup.vars = "Location.Type") +
      tm_shape(spots_median_sf %>%
                 filter(Location.Type == "HQ")) +
      tm_symbols(shape = tmap_icons("pic/gastech.png"),
                 size = 0.3,
                 popup.vars = "Location.Type") +
      tm_shape(spots_median_sf %>%
                 filter(Location.Type == "Industrial")) +
      tm_symbols(shape = tmap_icons("pic/industrial.png"),
                 size = 0.3,
                 popup.vars = "Location.Type") +
      tm_shape(spots_median_sf %>%
                 filter(Location.Type == "Leisure & Shopping")) +
      tm_symbols(shape = tmap_icons("pic/shopping.png"),
                 size = 0.3,
                 popup.vars = "Location.Type") +
      tm_shape(spots_median_sf %>%
                 filter(Location.Type == "Complex")) +
      tm_symbols(shape = tmap_icons("pic/complex.png"),
                 size = 0.3,
                 popup.vars = "Location.Type") +
      tm_shape(spots_median_sf %>%
                 filter(Location.Type == "Transport")) +
      tm_symbols(shape = tmap_icons("pic/transport.png"),
                 size = 0.3,
                 popup.vars = "Location.Type") +
      tm_layout(title= 'Exploring GPS Movements of Three Chosen Staff On A Selected Date', 
                title.position = c('left', 'top')) +
      tm_view(set.view = c(24.866975, 36.069965, 14))
  })
  
  #------------ Format Table output  
  
  output$id1table <- renderFormattable({
    formattable(spots_table %>%
                  filter(start_date == input$MapDate & Name == input$id1) %>%
                  dplyr::select(Start_Time, Duration_Hours, Location),
                align = c("c","r","c"),
                list(Duration_Hours = color_bar("#e7e7e7")))
  })  
  
  output$id2table <- renderFormattable({
    formattable(spots_table %>%
                  filter(start_date == input$MapDate & Name == input$id2) %>%
                  dplyr::select(Start_Time, Duration_Hours, Location),
                align = c("c","r","c"),
                list(Duration_Hours = color_bar("#e7e7e7")))
  })  
  
  output$id3table <- renderFormattable({
    formattable(spots_table %>%
                  filter(start_date == input$MapDate & Name == input$id3) %>%
                  dplyr::select(Start_Time, Duration_Hours, Location),
                align = c("c","r","c"),
                list(Duration_Hours = color_bar("#e7e7e7")))
  })  
  
  #------------ Selected ID output    
  
  output$selected_id1 <- renderText({
    paste(input$id1, "'s Movements")
  })
  
  output$selected_id2 <- renderText({
    paste(input$id2, "'s Movements")
  })
  
  output$selected_id3 <- renderText({
    paste(input$id3, "'s Movements")
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)