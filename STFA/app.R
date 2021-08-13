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
library(UpSetR)
library(dendextend)
library(heatmaply)
library(RColorBrewer)
library(shinyjs)


# Define UI for application that draws a histogram
ui <-  fluidPage(
  useShinyjs(),
  navbarPage(title = tagList("VAST 2021 Mini Challenge 2",actionLink("sidebar_button","",
                                                                     icon = icon("bars"),
                                                                     tags$style(".fa-bars {color:#FFFFFF}")
  )),
  #title = "VAST Mini Challenge 2", selected = "Introduction", collapsible = TRUE, inverse = FALSE,
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
           
           sidebarLayout(fluid = TRUE,
                         
                         # Sidebar for basic and advanced settings for ANOVA Plot
                         sidebarPanel(width = 2,
                                      h4(HTML("ANOVA Plot Settings"), align = "center"),
                                      
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
                                      #                                                      submitButton("Apply Changes", icon("refresh"))                                                         
                                      actionButton("submit","Submit")
                                      
                         ),
                         
                         # Show tab panels of file import, data cleaning and plot 
                         mainPanel(width = 10, fluid = TRUE,
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
                          The visualization aims to answer questions like 'Is the spending of user A significantly different from user B?' and 'Is the mean spend at this location significantly different from other cafes?'"), 
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
                                              fluidRow(column(12,
                                                              h4(HTML("<b>Exploratory Data Analysis</b>"))                                                                              
                                              )
                                              ),
                                              fluidRow(column(6,plotlyOutput("EDA_1A", height = 700, width = "100%")),
                                                       column(6,plotlyOutput("EDA_2A", height = 700, width = "100%")))
                                     ), 
                                     
                                     # Default chart 1
                                     tabPanel("ANOVA - Between Locations",
                                              br(),
                                              
                                              # Select location 
                                              fluidRow(column(12,
                                                              selectInput(
                                                                "locations",
                                                                label = ("Select locations and click 'Submit'"),
                                                                choices = location_list,
                                                                selected = c("Kalami Kafenion", "Guy's Gyros","Brew've Been Served","Jack's Magical Beans"),
                                                                multiple=TRUE)                                                                             
                                              )
                                              ),
                                              
                                              fluidRow(column(8, h4(HTML("<b>One-Way ANOVA</b>")),
                                                              plotOutput("ANOVAL", width = "100%", height = 600)),
                                                       column(4, h4(HTML("<b>Distribution of spend amount by location</b>")),
                                                              plotlyOutput("histL", width = "100%", height = 600))),
                                              br(),
                                              br()
                                     ),
                                     
                                     
                                     # Default chart 2
                                     tabPanel("ANOVA - Between Credit Cards",
                                              br(),
                                              
                                              # Select credit card number 
                                              fluidRow(column(12,
                                                              selectInput(
                                                                "ccnums",
                                                                label = ("Select credit card numbers and click 'Submit'"),
                                                                choices = cc_list,
                                                                selected = c("4795","5368","8129","2142"),
                                                                multiple=TRUE)                                                                              
                                              )
                                              ),
                                              
                                              
                                              fluidRow(column(8, h4(HTML("<b>One-Way ANOVA</b>")),
                                                              plotOutput("ANOVAC", width = "100%", height = 600)),
                                                       column(4, h4(HTML("<b>Distribution of spend amount by credit card numbers</b>")),
                                                              plotlyOutput("histC", width = "100%", height = 600))),
                                              br(),
                                              br()
                                     )
                                   )
                         )
           )
  ),
  
  # UpSet Tab - Explains how to use our Shiny app.
  
  tabPanel("Upset Chart",icon = icon("chart-bar"),
           
           sidebarLayout(
             
             # Sidebar for basic and advanced settings for Upset Plot
             sidebarPanel(width=3, fluid = TRUE,
                          h4(HTML("UpSet Plot Settings"), align = "center"),
                          
                          tabsetPanel(
                            tabPanel(
                              'Setting',
                              br(),
                              
                              # Show the sets from the dataset for multiple selection
                              htmlOutput("sets"),
                              
                              # No. of sets
                              #sliderInput(
                              #"setsize",
                              #label = ("Select no. of sets"),
                              #value = 5,
                              #min = 2,
                              #max = 20),
                              
                              # No. of intersections
                              numericInput(
                                "nintersections",
                                label = ("Limit no. of intersections"),
                                value = 50,
                                min = 1,
                                max = 60),
                              
                              # Order by degree or frequency
                              selectInput(
                                "order",
                                label = ("Order intersections by"),
                                choices = list("Degree" = "degree",
                                               "Frequency" = "freq"),
                                selected = "freq"),
                              
                              # Order descending or ascending
                              selectInput(
                                "decreasing",
                                label = ("Increasing/Decreasing"),
                                choices = list("Increasing" = FALSE,
                                               "Decreasing" = TRUE),
                                selected = TRUE),
                              
                              # To show empty intersections
                              checkboxInput(
                                "empty", 
                                label = "Show empty intersections", 
                                value = FALSE),
                              
                              # Users to submit settings
                              #                                              submitButton("Apply Changes", icon("refresh"))                                              
                              actionButton("submitsetting","Submit")
                              ,
                            ),
                            
                            tabPanel("Format",
                                     
                                     colourpicker::colourInput(
                                       "mainbarcolor",
                                       label = h6("Select main bar colour"),
                                       "#214F8A"
                                     ),
                                     
                                     colourpicker::colourInput(
                                       "setbarcolor",
                                       label = h6("Select set bar colour"),
                                       "#EA5D4E"
                                     ),
                                     
                                     sliderInput(
                                       "mbratio",
                                       label = h6("Bar : Matrix ratio"),
                                       value = 0.30,
                                       min = 0.20,
                                       max = 0.80,
                                       ticks = FALSE,
                                       step = 0.01
                                     ),
                                     
                                     numericInput(
                                       "intersection_title_scale",
                                       label = h6("Intersection Size Label Text Scale"),
                                       value = 1.8,
                                       min = 1,
                                       max = 10,
                                       step = 0.1
                                     ),
                                     
                                     numericInput(
                                       "set_title_scale",
                                       label = h6("Set Size Label Text Scale"),
                                       value = 1.8,
                                       min = 1,
                                       max = 10,
                                       step = 0.1
                                     ),
                                     
                                     numericInput(
                                       "intersection_ticks_scale",
                                       label = h6("Intersection Size Ticks Text Scale"),
                                       value = 1.8,
                                       min = 1,
                                       max = 10,
                                       step = 0.1
                                     ),
                                     
                                     numericInput(
                                       "set_ticks_scale",
                                       label = h6("Set Size Ticks Text Scale"),
                                       value = 1.8,
                                       min = 1,
                                       max = 10
                                     ),
                                     
                                     numericInput(
                                       "intersection_size_numbers_scale",
                                       label = h6("Intersection Size Numbers Text Scale"),
                                       value = 1.8,
                                       min = 1,
                                       max = 10,
                                       step = 0.1
                                     ),
                                     
                                     numericInput(
                                       "names_scale",
                                       label = h6("Set Names Text Scale"),
                                       value = 1.8,
                                       min = 1,
                                       max = 10,
                                       step = 0.1
                                     ),
                                     
                                     # Users to submit format
                                     #                                              submitButton("Apply Changes", icon("refresh"))                                              
                                     actionButton("submitformat","Submit")
                            )
                          )
             ),
             
             # Show tab panels of file import, data cleaning and plot 
             mainPanel(width = 9, fluid = TRUE,
                       tabsetPanel(
                         
                         # Instructions
                         tabPanel("Guide",
                                  br(),
                                  fluidRow(
                                    column(6,
                                           h4(HTML("<b>Understanding UpSet Plot</b>")),
                                           tags$div(
                                             tags$p("Understanding relationships between sets is a common visual challenge that many analysts face. 
                          Venn diagrams has been the traditional choice of graphics to used to make sense of such data.
                          However, as the number of sets increase, so does the strain on readers intepreting the complex Venn diagrams (see figure)."), 
                                             tags$p("The UpSet plots offer a novel way to view set data by the size of their intersections. 
                          It is focused on communicating the size and properties of the set aggregates and interactions."), 
                                             tags$p("It is made up by 3 parts: i) The total size of each set is represented on the bottom left barplot. 
                          ii) Every possible intersection is represented by the bottom plot. 
                          iii) Frequency of each intersection is shown on the top barplot."),
                                             tags$p("In our use case, we made use of credit card transaction data to showcase 
                          how an Upset plot can help to visualize the co-location of visitors, with locations forming the sets.
                          The visualization aims to answer questions like 'How many visitors visited location A, B, and D?' and 'What is this proportion compared to total number of visitors of location A?'"), 
                                             tags$a(href="https://cran.r-project.org/web/packages/UpSetR/", "Click here to find out more about UpSet Plot R package.")),
                                           br(),
                                           h4(HTML("<b>Steps</b>")),
                                           tags$ol(
                                             tags$li("Begin by exploring the credit card transaction dataset under the 'EDA tab."),
                                             tags$li("Select the locations which you would like to explore further and specify them under the side panel 'Select specific sets'."),
                                             tags$li("Select the upper limit of the number of interactions under 'Limit no. of intersections'."), 
                                             tags$li("Select how the interactions in the matrix should be ordered by - Frequency or Degree under 'Order intersections by'."), 
                                             tags$li("Select if the order should be decreasing or increasing under 'Increasing/Decreasing'."),
                                             tags$li("Check 'Show empty intersections' to show empty intersections."),
                                             tags$li("Click on 'Apply changes' button and view 'UpSet Plot' tab."),
                                             tags$li("The UpSet plot will show the size of the overlaps between locations under 'Location Intersections' and size of each set under 'Visitors Per Location'."),
                                             tags$li("Format the chart by selecting the bar colours and text sizes of your choice under 'Format' tab.")
                                           )
                                    ), 
                                    column(6,tags$img(src="https://pbs.twimg.com/media/DDkuD6aWsAAP4N3?format=jpg&name=medium", width = "100%"),
                                           tags$h6("Source: Xu et al. (2012). A fast and accurate SNP detection algorithm for next-generation sequencing data. Nature communications.")
                                    )),
                                  br(),
                                  br(),
                                  br()
                         ), 
                         
                         # EDA
                         tabPanel("EDA",
                                  br(),
                                  fluidRow(column(12,
                                                  h4(HTML("<b>Exploratory Data Analysis</b>"))                                                           
                                  )
                                  ),
                                  fluidRow(column(6,plotlyOutput("EDA_1", height = 600)),
                                           column(6,plotlyOutput("EDA_2", height = 600)))
                         ), 
                         
                         # User to plot chart
                         tabPanel("UpSet Plot", 
                                  br(),
                                  fluidRow(column(12,
                                                  h4(HTML("<b>Co-Location of Visitors</b>"))                                                                  
                                  )
                                  ),
                                  fluidRow(plotOutput("upset", height = 600))
                         )
                       )
             )
           )
  ),
  
  # Heatmap Tab - Exploring data using a parallel coords tab.
  
  #                  tabPanel("Heatmap",icon = icon("th"),
  #                           tabsetPanel(
  #                             tabPanel("Guide"),
  #                             tabPanel("Heatmap",
  #                           fluidRow(column(12,
  #                                           h4(HTML("<b>Heatmap Exploration</b>"))                                                                  
  #                           )),
  #                           fluidRow(
  #                             column(3,
  #                                    selectInput(inputId = "heat_distance",
  #                                                label = "Select Distance Method",
  #                                                choices = c("Euclidean"="euclidean",
  #                                                            "Maximum"="maximum",
  #                                                            "Manhattan"="manhattan",
  #                                                            "Canberra"="canberra",
  #                                                            "Binary"="binary",
  #                                                            "Minkowski"="minkowski"),
  #                                                selected = "euclidean")
  #                             ),
  #                             column(3,
  #                                    selectInput(inputId = "heat_method",
  #                                                label = "Select Clustering Method",
  #                                                choices = c("Ward.D"="ward.D",
  #                                                            "Ward.D2"="ward.D2",
  #                                                            "Single"="single",
  #                                                            "Complete"="complete",
  #                                                            "Average"="average",
  #                                                            "Mcquitty"="mcquitty",
  #                                                            "Median"="median",
  #                                                            "Centroid"="centroid"),
  #                                                selected = "complete")
  #                             ),
  #                             column(3,
  #                                    selectInput(inputId = "heat_den",
  #                                                label = "Select Dendogram Type",
  #                                                choices = c("Both"="both",
  #                                                            "Row"="row",
  #                                                            "Column"="column",
  #                                                            "None"="none"),
  #                                                selected = "both")
  #                             ),
  #                             column(3,
  #                                    selectInput(inputId = "heat_color",
  #                                                label = "Select Color Brewer Scheme",
  #                                                choices = colourbrewer,
  #                                                selected = "YlOrRd")
  #                             ),
  #                           ),
  #                           fluidRow(
  #                             style = "height:1200px;",
  #                             plotlyOutput("heatmaplyden", height = "100%")
  #                           )                           
  #                                      )
  #                           )
  #                           ),
  
  
  # Heatmap Dendogram Tab - Exploring data using a parallel coords tab.
  
  tabPanel("Heat Map ",icon = icon("th"),
           sidebarLayout(
             #           position = "left", fluid = TRUE,
             #         div(id = "Sidebar", 
             sidebarPanel(width = 3, fluid = TRUE,
                          h4("Heatmap Dendogram Settings", align = "center"),
                          selectInput(inputId = "heat_den",
                                      label = "Dendogram",
                                      choices = c("Both"="both",
                                                  "Row"="row",
                                                  "Column"="column",
                                                  "None"="none"),
                                      selected = "both"),
                          selectInput(inputId = "heat_ser",
                                      label = "Seriation",
                                      choices = c("OLO"="OLO",
                                                  "Mean"="mean",
                                                  "None"="none",
                                                  "GW"="GW"),
                                      selected = "OLO"),
                          selectInput(inputId = "heat_color",
                                      label = "Colour",
                                      choices = colourbrewer,
                                      selected = "YlOrRd"),
                          selectInput(inputId = "heat_distance",
                                      label = "Select Distance Method",
                                      choices = c("Euclidean"="euclidean",
                                                  "Maximum"="maximum",
                                                  "Manhattan"="manhattan",
                                                  "Canberra"="canberra",
                                                  "Binary"="binary",
                                                  "Minkowski"="minkowski"),
                                      selected = "euclidean"),
                          selectInput(inputId = "heat_method",
                                      label = "Select Clustering Method",
                                      choices = c("Ward.D"="ward.D",
                                                  "Ward.D2"="ward.D2",
                                                  "Single"="single",
                                                  "Complete"="complete",
                                                  "Average"="average",
                                                  "Mcquitty"="mcquitty",
                                                  "Median"="median",
                                                  "Centroid"="centroid"),
                                      selected = "complete"),
                          #                                                            h4("Row Dendogram", align = "center"),
                          #                                                            fluidRow(
                          #                                                              column(6,
                          #                                                                     selectInput(inputId = "heat_distance_row",
                          #                                                                                 label = "Distance Method",
                          #                                                                                 choices = c("Euclidean"="euclidean",
                          #                                                                                             "Maximum"="maximum",
                          #                                                                                             "Manhattan"="manhattan",
                          #                                                                                             "Canberra"="canberra",
                          #                                                                                             "Binary"="binary",
                          #                                                                                             "Minkowski"="minkowski"),
                          #                                                                                 selected = "euclidean")
                          #                                                                     ),
                          #                                                              column(6,
                          #                                                                     selectInput(inputId = "heat_method_row",
                          #                                                                                 label = "Clustering Method",
                          #                                                                                 choices = c("Ward.D"="ward.D",
                          #                                                                                             "Ward.D2"="ward.D2",
                          #                                                                                             "Single"="single",
                          #                                                                                             "Complete"="complete",
                          #                                                                                            "Average"="average",
                          #                                                                                             "Mcquitty"="mcquitty",
                          #                                                                                             "Median"="median",
                          #                                                                                            "Centroid"="centroid"),
                          #                                                                                 selected = "complete")
                          #                                                                    )),
                          sliderInput(inputId = "heat_cluster_row",
                                      label = ("Number of Row Clusters"),
                                      value = 3,
                                      min = 1,
                                      max = 10),
                          #                                                            h4("Column Dendogram", align = "center"),
                          #                                                            fluidRow(
                          #                                                              column(6,
                          #                                                                     selectInput(inputId = "heat_distance_column",
                          #                                                                                 label = "Distance Method",
                          #                                                                                 choices = c("Euclidean"="euclidean",
                          #                                                                                             "Maximum"="maximum",
                          #                                                                                             "Manhattan"="manhattan",
                          #                                                                                             "Canberra"="canberra",
                          #                                                                                             "Binary"="binary",
                          #                                                                                             "Minkowski"="minkowski"),
                          #                                                                                 selected = "euclidean")
                          #                                                              ),
                          #                                                              column(6,
                          #                                                                     selectInput(inputId = "heat_method_column",
                          #                                                                                 label = "Clustering Method",
                          #                                                                                 choices = c("Ward.D"="ward.D",
                          #                                                                                             "Ward.D2"="ward.D2",
                          #                                                                                             "Single"="single",
                          #                                                                                             "Complete"="complete",
                          #                                                                                             "Average"="average",
                          #                                                                                             "Mcquitty"="mcquitty",
                          #                                                                                             "Median"="median",
                          #                                                                                             "Centroid"="centroid"),
                          #                                                                                 selected = "complete")
                          #                                                              )),
                          sliderInput(inputId = "heat_cluster_column",
                                      label = ("Number of Column Clusters"),
                                      value = 3,
                                      min = 1,
                                      max = 10),
                          actionButton("submit_heatmap","Submit")
                          
                          #         )
             ),
             
             mainPanel(width = 9, fluid = TRUE,
                       #                   fluidRow(style = "height:10px;"),
                       tabsetPanel(
                         tabPanel("Guide",
                                  br(),
                                  fluidRow(
                                    column(6,
                                           h4(HTML("<b>Understanding Heat Map Feature</b>")),
                                           tags$div(
                                             tags$p("An interactive heat map dendogram visualisation is provided, in the following tab, to allow you to understand the location visit frequency of each staff."), 
                                             tags$p("This map visualisation comes in two parts: the heat map itself and the dendograms for both its row and column variables. The heat map allows you to zoom in, at your discretion, and see in detail their location preferences. 
                                                                                At the same time, the table below it shows the locations and duration of visit."), 
                                             tags$p("Using this map visualisation, we aim to answer questions like 'On an overall basis, which locations are most frequented by Staff 1 and for each location, who frequents them the most?"),
                                             tags$p("This visualisation takes advantage of the unique properties of the heatmaply package, which is built off plotly."), 
                                             tags$a(href="https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html", "Click here to find out more about heatmaply R package.")),
                                           br(),
                                           h4(HTML("<b>Steps</b>")),
                                           tags$ol(
                                             tags$li("Begin by exploring the heat map, along with its related staff and locations."),
                                             tags$li("Hover on any box to reveal staff name, location and visit frequency within a tooltip."),
                                             tags$li("Select the various heatmap dendogram settings along with the cluster count that you would like to focus on."), 
                                             tags$li("Click on 'Submit' button and the heat map will be updated."),
                                           ),
                                           br(),
                                           br(),
                                           br()
                                    ), 
                                    column(6,img(src = "guide_heatmap.png", width = "100%")
                                    ))),
                         tabPanel("Heatmap",
                                  br(),
                                  fluidRow(
                                    column(12,
                                           h4(HTML("<b>Heat Map Exploration</b>"))              
                                    )
                                  ),
                                  fluidRow(
                                    style = "height:1200px;",
                                    plotlyOutput("heatmaplyden", height = "100%")
                                  )
                         ))
                       
             ) #MainPanel closure
           )),
  
  # Exploring GPS Tab - Explores the GPS data and locations on a map.
  
  tabPanel("Spatial Map",icon = icon("map-marked-alt"),
           sidebarLayout(position = "left", fluid = TRUE,
                         sidebarPanel(tags$style(".well {background-color:#e7e7e7;}"),
                                      width = 3, fluid = TRUE,
                                      h4("Select Date", align = "center"),
                                      dateInput(inputId = "MapDate",
                                                min = ("2014-01-06"),
                                                max = ("2014-01-19"),
                                                value=("2014-01-06"),
                                                format = "d M",
                                                label = NULL),
                                      hr(style = "border-top: 3px solid #FFFFFF;"),
                                      h4("Compare Max Three Staff", align = "center"),
                                      
                                      # Select Staff ID 1
                                      
                                      selectInput(inputId="id1",
                                                  label="Select Staff ID 1",
                                                  choices = stafflist1,
                                                  selected="IT Helpdesk Nils Calixto"),
                                      fluidRow(
                                        column(6,
                                               colourpicker::colourInput(
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
                                               colourpicker::colourInput(
                                                 inputId = "id2colour",
                                                 label = "Select Line Colour 2",
                                                 palette = "limited",
                                                 showColour = "background",
                                                 "#0000FF"
                                               )
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
                                               colourpicker::colourInput(
                                                 inputId = "id3colour",
                                                 label = "Select Line Colour 3",
                                                 palette = "limited",
                                                 showColour = "background",
                                                 "#FF0000"
                                               )
                                        ),
                                        column(6,
                                               selectInput(inputId = "id3linetype",
                                                           label = "Select Line Type 3",
                                                           choices = linetypelist,
                                                           selected = "3")
                                        )
                                      ),
                                      actionButton("submit_map","Submit")
                                      #                                                      submitButton("Apply Changes", icon("refresh"))
                         ),
                         mainPanel(width = 9, fluid = TRUE,
                                   tabsetPanel(
                                     tabPanel("Guide",
                                              br(),
                                              fluidRow(
                                                column(6,
                                                       h4(HTML("<b>Understanding Spatial Map Feature</b>")),
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
                                              br(),
                                              fluidRow(column(12,
                                                              h4(HTML("<b>Map Exploration</b>"))              
                                              )
                                              ),
                                              tmapOutput("map", width = "100%", height = 600),
                                              tags$div(tags$br()),
                                              fluidRow(width = 12,
                                                       column(4, align = "center",
                                                              strong(textOutput("selected_id1")),
                                                              formattableOutput("id1table", width = "100%", height = 400),
                                                              style='border-right: 1px solid black'
                                                       ),
                                                       column(4, align = "center",
                                                              strong(textOutput("selected_id2")),
                                                              formattableOutput("id2table", width = "100%", height = 400),
                                                              style='border-right: 1px solid black'
                                                       ),
                                                       column(4, align = "center",
                                                              strong(textOutput("selected_id3")),
                                                              formattableOutput("id3table", width = "100%", height = 400)
                                                       )
                                              ))),
                         )
           )
  ),                           
  
  # Network Graph Tab - Understanding the linkages between locations at certain junctures and people.
  
  tabPanel("Network Graph",icon = icon("project-diagram"))
  )
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
              facet_wrap(~location, shrink = TRUE) +
              labs(y = "Frequency", x = "Price ($)") +
              theme_minimal() +
              theme(#panel.grid = element_blank(),
                #panel.grid.major = element_blank(),
                panel.background = element_blank(),
                strip.text.x = element_text(size=8),
                title = element_text(size=9, face = "bold", margin = margin(t = 40, r = 0, b = 100, l = 0)),
                axis.title.y = element_text(size=8, margin = margin(t = 0, r = 50, b = 0, l = 0)),
                axis.title.x = element_text(size=8),
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
              labs(y = "Frequency", x = "Price ($)") +
              scale_x_discrete(guide = guide_axis(n.dodge=3))+
              theme_minimal() +
              theme(#panel.grid = element_blank(),
                #panel.grid.major = element_blank(),
                panel.background = element_blank(),
                strip.text.x = element_text(size=8),
                title = element_text(size=9, face = "bold", margin = margin(t = 40, r = 0, b = 100, l = 0)),
                axis.title.y = element_text(size=8, margin = margin(t = 0, r = 50, b = 0, l = 0)),
                axis.title.x = element_text(size=8),
                axis.text = element_text(size=8))
    )
    
    isolate(ggplotly(hist))
  })
  
  #------------------------------Upset tab  
  
  # Ingest uploaded dataset
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)
  
  # Preview imported data set
  output$table <- renderDataTable(datafile(), options = list(scrollX = TRUE))
  
  # Formatting - to increase spacing between sets
  mat_prop <- reactive({
    mat_prop <- input$mbratio
  })
  
  bar_prop <- reactive({
    bar_prop <- (1 - input$mbratio)
  })
  
  # Setting - to show empty intersects
  emptyIntersects <- reactive({
    if(isTRUE(input$empty)){choice <- "on"
    return(choice)
    }
    else{
      return(NULL)
    }
  })
  
  # Setting - to allow users to select intended sets
  FindStartEnd <- function(data){
    startend <- c()
    for(i in 1:ncol(data)){
      column <- data[, i]
      column <- (levels(factor(column)))
      if((column[1] == "0") && (column[2] == "1" && (length(column) == 2))){
        startend[1] <- i
        break
      }
      else{
        next
      }
    }
    for(i in ncol(data):1){
      column <- data[ ,i]
      column <- (levels(factor(column)))
      if((column[1] == "0") && (column[2] == "1") && (length(column) == 2)){
        startend[2] <- i
        break
      }
      else{
        next
      }
    }
    return(startend)
  }
  
  startEnd <- reactive({
    startEnd <- FindStartEnd(My_data)
  })
  
  Specific_sets <- reactive({
    Specific_sets <- as.character(c(input$Select))
  })
  
  output$sets <- renderUI({
    if(is.null(My_data == T)){
      sets <-  selectInput("Select", 
                           ("Select at least two sets"),
                           choices = NULL,
                           multiple=TRUE, selectize=TRUE, selected = Specific_sets())
    }
    else{
      data <- My_data[startEnd()[1]:startEnd()[2]]
      topfive <- colSums(data)
      topfive <- as.character(head(names(topfive[order(topfive, decreasing = T)]), 5))
      sets <- selectInput('Select', 
                          ("Select specific sets"),
                          choices = as.character(colnames(My_data[ , startEnd()[1]:startEnd()[2]])),
                          multiple=TRUE, selectize=TRUE, selected = topfive)
    }
    return(sets)
    
  })
  
  
  # Default dataset
  My_data <- cc_csv
  
  # EDA plots
  output$EDA_1 <- renderPlotly({
    EDA_1 <- ggplot(EDA_visitors, 
                    aes(y=reorder(location, visitors), x=visitors,
                        text = paste('Visitors:', visitors,
                                     '<br>Location: ', location))) +
      geom_bar(stat = "identity") +
      labs(title = "Distinct visitors per location",
           x = "Visitors", y = "Locations") +
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
  
  output$EDA_2 <- renderPlotly({
    EDA_2 <- ggplot(EDA_visits, 
                    aes(y=reorder(location,count_name_occurr),
                        text = paste('Transactions:', count_name_occurr,
                                     '<br>Location: ', location))) +
      geom_bar(stat = "count") +
      labs(title = "No. of transactions per location",
           x = "Credit Card Transactions", y = "Locations") +
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
  
  # Upset plot
  # Show graph using renderplot
  output$upset <- renderPlot({
    
    # Plot graph after users click on Plot button
    input$submitsetting
    input$submitformat
    
    isolate(UpSetR::upset(data = My_data, 
                          sets = Specific_sets(),
                          nsets = 5, 
                          #nsets = input$setsize, 
                          nintersects = input$nintersections, 
                          order.by = input$order,
                          decreasing = input$decreasing,
                          empty.intersections = emptyIntersects(),
                          mb.ratio = c(as.double(bar_prop()), as.double(mat_prop())),
                          main.bar.color = input$mainbarcolor,
                          sets.bar.color = input$setbarcolor,
                          text.scale = c(input$intersection_title_scale, input$intersection_ticks_scale,
                                         input$set_title_scale, input$set_ticks_scale, input$names_scale,
                                         input$intersection_size_numbers_scale),
                          mainbar.y.label = "Location Intersections",
                          sets.x.label = "Visitors Per Location") 
    )
    
  })    
  
  #------------------------------Heatmap tab  
  
  # Toggle Sidebar
  
  observeEvent(input$sidebar_button,{
    shinyjs::toggle(selector = ".tab-pane.active div:has(> [role='complementary'])")
    
    js_maintab <- paste0('$(".tab-pane.active div[role=',"'main'",']")')
    
    runjs(paste0('
          width_percent = parseFloat(',js_maintab,'.css("width")) / parseFloat(',js_maintab,'.parent().css("width"));
          if (width_percent == 1){
            ',js_maintab,'.css("width","");
          } else {
            ',js_maintab,'.css("width","100%");
          }
          '))
  })
  
  
  output$heatmaplyden <- renderPlotly({
    
    # Plot graph after users click on Plot button
    input$submit_heatmap
    
    isolate(
      #        d <- dist(spots_heatmap3, method = "euclidean")
      d <- dist(spots_heatmap3, method = input$heat_distance)     
    )
    
    isolate(
      #    dend <- as.dendrogram(hclust(d, method = "complete"))
      dend <- as.dendrogram(hclust(d, method = input$heat_method))      
    )
    
    isolate(
      dend <- dendextend::seriate_dendrogram(dend, d)      
    )
    
    isolate(
      spots_heatmap3 <- spots_heatmap3[order.dendrogram(dend),]      
    )
    
    isolate(
      heatmaply(spots_heatmap3,
                #              dendrogram = "both",              
                dendrogram = input$heat_den,
                #              xlab = "Locations", ylab = "Staff Names", 
                main = "",
                seriate = input$heat_ser,
                #             scale = "column",
                margins = c(60,100,40,20),
                grid_color = "grey",
                grid_width = 0.00001,
                k_row = input$heat_cluster_row,
                k_col = input$heat_cluster_column,
                #              hclustfun_row = input$heat_method_row,
                #              hclustfun_col = input$heat_method_column,
                #              distfun_row = input$heat_distance_row,
                #              distfun_col = input$heat_distance_column,
                #        col = colorRamp(c("black", "blue"))(9),
                #              col = colorRampPalette(brewer.pal(9, "Blues")),
                col = colorRampPalette(brewer.pal(9, input$heat_color)),
                #        scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                #            low = "black", 
                #            high = "white", 
                #            midpoint = 15, 
                #            limits = c(0, 30)),
                #        titleX = FALSE,
                hide_colorbar = TRUE,
                branches_lwd = 0.1,
                label_names = c("Staff", "Location", "No. of Visits"),
                #        fontsize_row = 5, fontsize_col = 5,
                #        labCol = colnames(points_lasttry2),
                #        labRow = rownames(points_lasttry2),
                heatmap_layers = theme(axis.line=element_blank())
      )      
    )
    
  })
  
  
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
    
    # Plot map after users click on Plot button
    input$submit_map
    
    isolate(    tm_shape(sea_poly) +
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
                  tm_view(set.view = c(24.866975, 36.069965, 14)))
    
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