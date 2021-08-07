# Install and load key R libraries

#ipak <- function(pkg){
#    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#    if (length(new.pkg)) 
#        install.packages(new.pkg, dependencies = TRUE)
#    sapply(pkg, require, character.only = TRUE)
#}

#packages <- c("shiny","shinydashboard","readr","tidyverse","shinythemes",
#              "readxl","rsconnect")
#ipak(packages)

library(shiny)
library(shinydashboard)
library(readr)
library(tidyverse)
library(shinythemes)
library(readxl)
library(rsconnect)


# Define UI for application that draws a histogram
ui <-  navbarPage(title = "VAST Mini Challenge 2", selected = " Intro", collapsible = TRUE, inverse = FALSE, theme = shinytheme("flatly"),
                  
                    # Intro Tab - Mainly a write-up of our project.
                  
                    tabPanel(" Intro",icon = icon("chalkboard-teacher"),
                             tags$div(
                                 tags$h4("ABOUT"),
                                 "Write an intro part one here.",
                                 tags$br(), tags$br(),
                                 "Write intro part two here."
                             )),
                         
                    # User Guide Tab - Explains how to use our Shiny app.
                           
                    tabPanel(" User Guide",icon = icon("question-circle")),
                                    
                    # Exploring GPS Tab - Explores the GPS data and locations on a map.
                             
                    tabPanel(" Exploring GPS",icon = icon("map-marked-alt")),
                                      
                    # Parallel Coords Tab - Exploring data using a parallel coords tab.
                                      
                    tabPanel(" Parallel Coords",icon = icon("wave-square")),
                                               
                    # Event Timeline Tab - Exploring happenings via an event timeline.
                                               
                    tabPanel(" Event Timeline",icon = icon("stream")),
                                                        
                    # Network Graph Tab - Understanding the linkages between locations at certain junctures and people.
                                                        
                    tabPanel(" Network Graph",icon = icon("project-diagram")),

                    # Stats Graph Tab - Explores statistical analysis on our data.
                             
                    tabPanel(" Stats Graph",icon = icon("signal"))
                                                        
                           )


# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application 
shinyApp(ui = ui, server = server)
