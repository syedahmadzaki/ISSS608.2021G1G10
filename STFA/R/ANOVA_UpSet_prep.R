library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggstatsplot)
library(UpSetR)
#library(plyr)
library(dplyr)

# ANOVA ----------------------------------------------------------------------------------------------------------------------
# Data Prep for Default Datasets

cc_data <- read.csv("data/cc_data.csv", encoding = "UTF-8")
cc_data[grep("Katerina", cc_data$location),2] <- "Katerina's Cafe"

location_list <- cc_data %>%
  distinct(location) %>%  
  as.list(location)

cc_list <- cc_data %>%
  distinct(last4ccnum) %>%  
  as.list(last4ccnum)

# Data prep for EDA
EDA_location <- cc_data %>% 
  group_by(location) %>%
  summarise(spend = mean(price))
EDA_cc <- cc_data %>% 
  group_by(last4ccnum) %>%
  summarise(spend = mean(price))

# ANOVA ----------------------------------------------------------------------------------------------------------------------
# Import csv data function 

AcsvFileUI <- function(id, label = "CSV file") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  tagList(
    
    br(),
    fluidRow(column(10,h4(HTML("<b>Instructions</b>")),
                    p(
                      "Begin by uploading a correctly formatted .csv file."
                    ))),
    br(),
    
    # Prompt user to upload csv file
    fileInput(NS(id,"file"),
              label = "Upload own dataset (csv format)",
              buttonLabel = "Upload file"),
    
    # Users to indicate if file has header
    checkboxInput(NS(id,'heading'), label = 'Has Header', TRUE),
    
    # Users to indicate type of quote
    selectInput(NS(id,"quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    )),
    
    # Users to indicate no. of rows to preview
    numericInput(ns("rows"), "Rows to preview", 10, min = 1),
    
    br(),
    
    # Users to confirm using dataset
    actionButton("confirm","Plot"),
    
    br(),
    br()
  )
}

AcsvFileServer <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      userFile <- reactive({
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # Change input file into dataframe
      dataframe <- reactive({
        userFile_csv <- read.csv(userFile()$datapath,
                                 header = input$heading,
                                 quote = input$quote,
                                 stringsAsFactors = stringsAsFactors)
        head(userFile_csv, input$rows)
      })
      
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })
      
      
      
      return(dataframe)
      
    }
  )    
}

# UpSet ----------------------------------------------------------------------------------------------------------------------

# Default UpSet dataset
cc_data_2 <- cc_data %>% 
  distinct(location, last4ccnum) %>%
  mutate(present = 1)
cc_pivot <- cc_data_2 %>% 
  pivot_wider(last4ccnum, names_from = location, values_from = present, values_fill = 0) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate_all(as.integer) %>%
  as.data.frame()
write.csv(cc_pivot,"data/cc_pivot.csv")
cc_csv <- read.csv("data/cc_pivot.csv", header=T, sep="," ) %>%
  dplyr::select(-X)

# EDA datasets
EDA_visitors <- cc_data_2 %>% 
  group_by(location) %>%
  summarise(visitors = n())
EDA_visits <- cc_data %>% group_by(location) %>%
  mutate(count_name_occurr = n()) %>%
  ungroup()

# UpSet ----------------------------------------------------------------------------------------------------------------------
# Import csv data function 

csvFileUI <- function(id, label = "CSV file") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  tagList(
    
    br(),
    fluidRow(column(10,h4(HTML("<b>Instructions</b>")),
                    p(
                      "Begin by uploading a correctly formatted .csv file. A correctly formatted .csv file is encoded in binary (0 and 1) and set up so that each column represents a set. If an element is in the set it is represented as a 1 in that position. If an element is not in the set it is represented as a 0.
      
      Or you can choose to view the ready UpSet Plot based on the credit card transaction dataset from VAST Challenge Mini Case 2 in the Upset Plot tab."
                    ))),
    br(),
    
    # Select dataset from VAST challenge
    #radioButtons(
    #NS(id,'choose_data'),
    #label = ('Select dataset'),
    #choices = c(
    #'Credit Card dataset' = "cc_data",
    #'Upload own dataset' = "user_data")
    #),
    
    # Prompt user to upload csv file
    fileInput(NS(id,"file"),
              label = "Upload own dataset (csv format)",
              buttonLabel = "Upload file"),
    
    # Users to indicate if file has header
    checkboxInput(NS(id,'heading'), label = 'Has Header', TRUE),
    
    # Users to indicate type of quote
    selectInput(NS(id,"quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    )),
    
    # Users to indicate no. of rows to preview
    numericInput(ns("rows"), "Rows to preview", 10, min = 1),
    
    br(),
    
    # Users to confirm using dataset
    actionButton("confirm","Plot"),
    
    br(),
    br()
  )
}

csvFileServer <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      userFile <- reactive({
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # Change input file into dataframe
      dataframe <- reactive({
        userFile_csv <- read.csv(userFile()$datapath,
                                 header = input$heading,
                                 quote = input$quote,
                                 stringsAsFactors = stringsAsFactors)
        head(userFile_csv, input$rows)
      })
      
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })
      
      
      
      return(dataframe)
      
    }
  )    
}

