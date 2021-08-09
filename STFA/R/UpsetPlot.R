library(shiny)
library(tidyverse)
library(plyr)
library(ggplot2)
library(UpSetR)
library(colourpicker)

# ----------------------------------------------------------------------------------------------------------------------
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
    radioButtons(
      NS(id,'choose_data'),
      label = ('Select dataset'),
      choices = c(
        'Credit Card dataset' = "cc_data",
        'Upload own dataset' = "user_data")
    ),
    
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

# ----------------------------------------------------------------------------------------------------------------------
# To port over to main app later

ui <- fluidPage(
  
  # Application title
  titlePanel("UpSet Plot - Who Visits the Same Locations"),
  
  sidebarLayout(
    
    # Sidebar for basic and advanced settings for Upset Plot
    sidebarPanel(width=3,
      h4(HTML("UpSet Plot Settings")),
      
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
          actionButton("submitsetting","Submit"),
          
        ),
        tabPanel(
          "Format",
          
          colourInput(
            "mainbarcolor",
            label = h6("Select main bar colour"),
            "#214F8A"
          ),
          
          colourInput(
            "setbarcolor",
            label = h6("Select set bar colour"),
            "#EA5D4E"
          ),
          
          sliderInput(
            "plotheight",
            label = h6("Plot height"),
            value = 500,
            min = 300,
            max = 1000,
            ticks = FALSE,
            step = 10
          ),
          
          sliderInput(
            "plotwidth",
            label = h6("Plot width"),
            value = 700,
            min = 400,
            max = 1200,
            ticks = FALSE,
            step = 10
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
            max = 1000
          ),
          
          numericInput(
            "set_title_scale",
            label = h6("Set Size Label Text Scale"),
            value = 1.8,
            min = 1,
            max = 1000
          ),
          
          numericInput(
            "intersection_ticks_scale",
            label = h6("Intersection Size Ticks Text Scale"),
            value = 1.8,
            min = 1,
            max = 1000
          ),
          
          numericInput(
            "set_ticks_scale",
            label = h6("Set Size Ticks Text Scale"),
            value = 1.8,
            min = 1,
            max = 1000
          ),
          
          numericInput(
            "intersection_size_numbers_scale",
            label = h6("Intersection Size Numbers Text Scale"),
            value = 1.8,
            min = 1,
            max = 1000
          ),
          
          numericInput(
            "names_scale",
            label = h6("Set Names Text Scale"),
            value = 1.8,
            min = 1,
            max = 1000
          ),
          
          # Users to submit format
          actionButton("submitformat","Submit"),
          
        )
      )
      
    ),
    
    # Show tab panels of file import, data cleaning and plot 
    mainPanel(
      tabsetPanel(
        
        # User to plot chart
        tabPanel("UpSet Plot", 
                 fluidRow(plotOutput("upset", height = 700)),
                 fluidRow(plotOutput("upsetUser", height = 700)),
                 ),
        
        # User to upload data
        tabPanel("Import Own Data",
                 br(),
                 fluidRow(csvFileUI("datafile", "User data (.csv format)")),
                 fluidRow(dataTableOutput("table"))
                 )
        

      )
    )
  )
)

server <- function(input, output, session) {
  
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
  #cc_csv <- read.csv("cc_pivot.csv", header=T, sep="," ) %>%
    #dplyr::select(-X)
  My_data <- cc_csv
  
  #My_data <- reactiveVal()
  #observeEvent(
    #if(input$choose_data == "user_data"){ 
    #My_data <- datafile()
  #})
  #observeEvent(
    #if(input$choose_data == "cc_data"){ 
   # My_data <- cc_csv
    #})
  
  # Assign dataset 
  #eventReactive(
    #input$confirm, 
    #isolate(My_data <- datafile())
    #)
  
  
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
                  sets.x.label = "Visitors Per Location"
    ))

  })
  
}

shinyApp(ui, server)



