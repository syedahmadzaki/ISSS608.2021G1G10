library(shiny)
library(tidyverse)
library(plyr)
library(ggplot2)
library(UpSetR)

# ----------------------------------------------------------------------------------------------------------------------
# Import csv data function 

csvFileUI <- function(id, label = "CSV file") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  tagList(
    
    br(),
    h4(HTML("<b>Instructions</b>")),
    p(
      "Begin by uploading a correctly formatted .csv file. Or you can choose to select the credit card transaction dataset from VAST Challenge Mini Case 2."
    ),
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
      
      # Change dataframe into a reactive input for other modules
      userFile_csv = reactive(input$userFile_csv)
    
    }
  )    
}

# ----------------------------------------------------------------------------------------------------------------------
# Plot chart function

UpSetPlotUI <- function(id, label = "UpSet Plot") {
  
  ns <- NS(id)
  tagList(
    br(),
    
    sliderInput(
      NS(id,"setsize"),
      label = ("Select no. of sets"),
      value = 5,
      min = 2,
      max = 50),
    
    fluidRow(
      column(10,plotOutput(NS(id,"upset"))),
    
      column(10,dataTableOutput(NS(id,"upset_table"))), 
    )
  )
}

UpSetPlotServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session, ImProxy) {
      
      # Allow users to view all the sets available for selection
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
        startEnd <- FindStartEnd(My_data())
      })
      
      Specific_sets <- reactive({
        Specific_sets <- as.character(c(input$Select))
      })
      
      output$sets <- renderUI({
        if(is.null(My_data()) == T){
          sets <-  selectInput(NS(id,"Select"), h6("Select at least two sets : "),
                               choices = NULL,
                               multiple=TRUE, selectize=TRUE, selected = Specific_sets())
        }
        else{
          data <- My_data()[startEnd()[1]:startEnd()[2]]
          topfive <- colSums(data)
          topfive <- as.character(head(names(topfive[order(topfive, decreasing = T)]), 5))
          sets <- selectInput(NS(id,'Select'), h6("Select specific sets : "),
                              choices = as.character(colnames(My_data()[ , startEnd()[1]:startEnd()[2]])),
                              multiple=TRUE, selectize=TRUE, selected = topfive)
        }
        return(sets)
      })
      
      # temp >> attempt to try plotting using internal dataset
      cc_csv <- read.csv("cc_pivot.csv", header=T, sep="," ) %>%
        dplyr::select(-X)
      
      output$upset_table <- renderDataTable(head(cc_csv,10),
                                            options = list(scrollX = TRUE))
      
      # Show graph using render Image
      output$upset <- renderPlot({
        
        # Plot graph after users click on Plot button
        input$confirm
        
        # Unable to read from other modules
        #UpSetR::upset(data = ImProxy$userFile_csv, nsets = 6, order.by = "freq")

        # Tried reading from own folder but cannot recognize filename
        #UpSetR::upset(data = cc_csv, 
                      #nsets = input$setsize, 
                      #nintersects = input$nintersections, 
                      #order.by = input$order,
                      #decreasing = input$decreasing,
                      #empty.intersections = input$empty
                      #)
        
        UpSetR::upset(data = cc_csv, 
                      nsets = input$setsize, 
                      nintersects = 50, 
                      order.by = "freq",
                      decreasing = TRUE,
                      empty.intersections = "off"
        )
      })
      
    }
)}

# ----------------------------------------------------------------------------------------------------------------------
# Plot chart settings
# Can have 2 UI module syncing with 1 Server Module?

SettingsUI <- function(id, label = "Settings") {
  
  ns <- NS(id)
  tagList(
    
    tabsetPanel(
      tabPanel(
        'Setting',
        br(),
        
        # Show the sets from the dataset for multiple selection
        htmlOutput("sets"),
        
        # No. of intersections
        sliderInput(
          NS(id,"setsize"),
          label = ("Select no. of sets"),
          value = 5,
          min = 2,
          max = 50),
        
        # No. of intersections
        numericInput(
          NS(id,"nintersections"),
          label = ("Limit no. of intersections"),
          value = 50,
          min = 1,
          max = 60),
        
        # Order by degree or frequency
        selectInput(
          NS(id,"order"),
          label = ("Order intersections by"),
          choices = list("Degree" = "degree",
                         "Frequency" = "freq"),
          selected = "freq"),
        
        # Order descending or ascending
        selectInput(
          NS(id,"decreasing"),
          label = ("Increasing/Decreasing"),
          choices = list("Increasing" = FALSE,
                         "Decreasing" = TRUE),
          selected = TRUE),
        
        # To show empty intersections
        checkboxInput(NS(id,"empty"), 
                      label = "Show empty intersections", 
                      value = FALSE),
        
      ),
      tabPanel("Format"
      )
    )
    
  )
}

# ----------------------------------------------------------------------------------------------------------------------
# To port over to main app later

ui <- fluidPage(
  
  # Application title
  titlePanel("UpSet Plot - View Locations with Same Visitors"),
  
  sidebarLayout(
    
    # Sidebar for basic and advanced settings for Upset Plot
    sidebarPanel(width = 2,
      h4(HTML("UpSet Plot Settings")),
      SettingsUI("settings")
    ),
    
    # Show tab panels of file import, data cleaning and plot 
    mainPanel(
      tabsetPanel(
        
        # Step 1 - User to upload data
        tabPanel("Import Data",
                 br(),
                 csvFileUI("datafile", "User data (.csv format)"),
                 dataTableOutput("table")),
        
        # Step 2 - User to plot chart
        tabPanel("UpSet Plot", 
                 UpSetPlotUI("upset"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Ingest uploaded dataset
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)
  
  # Preview imported data set
  output$table <- renderDataTable({
    datafile()
  })
  
  # Upset plot
  UpSetPlotServer("upset")
  
}

shinyApp(ui, server)



