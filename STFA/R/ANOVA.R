library(shiny)
library(tidyverse)
library(plyr)
library(ggplot2)
library(plotly)
library(ggstatsplot)

# ----------------------------------------------------------------------------------------------------------------------
# Import csv data function 

AcsvFileUI <- function(id, label = "CSV file") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  tagList(
    
    br(),
    fluidRow(column(10,h4(HTML("<b>Instructions</b>")),
                    p(
                      "Begin by uploading a correctly formatted .csv file. 
                      Or you can choose to view the ready ANOVA Plot based on the credit card transaction dataset from VAST Challenge Mini Case 2 in the Upset Plot tab."
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


# ----------------------------------------------------------------------------------------------------------------------
# Plot chart function




# ----------------------------------------------------------------------------------------------------------------------
# To port over to main app later

ui <- fluidPage(
  
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
    selectInput(
      "cf",
      label = ("Select confidence level"),
      choices = list("0.9" = 0.9,
                     "0.95" = 0.95,
                     "0.99" = 0.99),
      selected = 0.95),
  
    # Users no. of bins for histogram
    sliderInput(
      "bins",
      label = ("No. of bins"),
      value = 5,
      min = 3,
      max = 30)

    ),
    
    # Show tab panels of file import, data cleaning and plot 
    mainPanel(
      tabsetPanel(
        
        # Default chart 2
        tabPanel("ANOVA - Between Locations",
                 br(),
                 
                 # Select location 
                 fluidRow(selectInput(
                   "locations",
                   label = ("Select locations"),
                   choices = location_list,
                   selected = c("Kalami Kafenion", "Guy's Gyros","Brew've Been Served","Albert's Fine Clothing"),
                   multiple=TRUE)),
                 
                 
                 fluidRow(column(7,plotOutput("ANOVAL", height = 900)),
                          column(5,plotlyOutput("histL", height = 900)))
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
                 
                 
                 fluidRow(column(7,plotOutput("ANOVAC", height = 900)),
                          column(5,plotlyOutput("histC", height = 900)))
        ),
        
        # User to upload data
        tabPanel("Import Own Data",
                 br(),
                 AcsvFileUI("datafile", "User data (.csv format)"),
                 dataTableOutput("table"))
        
        
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Ingest uploaded dataset
  datafile <- AcsvFileServer("datafile", stringsAsFactors = FALSE)
  
  # Preview imported data set
  output$table <- renderDataTable({
    datafile()
  })
  
  # Assign dataset
  cc_data <- read.csv("cc_data.csv")
  cc_data[grep("Katerina", cc_data$location),2] <- "Katerina's Cafe"
  ANOVA_data <- cc_anova
  
  # Create location and credit card list for selection
  location_list <- cc_data %>%
    distinct(location) %>%  
    as.list(location)
  
  cc_list <- cc_data %>%
    distinct(last4ccnum) %>%  
    as.list(last4ccnum)
    
  # Plot ANOVA
  output$ANOVAL <- renderPlot({
    ggbetweenstats(
      data = (ANOVA_data %>% 
        filter(location %in% input$locations)),
      x = location,
      y = price,
      type = input$type,
      #conf.level = input$cf,
      #outlier.tagging = TRUE,
      #outlier.label = last4ccnum, 
      title = "Difference in mean spend across locations",
      results.subtitle = TRUE,
      xlab = "Location",
      ylab = "Price ($)"
    )
  })
  
  output$ANOVAC <- renderPlot({
    ggbetweenstats(
      data = (ANOVA_data %>% 
                filter(last4ccnum %in% input$ccnums)),
      x = last4ccnum,
      y = price,
      type = input$type,
      #conf.level = input$cf,
      #outlier.tagging = TRUE,
      #outlier.label = last4ccnum, 
      title = "Difference in mean spend across credit card numbers",
      results.subtitle = TRUE,
      xlab = "Credit Card Numbers",
      ylab = "Price ($)"
    )
  })
  
  # Plot histogram
  output$histL <- renderPlotly({
    hist <- ggplot(ANOVA_data %>% 
                     filter(location %in% input$locations), aes(x=price)) +
      geom_histogram(bins = input$bins) +
      facet_wrap(~location) +
      labs(title = "Distribution of spend amount by location",
           y = "Frequency", x = "Price ($)")
    
    ggplotly(hist)
  })  
  
  output$histC <- renderPlotly({
    hist <- ggplot(ANOVA_data %>% 
                     filter(last4ccnum %in% input$ccnums), aes(x=price)) +
      geom_histogram(bins = input$bins) +
      facet_wrap(~last4ccnum) +
      labs(title = "Distribution of spend amount by location",
           y = "Frequency", x = "Price ($)")
    
    ggplotly(hist)
  })  
  
      
  
}

shinyApp(ui, server)