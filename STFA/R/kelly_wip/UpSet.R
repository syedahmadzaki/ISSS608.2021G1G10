library(shiny)
library(tidyverse)
library(plyr)
library(ggplot2)
library(gridExtra)
library(UpSetR)
library(RSVGTipsDevice)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UpSet Plot for Co-Location Investigation"),
    
    sidebarLayout(
        
        # Sidebar for basic and advanced settings for Upset Plot
        sidebarPanel(value="upset_plot",
            tabsetPanel(
                tabPanel(
                'Setting',
                htmlOutput("sets"),
                br(),
                
                # No. of intersections
                numericInput(
                    "nintersections",
                    label = ("Limit number of intersections shown to :"),
                    value = 55,
                    min = 1,
                    max = 60),
                
                # Order by degree or frequency
                selectInput(
                    "order",
                    label = ("Order by"),
                    choices = list("Degree" = "degree",
                                   "Frequency" = "freq"),
                    selected = "freq"),
                
                # To show empty intersections
                checkboxInput('empty', 
                              label = "Show empty intersections", 
                              value = FALSE),
                
                # To enable chart download
                radioButtons(
                    inputId = "filetype",
                    label = "File type",
                    choices = list("png", "svg", "pdf")),
                downloadButton(outputId = "down", 
                               label = "Download")
            ),
            tabPanel("Format"
                     )
            )
        ),

        # Show tab panels of file import, data cleaning and plot 
        mainPanel(
            tabsetPanel(
                
                # Step 1 - User to upload data
                tabPanel("Import Data",
                         br(),
                         h4(HTML("<u>Instructions</u>")),
                         p(
                             "Begin by uploading a correctly formatted .csv file.
                         A correctly formatted .csv file is encoded in binary and set up so that each
                         column represents a set, and each row represents an element. If an element is
                         in the set it is represented as a 1 in that position. If an element is not in
                         the set it is represented as a 0."
                         ),
                         br(),
                         
                         # Prompt user to upload csv file
                         fileInput("file",
                                   label = "Data",
                                   buttonLabel = "Upload file"),
                         
                         # Users to indicate if file has header
                         checkboxInput('header', label = 'Has Header', TRUE),
                         
                         # Users to indicate delimiter type
                         #radioButtons(
                             #'sep',
                             #label = ('Delimiter (leave blank to guess)'),
                             #choices = c(
                                 #Comma = ',',
                                 #Semicolon = ';',
                                 #Tab = '\t'
                             #),
                             #selected = ','
                         #),
                         
                         # Users to indicate quote
                         selectInput("quote", 
                                     label = "Quote", 
                                     choices = c(
                             "None" = "",
                             "Double quote" = "\"",
                             "Single quote" = "'"
                         )),
                         
                         # Users to indicate no. of rows to preview
                         #numericInput("rows", "Rows to preview",
                                      #10, min = 1),
                         
                         # Confirm settings to preview dataset
                         #tags$button(id="confirm1", 
                                     #type="button", 
                                     #class="btn action-button btn-large btn-primary", 
                                     #HTML('<i class="icon-star"></i>Preview Data')),
                         
                         # Show data table
                         dataTableOutput("table"),
                         
                         br()
                         ),
                
                # Step 2 - User to clean data if necessary
                tabPanel("Clean Data",
                         br(),
                         h4(HTML("<u>Instructions</u>")),
                         p(
                             "Begin by uploading a correctly formatted .csv file.
                         A correctly formatted .csv file is encoded in binary and set up so that each
                         column represents a set, and each row represents an element. If an element is
                         in the set it is represented as a 1 in that position. If an element is not in
                         the set it is represented as a 0."
                         ),
                         br()),
                
                # Step 3 - User to plot chart
                tabPanel("UpSet Plot",
                         plotOutput("distPlot"))
            )
           
        )
    )
)

# Define server logic required
server <- function(input, output, session) {
    
    # Read user upload csv file
    userFile <- reactive({
        #validate(need(input$file, message = FALSE))
        input$file
        })
    
    dataframe <- reactive({
        read.csv(userFile$datapath,
                 header = input$header,
                 #sep = input$sep,
                 quote = input$quote,
                 stringsAsFactors = FALSE)
        })
    
    #observe({
        #msg <- sprintf("File %s was uploaded", userFile()$name)
        #cat(msg, "\n")
        #})
    
    output$table <- renderDataTable({
        return(dataframe)
    })
    
    
    # Show error message if there is no data set uploaded or selected
    #output$plot_text <- renderUI({
        
        #if(is.null(My_data()) == T){
            #p(HTML("UpSet plot will show here. 
            #<br/> There is no data entered. 
                  #Please return to the Import Data tab to input your data or select default dataset."))
        #}
        #else{
            #HTML(" ")
        #}
    #})
    
}

csvFileServer <- function(id, stringsAsFactors) {
    moduleServer(
        id,
        function(input, output, session) {
            userFile <- reactive({
                validate(need(input$file, message = FALSE))
                input$file
            })
            
            dataframe <- reactive({
                read.csv(userFile()$datapath,
                         header = input$heading,
                         quote = input$quote,
                         stringsAsFactors = stringsAsFactors)
            })
            
            observe({
                msg <- sprintf("File %s was uploaded", userFile()$name)
                cat(msg, "\n")
            })
            return(dataframe)
        }
    )    
}

# Run the application 
shinyApp(ui = ui, server = server)
