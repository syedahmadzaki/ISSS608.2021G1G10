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
      ns('cc_data'),
      label = ('Select dataset'),
      choices = c(
        'Credit Card dataset',
        'Upload own dataset')
    ),
    
    # Prompt user to upload csv file
    fileInput(ns("file"),
              label = "Upload own dataset (csv format)",
              buttonLabel = "Upload file"),
    
    # Users to indicate if file has header
    checkboxInput(ns('heading'), label = 'Has Header', TRUE),

    # Users to indicate type of quote
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    )),
    
    # Users to indicate no. of rows to preview
    numericInput(ns("rows"), "Rows to preview", 10, min = 1)
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

# Clean data function

# Plot chart function


# ----------------------------------------------------------------------------------------------------------------------
# To port over to main app later

ui <- fluidPage(
  
  # Application title
  titlePanel("UpSet Plot for Co-Location Investigation"),
  
  sidebarLayout(
    
    # Sidebar for basic and advanced settings for Upset Plot
    sidebarPanel(),
    
    # Show tab panels of file import, data cleaning and plot 
    mainPanel(
      tabsetPanel(
        
        # Step 1 - User to upload data
        tabPanel("Import Data",
                 br(),
                 csvFileUI("datafile", "User data (.csv format)"),
                 dataTableOutput("table")),
        
        # Step 2 - User to clean data if necessary
        tabPanel("Clean Data"),
        
        # Step 3 - User to plot chart
        tabPanel("UpSet Plot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)
  
  # Preview imported data set
  output$table <- renderDataTable({
    datafile()
  })
  
  # Preview clean data set
  
  # Upset plot
  
}

shinyApp(ui, server)



