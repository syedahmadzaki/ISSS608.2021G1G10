library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UpSet Plot for Co-Location Investigation"),

    # Sidebar for basic and advanced settings for Upset Plot
    sidebarLayout(
        sidebarPanel(value="upset_plot",
            tabsetPanel(
                tabPanel(
                'Settings',
                htmlOutput("sets"),
                br(),
                numericInput(
                    "nintersections",
                    label = ("Limit number of intersections shown to :"),
                    value = 40,
                    min = 1,
                    max = 60),
                selectInput(
                    "order",
                    label = ("Order by"),
                    choices = list("Degree" = "degree",
                                   "Frequency" = "freq"),
                    selected = "freq"),
                checkboxInput('empty', 
                              label = "Show empty intersections", 
                              value = FALSE),
                radioButtons(
                    inputId = "filetype",
                    label = "File type",
                    choices = list("png", "svg", "pdf")),
                downloadButton(outputId = "down", 
                               label = "Download!"),
                sliderInput("bins",
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30)
            ),
            tabPanel("Format"
                     )
            )
        ),

        # Show tab panels of file import, data cleaning and plot 
        mainPanel(
            tabsetPanel(
                tabPanel("Import data",
                         br(),
                         h4(HTML("<u>Instructions</u>")),
                         p(
                             "To use this method, begin by uploading a correctly formatted .csv file.
                         A correctly formatted .csv file is encoded in binary and set up so that each
                         column represents a set, and each row represents an element. If an element is
                         in the set it is represented as a 1 in that position. If an element is not in
                         the set it is represented as a 0."
                         ),
                         br(),
                         fileInput("file",
                                   "Data",
                                   buttonLabel = "Upload...",
                                   accept = c(
                                       'text/csv',
                                       'text/comma-separated-values',
                                       'text/tab-separated-values',
                                       '.csv',
                                       '.tsv')),
                         checkboxInput('header', label = 'Header', TRUE),
                         radioButtons(
                             'sep',
                             label = ('Delimiter (leave blank to guess)'),
                             choices = c(
                                 Comma = ',',
                                 Semicolon = ';',
                                 Tab = '\t'
                             ),
                             selected = ','
                         ),
                         numericInput("skip","Rows to skip",
                                      0, min=0),
                         numericInput("rows", "Rows to preview",
                                      10, min = 1),
                         tags$button(id="confirm1", 
                                     type="button", 
                                     class="btn action-button btn-large btn-primary", 
                                     HTML('<i class="icon-star"></i>Plot!')),
                         br()
                         ),
                tabPanel("Clean Data"),
                tabPanel("UpSet Plot",
                         plotOutput("distPlot"))
            )
           
        )
    )
)

# Define server logic required
server <- function(input, output, session) {
    
    attr(input, "readonly") <- FALSE
    
    output$plot_text <- renderUI({
        
        if(is.null(My_data()) == T){
            p(HTML("This is where your plot will show! 
            <br/> There is no data entered. 
                   Return to the Import Data tab to input your data."))
        }
        else{
            HTML(" ")
        }
    })
    
    eventReactive(input$confirm1,{
        input$confirm1[1] <- 1
        input$confirm2[1] <- 0
        input$confirm3[1] <- 0
        pushed$B <- 1
        if(is.null(My_data())){
            withProgress(message = "Confirmed failure", value = 0, {setProgress(1)})
        }
        else{
            withProgress(message = "Confirmed success", value = 0, {setProgress(1)})
        }
        input$Select <- NULL
    })
    
    confirmed <- reactive({
        one <- input$confirm1[1]})
    
    My_dat <- reactive({  
        inFile <- input$file1
        
        if(pushed$B == 0 || length(pushed$B) > 1){
            return(NULL)
        }
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, header = input$header,
                 sep = input$sep, quote = input$quote)
    })
    
    venneulerData <- reactive({
        string <- input$venn
        string <- gsub("\n", "", string)
        if(string != ""){
            string <- as.list(unlist(strsplit(string, ",")))
            names <- lapply(string, function(x){x <- unlist(strsplit(x, "=")); x <- x[1]})
            names <- unlist(lapply(names, function(x){x <- gsub(" ", "", x)}))
            values <- as.numeric(unlist(lapply(string, function(x){x <- unlist(strsplit(x,"=")); x <- x[2]})))
            names(values) <- names
            venneuler <- upsetVenneuler(values)
            return(venneuler)
        }
    })
    
    output$data <- renderTable({
        head(My_data(), 10)
    })
    
    

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
