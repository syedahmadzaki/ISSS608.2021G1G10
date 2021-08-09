library(shiny)
library(tidyverse)
library(plyr)
library(ggplot2)
library(ggstatsplot)

# ----------------------------------------------------------------------------------------------------------------------
# Import csv data function 


# ----------------------------------------------------------------------------------------------------------------------
# Plot chart function


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
        
        # User to plot chart
        tabPanel("UpSet Plot", 
                 UpSetPlotUI("upset")),
        
        # User to upload data
        tabPanel("Import Own Data",
                 br(),
                 csvFileUI("datafile", "User data (.csv format)"),
                 dataTableOutput("table"))
        
        
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
  
  # ANOVA plot
  ANOVAServer("upset")
  
}

shinyApp(ui, server)