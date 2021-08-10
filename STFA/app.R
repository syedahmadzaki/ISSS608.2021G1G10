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

# Define UI for application that draws a histogram
ui <-  navbarPage(title = "VAST Mini Challenge 2", selected = " Intro", collapsible = TRUE, inverse = FALSE,
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
                  #theme = shinytheme("flatly"),
                  
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
                  
                  tabPanel(" Exploring GPS",icon = icon("map-marked-alt"),
                           #                           titlePanel("Using A Customised Map With Interactive Control"),
                           sidebarLayout(position = "left", fluid = TRUE,
                                         sidebarPanel(tags$style(".well {background-color:#e7e7e7;}"),
                                                      width = 3, fluid = TRUE,
                                                      h4("SELECT DATE", align = "center"),
                                                      dateInput(inputId = "MapDate",
                                                                min = ("2014-01-06"),
                                                                max = ("2014-01-19"),
                                                                #                               sliderInput(inputId = "MapDate",
                                                                #                                           min = as.Date("2014-01-06","%Y-%m-%d"),
                                                                #                                           max = as.Date("2014-01-19","%Y-%m-%d"),
                                                                value=("2014-01-06"),
                                                                format = "d M",
                                                                #                                           timeFormat="%e-%b",
                                                                #                                           step = 1,
                                                                label = NULL),
                                                      hr(style = "border-top: 3px solid #FFFFFF;"),
                                                      h4("COMPARE MAX THREE STAFF", align = "center"),
                                                      
                                                      # Select Staff ID 1
                                                      
                                                      selectInput(inputId="id1",
                                                                  label="Select Staff ID 1",
                                                                  choices = stafflist1,
                                                                  selected="IT Helpdesk Nils Calixto"),
                                                      fluidRow(
                                                        column(6,
                                                               colourInput(
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
                                                               colourInput(
                                                                 inputId = "id2colour",
                                                                 label = "Select Line Colour 2",
                                                                 palette = "limited",
                                                                 showColour = "background",
                                                                 "#0000FF"
                                                               )
                                                               #                                        selectInput(inputId = "id2colour",
                                                               #                                                    label = "Select Line Colour 2",
                                                               #                                                    choices = colourlist,
                                                               #                                                    selected = "blue")
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
                                                               colourInput(
                                                                 inputId = "id3colour",
                                                                 label = "Select Line Colour 3",
                                                                 palette = "limited",
                                                                 showColour = "background",
                                                                 "#FF0000"
                                                               )
                                                               #                                          selectInput(inputId = "id3colour",
                                                               #                                                      label = "Select Line Colour 3",
                                                               #                                                      choices = colourlist,
                                                               #                                                      selected = "red")
                                                        ),
                                                        column(6,
                                                               selectInput(inputId = "id3linetype",
                                                                           label = "Select Line Type 3",
                                                                           choices = linetypelist,
                                                                           selected = "3")
                                                        )
                                                      ),
                                                      submitButton("Apply Changes", icon("refresh"))
                                         ),
                                         mainPanel(width = 9, fluid = TRUE,
                                                   tabsetPanel(
                                                     tabPanel("Map",
                                                              tmapOutput("map", width = "100%", height = 600),
                                                              tags$div(tags$br()),
                                                              fluidRow(width = 12,
                                                                       column(4, align = "center",
                                                                              #                                           h4(textOutput("selected_id1"), align = "center"),
                                                                              #                                           h4("STAFF ID 1's MOVEMENTS", align = "center"),
                                                                              strong(textOutput("selected_id1")),
                                                                              formattableOutput("id1table", width = "100%", height = 400),
                                                                              style='border-right: 1px solid black'
                                                                       ),
                                                                       column(4, align = "center",
                                                                              #                                          h4("STAFF ID 2's MOVEMENTS", align = "center"),
                                                                              strong(textOutput("selected_id2")),
                                                                              #                                         textOutput("selected_id2"),
                                                                              formattableOutput("id2table", width = "100%", height = 400),
                                                                              style='border-right: 1px solid black'
                                                                       ),
                                                                       column(4, align = "center",
                                                                              strong(textOutput("selected_id3")),
                                                                              #                                           h4("STAFF ID 3's MOVEMENTS", align = "center"),
                                                                              #                                          textOutput("selected_id3"),
                                                                              formattableOutput("id3table", width = "100%", height = 400)
                                                                       )
                                                              )),
                                                     tabPanel("Instructions")),
                                                   
                                                   
                                                   #                                 column(4,
                                                   #                                        box(title = "Super Duper",
                                                   #                                            solidHeader = TRUE,
                                                   #                                            collapsible = TRUE,
                                                   #                                            background = "teal",
                                                   #                                          formattableOutput("id1table", width = "100%", height = 400))
                                                   #                                        ),
                                                   #                                 column(4,
                                                   #                                        box(formattableOutput("id2table", width = "100%", height = 400))
                                                   #                                        ),
                                                   #                                 column(4,
                                                   #                                        box(formattableOutput("id3table", width = "100%", height = 400))
                                                   #                                        )
                                                   #                               ),
                                                   #                                fluidRow(
                                                   #                                  column(DT::dataTableOutput("rawtable"), width = 12)),
                                         )
                           )
                  ),                           
                  
                  
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
server <- function(input, output,session) {
  
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
  
  #------------ Limit selectInput Line Colour options within Map tab
  
  #  observe({
  #    if(!is.null(c(input$id1colour, input$id2colour)))
  #      updateSelectInput(session, "id3colour", 
  #                        choices = colourlist[!(colourlist %in% c(input$id1colour, input$id2colour))], 
  #                        selected = isolate(input$id3colour) )
  #  })
  
  #  observe({
  #    if(!is.null(c(input$id2colour, input$id3colour)))
  #      updateSelectInput(session, "id1colour", 
  #                        choices = colourlist[!(colourlist %in% c(input$id2colour, input$id3colour))], 
  #                        selected = isolate(input$id1colour) )
  #  })
  
  #  observe({
  #    if(!is.null(c(input$id3colour, input$id1colour)))
  #      updateSelectInput(session, "id2colour", 
  #                        choices = colourlist[!(colourlist %in% c(input$id3colour, input$id1colour))], 
  #                        selected = isolate(input$id2colour) )
  #  })
  
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
    tm_shape(sea_poly) +
      tm_polygons(col = "#d0cfd4") +
      tm_shape(Kronos_sf_small) +
      tm_polygons(col = "#e7e7e7") +
      tm_shape(Abila_st_buffer) +
      tm_polygons(col = "white") +
      tm_shape(gps_path %>% filter(date == input$MapDate & Name==input$id1)) +
      tm_lines(col = input$id1colour,
               lwd = 2,
               lty = input$id1linetype,
               id = "RoleNName") +
      tm_shape(gps_path %>% filter(date == input$MapDate & Name==input$id2)) +
      tm_lines(col = input$id2colour,
               lwd = 2,
               lty = input$id2linetype,
               id = "RoleNName") +  
      tm_shape(gps_path %>% filter(date == input$MapDate & Name==input$id3)) +
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
      tm_view(set.view = c(24.866975, 36.069965, 14))
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