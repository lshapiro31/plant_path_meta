## Fungi Data Table Test App ##

library(shiny)
library(DT)
library(data.table)
library(leaflet)
library(shinythemes)

fungi_for_DT <- fread("fungi_for_DT.csv")

ui <- navbarPage(theme = shinytheme("flatly"),
                 title = "Example Fungi Database",
                 #Database tab
                 tabPanel("Database",
                          fluidRow(
                            column(1,
                                   checkboxGroupInput("show_vars", "Columns to display",
                                                      names(fungi_for_DT), selected = names(fungi_for_DT))
                                   
                            ),
                            
                            column(11,
                                   DT::dataTableOutput("mytable1")
                            )
                          )
                 ),
                 #Map tab
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              #user selects species to show data for
                              selectInput("select", label = h3("Select fungal species"), choices = names(fungi_data)
                              ),
                              #user selects date range to show data for
                              sliderInput("slider", label= h3("Select date"), min = 1800, max=2020, value=2000)
                            ),
                            mainPanel("Documented locations",
                                      uiOutput("leaf")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  
  #Output datatable with user selected columns
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(fungi_for_DT[, input$show_vars, drop = FALSE], extensions = "Buttons",
                  style = "bootstrap", escape = F, rownames = F, filter = "top", options = list(
                    pageLength = 10, autoWidth = T, dom = "Bftrip", buttons = c("copy", "csv", "excel")
                  ))
  })
  
  #define map size 
  output$leaf <- renderUI({
    leafletOutput("mymap", width = "100%", height = "800")
  })
  
  #define first map view on page
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  #change slider dates based on selected species date range
  observe({
    my_data <- fungi_data[[input$select]][[4]]
    
    updateSliderInput(session, "slider", min=min(my_data$date), 
                      max=max(my_data$date), 
                      value=max(my_data$date))
  })
  
  #change data for markers when user changes selected species
  marker_data <- reactive({
    subset_data <- fungi_data[[input$select]][[4]]
    subset_data <- subset_data[date<=input$slider, ]
    return(subset_data)
  })
  
  #update markers when user changes species or date
  observe({
    leafletProxy("mymap", data = marker_data()) %>%
      clearPopups() %>% 
      clearMarkers() %>%
      addMarkers(~lon, ~lat, popup = marker_data()$label)
  })
}


shinyApp(ui, server)  