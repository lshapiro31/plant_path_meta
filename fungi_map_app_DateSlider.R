## Fungi Data Table Test App ##

library(shinythemes)

ui <- navbarPage(theme = shinytheme("flatly"),
                 title = "Example Fungi Database",
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
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("select", label = h3("Select fungal species"), choices = names(fungi_data)
                              ),
                              sliderInput("slider", label= h3("Select date"), min = 1800, max=2020, value=2000)
                            ),
                            mainPanel("Documented locations",
                                      uiOutput("leaf")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(fungi_for_DT[, input$show_vars, drop = FALSE], extensions = "Buttons",
                  style = "bootstrap", escape = F, rownames = F, filter = "top", options = list(
                    pageLength = 10, autoWidth = T, dom = "Bftrip", buttons = c("copy", "csv", "excel")
                  ))
  })
  
  output$leaf <- renderUI({
    leafletOutput("mymap", width = "100%", height = "800")
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() 
  })
  
  observe({
    my_data <- fungi_data[[input$select]][[4]]
    
    updateSliderInput(session, "slider", min=min(my_data$date), 
                      max=max(my_data$date), 
                      value=max(my_data$date))
  })
  
  marker_data <- reactive({
    subset_data <- fungi_data[[input$select]][[4]]
    subset_data <- subset_data[subset_data$date<=input$slider, ]
    return(subset_data)
  })

  
  observe({
    leafletProxy("mymap", data = marker_data()) %>%
      clearPopups() %>% 
      clearMarkers() %>%
      addMarkers(~lon, ~lat, popup = marker_data()$label)
  })
}


shinyApp(ui, server)  