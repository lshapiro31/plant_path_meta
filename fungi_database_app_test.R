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
      sidebarPanel("Some inputs and options"
      ),
      mainPanel("Acrosporium cyparissiae documented locations",
        uiOutput("leaf")
      )
    )
  )
)

server <- function(input, output) {
  
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
    leaflet(data = mapping_df) %>%
      addTiles() %>%
      addMarkers(~lon, ~lat, popup=mapping_df$label)
  })
}


shinyApp(ui, server)  