library(shiny)

ui <- fluidPage(
  actionButton("Next", "Next"),
  radioButtons("choice", label = "", choices = list("A" = 1, "B" = 2, "C" = 3), selected = character(0)),
  textOutput("status"),
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(choice) {
      Shiny.onInputChange(choice, null);
    });
  ")
)

server <- function(input, output, session){
  
  output$status <- renderText({input$choice})
  
  observeEvent(input$Next, {

    session$sendCustomMessage(type = "resetValue", message = "choice")
  })
}

shinyApp(ui, server)