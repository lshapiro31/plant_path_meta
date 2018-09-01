library(shiny)
library(rhandsontable)
########APP#############
ui = fluidPage(
  shinyjs::useShinyjs(),
  
  div(id = "header",
      h1("Data entry")
  ),
  
  fluidRow(
    column(4,
           div(
             id = "form",
             helpText(h4("Submission")),
             helpText("Please double check that all info is correct before submitting"),
             actionButton("submit", "Submit", class = "btn-primary"),
             
             shinyjs::hidden(
               span(id = "submit_msg", "Submitting..."),
               div(id = "error",
                   div(br(), tags$b("Error: "), span(id = "error_msg"))
               )
             )
           ),
           
           shinyjs::hidden(
             div(
               id = "thankyou_msg",
               h3("Thanks, your response was submitted successfully!"),
               actionLink("submit_another", "Submit another response")
             )
           )
    ),
    column(8,
           div(
             id = "form2",
             
             rHandsontableOutput("hot", width = 1000, height = 600),
             
             shinyjs::hidden(
               span(id = "submit_msg", "Submitting..."),
               div(id = "error",
                   div(br(), tags$b("Error: "), span(id = "error_msg"))
               )
             )
           )
    )
  )
)
defaultDF <- data.frame(location = rep("place name", 5), dates = rep("YYYY", 5), hosts = rep("host", 5),
                        references = rep("ref", 5),
                        stringsAsFactors = F)
server <- function(input, output, session) {
  #handsontable for input values
  values = reactiveValues(DF = defaultDF)
  
  locations_table = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      DF = values$DF
    }
    values$DF = DF
    DF
  })
  
  output$hot <- renderRHandsontable({
    DF = locations_table()
    if (!is.null(DF))
      rhandsontable(DF, useTypes = F, stretchH = "all")
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::reset("form2")
      shinyjs::hide("form2")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::show("form2")
    shinyjs::hide("thankyou_msg")
    output$hot <- renderRHandsontable({
      values$DF  = defaultDF
      rhandsontable(values$DF, useTypes = F, stretchH = "all")
    })
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)