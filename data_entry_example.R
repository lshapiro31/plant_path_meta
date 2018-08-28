library(shiny)
library(rhandsontable)

# which fields get saved 
fieldsAll <- c("name", "alt_name", "taxonomy", "distribution", "disease", "host", "substrate", "other")

# which fields are mandatory
fieldsMandatory <- c("name")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

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
             helpText(h4("Nomenclature")),
             textInput("name", labelMandatory("Scientific Name"), ""),
             textInput("alt_name", "Comma separated alternative names(s)", ""),
             br(),
             br(),
             helpText(h4("Basic Data")),
             textInput("taxonomy", "GBIF ID (or other taxonomy database link)", ""),
             textInput("distribution", "Distribution (Eg. North America)", ""),
             textInput("disease", "Disease Type (eg. vascular wilt)", ""),
             textInput("host", "Common Hosts (eg. poaceae, confiers, etc.)", ""),
             textInput("substrate", "Common Substrates(s) (eg. infloresence)", ""),
             textInput("other", "Other Data", ""),
             br(),
             br(),
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


server <- function(input, output, session) {
  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  #handsontable for input values
  values = reactiveValues(DF = data.frame(location = rep("place name", 5), dates = rep("YYYY", 5), hosts = rep("host", 5),
                                          references = rep("ref", 5),
                                          stringsAsFactors = F))
  
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
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

