library(shiny)
library(rhandsontable)
library(aws.s3)

s3BucketName <- "my-unique-s3-bucket-name"
Sys.setenv("AWS_ACCESS_KEY_ID" = "key",
           "AWS_SECRET_ACCESS_KEY" = "secret",
           "AWS_DEFAULT_REGION" = "region")

saveData <- function(data) {
  # Create a temporary file to hold the data
  data <- t(data)
  file_name <- paste0(
    paste(
      get_time_human(),
      digest(data, algo = "md5"),
      sep = "_"
    ),
    ".csv"
  )
  file_path <- file.path(tempdir(), file_name)
  write.csv(data ,file_path, row.names = FALSE, quote = TRUE)
  
  # Upload the file to S3
  put_object(file = file_path, object = file_name, bucket = s3BucketName)
}

# which fields get saved 
fieldsAll <- c("name", "alt_name", "taxonomy", "distribution", "disease", "host", "substrate", "hot", "double_check")

# which fields are mandatory
fieldsMandatory <- c("name", "double_check")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}




########APP#############
ui = fluidPage(
  shinyjs::useShinyjs(),
  #shinyjs::inlineCSS(appCSS),
  
  div(id = "header",
      h1("Data entry for Plant Pathogen Meta-analysis"),
      h4("Please read",
         a(href = "data_entry_README.html",
           "these instructions", target="_blank")
      ),
      
      fluidRow(
        column(6,
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
                 br(),
                 br(),
                 helpText(h4("Submission")),
                 checkboxInput("double_check", labelMandatory("I've checked that all information is correct"), FALSE),
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
        column(6,
               rHandsontableOutput("hot")
        )
      )
  ))

# Define server logic required to draw a histogram
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
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })    
  
  #handsontable for input values#
  values = reactiveValues()
  
  data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF = data.frame(location = rep("place name", 5), date_1 = rep("YYYY", 5), host_1 = rep("host", 5),
                        reference_1 = rep("ref", 5),
                        stringsAsFactors = F)
      else
        DF = values[["DF"]]
    }
    
    values[["DF"]] = DF
    DF
  })
  
  output$hot <- renderRHandsontable({
    DF = data()
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
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
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
    shinyjs::hide("thankyou_msg")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

