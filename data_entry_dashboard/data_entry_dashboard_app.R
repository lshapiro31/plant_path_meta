library(shiny)
library(rhandsontable)
library(rdrop2)
library(rowr)
library(shinydashboard)

token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

outputDir <- "responses"

saveData <- function(data) {
  # Create a unique file name
  fileName <- sprintf("%s_%s.tsv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.table(data, filePath, row.names = FALSE, quote = FALSE, sep = "\t")
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir)
}

# which fields get saved 
fieldsAll <- c("name", "alt_name", "taxonomy", "distribution", "disease", "host", "substrate", "reservoir", "first_date", "other")

# which fields are mandatory
fieldsMandatory <- c("name")

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

defaultDF <- data.frame(location = rep("place name", 3), dates = rep("YYYY", 3), hosts = rep("host", 3),
                        references = rep("ref", 3),
                        stringsAsFactors = F)


########APP#############
ui = dashboardPage(
  #shinyjs::inlineCSS(appCSS),
  
  dashboardHeader(title = "Data Entry For Plant Pathogens", titleWidth = 350
                    ),
  
  dashboardSidebar(disable = T),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    shinyjs::hidden(
      span(id = "submit_msg", "Submitting..."),
      div(id = "error",
          div(br(), tags$b("Error: "), span(id = "error_msg"))
      )
    ),
    
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thanks, your response was submitted successfully!"),
        actionLink("submit_another", "Submit another response")
      )
    ),
    
    shinyjs::hidden(
      span(id = "submit_msg", "Submitting..."),
      div(id = "error",
          div(br(), tags$b("Error: "), span(id = "error_msg"))
      )
    ),
    
      fluidRow(
        div(id = "form",
          column(width = 4,
            box(title = "Instructions", width = NULL, status = "danger", solidHeader = T,
                  h2("Please read",
                    a(href = "data_entry_README.html",
                      "these instructions", target="_blank"), 
                    "before you begin"
                    )
                ),     
            
            box(title = "Nomenclature", width = NULL, status = "warning", solidHeader = T,
              textInput("name", labelMandatory("Scientific Name"), ""),
              textInput("alt_name", "Comma separated alternative names(s)", ""),
              helpText("* mandatory field, please enter a valid genus and species")
            ),
            
            box(title = "Route of Transmission", width = NULL, status = "info", solidHeader = T,
                checkboxGroupInput("transmission", label = "Choose all that apply", 
                                   choices = list("Soil reservoir" = "soil", 
                                                  "Wind" = "wind", 
                                                  "Water" = "water", 
                                                  "Seed (vertical)" = "seed", 
                                                  "Obligate Insect" = "oblig_insect",
                                                  "Facultative Insect" = "fac_insect",
                                                  "Mechanical" = "mechanical",
                                                  "Other or Unknown" = "unknown"
                                                  ))
            ), 
            
            box(title = "Submission", width = NULL, color = "maroon", solidHeader = T,
                  helpText("Please double check that all info is correct before submitting"),
                  actionButton("submit", "Submit", class = "btn-primary")
                )
          ),
        
          column(width = 8,  
              box(title = "Pathogen Data", width = NULL, status = "primary", solidHeader = T,
                textInput("taxonomy", "GBIF ID (not required if you enter valid genus/species)", ""),
                textInput("distribution", "Distribution (Eg. North America)", ""),
                textInput("disease", "Disease Type (eg. vascular wilt)", ""),
                textInput("host", "Common Hosts (eg. poaceae)", ""),
                textInput("substrate", "Site of Entry into Plant", ""),
                textInput("reservoir", "Environmental Reservoirs", ""),
                textInput("first_date", "Year First Named or Described", ""),
                textInput("other", "Other Data", "")
              ),
              
              box(title = "Occurence Records", width = NULL, status = "success", solidHeader = T,
                  h4("In this table enter the documented locations for the pathogen. 
                For each documented location, please enter the associated host plant, date observed, and reference. 
                If there are multiple records (different host plants or references/dates) for a location, 
                enter values as a", strong("semicolon"), "separated list. Please fill out host, date, and reference for each entry.
                If you do not know one of the values, enter 'unknown' instead of a blank value.
                Please read", a(href = "data_entry_README.html",
                                "these instructions", target="_blank"), "for more detail on working with this table."),
                  rHandsontableOutput("hot", width = 1000)
              )
          )
      )
    )
  )
)
    

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
  
  #handsontable for input values#
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
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data2 <- data.frame(transmission = paste(input$transmission, collapse = ","))
    data <- cbind.fill(locations_table(), t(data), data2, fill = NA)
    data
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

