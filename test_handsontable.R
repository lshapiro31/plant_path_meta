library(shiny)
library(rhandsontable)
library(dplyr)
library(magrittr)


df.0 <- data.frame(
  X1 = rep(NA_integer_, 10),
  X2 = NA_integer_,
  stringsAsFactors = F)


df.1 <- data.frame(
  X1 = rep(1, 10),
  X2 = c(-10:-1),
  stringsAsFactors = F)



calc.df <- function(fdf){
  
  fdf <- fdf %>%
    mutate(
      Y1 = X1 + X2,
      Y2 = ifelse(Y1 > 10, 10, 20)
    )
  
  return(fdf)
}


ui <- shinyUI(
  basicPage(
    actionButton("ab_reset_1", "Reset"),
    actionButton("ab_reset_2", "Get some data"),
    rHandsontableOutput("hot.df")
  )
)



server <- function(input, output, session) {
  values <- reactiveValues(
    hot_df = df.0,
    reset_b1 = 0,
    reset_b2 = 0,
    reset_val = 0
  )
  
  
  df.hot <- reactive({
    
    df <- NULL
    
    if(!is.null(input$hot.df)){
      df <- hot_to_r(input$hot.df)
    } else if(!is.null(isolate(values$hot_df))) {
      df <- isolate(values$hot_df)
    }
    
    if(!is.null(df)){
      df <- calc.df(df)
      values$hot_df <- df
    }
    
    df
  }) 
  
  
  output$hot.df <- renderRHandsontable({
    
    input$ab_reset_1
    input$ab_reset_2
    
    if (isolate(values$reset_val)) {
      df <- values$hot_df
    } else {
      df <- df.hot()
      values$reset_val <- 0
    }
    
    if(!is.null(df)){
      rhandsontable(df, useTypes = TRUE)
    }
  }) 
  
  
  observeEvent(input$ab_reset_1, {
    values$hot_df <- calc.df(df.0)
    values$reset_val <- 1
  })
  
  
  observeEvent(input$ab_reset_2, {
    values$hot_df <- calc.df(df.1)
    values$reset_val <- 1
  })
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))