#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(tidyverse)
library(shinyjs)
library(wordVectors)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: green;
                    font-size: 60px;
                    line-height: 100px;
                    }
                    .container-fluid {
                    width: 900px;
                    }
                    "))
    ),
   
   # Application title
   titlePanel("VSM Test (using Viral Texts data)"),
   sidebarLayout(
     sidebarPanel(
   # Sidebar with a inputs
   textInput("word1", "Enter a word", ""),
   textInput("word2", "Enter another word", ""),
   numericInput("count", "Number of words to view:", 10)
 
     ),
      # Show a plot of the generated distribution
   mainPanel(
     DT::dataTableOutput("table")
   )
   )
)

# Define server logic required to display table
server <- function(input, output) {
  dataset <- reactive(
    {
      validate(
        need(input$word1 != "", "\U21e6 Start here.")
      )
      data <- vtData_model50 %>% closest_to(input$word1, input$count) %>% as_data_frame()
      
      if (input$word2 != "") {
        data <- vtData_model50 %>% closest_to(c(input$word1,input$word2), input$count) %>% as_data_frame()
      }
      data
    }
  )
 output$table <- DT::renderDataTable(DT::datatable({dataset()}, colnames=c("Word", "Similarity to word(s)"), options = list(paging = FALSE,searching = TRUE)))
  
}



# Run the application 
shinyApp(ui = ui, server = server)

