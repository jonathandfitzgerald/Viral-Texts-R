library(shiny)
library(ggplot2)  # for the diamonds dataset
library(shinyjs)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
packageVersion('plotly')
useShinyjs()
# vtData <- read_csv(file = "data/genreClass-2-19-17.csv")
# vtData$text=gsub("(\")|(\')","\\\\'",vtData$text)
# vtData$text = gsub("\r?\n|\r", "\\s", vtData$text)
# vtData <- vtData[c(-1)]
# pca4plotly <- read_csv(file = "data/pca4plotly-6-23-17.csv")
# pca4plotly$text < pca4plotly$text %>% strwrap()


server <- function(input, output) {

  
  # Database Tab
  # choose columns to display
  
  # Filter data based on selections
  
  dataset <- reactive(
    {
      data = vtData
      if (input$classified_genre != "All") 
        
      {
        data <- data[data$classified_genre == input$classified_genre,]
      }
      
      data
      
    }
  )
  
  output$mytable1 <- DT::renderDataTable(DT::datatable(dataset(), options = list(columnDefs = list(list(
    targets = 4,
    render = JS(
      "function(data, type, row, meta) {",
      "return type === 'display' && data.length > 200 ?",
      "'<span title=\"' + data + '\">' + data.substr(0, 200) + '<a href=\"#\" onClick=\"alert(\\'' + data + '\\');\"> ...Read More</a></span>' : data;",
      "}")
  )))))
  
  
  # PCA Tab
  # customize the length drop-down menu; display 5 rows per page by default
  output$allBibPlot <- renderPlotly({
    
    if (input$Type2 == "All") {
      p <- plot_ly(pca4plotly, x = ~PC1, y = ~PC2, z = ~PC3, color = ~classified_genre, colors = viridis_pal(option = "C")(10), opacity = .5, text = ~text, marker = list(size = 4)) %>%
        add_markers() %>%
        layout(scene = list(xaxis = list(title = 'PC1'),
                            yaxis = list(title = 'PC2'),
                            zaxis = list(title = 'PC3')))
    }
    
    if (input$Type2 != "All") {
      pca4plotly <- pca4plotly %>% filter(genre == input$Type2)

      p <- plot_ly(pca4plotly, x = ~PC1, y = ~PC2, z = ~PC3, color = ~classified_genre, colors = viridis_pal(option = "C")(10), opacity = .5, text = ~text, marker = list(size = 4)) %>%
        add_markers() %>%
        layout(scene = list(xaxis = list(title = 'PC1'),
                            yaxis = list(title = 'PC2'),
                            zaxis = list(title = 'PC3')))
    }
    p
  })
  
}


ui <- fluidPage(
  title = 'VT Data Test',
  titlePanel("VT Data Test"),
  sidebarLayout(
    sidebarPanel(tags$head(tags$style("#allBibPlot,#GenderPlot{height:80vh !important;}.hovertext{width:200px;}")),
      
      # DATABASE PANEL
      conditionalPanel(
        
        'input.dataset === "Data"',
        div(
          id = "tab1",
          selectInput("classified_genre",
                      "Genre:",
                      c("All",
                        sort(unique(as.character(vtData$classified_genre))))),
          
          #actionButton("resetYear", "Reset Year"),
          
          hr()
        )
      ),
      
      
      # PCA PANEL
      conditionalPanel(
        'input.dataset === "PCA"',
        selectInput("Type2",
                    "Genre:",
                    c("All",
                      sort(unique(as.character(pca4plotly$genre))))),
      
        hr(),
        helpText('Display all records by default.')
      ),
     

      
      p("This project is in BETA. Email", a("Fitz", href="mailto:jonathanfitzgerald@me.com"), "with any suggestions, bug reports, etc."),
      p("Twitter:", a("@jon_fitzgerald", href="http://www.twitter.com/jon_fitzgerald"))
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('Data', DT::dataTableOutput('mytable1')),
        tabPanel('PCA', plotlyOutput("allBibPlot"))
      )
    )
  )
)

shinyApp(ui = ui, server = server)