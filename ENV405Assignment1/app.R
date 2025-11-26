library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(openair)

ui <- fluidPage(
  titlePanel("ENV405 wind analysis app"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file_wind",
        "Upload wind CSV file",
        accept = c(".csv")
      ),
      helpText("If no file is uploaded, the app uses data/wind.csv.")
    ),
    
    mainPanel(
      plotOutput("wind_rose")
    )
  )
)

server <- function(input, output, session) {
  
  wind <- reactive({
    df <- if (!is.null(input$file_wind)) {
      read_csv(input$file_wind$datapath, show_col_types = FALSE)
    } else {
      read_csv("wind.csv", show_col_types = FALSE)
    }
    
    df %>%
      mutate(date = ymd_hms(date))
  })
  
  output$wind_rose <- renderPlot({
    df <- wind()
    
    windRose(
      mydata       = df,
      ws           = "ws",
      wd           = "wd",
      paddle       = TRUE,
      key.position = "right",
      main         = "Wind rose"
    )
  })
}

shinyApp(ui = ui, server = server)

           
           