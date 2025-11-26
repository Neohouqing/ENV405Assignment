library(shiny)
library(readr)
library(dplyr)
library(lubridate)

ui <- fluidPage(
  titlePanel("ENV405 wind analysis app"),
  
  mainPanel(
    plotOutput("wind_rose")
  )
)

server <- function(input, output, session) {
  
  wind <- reactive({
    read_csv("data/wind.csv", show_col_types = FALSE) %>%
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
      main         = "Wind rose for ENV405 wind.csv"
    )
  })
}

shinyApp(ui = ui, server = server)

           
           
           
           