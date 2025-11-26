library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(openair)
library(ggplot2)

ui <- fluidPage(
  titlePanel("ENV405 wind analysis app"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file_wind",
        "Upload wind CSV file",
        accept = c(".csv")
      ),
      helpText("If no file is uploaded, the app uses wind.csv.")
    ),
    mainPanel(
      plotOutput("wind_rose"),
      tableOutput("wind_table"),
      plotOutput("wind_bar"),
      plotOutput("wind_ts")
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
      main         = "Wind rose for uploaded or default wind.csv"
    )
  })
  
  wd_to_sector <- function(wd) {
    wd <- wd %% 360
    cuts   <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
    labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    cut(
      wd,
      breaks = cuts,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE
    )
  }
  
  wind_freq <- reactive({
    wind() %>%
      filter(!is.na(wd)) %>%
      mutate(direction_sector = wd_to_sector(wd)) %>%
      count(direction_sector, name = "count") %>%
      mutate(
        total            = sum(count),
        rel_freq         = count / total,
        rel_freq_percent = round(rel_freq * 100, 1)
      ) %>%
      arrange(direction_sector)
  })
  
  output$wind_table <- renderTable({
    wind_freq()
  })
  
  output$wind_bar <- renderPlot({
    ggplot(wind_freq(), aes(x = direction_sector, y = rel_freq_percent)) +
      geom_col() +
      labs(
        title = "Relative frequency of wind directions",
        x     = "Wind direction sector",
        y     = "Relative frequency (%)"
      ) +
      theme_minimal()
  })
  
  output$wind_ts <- renderPlot({
    df <- wind()
    
    ggplot(df, aes(x = date, y = ws)) +
      geom_line() +
      labs(
        title = "Wind speed time series",
        x     = "Time",
        y     = "Wind speed (m/s)"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

           