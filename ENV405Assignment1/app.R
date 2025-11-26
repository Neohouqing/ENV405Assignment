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
        accept = ".csv"
      ),
      helpText("If no file is uploaded, the app uses data/wind.csv.")
    ),
    
    mainPanel(
      plotOutput("wind_rose"),
      tableOutput("wind_freq_table"),
      plotOutput("wind_freq_bar"),
      tableOutput("wind_speed_summary"),
      plotOutput("wind_speed_hist")
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
      main         = "Wind rose for provided wind data"
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
      mutate(direction = wd_to_sector(wd)) %>%
      count(direction, name = "count") %>%
      mutate(
        total             = sum(count),
        rel_freq          = count / total,
        rel_freq_percent  = round(rel_freq * 100, 1)
      ) %>%
      arrange(direction)
  })
  
  output$wind_freq_table <- renderTable({
    wind_freq()
  })
  
  output$wind_freq_bar <- renderPlot({
    df <- wind_freq()
    
    ggplot(df, aes(x = direction, y = rel_freq_percent)) +
      geom_col() +
      labs(
        title = "Relative frequency of wind directions",
        x = "Direction sector",
        y = "Relative frequency (%)"
      ) +
      theme_minimal()
  })
  
  output$wind_speed_summary <- renderTable({
    wind() %>%
      summarise(
        n        = n(),
        mean_ws  = mean(ws, na.rm = TRUE),
        sd_ws    = sd(ws, na.rm = TRUE),
        min_ws   = min(ws, na.rm = TRUE),
        max_ws   = max(ws, na.rm = TRUE)
      )
  })
  
  output$wind_speed_hist <- renderPlot({
    df <- wind()
    
    ggplot(df, aes(x = ws)) +
      geom_histogram(bins = 20, boundary = 0, closed = "left") +
      labs(
        title = "Distribution of wind speed",
        x = "Wind speed (m/s)",
        y = "Count"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

           