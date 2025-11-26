library(shiny)
library(readr)
library(dplyr)
library(lubridate)
# 注意：这里暂时没有 library(openair)，下一次 commit 再修

#-----------------------------
# 1. UI
#-----------------------------
ui <- fluidPage(
  titlePanel("ENV405 wind analysis app - Commit 1"),
  
  mainPanel(
    plotOutput("wind_rose")
  )
)

#-----------------------------
# 2. Server
#-----------------------------
server <- function(input, output, session) {
  
  # 读入 wind.csv，并把 date 转成时间格式
  wind <- reactive({
    read_csv("data/wind.csv", show_col_types = FALSE) %>%
      mutate(date = ymd_hms(date))
  })
  
  # 画风向玫瑰图（这里会报错，因为还没有加载 openair 包）
  output$wind_rose <- renderPlot({
    df <- wind()
    
    windRose(
      mydata       = df,
      ws           = "ws",   # 风速列
      wd           = "wd",   # 风向列
      paddle       = TRUE,
      key.position = "right",
      main         = "Wind rose for ENV405 wind.csv (Commit 1)"
    )
  })
}

#-----------------------------
# 3. 启动 app
#-----------------------------
shinyApp(ui = ui, server =server)
           
           
           
           