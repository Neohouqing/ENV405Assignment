# wind_analysis.R

# 加载必要的包
library(readr)
library(dplyr)
library(lubridate)
library(openair)   # 用于 windRose()
library(ggplot2)

# 1. 读入数据 ------------------------------------------------------------wind <- read_csv("/Users/neo/Desktop/ENV405 统计/wind.csv")
wind <- read_csv("/Users/neo/Desktop/ENV405 统计/wind.csv")


# 简单查看结构
glimpse(wind)

# 把 date 转成时间格式
wind <- wind %>%
  mutate(date = ymd_hms(date))

# 2. 画一个最基本的风向玫瑰图 -------------------------------------------

windRose(
  mydata = wind,
  ws = "ws",    # 风速列名
  wd = "wd",    # 风向列名
  paddle = TRUE,
  key.position = "right",
  main = "Wind rose for ENV405 provided wind.csv data"
)

# 3. 计算各风向的频率表 ---------------------------------------------------

# 辅助函数：把角度风向 wd 映射为 8 个方位（N, NE, E, SE, S, SW, W, NW）
wd_to_sector <- function(wd) {
  # 确保在 0-360 范围内
  wd <- wd %% 360
  
  # 定义 8 个扇区的边界（0-45, 45-90, ..., 315-360）
  cuts   <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
  labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  
  cut(
    wd,
    breaks = cuts,
    labels = labels,
    include.lowest = TRUE,
    right = FALSE  # 区间左闭右开 [0,45), [45,90) ...
  )
}

wind_freq <- wind %>%
  filter(!is.na(wd)) %>%
  mutate(direction_sector = wd_to_sector(wd)) %>%
  count(direction_sector, name = "count") %>%
  mutate(
    total = sum(count),
    rel_freq = count / total,
    rel_freq_percent = round(rel_freq * 100, 1)
  ) %>%
  arrange(direction_sector)

wind_freq

# 4. 可选：画一个条形图展示频率 ------------------------------------------

ggplot(wind_freq, aes(x = direction_sector, y = rel_freq_percent)) +
  geom_col() +
  labs(
    title = "Relative frequency of wind directions",
    x = "Wind direction sector",
    y = "Relative frequency (%)"
  ) +
  theme_minimal()
1
