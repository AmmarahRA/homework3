pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
final.data <- readRDS("data/output/TaxBurden_Data.rds")

#1

final.data %>% filter()

final.data %>% ggplot(aes(x = year, y = )) + 
  stat_summary(fun.y = "mean", geom="bar") +
  labs(title = "", x = "Year", y = "")