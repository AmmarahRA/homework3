pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
final.data <- readRDS("data/output/TaxBurden_Data.rds")

#Summarise the Data

#1

tab_1 <- final.data %>% group_by(state) %>%
  mutate(pct_change = (tax_state-lag(tax_state))/lag(tax_state))

fig_1<- tab_1 %>% filter(Year <= 1985) %>% group_by(state) %>% 
  ggplot(aes(x = Year, y = pct_change)) + 
  stat_summary(fun.y = "mean", geom="bar") +
  labs(title = "Proportion of States with Change in Tax, 1970-1985", x = "Year", y = "Change in Tax")

#2

fig_2 <- final.data %>% filter(Year<= 2018) %>%
  summarise(avg_price = mean(final.data$cost_per_pack)) %>%
  summarise(avg_tax = mean(final.data$tax_dollar)) %>%
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = avg_price), colour = "red") + 
  geom_line(aes(y = avg_tax), colour = 'blue') +
  labs(title = "Average Price and Tax from 1970-2018", x = "Year", y = "Average Price and Tax")

#3










