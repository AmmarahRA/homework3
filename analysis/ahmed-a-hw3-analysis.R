pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
final.data <- readRDS("data/output/TaxBurden_Data.rds")

#Summarise the Data

#1
states <- unique(final.data$state) 

states_list <- list()
for (i in states) {
  state_data <- filter(final.data, state == i)
  state_data2 <- state_data %>% mutate(tax_change = c(0,ifelse(diff(tax_state) != 0, 1, 0)))
  states_list[[i]] <- state_data2
}

final.data2<- do.call(rbind, states_list)

tab_1 <- final.data2%>% filter(Year <= 1985) %>% 
  group_by(Year)%>% 
  summarize(prop = sum(tax_change == 1)/n())

fig_1<- ggplot(tab_1, aes(Year, prop)) + 
  geom_bar(stat = 'identity', colour="black") + 
  labs(title = "Proportion of States with Change in Tax, 1970-1985", x = "Year", y = "Proportion of States") +
  theme_bw()

fig_1

#2

tab_2 <- final.data %>% filter(Year <= 2018) %>% 
  group_by(Year)%>%
  summarise(avg_price = mean(cost_per_pack), avg_tax = mean(tax_dollar))

fig_2 <- ggplot(tab_2, aes(x = Year)) + 
  geom_line(aes(Year, avg_price), colour = "red") + 
  geom_line(aes(Year, avg_tax), colour = 'blue') +
  labs(title = "Average Price and Tax from 1970-2018", x = "Year", y = "Average Price and Tax") +
  theme_bw()

fig_2

#3

cig_data_diff <- final.data %>%
  group_by(state) %>%
  summarise(price_diff = cost_per_pack[Year == mean(2018)] - cost_per_pack[Year == mean(1970)]) %>%
  arrange(desc(price_diff))

# Select the top 5 states with the highest increase in cigarette prices
top_5 <- cig_data_diff %>%
  slice(1:5)

tab_3<- final.data %>% filter (state %in% top_5$state & Year %in% c(1970:2018))%>% 
  group_by(Year, state)%>% 
  summarize(avg_sales = mean(sales_per_capita))

fig_3 <- ggplot(tab_3, aes(Year, avg_sales))+
  geom_line(aes(color = state))+
  labs(title = "Average Sales per Capita", x = "Year",y = "Average Sales per Capita")+
  theme_bw()

fig_3

#4

bottom_5 <- cig_data_diff %>%
  slice_min(price_diff, n = 5, with_ties = FALSE)

tab_4<- final.data %>% filter (state %in% bottom_5$state & Year %in% c(1970:2018))%>% 
  group_by(Year, state)%>% 
  summarize(avg_sales = mean(sales_per_capita))

fig_4 <- ggplot(tab_4, aes(Year, avg_sales))+
  geom_line(aes(color = state))+
  labs(title = "Average Sales per Capita", x = "Year",y = "Average Sales per Capita")+
  theme_bw()

fig_4

#6

