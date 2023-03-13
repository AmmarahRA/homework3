install.packages("fixest")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest)
final.data <- readRDS("data/output/TaxBurden_Data.rds")

#Summarise the Data

#1

final.data<- final.data %>% group_by(state) %>% arrange(state, Year) %>%
  mutate(tax_change = tax_state - lag(tax_state),
         tax_change_d = ifelse(tax_change ==0,0,1),
         price_cpi = cost_per_pack*(229.5939/index) ,
         tax_cpi = tax_dollar*(229.5939/index)) 

fig_1<- final.data %>% group_by(Year) %>% filter(Year <=1985) %>%
  summarise(mean_change=mean(tax_change_d)) %>% 
  ggplot(aes(x = as.factor(Year), y=mean_change)) + 
  geom_bar(stat = 'identity', colour="black", fill="red") + 
  labs(title = "Proportion of States with Change in Tax, 1970-1985", x = "Year", y = "Proportion of States") +
  ylim(0,1) +
  theme_bw()

fig_1

#2

tab_2 <- final.data %>% filter(Year <= 2018) %>% 
  group_by(Year)%>%
  summarise(avg_price = mean(price_cpi), avg_tax = mean(tax_cpi))

fig_2 <- ggplot(tab_2, aes(x = Year)) + 
  geom_line(aes(Year, avg_price), colour = "red") + 
  geom_line(aes(Year, avg_tax), colour = 'blue') +
  labs(title = "Average Price and Tax from 1970-2018", x = "Year", y = "Average Price and Tax") +
  geom_text(data = tab_2 %>% filter(Year == 2012),
            aes())
  theme_bw()

fig_2

#3

cig_data_diff <- final.data %>%
  group_by(state) %>%
  summarise(price_diff = price_cpi[Year == mean(2018)] - price_cpi[Year == mean(1970)]) %>%
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

#5
#grpah avg sales for both groups in one graph 
#fig_5 <- 

#6

data_6 <- final.data %>% filter(Year %in% c(1970:1990))

data_6$log_sales <- log(data_6$sales_per_capita)
data_6$log_price <- log(data_6$price_cpi)

reg6<- lm(formula = log_price ~ log_sales, data = data_6)
reg6

#7
iv7 <- feols(log_sales ~ 1 | log_price ~ tax_cpi, data = data_6)
iv7

#8

#first stage is lnprice on total tax
# reducded form is ln sales on total tax







