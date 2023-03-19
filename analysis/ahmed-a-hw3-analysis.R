install.packages("fixest")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest, modelsummary)
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
  annotate("text", x = 2016, y = 7.2, label = "Average Price", colour = "black", size = 3) +
  annotate("text", x = 2016, y = 3.2, label = "Average Tax", colour = "black", size = 3) +
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

tab_3_2 <- tab_3 %>% group_by(Year) %>% summarise_at(vars(avg_sales), list(avg_sale = mean))
tab_4_2 <- tab_4 %>% group_by(Year) %>% summarise_at(vars(avg_sales), list(avg_sale = mean))

fig_5 <- ggplot() + 
  geom_line(data = tab_3_2, aes(x = Year, y = avg_sale), colour = "blue") +
  geom_line(data = tab_4_2, aes(x = Year, y = avg_sale), colour = "red") +
  labs(title = "Average Sales in States with Highest and Lowest Tax Increase", x = "Year", y = "Average Sales per Capita") +
  annotate("text", x = 2016, y = 10.2, label = "Top 5 States", colour = "black", size = 3) +
  annotate("text", x = 2016, y = 50.2, label = "Bottom 5 States", colour = "black", size = 3) +
  theme_bw()

fig_5

#6

data_6 <- final.data %>% filter(Year %in% c(1970:1990))

data_6$log_sales <- log(data_6$sales_per_capita)
data_6$log_price <- log(data_6$price_cpi)

reg6<- lm(formula = log_sales ~ log_price, data = data_6)
summary(reg6)

#7
iv7 <- feols(log_sales ~ 1 | log_price ~ tax_cpi, data = data_6)
summary(iv7)

#8

#first stage 

step_one<- lm(log_price ~ tax_cpi, data = data_6)
summary(step_one)
  
#reduced form

step_two<- lm(log_sales ~ tax_cpi, data = data_6)
summary(step_two)

#9 

data_9 <- final.data %>% filter(Year %in% c(1991:2015))

data_9$log_sales <- log(data_9$sales_per_capita)
data_9$log_price <- log(data_9$price_cpi)

reg9 <- lm(formula = log_sales ~ log_price, data = data_9)
summary(reg9)

iv9 <- feols(log_sales ~ 1 | log_price ~ tax_cpi, data = data_9)
summary(iv9)

#first stage 

step_one_9<- lm(log_price ~ tax_cpi, data = data_9)
summary(step_one_9)

#reduced form

step_two_9<- lm(log_sales ~ tax_cpi, data = data_9)
summary(step_two_9)

modelsummary(list("OLS" = reg6, "IV" = iv7, "OLS" = reg9, "IV" = iv9),
             title = "Elasticity Estimates",
             coef_map = c('log_price'="Log Price",
                          'fit_log_price' = "Log Price"),
             gof_map = list(list("raw" = "nobs", "clean"="N", "fmt" = 0),
                            list("raw"="r.squared", "clean"="R<sup>2</sup>", "fmt" =2)) %>% 
  add_header_above(c(" " =1, "1970 - 1990" = 2, "1991 - 2015" = 2)))

save.image("Hw3_workspace.Rdata")


