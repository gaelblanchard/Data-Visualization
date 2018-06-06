#Gael Blanchard
library(e1071)
library(tidyr)
library(dplyr)
library(devtools)
library(ggplot2)
library(ggcorrplot)
library(gganimate)
library(gapminder)
setwd("/Users/ak/Desktop/Visualization_Portfolio")

# 2. Honey Production in the USA
honey_data <- data.frame(read.csv("honeyproduction.csv", header = TRUE, sep = ","))
summary(honey_data)
states_honey <- honey_data$state
summary(states_honey)
#Number of colonies, Yield Per Colonies, Total Prod
#Stocks and ProdValue per year (Mean and Totals)
summary_honey_per_year <- honey_data %>% group_by(year) %>% summarize(
                        numcol = mean(numcol),
                        yieldpercol = mean(yieldpercol),
                        totalprod = mean(totalprod),
                        stocks = mean(stocks),
                        prodvalue = mean(prodvalue),
                        total_numcol = sum(numcol),
                        total_yieldpercol = sum(yieldpercol),
                        total_totalprod = sum(totalprod),
                        total_stocks = sum(stocks),
                        total_prodvalue = sum(prodvalue)
                        )
#Graph avg(numcol,yieldpercol,totalprod,stocks,prodvalue) per year
average_honey_values_over_time <- summary_honey_per_year %>%
  select(year, totalprod, stocks, prodvalue) %>%
  gather(key = "variable", value = "value", -year)

ggplot(average_honey_values_over_time, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()

average_honey_yield_over_time <- summary_honey_per_year %>%
  select(year, yieldpercol) %>%
  gather(key = "variable", value = "value", -year)

ggplot(average_honey_yield_over_time, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()

average_honey_colonies_over_time <- summary_honey_per_year %>%
  select(year, numcol) %>%
  gather(key = "variable", value = "value", -year)

ggplot(average_honey_colonies_over_time, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()

#Graph their totals by year
total_honey_values_over_time <- summary_honey_per_year %>%
  select(year, total_totalprod, total_stocks, total_prodvalue) %>%
  gather(key = "variable", value = "value", -year)

ggplot(total_honey_values_over_time, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()

total_honey_yield_over_time <- summary_honey_per_year %>%
  select(year, total_yieldpercol) %>%
  gather(key = "variable", value = "value", -year)

ggplot(total_honey_yield_over_time, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()

total_honey_colonies_over_time <- summary_honey_per_year %>%
  select(year, total_numcol) %>%
  gather(key = "variable", value = "value", -year)

ggplot(total_honey_colonies_over_time, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()

#Top 3 avg prodvalue states
mean_honey_prodvalue_all_time <- honey_data %>% group_by(state) %>% summarize(
                        prodvalue = mean(prodvalue)
                        )
mean_honey_prodvalue_all_time <- mean_honey_prodvalue_all_time[order(mean_honey_prodvalue_all_time$prodvalue),]
top_prod_value_states_data <- tail(mean_honey_prodvalue_all_time,3)
total_prod_value_states <- top_prod_value_states_data$state
states_honey_per_year <- honey_data[honey_data$state %in% total_prod_value_states,]
states_honey_per_year <- states_honey_per_year %>% group_by(state,year) %>% summarize(
                        numcol = mean(numcol),
                        yieldpercol = mean(yieldpercol),
                        totalprod = mean(totalprod),
                        stocks = mean(stocks),
                        prodvalue = mean(prodvalue)
                        )
  #animated bubble chart
  #ani_bubble_chart <- ggplot(states_honey_per_year, aes(prodvalue, totalprod, size = stocks, frame = year)) +
   # geom_point() +
    #geom_smooth(aes(group = year), 
     #           method = "lm", 
      #          show.legend = FALSE) +
    #facet_wrap(~state, scales = "free") +
    #scale_x_log10()
#gganimate(ani_bubble_chart)
states_2000 <- states_honey_per_year[states_honey_per_year$year == 2000,]
honey_top_2000 <- states_2000 %>%
  select(state, totalprod , stocks, prodvalue) %>%
  gather(key = "variable", value = "value", -state)

ggplot(honey_top_2000, aes(x = state, y = value, fill=variable), xlab="State", ylab="Value") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#For a specified state
alaska_honey_data <- honey_data[honey_data$state == "AL",]
alaska_honey_per_year <- honey_data %>% group_by(year) %>% summarize(
                        total_numcol = numcol,
                        total_yieldpercol = yieldpercol,
                        total_totalprod = totalprod,
                        total_stocks = stocks,
                        total_prodvalue = prodvalue
                        )

#Graph Alaska totals by year
total_honey_values_over_time <- alaska_honey_per_year %>%
  select(year, total_totalprod, total_stocks, total_prodvalue) %>%
  gather(key = "variable", value = "value", -year)

ggplot(total_honey_values_over_time, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()

total_honey_yield_over_time <- alaska_honey_per_year %>%
  select(year, total_yieldpercol) %>%
  gather(key = "variable", value = "value", -year)

ggplot(total_honey_yield_over_time, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()

total_honey_colonies_over_time <- alaska_honey_per_year %>%
  select(year, total_numcol) %>%
  gather(key = "variable", value = "value", -year)

ggplot(total_honey_colonies_over_time, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()
#NEXT: 3.Grammar Product Reviews