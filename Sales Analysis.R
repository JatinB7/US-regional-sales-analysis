#load libraries
library(readxl)
library(tidyverse)  # For data manipulation and visualization (includes ggplot2, dplyr, tidyr)
library(lubridate)  # For handling dates
library(forecast)   # For forecasting
library(cluster) 

#read data
sales_data <- read_excel("C:/Users/sujan/OneDrive/Desktop/capstone R project/US Regional Sales.xlsx")
View(sales_data)

#Sales Performance Analysis

#Data preparation
sales_data$OrderDate <- as.Date(sales_data$OrderDate, format = "%A, %B %d, %Y")
summary(sales_data)

# Exploratory data analysis
sales_summary <- sales_data %>%
  group_by(Region, ProductName) %>%
  summarise(TotalSales = sum(UnitPrice * OrderQuantity), na.rm = TRUE)

#Visualization for sales trend
ggplot(sales_summary, aes(x = ProductName, y = TotalSales, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sales Performance by Region and Product")


#Feature Engineering
customer_features <- sales_data %>%
  group_by(CustomerID) %>%
  summarise(
    TotalSales = sum(UnitPrice * OrderQuantity, na.rm = TRUE),
    AvgSales = mean(UnitPrice * OrderQuantity, na.rm = TRUE),
    OrderCount = n()
  )
#Clustering
set.seed(123)
kmeans_result <- kmeans(customer_features[, -1], centers = 5)
customer_features$Cluster <- kmeans_result$cluster

#visual
pairs(customer_features[, -1], col = customer_features$Cluster)

#Predictive Sales Forecasting
sales_ts <- sales_data %>%
  mutate(Month = floor_date(OrderDate, unit = "month")) %>%
  group_by(Month) %>%
  summarise(MonthlySales = sum(UnitPrice * OrderQuantity, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(Month = seq(min(Month), max(Month), by = "month"), fill = list(MonthlySales = 0))

# Converting to ts object for forecasting
sales_time_series <- ts(sales_ts$MonthlySales, frequency = 12)  # assuming monthly data with yearly seasonality

# Forecasting using Exponential Smoothing
fit <- ets(sales_time_series)
future_forecast <- forecast(fit, h = 12)  # forecast for the next 12 months

# Plotting the forecast
autoplot(future_forecast) +
  labs(title = "12-Month Sales Forecast", x = "Month", y = "Forecasted Sales", subtitle = "Using ETS Model") +
  theme_minimal()


# Checking the accuracy of the model on historical data
accuracy(fit)



