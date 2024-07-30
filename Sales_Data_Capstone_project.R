# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(readxl)
# Load the data
data <- read_excel("C:/Users/palas/OneDrive/Documents/Desktop/capstone_data.xlsx")
print(head(data))
# Check for missing values and handle them
print(sum(is.na(data)))
# Convert Month to a factor with ordered levels
data$Month <- factor(data$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), ordered = TRUE)
# Summarize data by Region and Month
monthly_sales <- data %>%
  group_by(Region, Month) %>%
  summarize(TotalSales = sum(TotalSales), NumberOfCustomers = sum(NumberOfCustomers), AvgTransactionValue = mean(AverageTransactionValue))
print(head(monthly_sales))
# Plot total sales by region
ggplot(data, aes(x = Region, y = TotalSales, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Region", x = "Region", y = "Total Sales")
# Plot total sales by month
ggplot(data, aes(x = Month, y = TotalSales, group = Region, color = Region)) +
  geom_line() +
  labs(title = "Total Sales by Month", x = "Month", y = "Total Sales")
# Histogram of Total Sales
ggplot(data, aes(x = TotalSales)) +
  geom_histogram(binwidth = 2000, fill = "blue", color = "black") +
  labs(title = "Distribution of Total Sales", x = "Total Sales", y = "Frequency")
# Plot number of customers by month
ggplot(data, aes(x = Month, y = NumberOfCustomers, group = Region, color = Region)) +
  geom_line() +
  labs(title = "Number of Customers by Month", x = "Month", y = "Number of Customers")
avg_trans_value_stats <- data %>%
  summarize(Mean = mean(AverageTransactionValue), Median = median(AverageTransactionValue), SD = sd(AverageTransactionValue))

print(avg_trans_value_stats)
# Trendline of sales over time
ggplot(data, aes(x = as.numeric(Month), y = TotalSales, group = Region, color = Region)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Trendline of Sales Over Time", x = "Month", y = "Total Sales")
# Seasonal sales pattern
ggplot(data, aes(x = Month, y = TotalSales, group = Region, color = Region)) +
  geom_line() +
  labs(title = "Seasonal Sales Pattern by Region", x = "Month", y = "Total Sales")
# Scatter plot of region-based sales over the number of customers
ggplot(data, aes(x = NumberOfCustomers, y = TotalSales, color = Region)) +
  geom_point() +
  labs(title = "Region-Based Sales Over Number of Customers", x = "Number of Customers", y = "Total Sales")
# Plot 1: Average Transaction Value by Region
ggplot(data, aes(x = Region, y = Average_Transaction_Value, fill = Region)) +
  geom_boxplot() +
  labs(title = "Average Transaction Value by Region", x = "Region", y = "Average Transaction Value")

# Plot 2: Total Sales by Store ID
ggplot(data, aes(x = Store_ID, y = Total_Sales, fill = as.factor(Store_ID))) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Store ID", x = "Store ID", y = "Total Sales")

# Plot 3: Number of Customers by Store ID
ggplot(data, aes(x = Store_ID, y = Number_Of_Customers, fill = as.factor(Store_ID))) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Customers by Store ID", x = "Store ID", y = "Number of Customers")

# Plot 4: Scatter plot of Total Sales vs Number of Customers
ggplot(data, aes(x = Number_Of_Customers, y = Total_Sales, color = Region)) +
  geom_point() +
  labs(title = "Total Sales vs Number of Customers", x = "Number of Customers", y = "Total Sales")