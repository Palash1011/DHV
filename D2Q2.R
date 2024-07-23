# Load necessary libraries
library(ggplot2)
# Create the dataset
temperature_data <- data.frame(
  Week = rep(as.Date(c("2023-01-01", "2023-01-08", "2023-01-15")), each = 3),
  City = rep(c("New York", "Los Angeles", "Chicago"), times = 3),
  Temperature = c(-2, 15, -5, 0, 17, -3, 2, 18, -2)
)
# Plot
ggplot(temperature_data, aes(x = Week, y = Temperature, color = City)) +
  geom_line() +
  geom_point() +
  labs(title = "Weekly Average Temperature",
       x = "Week",
       y = "Average Temperature (°C)") +
  theme_minimal()