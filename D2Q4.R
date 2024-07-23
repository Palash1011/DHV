# Load necessary libraries
library(ggplot2)
library(reshape2)

# Create the dataset
satisfaction_data <- data.frame(
  Category = c("Product Quality", "Customer Service", "Value for Money"),
  Very_Dissatisfied = c(5, 10, 8),
  Dissatisfied = c(15, 20, 12),
  Neutral = c(25, 30, 28),
  Satisfied = c(35, 25, 32),
  Very_Satisfied = c(20, 15, 20)
)

# Melt the dataset
satisfaction_data_melted <- melt(satisfaction_data, id.vars = "Category")

# Plot
ggplot(satisfaction_data_melted, aes(fill = variable, y = value, x = Category)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Customer Satisfaction Ratings",
       x = "Category",
       y = "Count") +
  theme_minimal()
