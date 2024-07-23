# Load necessary libraries
library(ggplot2)

# Create the dataset
sales_data <- data.frame(
  Month = rep(month.abb[1:11], times = 3),
  Product = rep(c("Product A", "Product B", "Product C"), each = 11),
  Sales = c(500, 600, 700, 800, 750, 850, 900, 950, 1000, 1050, 1100,
            300, 320, 350, 400, 420, 450, 480, 500, 550, 580, 600,
            200, 250, 300, 350, 400, 420, 450, 470, 500, 530, 550)
)

# Plot
ggplot(sales_data, aes(x = Product, y = Sales, fill=Product)) +
  geom_violin() +
  labs(title = "Sales Performance Over a Year",
       x = "Product",
       y = "Sales") +
  theme_minimal()
