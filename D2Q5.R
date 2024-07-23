# Load library (if not already installed, use install.packages("ggplot2"))
library(ggplot2)
# Create the data frame (replace with your actual data)
data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Jan", "Feb", "Mar", "Jan", "Feb", "Mar"),
  Employee = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
  Score = c(85, 88, 90, 90, 92, 94, 78, 80, 82)
)
# Create the histogram
ggplot(data, aes(x = Score)) +
  geom_histogram(bins = 5) +  # Adjust number of bins (default is 30)
  labs(title = "Distribution of Monthly Performance Scores", x = "Score", y = "Frequency") +
  theme_classic()