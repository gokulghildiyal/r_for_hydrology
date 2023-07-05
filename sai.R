# Step 1: Install and load required packages
install.packages("zoo")
install.packages("ggplot2")
library(zoo)
library(ggplot2)

# Step 2: Read CSV file into a data frame
data <- read.csv("path/to/your/csv/file.csv")

# Step 3: Convert date column to proper date format
data$date <- as.Date(data$date)



# Step 4: Calculate mean and standard deviation
mean_val <- mean(data$value)
sd_val <- sd(data$value)

# Step 5: Calculate Standardized Anomaly Index (SAI)
data$sai <- (data$value - mean_val) / sd_val

# Step 6: Plot SAI values over time
ggplot(data, aes(x = date, y = sai)) +
  geom_line() +
  labs(x = "Date", y = "Standardized Anomaly Index") +
  ggtitle("Standardized Anomaly Index") +
  theme_minimal()





