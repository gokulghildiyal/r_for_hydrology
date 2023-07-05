# Load required packages
library(dplyr)
library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)

# Read the CSV file
data <- read_csv("disc.csv")
head(data)

# Initialize Storage column with NA values
data$Storage <- NA

# Calculate groundwater storage
storage <- 0
for (i in 1:nrow(data)) {
  storage <- data$ppt[i] - data$PET[i] - data$disc[i]
  data$Storage[i] <- storage
}
head(data)
# Write the updated data to CSV file
write_csv(data, "disc.csv")