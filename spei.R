# Load required packages
library(readr)
library(dplyr)
library(SPEI)
library(ggplot2)
library(lmomco)

# Read the CSV file
data <- read_csv("disc.csv")
head(data)

# Convert the date column to Date format
data$time <- as.Date(data$time, format = "%d-%m-%Y")  # Assuming the date column is named "date"
head(data)

CWBL <- data$ppt - data$PET
spei1 <- spei(CWBL, 1)
spei3 <- spei(CWBL, 3)
spei6 <- spei(CWBL, 6)
spei9 <- spei(CWBL, 9)
spei12 <- spei(CWBL, 12)
p1 <- plot(spei(ts(CWBL, freq = 12, start = c(2001,1)), 12, ref.start = c(2001,1),
          ref.end = c(2021,1)))
p2 <- p1 + labs(x = "Date", y = "SPEI") +
  ggtitle("Standardized Precipitation Evapotranspiration Index (SPEI) Plot") 
print(p2)
# plot for 9 month SPEI
plot(spei(ts(CWBL, freq = 12, start = c(2001,1)), 9, ref.start = c(2001,1),
          ref.end = c(2021,1)))
# plot for 6 month SPEI
plot(spei(ts(CWBL, freq = 12, start = c(2001,1)), 6, ref.start = c(2001,1),
          ref.end = c(2021,1)))
# plot for 3 month SPEI
plot(spei(ts(CWBL, freq = 12, start = c(2001,1)), 3, ref.start = c(2001,1),
          ref.end = c(2021,1)))
# plot for 1 month SPEI
plot(spei(ts(CWBL, freq = 12, start = c(2001,1)), 1, ref.start = c(2001,1),
          ref.end = c(2021,1)))
