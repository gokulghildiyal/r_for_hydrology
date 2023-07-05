# estimation of ET using r code
# Load required packages
library(tidyverse)
library(lubridate)
library(ggplot2)

# Read the CSV file
data <- read_csv("your_data.csv")  # Update with your file path and name

# Convert date column to Date format
data$date <- ymd(data$date)  # Assuming the date column is named "date"

# Calculate ET using the Penman-Monteith equation
data$ET <- with(data, {
  # Constants
  Rn <- 0.408 * (0.34 - (0.14 * sqrt(ea))) * (Rs - G)
  G <- 0.063 * (Tmean + 273.16) * (0.24 * (Tmax - Tmin))
  delta <- (4098 * (0.6108 * exp((17.27 * Tmean) / (Tmean + 237.3)))) / ((Tmean + 237.3)^2)
  gamma <- 0.665 * 10^-3 * P
  
  # Numerator
  numerator <- delta * Rn + gamma * (6.43 * (1 + 0.536 * U2) * (ea - ed))
  
  # Denominator
  denominator <- delta + gamma * (1 + 0.34 * U2)
  
  # Calculate ET
  numerator / denominator
})

# Where:
# ET: Evapotranspiration (mm/day)
# Rn: Net radiation (MJ/m^2/day)
# G: Soil heat flux density (MJ/m^2/day)
# Tmean: Mean daily temperature (°C)
# Tmax: Maximum daily temperature (°C)
# Tmin: Minimum daily temperature (°C)
# ea: Actual vapor pressure (kPa)
# ed: Saturation vapor pressure (kPa)
# U2: Wind speed at 2 meters above the ground (m/s)
# P: Atmospheric pressure (kPa)
# Rs: Solar radiation (MJ/m^2/day)
# Note: Make sure the units of the variables in your CSV file match the units used in the equation

# Print the resulting data
print(data)

# Plot the ET data
p1 <- ggplot(data, aes(x = date, y = ET)) +
  geom_line() +
  labs(x = "Date", y = "Evapotranspiration (mm/day)", title = "Evapotranspiration") +
  theme_minimal()
print(p1)




