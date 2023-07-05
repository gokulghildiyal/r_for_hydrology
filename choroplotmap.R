# Required packages
library(rgdal)
library(ggplot2)
library(sf)
library(rvest)
library(dplyr)
library(viridis)
library(ggrepel)
library(ggthemes)
library(forcats)

# Read the shapefile
shape <- st_read("Admin__2.shp")

# Read the CSV file
data <- read.csv("iso_avg.csv")
head(data)
str(data)

# Remove rows with null values
data <- na.omit(data)
head(data)
str(data)



# Step 3: Prepare the data for plotting
merged_data <- merge(shape, data, by.x = "ST_NM", by.y = "ST_NM", all.x = TRUE)
sf_data <- st_as_sf(merged_data)

# Step 4: Filter out NA values
sf_data_filtered <- sf_data %>% filter(!is.na(H2avg))

# Step 6: Plot the choropleth map with monthly faceting
map <- ggplot() +
  geom_sf(data = sf_data_filtered, aes(fill = H2avg)) +
  scale_fill_viridis(name = "H2avg") +
  theme_void() +
  facet_wrap(~ month, nrow = 3)

print(map)