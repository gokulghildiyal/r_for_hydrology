# Load the required library
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggthemes)
library(lubridate)
library(rlang)
library(ggpmisc)
library(gtable)
library(ggpubr)
library(gridExtra)

# Read the CSV file
data <- read.csv("isot1.csv")
head(data)

# Remove rows with null values
data <- na.omit(data)
head(data)

# Format the date column
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Create the scatter plot
pp1 <- ggplot(data, aes(x = O18, y = H2)) +
  geom_point(aes(col = Site)) +
  labs(x = "Date", y = "Value") +
  ggtitle("Scatter Plot") + theme_classic() + geom_smooth(formula = y~x+0, method = 'lm', se = FALSE) + 
  labs(x = 'δ18O', y = 'δH') + 
  stat_cor(method = "pearson", label.x = -20,label.y = 20, size = 8) +
  stat_regline_equation(label.x = -20,label.y = 0, size = 8) +
  theme(legend.text=element_text(size=8),
        legend.title = element_text(size=8, hjust = 0.5),
        legend.key.height = unit(0.5,"cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.position = c(0.8, 0.4)) +
  guides(fill=guide_legend(title="Discipline"))
  
print(pp1)

# facet plot
pp1 <- ggplot(data, aes(x = O18, y = H2), col = Site) +
  geom_point() +
  labs(x = "Date", y = "Value") +
  ggtitle("Scatter Plot") + theme_classic() + geom_smooth(formula = y~x+0, method = 'lm', se = FALSE) + labs(x = 'δ18O', y = 'δH') + 
  stat_cor(method = "pearson") + facet_wrap(~Site) + stat_regline_equation(label.x = -20,label.y = -15, size = 4)

print(pp1)

pp1 <- ggplot(data, aes(x=date, y = O18)) + geom_line() + theme_bw() + labs(x = 'Year', y = 'δ18O', se = FALSE) + facet_wrap(~Site)
print(pp1)

pp2 <- ggplot(data, aes(x=Site, y = O18)) + stat_boxplot(geom = "errorbar")+ geom_boxplot() + labs(x = 'δ18O', y = 'Site') + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(pp2)

# Create the plot
plot <- ggplot(data, aes(x = x_column)) +
  geom_line(aes(y = y1_column, color = "Y1"), size = 1) +
  geom_line(aes(y = y2_column * scaling_factor, color = "Y2"), size = 1) +
  scale_color_manual(values = c("Y1" = "red", "Y2" = "blue")) +
  labs(x = "X Axis", y = "Y Axis", color = "Legend") +
  theme_minimal() +
  theme(legend.position = "top")

# Display the plot
print(plot)