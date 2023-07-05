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
pulp <- read.csv('time2.csv')
head(pulp)

pulp$new_date <- mdy(pulp$date)
head(pulp)
str(pulp)

pp1 <- ggplot(pulp, aes(x=new_date, y = temprature)) + geom_line() + theme_bw() + labs(x = 'Year', y = 'Temprature (C)') 

pp2 <- ggplot(pulp, aes(x=new_date, y = rela_hum)) + geom_line() + theme_bw() + labs(x = 'Year', y = 'Relative Humidity (%)')

pp3 <- ggplot(pulp, aes(x=new_date, y = precipitation)) + geom_line() + theme_bw() + labs(x = 'Year', y = 'Precipitation (mm)') 

pp4 <- ggplot(pulp, aes(x=new_date, y = wind_speed)) + geom_line() + theme_bw() + labs(x = 'Year', y = 'Wind Speed (m/s)') 

grid.arrange(pp1,pp2,pp3,pp4, ncol(0), nrow(4))
npp <- melt(pulp, measure.vars = c("temprature", "rela_hum","wind_speed", "precipitation", "Actual_ET"))
luke1 <- ggplot(npp, aes(new_date, value), geom_abline()) + geom_point() + facet_grid(variable~., scales = "free_y") + geom_line() + 
  geom_smooth(method = 'lm', se = FALSE, formula = y~x, color="black")  
print(luke1)  
  ggplot(pulp, aes(x = rela_hum, y = temprature)) + geom_point() + theme_bw() + geom_smooth(method = 'lm', se = FALSE) + labs(x = 'Precipitation (mm)', y = 'Temprature (C)')
print(luke1)
