library(ggplot2)
library(dplyr)
library(ggthemes)
library(reshape2)
library(ggthemes)
library(lubridate)
library(rlang)
library(ggpmisc)
library(gtable)
library(ggpubr)
library(gridExtra)
ft <- read.csv('Nashik_set.csv')
head(ft)
luke <- ggplot(ft, aes(x = oxygen, y = due)) + geom_point(aes(shape=Type), size = 2) + theme_bw() + geom_smooth(method = 'lm', se = FALSE) + labs(x = 'δ18O', y = 'δH') + stat_cor(method = "pearson") 
print(luke)
temp_model <- lm(oxygen ~ due, ft)
print(summary(temp_model))
luke1 <- ggplot(ft, aes(x = oxygen, y = d_excess)) + geom_point(aes(shape=Type), size = 2) + theme_bw() + geom_smooth(method = 'lm', se = FALSE) + labs(x = 'δ18O', y = 'd-excess')
print(luke1)
luke2 <- ggplot(ft, aes(x = oxygen, y = depth)) + geom_point(alpha = 4, aes(shape=Type), size = 2) + theme_bw() + labs(x = 'δ18O', y = 'Depth (ft)') + scale_y_reverse() +
  theme(legend.text=element_text(size=8),
        legend.title = element_text(size=8, hjust = 0.5),
        legend.key.height = unit(0.5,"cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.position = c(0.1, 0.2)) +
  guides(fill=guide_legend(title="Discipline"))
print(luke2)
luke3 <- ggplot(ft, aes(x = oxygen, y = depth)) + geom_point(aes(shape=Type), size = 2) + theme_bw() + labs(x = 'δ18O', y = 'Depth (ft)') + scale_y_reverse() + facet_wrap(~Type) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
print(luke3)
