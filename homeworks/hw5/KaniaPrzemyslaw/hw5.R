library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(gridExtra)

p <- read.csv("beer-consumption-per-person.csv")
p <- p %>% 
  filter(Entity == "Poland") %>% 
  filter(Year >= 1991 & Year <= 2001) %>% 
  rename_with(~ "Liters", .cols = 4)

line_plot <- ggplot(p, aes(x = Year, y = Liters)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  scale_x_continuous(breaks = seq(1991, 2001, by = 2)) +
  labs(title = "Spożycie piwa na osobę w Polsce (1991–2001)",
       x = "Rok", y = "Litry") +
  theme_minimal()

pie_data <- p
pie_data$labels <- paste(p$Year)
pie_plot <- ggplot(pie_data, aes(x = "", y = Liters, fill = factor(Year))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Wykres kołowy spożycia piwa", fill = "Rok") +
  theme_void() +
  theme(legend.position = "right")
pie_plot

grid.arrange(line_plot, pie_plot, ncol = 2)





