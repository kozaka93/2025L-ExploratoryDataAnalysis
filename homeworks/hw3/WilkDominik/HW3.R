library(ggplot2)
library(dplyr)
library(eurostat)
library(sf)
library(rnaturalearth)
library(patchwork)

setwd("D:/Studia/4 semest/Eksploracja/HM3")
df <- read.csv("2019.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")

europe <- world[world$continent == "Europe", ]

europe <- europe %>% 
  mutate(sovereignt = recode(sovereignt, "Republic of Serbia" = "Serbia",
                             "Czechia" = "Czech Republic"))

europe_hapiness <- europe %>% 
  left_join(df, by = c("sovereignt" = "Country.or.region"))

map_1 <- ggplot(data = europe_hapiness) +
  geom_sf(aes(fill = Score), color = "black", size = 0.5) +
  scale_x_continuous(limits = c(-10, 45)) +
  scale_y_continuous(limits = c(35, 70)) +
  theme_void() +
  scale_fill_fermenter(palette = 8, direction = 1) +
  labs(title = "Poziom szczęścia z życia populacji danego kraju w skali 0-10") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.key.size = unit(0.7, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 8)
  ) 
  

map_2 <- ggplot(data = europe_hapiness) +
  geom_sf(aes(fill = GDP.per.capita), color = "black", size = 0.5) +
  scale_x_continuous(limits = c(-10, 45)) +
  scale_y_continuous(limits = c(35, 70)) +
  theme_void() +
  scale_fill_fermenter(palette = 10, direction = 1) +
  labs(title = paste(strwrap("Udział PKB per capita w ocenie szczęścia", 
                             width = 30), collapse = "\n")) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8, angle = 45),
    legend.title = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 7)
  ) 

map_3 <-ggplot(data = europe_hapiness) +
  geom_sf(aes(fill = Social.support), color = "black", size = 0.5) +
  scale_x_continuous(limits = c(-10, 45)) +
  scale_y_continuous(limits = c(35, 70)) +
  theme_void() +
  scale_fill_fermenter(palette = 7, direction = 1, 
                       n.breaks = 7) +
  labs(title = paste(strwrap("Udział poziomu wsparcia socjalnego w ocenie szczęścia", 
                             width = 30), collapse = "\n")) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8, angle = 60),
    legend.title = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 7)
  ) 

map_4 <- ggplot(data = europe_hapiness) +
  geom_sf(aes(fill = Healthy.life.expectancy), color = "black", size = 0.5) +
  scale_x_continuous(limits = c(-10, 45)) +
  scale_y_continuous(limits = c(35, 70)) +
  theme_void() +
  scale_fill_fermenter(palette = 15, direction = 1) +
  labs(title = paste(strwrap("Udział oczekiwanej długość życia w zdrowiu w ocenie szczęścia", 
                             width = 30), collapse = "\n")) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8, angle = 45),
    legend.title = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 7)
  ) 


map_5 <- ggplot(data = europe_hapiness) +
  geom_sf(aes(fill = Freedom.to.make.life.choices), color = "black", size = 0.5) +
  scale_x_continuous(limits = c(-10, 45)) +
  scale_y_continuous(limits = c(35, 70)) +
  theme_void() +
  scale_fill_fermenter(palette = 3, direction = 1) +
  labs(title = paste(strwrap("Udział wolności w podejmowaniu decyzji w ocenie szczęścia", 
                             width = 30), collapse = "\n")) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8, angle = 45),
    legend.title = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 7)
  ) 


map_final <- (map_1 | ( (map_2/map_3) | (map_4 / map_5))) + 
  plot_annotation(title = "Szczęście z życia obywateli w krajach Europy", 
                  caption = "Dane pochodza z raportu z 2019 roku",
                  theme = theme(plot.title = element_text(size = 18, hjust = 0.5)))


