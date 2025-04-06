
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

#Potrzebne biblioteki

co2 <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

#Ściągam dane bezpośrednio ze strony OWID

co2_2022 <- co2 %>% filter(year==2022, !is.na(co2_per_capita)) %>%  mutate(country = case_when(
  country == "United States" ~ "USA",
  country == "United Kingdom" ~ "UK",
  country == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
  country == "Czechia" ~ "Czech Republic",
  TRUE ~ country
))

#Ujednolicam ręcznie nazwy naistotniejszych krajów które różnią się między danymi z OWID a map_data

w1 <- map_data("world")
world_co2 <- left_join(w1, co2_2022, by = c("region" = "country"))

#Łączę dane

ggplot() + 
  geom_polygon(
    data = world_co2,
    aes(x = long, y = lat, fill = co2_per_capita, group = group),
    color = "gray40", size = 0.2
  ) +
  coord_quickmap() +
  scale_fill_gradient(
    low = "lightgreen",
    high = "darkred",
    name = "CO2
(tony rocznie na osobę)"
  ) +
  theme_void()+ ylim(-60, 85) + 
  labs(title= paste("Emisja CO2 per capita w roku 2022") , subtitle="Dane pochodzące ze strony OWID")+
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", color = "darkgray", size = 12, hjust = 0.5),
        legend.background = element_rect(color = NA, fill = "white"),
        legend.title = element_text(hjust = 0, color = "darkgray",face = "bold"),
        legend.position = c(0.05, 0.15),legend.justification = c(0, 0),aspect.ratio = 0.47,
        legend.title.align = 0,
        panel.border = element_rect(color = "lightgray", fill = NA, size = 1))

#tworzę wykres