library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(tidyr)
library(countrycode)

data = read.csv("share-of-population-urban.csv", header = TRUE)
names(data)[4] <- "urban"

data2 = data %>% mutate(continent = countrycode(Entity, origin = "country.name", destination = "continent")) %>% 
  filter(Year %in% c(1990,2023) & continent == 'Asia') %>% 
  pivot_wider(names_from = Year, values_from = urban) %>%
  mutate(delta = `2023` - `1990`)


world_map = map_data("world")
map = world_map %>% filter(region %in% data2$Entity)
map_data = map %>% left_join(data2, by=c('region' = 'Entity'))

ca_base = ggplot(data = map_data, mapping = aes(x = long, y = lat, group = group)) + 
  coord_map("mollweide") +
  geom_polygon(color = "black", fill = "gray")

ca_base + 
  geom_polygon(data = map_data, aes(fill = delta)) +
  geom_polygon(color = "black", fill = NA) +
  theme_void() +
  labs(
    title = "Zmiana udziału ludności miejskiej dla Azji między 1990 a 2023 rokiem",
    fill = "Zmiana [p.p.]"
  ) +
  scale_fill_gradient(
    low = "yellow",
    high = 'darkred',
    name = "Zmiana [p.p.]"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.title.position = "plot"
  )



