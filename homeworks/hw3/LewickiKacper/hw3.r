library(dplyr)
library(tidyr)
library(maps)
library(mapdata)
library(ggplot2)

df <- read.csv("wildfires.csv")

df <- df %>%
  mutate(
    Annual.area.burnt.per.wildfire.in.hectors = replace_na(Annual.area.burnt.per.wildfire.in.hectors, 0),
    Annual.number.of.fires = replace_na(Annual.number.of.fires, 0) 
  ) %>%
  group_by(Entity) %>%
  summarise(area_burnt = sum(Annual.number.of.fires * Annual.area.burnt.per.wildfire.in.hectors/100, na.rm = TRUE))



world_map <- map_data("world")

world_map <- world_map %>%
  left_join(df, by = c("region" = "Entity"))

ggplot(world_map, aes(x = long, y = lat, group = group, fill = area_burnt)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey50") +
  theme_minimal() +
  labs(title = "Powierzchnia (w km2) spalona w wyniku pożarów lasów w latach 2012 - 2025", fill = "Powierzchnia")
