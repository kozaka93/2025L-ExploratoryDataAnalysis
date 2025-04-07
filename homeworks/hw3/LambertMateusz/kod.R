library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(mapproj)

dane <- read.csv("world-happiness-report-2021.csv", header = TRUE, sep = ",")

dane <- dane %>% 
  mutate(Regional.indicator = ifelse(Country.name %in% c("Ukraine", "Belarus", "Moldova"), "Europe", Regional.indicator))

dane_europe <- dane %>% 
  filter(grepl("Europe", Regional.indicator)) %>% 
  mutate(Country.name = tolower(Country.name))
world_map <- map_data("world") %>% 
  mutate(region = tolower(region))

merged <- inner_join(world_map, dane_europe, by = c("region" = "Country.name"))

min_val <- min(merged$Ladder.score, na.rm = TRUE)
med_val <- median(merged$Ladder.score, na.rm = TRUE)
max_val <- max(merged$Ladder.score, na.rm = TRUE)

p <- ggplot(merged, aes(x = long, y = lat, group = group, fill = Ladder.score)) +
  geom_polygon(color = "grey50") +
  coord_map("mercator", xlim = c(-25, 60), ylim = c(34, 72)) +
  scale_fill_distiller(
    palette = "Spectral", 
    direction = -1, 
    name = "Wskaźnik szczęścia",
    limits = c(min_val, max_val),
    breaks = c(min_val, med_val, max_val),
    labels = round(c(min_val, med_val, max_val), 1)
  ) +
  labs(
    title = "Poziom szczęścia w państwach europejskich w 2021",
    caption = "Wskaźnik szczęścia Gallup World Poll opiera się na subiektywnej ocenie życia przez respondentów w skali od 0 do 10, \nuwzględniając m.in. dochód, zdrowie, wsparcie społeczne, wolność, hojność i postrzeganie korupcji."
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "roboto"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0.5)
  )
ggsave("mapka.pdf", plot = p, width = 12, height = 10, device = cairo_pdf)