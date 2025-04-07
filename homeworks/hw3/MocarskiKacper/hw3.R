install.packages(c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata", "viridis"))

library(tidyverse)
library(sf)
library(rnaturalearth)
library(viridis)

world <- ne_countries(scale = "medium", returnclass = "sf")

emissions <- read_csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

co2_latest <- emissions %>%
  filter(year == max(year), !is.na(iso_code), !str_detect(iso_code, "OWID")) %>%
  select(iso_code, country, co2_per_capita)

map_data <- world %>%
  left_join(co2_latest, by = c("iso_a3" = "iso_code"))


ggplot(map_data) +
  geom_sf(aes(fill = co2_per_capita), color = "gray40", size = 0.1) +
  scale_fill_viridis(
    option = "plasma",  
    na.value = "lightgray",
    name = "CO2 per capita\n(tony)",
    direction = -1      
  ) +
  theme_minimal() +
  labs(
    title = "Emisja dwutlenku węgla na osobę na świecie",
    subtitle = "Dane: Our World in Data (ostatni dostępny rok)",
    caption = "Źródło: https://ourworldindata.org/co2-emissions"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )


ggsave("mapa_emisji_CO2.png", width = 10, height = 6)


