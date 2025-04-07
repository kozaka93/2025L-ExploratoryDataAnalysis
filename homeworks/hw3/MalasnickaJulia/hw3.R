# Źródło danych:
# https://ec.europa.eu/eurostat/databrowser/view/AGR_R_MILKPR/default/table?lang=en

library(sf)
library(dplyr)
library(ggplot2)
library(giscoR)

df <- read.csv(gzfile("estat_agr_r_milkpr_en.csv.gz"))
df <- df %>% 
  select(geo, TIME_PERIOD, OBS_VALUE)

df$NUTS_ID <- sub(":.*", "", df$geo)

df_latest <- df %>%
  group_by(NUTS_ID) %>%
  filter(TIME_PERIOD == max(TIME_PERIOD)) %>%
  summarise(OBS_VALUE = mean(OBS_VALUE))

nuts <- gisco_get_nuts(res = "20", nuts_level = 2, year = 2021, epsg = "3035")

nuts_data <- nuts %>%
  left_join(df_latest, by = "NUTS_ID") %>%
  filter(st_coordinates(st_centroid(geometry))[,1] > 2500000,
         st_coordinates(st_centroid(geometry))[,1] < 7500000,
         st_coordinates(st_centroid(geometry))[,2] > 1300000,
         st_coordinates(st_centroid(geometry))[,2] < 5500000)

countries <- gisco_get_countries(res = "20", epsg = "3035") %>%
  st_crop(xmin = 2500000, xmax = 7500000, ymin = 1300000, ymax = 5500000)

ggplot() +
  geom_sf(data = nuts_data, aes(fill = OBS_VALUE), color = NA) +
  geom_sf(data = countries, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "C", na.value = "grey") +
  theme_minimal() +
  coord_sf(crs = st_crs(3035), expand = FALSE) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.3)) +
  labs(fill = "Cena mleka", title = "Ceny mleka w regionach Europy")
