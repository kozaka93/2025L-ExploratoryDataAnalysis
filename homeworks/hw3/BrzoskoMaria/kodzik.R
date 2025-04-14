install.packages("readxl", repos = "https://cloud.r-project.org/")
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("maps")
install.packages("mapdata")
install.packages("mapproj")
library(dplyr)
library(readxl)
library(ggplot2)
library(maps)
library(mapdata)
library(mapproj)

e_coli <- read_excel("e-coli.xlsx")

e_coli_0 <- e_coli %>% 
  select(country = 1,
         r  = 12) %>% 
  filter(!country %in% c("Country", "EU/EEA (population-\r\nweighted mean)", ". : No data", NA)) %>% 
  mutate(r = as.numeric(r))

world <- map_data("world")

map_europe <- world %>%
  filter(long > -25, long < 45, lat > 32, lat < 72) %>% 
  filter(!region %in% c("Greenland")) %>% 
  left_join(e_coli_0, by = c("region" = "country"))

map_europe <- map_europe %>%
  mutate(r_0 = cut(r,
                     breaks = c(30, 40, 50, 60, 70, 80),
                     labels = c("30-40", "40-50", "50-60", "60-70", "70-80"))) %>%
  mutate(r_0 = addNA(r_0))

levels(map_europe$r_0)[is.na(levels(map_europe$r_0))] <- "No data"

ggplot() +
  geom_polygon(data = map_europe, aes(x = long, y = lat, group = group, fill = r_0), color = "white", linewidth = 0.3) +
  scale_fill_manual(
    name = "Oporność [%]",
    values = c(
      "30-40"   = "#ffccd5",
      "40-50"   = "#ff8fa3",
      "50-60"   = "#ff4d6d",
      "60-70"   = "#a4133c",
      "70-80"   = "#590d22",
      "No data" = "#dadada")) +
  coord_map("mercator") +
  theme_void() +
  labs(title = "Oporność pałeczki okrężnicy na aminopenicyliny",
       subtitle = "Mapa przedstawia oporność bakterii pałeczki okrężnicy\n(procent wszystkich testów, w których bakteria okazała się oporna)\nna antybiotyki z grupy aminopenicylin w różnych krajach Europy") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10))
