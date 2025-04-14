#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("maps")
#install.packages("mapdata")


library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
students_data<-read.csv("C:/Users/marys/Downloads/estat_educ_uoe_enrt01_filtered_en.csv")


world_map <- map_data("world") %>%
  mutate(region = tolower(region))

students_data <- students_data %>%
  select(geo, TIME_PERIOD, OBS_VALUE)
#View(students_data)
world_map

students_data <- students_data %>%
  mutate(region = tolower(geo),
         region = case_when(
           region == "czechia" ~ "czech republic",
           region == "north macedonia" ~ "macedonia",
           region == "bosnia and herzegovina" ~ "bosnia and herzegovina",
           region == "türkiye" ~ "turkey",
           TRUE ~ region
         ))

map_joined <- left_join(world_map, students_data, by = "region")

ggplot(map_joined, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = OBS_VALUE), color = "white") +
  scale_fill_gradient(
    name = "liczba studentow",
    low = "lightyellow",
    high = "darkred",
    na.value = "gray90"
  ) +
  labs(
    title = "liczba studentów w krajach unii europejskiej"
  )+
  coord_quickmap(xlim = c(-25, 45), ylim = c(34, 72)) +
  theme_void()


