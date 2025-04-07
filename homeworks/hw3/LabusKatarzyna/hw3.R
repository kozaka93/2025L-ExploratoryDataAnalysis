library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(countrycode)
library(gganimate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(glue)
library(plotly)
library(viridis)

data2 <- read.csv("FAOSTAT_data_en_4-7-2025.csv")

w1 <- map_data("world")

europe <- subset(w1, region %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                                      "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                                      "Croatia", "Cyprus", "Czech Republic","Denmark","Estonia","Finland", 
                                      "France","Georgia", "Germany", "Greece","Hungary","Iceland", 
                                      "Ireland", "Italy","Kazakhstan", "Kosovo", "Latvia","Liechtenstein", 
                                      "Lithuania", "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                                      "Macedonia", "Netherlands","Norway","Poland","Portugal","Romania",
                                      "Russia","San Marino","Serbia","Slovakia","Slovenia","Spain",
                                      "Sweden","Switzerland","Turkey","Ukraine","UK","Vatican"))







data2$country_standard <- countrycode(data2$Area, origin = 'country.name', destination = 'country.name')
w1$country_standard <- countrycode(w1$region, origin = 'country.name', destination = 'country.name')
europe$country_standard <- countrycode(europe$region, origin = 'country.name', destination = 'country.name')
data2_2$country_standard <- countrycode(data2_2$Area, origin = 'country.name', destination = 'country.name')


p <- europe %>% left_join(data2 %>%
                       filter(Item == "Apples", Unit == "t") %>%
                       group_by(country_standard, Year) %>%
                       summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")) %>%
  mutate(hover_label = glue("{country_standard}\n{formatC(Value, big.mark = ' ', format = 'd')} ton")) %>% 
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(mapping = aes(fill = Value/100, text = hover_label)) +
  coord_fixed(ratio=1.6, xlim = c(-25, 80)) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Uprawa jabłek w krajach europejskich w 2023 roku",
       fill = "Wartość [100t]") +
  theme_minimal() 

ggplotly(p, tooltip = "text")
