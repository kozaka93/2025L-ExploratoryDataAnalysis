library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
#install.packages("gapminder")
library(gapminder)
#View(gapminder)

country <- map_data("world")

kraje <- gapminder %>% 
  filter(continent == "Europe" & year =="2007") %>% 
  left_join(country, by = c("country" = "region"))


mapa <- ggplot(data = kraje, mapping = aes(x = long, y = lat, group = group, fill = lifeExp))+
  geom_polygon(color = "white") + 
  coord_fixed(ratio = 1.3) +       
  scale_fill_fermenter(palette = "GnBu", direction = 1)+
  theme_minimal() +
  labs(title = "Przewidywana długość życia w Europie (2007)",
       subtitle = "Źródło: Gapminder",
       fill = "Oczekiwana długość życia")

mapa

