
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(scales)

coffee_bean_production <- read.csv('coffee-bean-production.csv')

# produkcja w tonach, rok 2024

coffee <- coffee_bean_production %>%
  filter(Year == '2023', Code != "") %>%
  mutate(Production = Coffee..green...00000656....Production...005510....tonnes) %>%
  mutate(Entity = recode(Entity,   #zmieniam nazwy krajów tak, żeby pasowały do ramki danych world
                         "Cote d'Ivoire" = "Ivory Coast",
                         "Congo" = "Republic of Congo",
                         "East Timor" = "Timor-Leste",
                         "United States" = "USA",
                         "Saint Vincent and the Grenadines" = "Saint Vincent",  # w ramce danych world "Saint Vincent" oraz "Grenadines" to osobne kraje, rozdzielam je (później duplikuję i tworzę "Grenadines")
                         "Trinidad and Tobago" = "Trinidad"                    # jak wyżej
  )) %>%
  filter(!Entity %in% c("World", "Melanesia", "Polynesia"))  # nie są to kraje, ale były w tej ramce danych

coffee <- coffee %>% bind_rows(
  coffee %>%
    filter(Entity == "Trinidad") %>%
    mutate(Entity = "Tobago"),
  coffee %>%
    filter(Entity == "Saint Vincent") %>%
    mutate(Entity = "Grenadines")) %>%
select(Entity, Production)

world <- map_data("world")

world_coffee <- world %>% left_join(coffee, by = c("region" = "Entity"))

ggplot(world_coffee, aes(x = long, y = lat, group = group, fill = Production)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_gradientn(colors = c("#f5f5dc", "#d2b48c", "#a47148", "#7b3f00", "#3e1f00"),
    na.value = "grey90",
    labels = label_comma()) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Produkcja ziaren kawy według kraju (2023)", fill = "Produkcja (w tonach)", x="", y="")
