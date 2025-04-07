library(eurostat)
library(maps)
library(mapdata)

options(scipen = 20)
  
country <- map_data("world")

sheep_population <- get_eurostat("apro_mt_lssheep")

# unique(label_eurostat(sheep_population)$animals)

country_names <- c(
  "Albania", "Austria", "Bosnia and Herzegovina", "Belgium", "Bulgaria",
  "Switzerland", "Cyprus", "Czech Republic", "Germany", "Denmark",
  "Estonia", "Greece", "Spain", "Finland", "France",
  "Croatia", "Hungary", "Ireland", "Iceland", "Italy",
  "Lithuania", "Luxembourg", "Latvia", "Montenegro", "North Macedonia",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania",
  "Serbia", "Sweden", "Slovenia", "Slovakia",
  "UK", "Kosovo")

sheep <- sheep_population %>%
  filter(substr(TIME_PERIOD,1,4) > 2000) %>%
  filter(animals == "A4100") %>% 
  group_by(geo) %>% 
  summarise(owce  = mean(values)) %>%
  rename(region = geo) %>% 
  filter(!(region %in% c("EU15", "EU25", "EU27_2007", "EU27_2020", "EU28", "TR")))

# niestety dane z eurostatu dla niektórych państw były bardzo ubogie, 
# stąd branie średniej z ostatnich 25 lat. Turcje odrzucamy, bo odstaje geograficznie.

sheep$region <- country_names

country %>%
  filter(region %in% sheep$region) %>% 
  left_join(sheep) %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = owce), color = "black") +
  coord_map("mollweide") +
  labs(title = "Liczba owiec w wybranych państwach europejskich.",
     subtitle = "Wynik uśredniony z ostatnich 25 lat z powodu braków w danych.",
     fill = "Owce (tys.)",
     x = NULL, y = NULL) +
  scale_fill_gradient2(high = "darkgreen")
