#źródło danych: https://ourworldindata.org/grapher/total-alcohol-consumption-per-capita-litres-of-pure-alcohol
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
w <- map_data("world")

alkohol_swiat <- read.csv("https://ourworldindata.org/grapher/total-alcohol-consumption-per-capita-litres-of-pure-alcohol.csv?v=1&csvType=full&useColumnShortNames=true")
alkohol_swiat_zgodny <- alkohol_swiat %>%
  rename(region = Entity, spozycie_2020 = `sh_alc_pcap_li`) %>% 
  filter(Year == 2020) %>% 
  mutate(region = case_when(
    region == "United States" ~ "USA",
    region == "Czechia" ~ "Czech Republic",
    region == "United Kingdom" ~ "UK",
    region == "Cote d'Ivoire" ~ "Ivory Coast",
    region == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    region == "Congo" ~ "Republic of Congo",
    TRUE ~ region
  ))
  #wyżej uzgadniam dane, bo niektóre państwa są inaczej ponazywane w world 
  #z mapdaty i w zimportowanych danych, z czego nie wszystkie państwa mają dane
  #(np Grenlandia, Sudan Płd.), co widać zresztą na mapce z linku na samej górze

swiat_alkoholu <- w %>% 
  left_join(alkohol_swiat_zgodny, by="region")

ggplot(data = swiat_alkoholu, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = spozycie_2020), color = "black", linewidth = 0.2) +
  coord_fixed(1.3) +
  scale_fill_gradientn(
    colours = c("yellow", "orange", "brown"),
    na.value = "lightgray",
    name = "Alkohol\n[litr/os.]"
  ) +
  theme_void() +
  labs(title = "Średnie spożycie alkoholu na osobę w roku 2020",
       caption = "Źródło: Our World in Data") +
  theme(legend.position = "right")
