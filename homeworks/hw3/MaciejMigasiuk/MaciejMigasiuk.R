library(tmap)
library(dplyr)
library(tidyr)
library(readr)
library(maps)
library(mapdata)
library(ggplot2)

# Dane EuroStat

All_population <- read.csv("Populacja.csv")
All_population <- All_population%>%
  select(geo, TIME_PERIOD, OBS_VALUE) %>%
  pivot_wider(
    names_from = TIME_PERIOD,
    values_from = OBS_VALUE
  )%>%
  rename(
    All_2013 = `2013`,
    All_2022 = `2022`
  )

Imigrants <- read.csv("xd2.csv")
Imigrants<- Imigrants%>%
  select(geo, TIME_PERIOD, OBS_VALUE) %>%
  pivot_wider(
    names_from = TIME_PERIOD,
    values_from = OBS_VALUE
  )

Imigrants_percent <- left_join(Imigrants, All_population, by = "geo") %>%
  filter(geo != 'Romania' & geo !="United Kingdom")%>%
  mutate(percent_2022 =`2022`/All_2022*100 ,
         percent_2013 =`2013`/All_2022*100 
         ) %>%
  mutate(growth = percent_2022-percent_2013)



combined_data <- Imigrants_percent



world_map <- map_data("world")
europe_map <- world_map %>%
  filter(region %in%c("Albania", "Austria", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
                      "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", 
                      "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", 
                      "Netherlands", "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia", "Slovakia", 
                      "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "UK", 
                      "Montenegro", "North Macedonia"))
europe_mapa <- ggplot() + 
  geom_polygon(data = europe_map, aes(x = long, y = lat, group = group))


combined_data <- left_join(Imigrants_percent,Crime_df, by = "geo") %>%
  filter(geo !='Liechtenstein')

europe_data <- europe_map %>%
  left_join(combined_data, by = c("region" = "geo"))

colnames(europe_data)

# Mapa z kolorem wg growth i tekstem wg growth_crime
europe_mapa_final <- ggplot() +
  geom_polygon(data = europe_data, aes(x = long, y = lat, group = group, fill = growth), 
               color = "white") +
  scale_fill_gradient2(
    low = "green", mid = "gold1", high = "red2", midpoint = 0,
    name = "Wzrost % imigrantów"
  ) +
  labs(title = "Ewolucja liczebności imigrantów w Europie ",
       subtitle = "Procentowa zmiana udziału imigrantów w populacji w okresie 2013 - 2022 ",
       x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        panel.background = element_rect(fill="snow"),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Wyświetl mapę
europe_mapa_final
 
