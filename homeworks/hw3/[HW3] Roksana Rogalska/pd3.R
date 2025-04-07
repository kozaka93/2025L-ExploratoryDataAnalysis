library(maps)
library(mapdata)
library(dplyr)
library(patchwork)
library(ggplot2)

covid <- read.csv('covid2.csv', sep = ';')
population <- read.csv('world_population.csv')

covid <- covid %>%
  mutate(across(`X2020`:`X2023`, ~ gsub(" ", "", .))) %>%
  mutate(across(`X2020`:`X2023`, ~ na_if(., ":"))) %>%
  mutate(across(`X2020`:`X2023`, as.numeric)) %>% 
  mutate(suma = ifelse(
    is.na(X2020) | is.na(X2021) | is.na(X2022), 
    NA, 
    X2020 + X2021 + X2022
  ))

countries <- covid$Country
world <- map_data('world', countries)

map_data_joined <- world %>%
  left_join(covid, by = c("region" = "Country"))

country_population <- map_data_joined %>%
  left_join(population, by = c("region" = "Country.Territory"))

smierci <- country_population %>% 
  mutate(death_index = ifelse(
    is.na(suma), 
    NA, 
    suma / as.numeric(X2022.Population) * 100000
  ))

map1 <- ggplot(map_data_joined, aes(x = long, y = lat, group = group, fill = X2020)) +
  geom_polygon(color = "black") +
  coord_fixed(1.3) +
  scale_fill_gradient(
    name = "Liczba zgonów",
    low = 'lavenderblush',
    high = 'deeppink',
    na.value = "lavender"
  ) +
  theme_minimal() +
  labs(title = "Zgony na COVID-19 w Europie w 2020 roku",
       caption = "Fioletowy kolor oznacza brak danych dla danego kraju.") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

map2 <- ggplot(smierci, aes(x = long, y = lat, group = group, fill = death_index)) +
  geom_polygon(color = "black") +
  coord_fixed(1.3) +
  scale_fill_gradient(
    name = "Liczba zgonów w przeliczeniu \n na 100 tysięcy mieszkańców",
    low = 'lavenderblush',
    high = 'deeppink',
    na.value = "lavender"
  ) +
  theme_minimal() +
  labs(title = "Zgony na COVID-19 w Europie w latach 2020-2022 \nw przeliczeniu na 100tys. mieszkańców",
       caption = "Fioletowy kolor oznacza brak danych dla danego kraju.") + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    )

map1 / map2
