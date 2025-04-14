
# Wczytanie pakietów
library(readr)
library(dplyr)
library(ggplot2)
library(tmap)
library(tidyr)
library(readr)
library(maps)
library(mapdata)



# Wczytanie danych
# dane z USA, India, China, Brazil, Nigeria, Bangladesh, Mexico, Indonesia, Pakistan, Ethiopia
# Wszystkie wykresy dotyczą obszarów dotkniętych zanieczyszczeniami wody

dane <- read_csv("water_pollution_disease.csv")

dane <- dane %>%
  select(
    Country,
    diarrhea_rate = `Diarrheal Cases per 100,000 people`,
    cholera_rate = `Cholera Cases per 100,000 people`,
    gdp_per_capita = `GDP per Capita (USD)`
  ) %>%
  group_by(Country) %>%
  summarise(
    liczba_wypadkow = n(), #liczna wpisów dla danego kraju
    diarrhea_rate = sum(diarrhea_rate, na.rm = TRUE),
    cholera_rate = sum(cholera_rate, na.rm = TRUE),
    gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE),
    .groups = "drop"
  )
dane$Country <- tolower(dane$Country)
# Dane mapy świata
mapa <- map_data("world")

# Połączenie danych z mapą
mapa$region <- tolower(mapa$region)
mapa_dane <- left_join(mapa, dane, by = c("region" = "Country"))



# Tworzenie mapy z przypadkami biegunki
mapa_diarrhea <- ggplot(mapa_dane, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = diarrhea_rate), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma",
                       na.value = "gray90",
                       name = "Przypadki\nbiegunki",
                       ) +
  labs(
    title = "Występowanie przypadków biegunki",
    subtitle = "Na 100 000 mieszkańców",
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
mapa_diarrhea

# Tworzenie mapy z przypadkami cholery
mapa_cholera <- ggplot(mapa_dane, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cholera_rate), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma",
                       na.value = "gray90",
                       name = "Przypadki\ncholery",
                       ) +
  labs(
    title = "Występowanie przypadków cholery",
    subtitle = "Na 100 000 mieszkańców",

  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
mapa_cholera

# Tworzenie mapy z gdp
mapa_gdp <- ggplot(mapa_dane, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gdp_per_capita), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma",
                       na.value = "gray90",
                       name = "GDP\nper\ncapita",
  ) +
  labs(
    title = "Gdp per Capita",
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
mapa_gdp

# Tworzenie mapy z liczbą zajerestrowanych wypadków
mapa_wypadkow <- ggplot(mapa_dane, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = liczba_wypadkow), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma",
                       na.value = "gray90",
                       name = "liczba\nwypadków",
  ) +
  labs(
    title = "Liczba zarejestrowanych wypadków",
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
mapa_wypadkow



