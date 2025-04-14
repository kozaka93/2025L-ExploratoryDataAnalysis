# Pakiety
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

# Wczytaj dane
dane <- read.csv("total-alcohol-consumption-per-capita-litres-of-pure- ahcool new.csv", stringsAsFactors = FALSE)
colnames(dane)[4] <- "alcohol_consumption"

# Filtrowanie danych z 2018 roku
dane_2018 <- dane %>%
  filter(Year == 2018) %>%
  select(country = Entity, alcohol_consumption) %>%
  mutate(alcohol_consumption = as.numeric(alcohol_consumption))

# Kategorie
dane_2018 <- dane_2018 %>%
  mutate(kategoria = cut(alcohol_consumption,
                         breaks = c(0, 2, 5, 8, 12, Inf),
                         labels = c("0–2 L", "2–5 L", "5–8 L", "8–12 L", "12+ L"),
                         right = FALSE))

# Poprawki nazw państw
dane_2018$country[dane_2018$country == "United States"] <- "USA"
dane_2018$country[dane_2018$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
dane_2018$country[dane_2018$country == "Czechia"] <- "Czech Republic"
dane_2018$country[dane_2018$country == "Myanmar"] <- "Burma"
dane_2018$country[dane_2018$country == "Eswatini"] <- "Swaziland"
dane_2018$country[dane_2018$country == "North Macedonia"] <- "Macedonia"

# Mapa świata
mapa <- map_data("world")

# Połączenie danych
mapa_dane <- mapa %>%
  left_join(dane_2018, by = c("region" = "country"))

# Budowa wykresu
mapa_alko <- ggplot(mapa_dane, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = kategoria), color = "gray80", linewidth = 0.05) +
  scale_fill_manual(
    values = c("0–2 L" = "#edf8fb",
               "2–5 L" = "#b2e2e2",
               "5–8 L" = "#66c2a4",
               "8–12 L" = "#2ca25f",
               "12+ L" = "#006d2c"),
    na.value = "lightgrey",
    name = "Spożycie alkoholu\nna osobę (15+)"
  ) +
  labs(title = "Spożycie alkoholu per capita na świecie (2018)",
       caption = "Źródło: WHO / Kaggle") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),  # Polskie znaki!
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

# Wyświetl mapę
print(mapa_alko)

# Zapisz jako JPG
ggsave("mapa_spozycie_alkoholu_2018.jpg", plot = mapa_alko, width = 12, height = 6, dpi = 300)
