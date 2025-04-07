library(tidyverse)
install.packages("sf")
library(sf)
install.packages("rnaturalearth")
library(rnaturalearth)
install.packages("rnaturalearthdata")
library(rnaturalearthdata)

# Wczytanie danych PKB per capita z Banku Światowego
gdp_data <- read_csv("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_26433.csv")
gdp_data <- gdp_data %>%
  select(-ncol(gdp_data))
# Wybór najnowszego dostępnego roku (przyjmujemy ostatnią kolumnę z danymi)
latest_year <- tail(names(gdp_data), 1)  # Wybieramy tylko ostatnią kolumnę

# Wybór odpowiednich kolumn i zmiana nazw
gdp_data2 <- gdp_data %>%
  select(`Country Name`, `Country Code`, latest_year) %>%  # Wybór odpowiednich kolumn
  rename(country = `Country Name`, iso_a3 = `Country Code`, gdp_per_capita = latest_year) 

# Wczytanie danych geograficznych krajów
world <- ne_countries(scale = "medium", returnclass = "sf")

# Połączenie danych geograficznych z danymi PKB
world_gdp <- world %>%
  left_join(gdp_data2, by = "iso_a3")  # Używamy gdp_

ggplot(data = world_gdp) +
  geom_sf(aes(fill = gdp_per_capita), color = "white") +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log",
    na.value = "grey50",
    name = "PKB per capita (USD)"
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 15,
    barheight = 0.8,
    label.position = "bottom",
    label.hjust = 0.5
  )) +
  theme_minimal() +
  labs(
    title = "PKB per capita na świecie",
    subtitle = paste("Dane za rok", latest_year),
    caption = "Źródło: Bank Światowy\nKolor szary: brak danych"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  )

