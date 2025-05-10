# Jajeczna inflacja w UE na przestrzeni ostatnich lat

# Źródło danych:
# https://ec.europa.eu/eurostat/web/products-eurostat-news/w/ddn-20250417-1
# https://businessinsider.com.pl/wiadomosci/jajeczna-inflacja-o-ile-podrozaly-jajka-od-ostatnich-swiat-polska-na-tle-ue/xg3dhy3

# Wykres, który będę poprawiać:
# https://ocdn.eu/pulscms-transforms/1/ypwk9kpTURBXy9hMGY5YWYzYjg5OWI4ZjJkOGYzZTI2ZmY4ZDZjYTZlMC5wbmeRkwLNBWQA3gACoTAGoTEB

# Błędy na wykresie:
# 1) Brak tytułu
# 2) Brak podpisów osi - zwłaszcza, że jedne z danych są procentowe
# 3) Po prawej jest jakiś pasek pokazujący małą wersje wykresu - co jest zbędne

# install.packages("eurostat")

# Dane, z których będę korzystać
library(eurostat)

library(dplyr)
library(ggplot2)
library(scales)
library(readr)
library(jsonlite)
library(tidyverse)

# Wczytanie danych

url <- paste0(
  "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/",
  "prc_hicp_manr?coicop=CP01147&time=2025-03"
)
j  <- fromJSON(url, simplifyDataFrame = FALSE)

val_vec <- unlist(j$value)


geo_lookup  <- j$dimension$geo$category$index
geo_inverse <- names(geo_lookup)[match(names(val_vec), geo_lookup)]

eggs <- tibble(
  geo   = geo_inverse,
  rate  = as.numeric(val_vec)
)

labels <- j$dimension$geo$category$label
eggs <- eggs %>%
  mutate(country = labels[geo]) %>% 
  filter(!(geo %in% c("EA", "EA19", "EA20", "EEA", "EU", "EU27_2020")))


# View(eggs)

# Tworzenie ulepszonego wykresu

# Najpierw dodam opisy czy mamy doczynienia ze spadkiem czy ze wzrostm oraz posoruje dane

eggs <- eggs %>% 
  mutate(change = if_else(
    rate >= 0, "positive", "negative")) %>% 
  arrange(desc(rate))

ggplot(eggs, aes(
    x = rate,
    y = fct_reorder(as.character(country), rate),
    fill = change)) +
  
  geom_col(width = 0.7) +
  
  scale_fill_manual(values = c("positive" = "#1f77b4",
                               "negative" = "#d62728"),
                    name = NULL) +
  
  geom_vline(xintercept = 0, colour = "grey40") +
  
  geom_text(aes(label = rate,
            hjust = if_else(rate >= 0, -0.15, 1.15),
            size = 3)) +
  
  scale_x_continuous(
    limits = c(-5, 50),
    breaks = seq(-5, 50, by = 5),
    labels = scales::label_number(accuracy = 1, suffix = " %")
  ) +
  
  labs(
    title    = "Change in egg prices – March 2025 vs March 2024 (y/y)",
    x        = "year-on-year change (%)",
    y        = NULL,
    caption  = "Source: Eurostat, CP01147 – Eggs"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  ) +
  
  guides(size = "none") 



# Dlczego ten wykres jest lepszy?
# Po pierwsze opisane jest co się znajduje na wykresie (tytuł i tytuły osi),
# kolory ułatwiające zobaczenie gdzie jest wzorst a gdzie spadek ceny.
# Zbędna "mini" wersja wykresu została usunięta.

