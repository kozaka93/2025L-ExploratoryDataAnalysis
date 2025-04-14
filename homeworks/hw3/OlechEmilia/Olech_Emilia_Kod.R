library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(plotly)
library(glue)

# Źródło danych: Netflix Movies and TV Shows dataset z Kaggle
# Link: https://www.kaggle.com/datasets/shivamb/netflix-shows?resource=download
# Dane obejmują produkcje dostępne na Netflix do roku 2021.
# Na mapie kolorami oznaczona jest łączna liczba produkcji z danego kraju (filmy + seriale), 
# przeskalowana logarytmicznie dla lepszej wizualizacji różnic.
# Po najechaniu kursorem na dany kraj w dymku pojawia się:
# Liczba filmów, Liczba seriali, Średnia długość filmu (w minutach) i Średnia liczba sezonów dla seriali.
# Na wykresie chcę pokazać globalny udział poszczególnych krajów w produkcji treści dostępnych na Netflix, 
# z podziałem na filmy i seriale oraz z dodatkowymi informacjami o średniej długości filmów i 
# średniej liczbie sezonów seriali.

netflix <- read_csv("netflix_titles.csv")

# Przygotowanie danych
netflix_clean <- netflix %>%
  filter(!is.na(country), !is.na(duration)) %>%        # Usuwamy rekordy bez kraju lub bez czasu trwania
  separate_rows(country, sep = ",\\s*") %>%            # Rozdzielamy, jeśli tytuł jest przypisany do wielu krajów
  mutate(duration_num = as.numeric(str_extract(duration, "\\d+")))  # Wyciągamy liczby z pola 'duration'

# Statystyki dla filmów
movies_stats <- netflix_clean %>%
  filter(type == "Movie") %>%
  group_by(country) %>%
  summarise(
    movies_count = n(),
    avg_movie_duration = mean(duration_num, na.rm = TRUE)
  )

# Statystyki dla seriali
tv_stats <- netflix_clean %>%
  filter(type == "TV Show") %>%
  group_by(country) %>%
  summarise(
    tv_shows_count = n(),
    avg_seasons = mean(duration_num, na.rm = TRUE)
  )

# Połączenie danych o filmach i serialach
country_stats <- full_join(movies_stats, tv_stats, by = "country") %>%
  mutate(
    total_titles = coalesce(movies_count, 0) + coalesce(tv_shows_count, 0),  # Łączna liczba tytułów
    log_titles = log1p(total_titles)                                        # Logarytm liczby tytułów (lepsza skala do wizualizacji)
  )

# Przygotowanie mapy świata
world <- ne_countries(scale = "medium", returnclass = "sf")

# Uzgadnianie nazw krajów między danymi a mapą
country_stats <- country_stats %>%
  mutate(country = recode(country,
                          "United States" = "United States of America",
                          "Russia" = "Russian Federation",
                          "Vietnam" = "Viet Nam",
                          "South Korea" = "Korea, Republic of",
                          "Iran" = "Iran (Islamic Republic of)",
                          "Tanzania" = "United Republic of Tanzania",
                          "Venezuela" = "Venezuela (Bolivarian Republic of)"
  ))

# Połączenie danych statystycznych z danymi geograficznymi
map_data <- left_join(world, country_stats, by = c("name" = "country"))

#️ Przygotowanie tekstów do dymków w wykresie 
map_data <- map_data %>%
  mutate(
    tooltip_text = glue::glue(
      "<b>{name}</b><br>",
      "🎬 Filmy: {coalesce(movies_count, 0)}<br>",
      "📺 Seriale: {coalesce(tv_shows_count, 0)}<br>",
      "⏱ Śr. długość filmu: {round(coalesce(avg_movie_duration, 0), 1)} min<br>",
      "📈 Śr. liczba sezonów: {round(coalesce(avg_seasons, 0), 1)}"
    )
  )

# Kolory do mapy
strong_colors <- c("#8B0000", "#FF4500", "#FFA500")

# Tworzenie mapy
p <- ggplot(map_data) +
  geom_sf(aes(fill = log_titles, text = tooltip_text), color = "black", size = 0.1) +  
  scale_fill_gradientn(
    colours = strong_colors,
    na.value = "grey30",
    name = "Log liczby tytułów"
  ) +
  annotate(
    "text",
    x = -30,
    y = 100,
    label = "🎥 Netflix: Filmy i Seriale na Świecie",
    color = "#ffffff",
    size = 7,
    family = "Arial Black"
  ) +
  theme_void() +  
  theme(
    plot.background = element_rect(fill = "#141414", color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    legend.background = element_rect(fill = "#141414", color = NA),
    legend.text = element_text(color = "#f5f5f1", family = "Arial Black"),
    legend.title = element_text(color = "#f5f5f1", family = "Arial Black")
  )

# Interaktywny wykres
fig <- ggplotly(p, tooltip = "text")

# Wyświetlenie wykresu
fig





