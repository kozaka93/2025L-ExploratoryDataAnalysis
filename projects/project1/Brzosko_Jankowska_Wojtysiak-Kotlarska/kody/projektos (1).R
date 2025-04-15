# 📦 Pakiety
library(dplyr)
library(tidyr)
library(stringr)
library(treemap)

# 📁 Wczytanie danych
action <- read.csv("action.csv")
adventure <- read.csv("adventure.csv")
animation <- read.csv("animation.csv")
crime <- read.csv("crime.csv")
family <- read.csv("family.csv")
fantasy <- read.csv("fantasy.csv")
history <- read.csv("history.csv")
horror <- read.csv("horror.csv")
mystery <- read.csv("mystery.csv")
romance <- read.csv("romance.csv")
scifi <- read.csv("scifi.csv")
thriller <- read.csv("thriller.csv")
war <- read.csv("war.csv")
movies <- read.csv("Movies_2023_03_20.csv")
movies <- movies[ ,c(1,2,3)]

# 🔄 Łączenie danych
tab <- bind_rows(action, adventure, animation, crime, family, fantasy, 
                 history, horror, mystery, romance, scifi, thriller, war)
tab <- tab[, c(2, 6)]

unique_tab <- tab %>%
  group_by(movie_name) %>%
  slice(1)

unique_movies <- movies %>%
  group_by(Original_title) %>%
  slice(1)

tablica <- inner_join(unique_movies, unique_tab, by = c("Original_title" = "movie_name"))

# 🎬 Top 200 filmów
tablica <- tablica %>%
  arrange(desc(Number_of_votes)) %>%
  top_n(200)

# 🎭 Główne gatunki
main_genres <- c("Action", "Adventure", "Comedy", "Crime", "Mystery", "Romance", "Horror", "Sci-Fi")

df <- tablica %>%
  mutate(genre = str_split(genre, ", ")) %>%
  unnest(genre) %>%
  mutate(main_genre = ifelse(genre %in% main_genres, genre, NA)) %>%
  fill(main_genre, .direction = "down") %>%
  mutate(subgenre = ifelse(genre %in% main_genres, NA, genre)) %>%
  filter(!is.na(main_genre))

# 📊 Liczenie podgatunków
genre_counts <- df %>%
  group_by(main_genre, subgenre) %>%
  summarise(movie_count = n()) %>%
  ungroup()

# 🌟 Grupowanie podgatunków
top_n <- 3
genre_counts <- genre_counts %>%
  group_by(main_genre) %>%
  mutate(rank = rank(-movie_count, ties.method = "first")) %>%
  mutate(subgenre = ifelse(rank > top_n, "Other", subgenre)) %>%
  select(-rank) %>%
  group_by(main_genre, subgenre) %>%
  summarise(movie_count = sum(movie_count)) %>%
  ungroup()

# 🗣️ Tłumaczenie gatunków
genre_counts$main_genre <- recode(genre_counts$main_genre,
                                  "Action" = "Akcja",
                                  "Adventure" = "Przygodowy",
                                  "Comedy" = "Komedia",
                                  "Crime" = "Kryminał",
                                  "Mystery" = "Tajemnica",
                                  "Romance" = "Romans",
                                  "Horror" = "Horror",
                                  "Sci-Fi" = "Science-fiction"
)

genre_counts$subgenre <- recode(genre_counts$subgenre,
                                "Superhero" = "Superbohaterski",
                                "Slasher" = "Slasher",
                                "Space" = "Kosmiczny",
                                "Other" = "Inne"
)

# 📈 Procentowy udział
genre_share <- genre_counts %>%
  group_by(main_genre) %>%
  summarise(total = sum(movie_count)) %>%
  ungroup() %>%
  mutate(percentage = round(100 * total / sum(total), 1))

genre_counts <- genre_counts %>%
  left_join(genre_share %>% select(main_genre, percentage), by = "main_genre") %>%
  mutate(main_genre_label = paste0(main_genre, " (", percentage, "%)"))

# 🎨 Retro paleta bez zieleni
custom_palette <- c(
  "#e0c3b2",  # gorzka czekolada
  "#DDB892",  # ochra
  "#A44A3F",  # cegła
  "#6C757D",  # grafit
  "#bcc5bc",  # pudrowa śliwka
  "#E07A5F",  # rdza
  "#3A405A",  # granat
  "#d08e97"   # bordo
)

# 🖼️ Zapis z przezroczystym tłem
png("treemap_filmweb_przezroczysty.png", width = 1600, height = 1100, res = 150, bg = "transparent")

layout(matrix(c(1, 2), nrow = 2), heights = c(1, 8))

# Tytuł
par(mar = c(0, 0, 0, 0))
plot.new()
title(
  "Udział procentowy głównych gatunków i podgatunków\n200 najpopularniejszych filmów na Filmwebie",
  family = "Arial", cex.main = 2, line = -2, col.main = "#3A405A"  # granatowy tekst tytułu
)

# Treemap
par(mar = c(2, 2, 2, 2))
treemap(
  genre_counts,
  index = c("main_genre_label", "subgenre"),
  vSize = "movie_count",
  vColor = "movie_count",
  palette = custom_palette,
  title = "",
  fontsize.title = 0,
  fontfamily.labels = "Arial",
  fontsize.labels.main = 14,
  fontsize.labels.sub = 9,
  position.legend = "none",
  border.col = "white",
  overlap.labels = 0.5
)

dev.off()
