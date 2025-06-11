library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
library(tidyr)
library(ggbeeswarm)

df23<- read.csv('spotify-2023.csv')
df<-read.csv('data.csv')
dgatunki <- read.csv("dataset.csv")

pom <- dgatunki%>% 
  group_by(track_genre) %>% 
  summarise(median_popularity = median(popularity),
            mean_popularity = mean(popularity))


df$release_date <- as.character(df$release_date)

df <- df %>%
  separate(release_date, into = c("month", "day", "year"), sep = "/", fill = "left", extra = "drop") %>%
  mutate(
    day = as.integer(day),
    month = as.integer(month),
    year = case_when(
      is.na(year) ~ NA_integer_,
      nchar(year) == 4 ~ as.integer(year),
      nchar(year) == 2 & as.integer(year) >= 21 ~ as.integer(paste0("19", year)),
      nchar(year) == 2 ~ as.integer(paste0("20", year)),
      TRUE ~ NA_integer_
    )
  )
df$month <- as.integer(df$month)

key_labels <- c(
  "C", "C♯/D♭", "D", "D♯/E♭", "E", "F",
  "F♯/G♭", "G", "G♯/A♭", "A", "A♯/B♭", "B"
)

df$key_name <- key_labels[df$key + 1]

A24 <- read.csv('Aga2024.csv')
A23 <-read.csv('Aga2023.csv')
A22 <-read.csv('Aga2022.csv')
A21 <-read.csv('Aga2021.csv')
A20 <-read.csv('Aga2020.csv')

E24 <- read.csv('Ewa2024.csv')
E23<- read.csv('Ewa2023.csv')
E22<- read.csv('Ewa2022.csv')
E21<- read.csv('Ewa2021.csv')
E20<- read.csv('Ewa2020.csv')

P24<- read.csv('Piotr2024.csv')
P23<- read.csv('Piotr2023.csv')
P22<- read.csv('Piotr2022.csv')
P21<- read.csv('Piotr2021.csv')
P20<- read.csv('Piotr2020.csv')


extract_year <- function(x) {
  as.numeric(substr(as.character(x), 1, 4))
}


A24$person <- "Aga"
A24$listen_year <- 2024
A24$release_year <- extract_year(A24$Release.Date)

A23$person <- "Aga"
A23$listen_year <- 2023
A23$release_year <- extract_year(A23$Release.Date)

A22$person <- "Aga"
A22$listen_year <- 2022
A22$release_year <- extract_year(A22$Release.Date)

A21$person <- "Aga"
A21$listen_year <- 2021
A21$release_year <- extract_year(A21$Release.Date)

A20$person <- "Aga"
A20$listen_year <- 2020
A20$release_year <- extract_year(A20$Release.Date)


E24$person <- "Ewa"
E24$listen_year <- 2024
E24$release_year <- extract_year(E24$Release.Date)

E23$person <- "Ewa"
E23$listen_year <- 2023
E23$release_year <- extract_year(E23$Release.Date)

E22$person <- "Ewa"
E22$listen_year <- 2022
E22$release_year <- extract_year(E22$Release.Date)

E21$person <- "Ewa"
E21$listen_year <- 2021
E21$release_year <- extract_year(E21$Release.Date)

E20$person <- "Ewa"
E20$listen_year <- 2020
E20$release_year <- extract_year(E20$Release.Date)


P24$person <- "Piotr"
P24$listen_year <- 2024
P24$release_year <- extract_year(P24$Release.Date)

P23$person <- "Piotr"
P23$listen_year <- 2023
P23$release_year <- extract_year(P23$Release.Date)

P22$person <- "Piotr"
P22$listen_year <- 2022
P22$release_year <- extract_year(P22$Release.Date)

P21$person <- "Piotr"
P21$listen_year <- 2021
P21$release_year <- extract_year(P21$Release.Date)

P20$person <- "Piotr"
P20$listen_year <- 2020
P20$release_year <- extract_year(P20$Release.Date)

all_data <- rbind(A20, A21, A22, A23, A24, E20, E21, E22, E23, E24, P20, P21, P22, P23, P24)

df_counts <- data.frame(
  dataframe = c("all_data", "df23", "dgatunki", "df"),
  rows = c(nrow(all_data), nrow(df23), nrow(dgatunki), nrow(df))
)

zmienne1 <- c('danceability','valence','energy','acousticness')
etykiety1 <- c('taneczność', 'pozytywność', 'energia', 'akustyczność')
choices1 <- setNames(zmienne1, etykiety1)
osoby_kolory <- c(
  "Wybrani (razem)" = "black",
  "Aga" = "#b74719",
  "Ewa" = "#1c4e49", 
  "Piotr" = "#837937"
)

zmienne2 <- c('in_spotify_charts','in_apple_charts','in_deezer_charts','in_shazam_charts')
etykiety2 <- c('Spotify', 'Apple Music', 'Deezer', 'Shazam')
choices2 <- setNames(zmienne2, etykiety2)

extract_year <- function(date_col) {
  date_col <- as.character(date_col)
  as.numeric(substr(date_col, 1, 4))}

