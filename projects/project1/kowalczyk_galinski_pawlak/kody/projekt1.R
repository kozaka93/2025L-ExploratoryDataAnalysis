
install.packages("ggplot2")  # Jeśli jeszcze nie masz ggplot2
library(ggplot2)

library(rvest)
library(dplyr)

install.packages("patchwork")
library(patchwork)

#TRANSFERMARKT

#2023/2024
page1 <- read_html("https://www.ekstraklasa.org/statystyki/druzynowe")

tabela2324 <- page1 %>%
  html_node(xpath = '//*[@id="yw1"]') %>%
  html_table(fill = TRUE)

tabela2324 <- tabela2324[, -1]
tabela2324 <- tabela2324[, -7]
tabela2324 <- tabela2324[-1, ]

#2022/2023
page2 <- read_html("https://www.transfermarkt.pl/pko-bp-ekstraklasa/startseite/wettbewerb/PL1/plus/?saison_id=2022")

tabela2223 <- page2 %>%
  html_node(xpath = '//*[@id="yw1"]') %>%
  html_table(fill = TRUE)

tabela2223 <- tabela2223[, -1]
tabela2223 <- tabela2223[, -7]
tabela2223 <- tabela2223[-1, ]

#2021/2022
page3 <- read_html("https://www.transfermarkt.pl/pko-bp-ekstraklasa/startseite/wettbewerb/PL1/plus/?saison_id=2021")

tabela2122 <- page3 %>%
  html_node(xpath = '//*[@id="yw1"]') %>%
  html_table(fill = TRUE)

tabela2122 <- tabela2122[, -1]
tabela2122 <- tabela2122[, -7]
tabela2122 <- tabela2122[-1, ]

#2020/2021
page4 <- read_html("https://www.transfermarkt.pl/pko-bp-ekstraklasa/startseite/wettbewerb/PL1/plus/?saison_id=2020")

tabela2021 <- page4 %>%
  html_node(xpath = '//*[@id="yw1"]') %>%
  html_table(fill = TRUE)

tabela2021 <- tabela2021[, -1]
tabela2021 <- tabela2021[, -7]
tabela2021 <- tabela2021[-1, ]

#EKSTRAKLASA ORG

#2023/2024
stats_table <- read.csv("ekstraklasa.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
stats_table <- stats_table[seq(1, nrow(stats_table), by = 2), ]


#duza bestia
full<-read.csv("full_data.csv")


#wykres wygranych od kursu
test1<-full %>% 
  filter(League=="pko-bp-ekstraklasa",is.na(WIN_BET)==FALSE) %>% 
  group_by(WIN_BET) %>% 
  summarise(n=n()) %>% 
  arrange(-n)

ggplot(test1, aes(x = WIN_BET, y = n)) +
  geom_point(color = "blue", size = 3) +    # Punkty na wykresie
  geom_smooth(method = "loess", se = FALSE, color = "red") +  # Linia trendu
  labs(title = "Liczba wygranych a kurs",
       x = "Kurs drużyny wygranej",
       y = "Liczba wygranych") +
  theme_minimal()


#roznica kursow a roznica goli
test2<-full %>% 
  filter(League=="pko-bp-ekstraklasa",is.na(H_BET)==FALSE,is.na(A_BET)==FALSE) %>% 
  mutate(roznk=abs(H_BET-A_BET), rozng=abs(H_Score-A_Score)) %>% 
  select(roznk, rozng) %>% 
  filter(roznk<7)


ggplot(test2, aes(x = roznk, y = rozng)) +
  geom_density_2d_filled(color = "red") +  # Kontury gęstości
  #geom_point(alpha = 0.5) +  # Punkty dla referencji
  labs(title = "Wykres gęstości różnicy goli od różnicy kursów",
       x = "Różnica w kursach",
       y = "Różnica w golach") +theme_minimal()+xlim(0,4)+ylim(0,4)

library(dplyr)
library(ggplot2)

test3 <- full %>% 
  filter(League == "pko-bp-ekstraklasa", H_Score > A_Score, H_Ball_Possession != "") %>%
  mutate(H_Ball_Possession = gsub("%", "", H_Ball_Possession),       # usuwa %
         H_Ball_Possession = gsub(",", ".", H_Ball_Possession),      # zamienia przecinki na kropki (jeśli są)
         H_Ball_Possession = as.numeric(H_Ball_Possession)) %>%      # konwertuje na numeric
  filter(!is.na(H_Ball_Possession)) %>%
  mutate(Possession_Group = cut(H_Ball_Possession, 
                                breaks = seq(0, 100, by = 5),
                                include.lowest = TRUE,
                                right = FALSE,
                                labels = paste0(seq(0, 95, by = 5), "-", seq(5, 100, by = 5), "%"))) %>%
  group_by(Possession_Group) %>% 
  summarise(n = n(), .groups = 'drop')

p1 <- ggplot(test3, aes(x = Possession_Group, y = n)) +
  geom_col(fill = "#2c7fb8", color = "black") +
  
  coord_flip() +
  scale_y_continuous(limits = c(0, 300)) +
  labs(
    title = "Zwycięstwa a posiadanie - Dom",
    y = "Liczba zwycięstw",
    x = "Posiadanie piłki (w przedziałach %)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "#f7f7f7", color = NA)
  )



library(dplyr)
library(ggplot2)

test4 <- full %>% 
  filter(League == "pko-bp-ekstraklasa", H_Score < A_Score, A_Ball_Possession != "") %>%
  mutate(A_Ball_Possession = gsub("%", "", A_Ball_Possession),       # usuwa %
         A_Ball_Possession = gsub(",", ".", A_Ball_Possession),      # zamienia przecinki na kropki (jeśli są)
         A_Ball_Possession = as.numeric(A_Ball_Possession)) %>%      # konwertuje na numeric
  filter(!is.na(A_Ball_Possession)) %>%
  mutate(Possession_Group = cut(A_Ball_Possession, 
                                breaks = seq(0, 100, by = 5),
                                include.lowest = TRUE,
                                right = FALSE,
                                labels = paste0(seq(0, 95, by = 5), "-", seq(5, 100, by = 5), "%"))) %>%
  group_by(Possession_Group) %>% 
  summarise(n = n(), .groups = 'drop')

p2 <- ggplot(test4, aes(x = Possession_Group, y = n)) +
  geom_col(fill = "#2c7fb8", color = "black") +
  
  coord_flip() +
  scale_y_continuous(limits = c(0, 300)) +
  labs(
    title = "Zwycięstwa a posiadanie - Wyjazd",
    y = "Liczba zwycięstw",
    x = "Posiadanie piłki (w przedziałach %)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "#f7f7f7", color = NA)
  )




library(ggplot2)
library(dplyr)

# Przykładowa modyfikacja danych, by uzyskać zmienną 'result'
# Zakładamy, że kolumna WIN zawiera np. "H", "A" lub "D" (remis)
data <- full %>%filter(League=="pko-bp-ekstraklasa") %>% 
  mutate(result = case_when(
    WIN == "Home" ~ "Gospodarze",
    WIN == "Away" ~ "Goście",
    TRUE ~ "Remis"
  ),
  # Różnica strzałów na bramkę między gospodarzem a gośćmi
  shots_diff = abs(H_Shots_on_Goal - A_Shots_on_Goal)
  )

library(patchwork)

p2 <- ggplot(data, aes(x = H_Goal_Attempts, y = A_Goal_Attempts, color = result, size = shots_diff)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Porównanie prób strzałów",
    x = "Gospodarze",
    y = "Goście",
    color = "Wynik",         # Zmiana nazwy legendy dla result
    size = "Różnica strzałów" # Zmiana nazwy legendy dla shots_diff
  ) +
  scale_color_manual(values = c("Gospodarze" = "blue", "Goście" = "red", "Remis" = "green")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))



p1 <- ggplot(data, aes(x = H_Goal_Attempts, y = A_Goal_Attempts, color = result, size = shots_diff)) +
  geom_point(alpha = 0.7, stroke = 0.9, shape = 21, fill = "white") +  # lepszy kontrast + kontur
  labs(
    title = "Porównanie prób strzałów",
    x = "Gospodarze",
    y = "Goście",
    color = "Wynik",
    size = "Różnica strzałów"
  ) +
  scale_color_manual(values = c("Gospodarze" = "blue", "Goście" = "red", "Remis" = "green")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )




p2 <- ggplot(data, aes(x = H_Attacks, y = A_Attacks)) +
  geom_density_2d_filled() +
  facet_wrap(~ Round, ncol = 4) +
  labs(
    title = "Gęstość ataków w meczach ekstraklasy - podział wg rundy",
    x = "Ataki gospodarzy",
    y = "Ataki gości"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))

p3 <- ggplot(data, aes(x = result, y = H_Ball_Possession, fill = result)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Posiadanie piłki gospodarzy w zależności od wyniku meczu",
    x = "Wynik meczu",
    y = "Posiadanie piłki (%)"
  ) +
  scale_fill_manual(values = c("Gospodarze" = "blue", "Goście" = "red", "Remis" = "gray")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))

# Łączenie wykresów
p1 + p2 + p3 + plot_layout(ncol = 2)


library(ggplot2)
library(dplyr)
library(tidyr)

# Filtrowanie danych tylko dla Legii i Lecha
teams_data <- data %>%
  filter(Home %in% c("Legia Warszawa", "Lech Poznań") | Away %in% c("Legia Warszawa", "Lech Poznań")) %>%
  mutate(Team = ifelse(Home == "Legia Warszawa" | Away == "Legia Warszawa", "Legia Warszawa", "Lech Poznań")) %>%
  group_by(Team) %>%
  summarise(
    `Posiadanie Piłki` = mean(c(H_Ball_Possession[Away == "Legia Warszawa"], A_Ball_Possession[Home == "Legia Warszawa"]), na.rm = TRUE),
    `Strzały Celne` = mean(c(H_Shots_on_Goal[Away == "Legia Warszawa"], A_Shots_on_Goal[Home == "Legia Warszawa"]), na.rm = TRUE),
    `Ataki` = mean(c(H_Attacks[Away == "Legia Warszawa"], A_Attacks[Home == "Legia Warszawa"]), na.rm = TRUE),
    `Niebezpieczne Ataki` = mean(c(H_Dangerous_Attacks[Away == "Legia Warszawa"], A_Dangerous_Attacks[Home == "Legia Warszawa"]), na.rm = TRUE),
    `Faule` = mean(c(H_Fouls[Away == "Legia Warszawa"], A_Fouls[Home == "Legia Warszawa"]), na.rm = TRUE)
  ) %>%
  pivot_longer(-Team, names_to = "Statystyka", values_to = "Wartość")

ggplot(teams_data, aes(x = Statystyka, y = Team, fill = Wartość)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmapa porównująca Legię Warszawa i Lecha Poznań", fill = "Średnia wartość") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

install.packages("ggalluvial")
library(ggalluvial)

# Tworzenie danych do Sankey Diagram
sankey_data <- data %>%
  filter(Home %in% c("Legia Warszawa", "Lech Poznań") | Away %in% c("Legia Warszawa", "Lech Poznań")) %>%
  select(Home, Away, H_Attacks, A_Attacks, H_Shots_on_Goal, A_Shots_on_Goal) %>%
  pivot_longer(cols = starts_with("H_") | starts_with("A_"), names_to = "Kategoria", values_to = "Liczba")

ggplot(sankey_data, aes(axis1 = Home, axis2 = Away, y = Liczba)) +
  geom_alluvium(aes(fill = Home), width = 1/12) +
  geom_stratum(width = 1/12, fill = "gray", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_fill_manual(values = c("Legia Warszawa" = "blue", "Lech Poznań" = "red")) +
  theme_minimal() +
  labs(title = "Sankey Diagram - Przepływ akcji między drużynami")


install.packages("fmsb")
library(fmsb)

# Przygotowanie danych do wykresu radarowego
radar_data <- teams_data %>%
  pivot_wider(names_from = Team, values_from = Wartość) %>%
  as.data.frame()

# Dodanie wartości maksymalnych i minimalnych dla poprawnej skali
radar_data <- rbind(
  c(100, 100),   # Maksymalne wartości
  c(0, 0),       # Minimalne wartości
  radar_data[, -1] # Faktyczne dane
)

# Rysowanie wykresu radarowego
radarchart(radar_data, axistype = 2,
           pcol = c("blue", "red"), pfcol = c("blue", "red"), plwd = 2,
           title = "Porównanie Legii Warszawa i Lecha Poznań")
legend("topright", legend = c("Legia Warszawa", "Lech Poznań"), col = c("blue", "red"), lwd = 2)

install.packages("ggridges")
library(ggridges)

ggplot(data, aes(x = H_Ball_Possession, y = factor(Home), fill = Home)) +
  geom_density_ridges(alpha = 0.8) +
  scale_fill_manual(values = c("Legia Warszawa" = "blue", "Lech Poznań" = "red")) +
  theme_minimal() +
  labs(title = "Rozkład posiadania piłki dla Legii i Lecha", x = "Posiadanie piłki (%)", y = "Drużyna")




library(ggplot2)
library(dplyr)

library(ggridges)
library(fmsb)

ggplot(lech_legia, aes(x = Date, y = H_Score - A_Score, color = Home)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Historia wyników Lecha z Legią",
       x = "Data meczu",
       y = "Różnica bramek (Lech - Legia)") +
  scale_color_manual(values = c("Legia Warszawa" = "red", "Lech Poznań" = "blue")) +
  theme_minimal()

ggplot(data, aes(x = Home, y = H_Shots_on_Goal, fill = Home)) +
  geom_violin(alpha = 0.7) +
  labs(title = "Liczba strzałów celnych drużyn gospodarzy",
       x = "Drużyna",
       y = "Strzały celne") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data, aes(x = Home, y = H_BET, fill = Home)) +
  geom_violin(alpha = 0.7) +
  labs(title = "Rozkład kursów na gospodarzy (H_BET)",
       x = "Drużyna",
       y = "Kurs na wygraną gospodarza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = WIN_BET, y = H_BET, fill = WIN_BET)) +
  geom_violin(alpha = 0.7) +
  labs(title = "Kursy na gospodarzy a faktyczny wynik",
       x = "Faktyczny wynik (H/X/A)",
       y = "Kurs na gospodarza (H_BET)") +
  theme_minimal()


library(ggplot2)
library(ggridges)

ggplot(data, aes(x = H_BET, y = Home, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "magma") +
  labs(title = "Rozkład kursów na gospodarzy (H_BET)",
       x = "Kurs na wygraną gospodarza",
       y = "Drużyna") +
  theme_minimal()



ggplot(data, aes(x = H_Fouls, y = Home, fill=stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "inferno") +
  labs(title = "Rozkład liczby fauli gospodarzy",
       x = "Faule",
       y = "Drużyna") +
  theme_minimal()

ggplot(data_filtered, aes(x = H_Fouls, y = Home)) +
  geom_density_ridges(
    scale = 2,
    rel_min_height = 0.01,
    fill = "#E74C3C",       # jeden, dobrze widoczny kolor (tu czerwony)
    color = "white",#cienki biały kontur
    alpha = 0.8             # lekka przezroczystość
  ) +
  coord_cartesian(xlim = c(0, 30)) +
  labs(
    title = "Rozkład liczby fauli gospodarzy",
    x = "Faule",
    y = "Drużyna"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )


library(dplyr)
library(ggplot2)

# Procent wygranych u siebie
home_wins <- full %>%
  filter(League == "pko-bp-ekstraklasa") %>%
  group_by(Team = Home) %>%
  summarise(
    Home_Games = n(),
    Home_Wins = sum(H_Score > A_Score),
    .groups = 'drop'
  )

# Procent wygranych na wyjeździe
away_wins <- full %>%
  filter(League == "pko-bp-ekstraklasa") %>%
  group_by(Team = Away) %>%
  summarise(
    Away_Games = n(),
    Away_Wins = sum(A_Score > H_Score),
    .groups = 'drop'
  )

# Połączenie i obliczenie procentów
# ZAMIANA inner_join NA:
win_pct <- full_join(home_wins, away_wins, by = "Team") %>%
  mutate(
    Home_Games = replace_na(Home_Games, 0),
    Home_Wins = replace_na(Home_Wins, 0),
    Away_Games = replace_na(Away_Games, 0),
    Away_Wins = replace_na(Away_Wins, 0),
    Home_Win_Pct = ifelse(Home_Games > 0, 100 * Home_Wins / Home_Games, NA),
    Away_Win_Pct = ifelse(Away_Games > 0, 100 * Away_Wins / Away_Games, NA)
  )

# Zmieniamy na long format (do połączenia punktów linią)
library(tidyr)

win_pct_long <- win_pct %>%
  select(Team, Home_Win_Pct, Away_Win_Pct) %>%
  pivot_longer(cols = c(Home_Win_Pct, Away_Win_Pct),
               names_to = "Location",
               values_to = "WinPct")

# Sortujemy drużyny wg średniego % wygranych
win_pct$avg <- (win_pct$Home_Win_Pct + win_pct$Away_Win_Pct)/2
win_pct <- win_pct %>% arrange(avg)

# Dodajemy kolejność do rysowania wykresu w dobrej kolejności
win_pct_long$Team <- factor(win_pct_long$Team, levels = win_pct$Team)

win_pct_long <- win_pct %>%
  select(Team, Home_Win_Pct, Away_Win_Pct) %>%
  pivot_longer(cols = c(Home_Win_Pct, Away_Win_Pct),
               names_to = "Location",
               values_to = "WinPct") %>%
  filter(!is.na(WinPct))


p <- ggplot(win_pct_long, aes(x = WinPct, y = Team, group = Team)) +
  geom_line(color = "#a6bddb", size = 1.5) +  # linia łącząca punkty
  geom_point(aes(color = Location), size = 4) +
  scale_color_manual(values = c("Home_Win_Pct" = "#1f78b4", "Away_Win_Pct" = "#33a02c"),
                     labels = c("Home", "Away")) +
  labs(
    title = "Procent wygranych u siebie vs na wyjeździe",
    x = "Procent wygranych (%)",
    y = "Drużyna",
    color = "Lokalizacja"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = "gray20"),
    legend.position = "bottom"
  )





ekstra <- full %>%
  filter(League == "pko-bp-ekstraklasa")

# Obliczamy dla każdego zespołu ile wygrał meczów jako gospodarz i jako gość
win_pct <- ekstra %>%
  mutate(
    Home_Win = ifelse(WIN == "Home", 1, 0),
    Away_Win = ifelse(WIN == "Away", 1, 0)
  ) %>%
  group_by(Home) %>%
  summarise(
    Home_Games = n(),
    Home_Wins = sum(Home_Win),
    .groups = "drop"
  ) %>%
  rename(Team = Home) %>%
  full_join(
    ekstra %>%
      group_by(Away) %>%
      summarise(
        Away_Games = n(),
        Away_Wins = sum(Away_Win),
        .groups = "drop"
      ) %>%
      rename(Team = AwayTeam),
    by = "Team"
  ) %>%
  mutate(
    Home_Win_Pct = ifelse(Home_Games > 0, 100 * Home_Wins / Home_Games, NA),
    Away_Win_Pct = ifelse(Away_Games > 0, 100 * Away_Wins / Away_Games, NA)
  )


win_pct_long <- win_pct %>%
  select(Team, Home_Win_Pct, Away_Win_Pct) %>%
  pivot_longer(cols = c(Home_Win_Pct, Away_Win_Pct),
               names_to = "Location",
               values_to = "WinPct") %>%
  filter(!is.na(WinPct))  # tylko tam, gdzie są dane

# Sortowanie drużyn
avg_win <- win_pct %>%
  mutate(avg = rowMeans(cbind(Home_Win_Pct, Away_Win_Pct), na.rm = TRUE)) %>%
  arrange(avg)

win_pct_long$Team <- factor(win_pct_long$Team, levels = avg_win$Team)


p <- ggplot(win_pct_long, aes(x = WinPct, y = Team, group = Team)) +
  geom_line(color = "#c6dbef", size = 1.5) +
  geom_point(aes(color = Location), size = 4) +
  scale_color_manual(
    values = c("Home_Win_Pct" = "#1f78b4", "Away_Win_Pct" = "#33a02c"),
    labels = c("Dom", "Wyjazd")
  ) +
  labs(
    title = "Procent wygranych: Dom vs Wyjazd (Ekstraklasa)",
    x = "Procent wygranych (%)",
    y = "Drużyna",
    color = "Miejsce meczu"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray85")
  )

p





library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Filtrowanie ligi i usunięcie NA w kolumnie WIN
ekstra <- full %>%
  filter(League == "pko-bp-ekstraklasa", !is.na(WIN))

# 2. Obliczanie wygranych u siebie
home_stats <- ekstra %>%
  group_by(Team = Home) %>%
  summarise(
    Home_Games = n(),
    Home_Wins = sum(WIN == "Home"),
    .groups = "drop"
  )

# 3. Obliczanie wygranych na wyjeździe
away_stats <- ekstra %>%
  group_by(Team = Away) %>%
  summarise(
    Away_Games = n(),
    Away_Wins = sum(WIN == "Away"),
    .groups = "drop"
  )

# 4. Łączenie danych i liczenie procentów
win_pct <- full_join(home_stats, away_stats, by = "Team") %>%
  mutate(
    Home_Win_Pct = ifelse(Home_Games > 0, 100 * Home_Wins / Home_Games, NA),
    Away_Win_Pct = ifelse(Away_Games > 0, 100 * Away_Wins / Away_Games, NA)
  )

# Długi format
win_pct_long <- win_pct %>%
  select(Team, Home_Win_Pct, Away_Win_Pct) %>%
  pivot_longer(cols = c(Home_Win_Pct, Away_Win_Pct),
               names_to = "Location",
               values_to = "WinPct") %>%
  filter(!is.na(WinPct))  # Usuwamy brakujące

# Sortowanie drużyn po średnim % wygranych
avg_order <- win_pct %>%
  mutate(avg = rowMeans(cbind(Home_Win_Pct, Away_Win_Pct), na.rm = TRUE)) %>%
  arrange(avg) %>%
  pull(Team)

win_pct_long$Team <- factor(win_pct_long$Team, levels = avg_order)


p <- ggplot(win_pct_long, aes(x = WinPct, y = Team, group = Team)) +
  geom_line(color = "#c6dbef", linewidth = 1.5) +
  geom_point(aes(color = Location), size = 4) +
  scale_color_manual(
    values = c("Home_Win_Pct" = "#1f78b4", "Away_Win_Pct" = "#33a02c"),
    labels = c("Wyjazd", "Dom")
  ) +
  labs(
    title = "Procent wygranych: Dom vs Wyjazd",
    x = "Procent wygranych (%)",
    y = "Drużyna",
    color = "Miejsce meczu"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray85")
  )







