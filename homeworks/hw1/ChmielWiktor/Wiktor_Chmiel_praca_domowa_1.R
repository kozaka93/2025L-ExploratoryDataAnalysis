# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, binarna
# Age - ilościowa, ilorazowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep - jakościowa, porządkowa
# Physical.Activity.Level - ilościowa, ilorazowa
# BMI.Category - jakościowa, porządkowa
# Sleep.Disorder - jakościowa, nominala


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(BMI.Category) %>% 
  summarise(mean.Heart.Rate = mean(Heart.Rate, na.rm = TRUE)) %>% 
  arrange(desc(mean.Heart.Rate))



df %>% 
  group_by(BMI.Category) %>% 
  summarise(mean.Heart.Rate = mean(Heart.Rate, na.rm = TRUE)) %>% 
  arrange(desc(mean.Heart.Rate)) %>% 
  head(1)

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(mean.Sleep.Duration = mean(Sleep.Duration)) %>% 
  arrange(desc(mean.Sleep.Duration)) %>% 
  head(1)


########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarise(n()) %>% 
  arrange(desc(`n()`)) %>% 
  head()

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = Gender, values_from = n, values_fill = 0) %>% 
  filter(Male > Female) %>% 
  select(Occupation)
  


########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: 

df_bonus <- df %>% 
  mutate(cis_skr = as.integer(substr(Blood.Pressure, 1, 3)),
         cis_roz = as.integer(substr(Blood.Pressure, 5, 6)),
         roznica = cis_skr - cis_roz)

zawody <- df_bonus %>% 
  group_by(Occupation) %>%
  summarise(
    liczba_osob = n(),
    srednia_roznica = mean(roznica)) %>%
  filter(liczba_osob > 20) %>%
  arrange(desc(srednia_roznica)) %>%
  head(3)

res <- df_bonus %>% 
  filter(Occupation == zawody$Occupation[2]) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(srednia = mean(roznica), mediana = median(roznica))
  
########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: 

part1 <- df_bonus %>% 
  group_by(Occupation) %>% 
  summarise(sredni_ruch = mean(Physical.Activity.Level)) %>% 
  arrange(desc(sredni_ruch)) %>% 
  head(3)
  
df_grup <- df_bonus %>% 
  mutate(grupa = case_when(
    Age >= 50 ~ 1,
    TRUE ~ 2
  ))

srednie_wszytkich <- mean(df_bonus$roznica)

part2 <- df_grup %>% 
  filter(Occupation %in% part1$Occupation) %>% 
  group_by(grupa) %>% 
  summarise(sr_tetno = mean(roznica)) %>% 
  mutate(roznica_miedzy_wszytkimi = abs(sr_tetno - srednie_wszytkich))




########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: 
min_age <- min(df$Age)
max_age <- max(df$Age)
# pomaga znależć ilość kategori


zad_8_1 <- df %>% 
  mutate(kategoria = case_when(
    Age < 30 ~ "grupa_3",
    Age < 40 ~ "grupa_4",
    Age < 50 ~ "grupa_5",
    Age < 60 ~ "grupa_6"
  ))


mężczyźni <- zad_8_1 %>% 
  group_by(Gender, kategoria, Occupation) %>% 
  summarise(n = n()) %>% 
  arrange(kategoria, Gender, desc(n)) %>% 
  group_by(kategoria, Gender) %>%
  filter(n == max(n)) %>% 
  ungroup() %>% 
  select(kategoria, Gender, Occupation, n)
  

