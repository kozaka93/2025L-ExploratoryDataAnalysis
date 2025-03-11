# HW1 Michał Grzegory


# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, binarna
# Age - ilościowa, ilorazowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep - ilościowa, przedziałowa
# Physical.Activity.Level - ilościowa, przedziałowa
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder - jakościowa, nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

#średnie tętna
df %>% 
  group_by(BMI.Category) %>% 
  summarise(mean = mean(Heart.Rate))

#najwyższe średnie tętno
df %>% 
  group_by(BMI.Category) %>% 
  summarise(mean = mean(Heart.Rate)) %>% 
  arrange(-mean) %>% 
  head(1)

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Stress.Level < 7) %>% 
  select(Occupation, Sleep.Duration, Stress.Level) %>% 
  group_by(Occupation) %>% 
  summarise(mean = mean(Sleep.Duration)) %>% 
  arrange(-mean) %>% 
  head(1)
  

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
#pokażemy 5 najczęściej
df %>% 
  select(Occupation, Sleep.Disorder) %>% 
  filter(Sleep.Disorder != 'None') %>% 
  group_by(Occupation) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(5)

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(Occupation, Gender, BMI.Category) %>% 
  filter(BMI.Category == 'Obese') %>% 
  group_by(Occupation, Gender) %>% 
  summarise(Male = sum(ifelse(Gender == "Male", 1, 0)), 
            Female = sum(ifelse(Gender == "Female", 1, 0))) %>% 
  filter(Male > Female) %>% 
  select(Occupation, Male)
  

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

###
df %>% 
  mutate(skurczowe = as.numeric(sub("/.*", "", Blood.Pressure)),
         rozkurczowe = as.numeric(sub(".*/", "", Blood.Pressure)),
         roznica_cisnien = skurczowe - rozkurczowe) %>%
  group_by(Occupation) %>% 
  filter(n() > 20) %>%
  summarise(srednia_roznica= mean(roznica_cisnien, na.rm = TRUE), n = n()) %>% 
  arrange(desc(srednia_roznica)) %>%
  head(3)

###widzimy, że jest Lawyer z powyższego

df %>% 
  select(Occupation, Sleep.Duration, Quality.of.Sleep) %>% 
  filter(Occupation == "Lawyer") %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(srednia = mean(Sleep.Duration), mediana = median(Sleep.Duration))
  

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

zawody <- df %>% 
  select(Occupation, Physical.Activity.Level) %>% 
  group_by(Occupation) %>% 
  summarise(mean = mean(Physical.Activity.Level)) %>%
  arrange(-mean) %>% 
  head(3) %>% 
  pull(Occupation)

df %>%
  mutate(Age_Group = ifelse(Age >= 50, "50+", "50>")) %>%
  group_by(Age_Group) %>%
  summarise(srednie_t_wszyscy = mean(Heart.Rate, na.rm = TRUE),
            srednie_t_zawody = mean(Heart.Rate[Occupation %in% zawody], na.rm = TRUE)) %>%
  mutate(roznica_tetno = srednie_t_zawody - srednie_t_wszyscy)

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>%
  mutate(kubelek_wiek = cut(Age, breaks = seq(0, max(Age, na.rm = TRUE) + 10, by = 10), right = FALSE)) %>% 
  group_by(kubelek_wiek, Gender, Occupation) %>% 
  summarise(Count = n(), .groups = "drop") %>%
  arrange(kubelek_wiek, Gender, desc(Count)) %>%
  group_by(kubelek_wiek, Gender) %>%
  slice_max(Count, n = 1, with_ties = FALSE) %>%
  ungroup() -> top_zawody

#statystyki
stress_stats <- df %>%
  mutate(kubelek_wiek = cut(Age, breaks = seq(0, max(Age, na.rm = TRUE) + 10, by = 10), right = FALSE)) %>% 
  group_by(kubelek_wiek, Gender) %>%
  summarise(srednia_stres = mean(Stress.Level, na.rm = TRUE),
            mediana_stres = median(Stress.Level, na.rm = TRUE),
            odstnd_stres = sd(Stress.Level, na.rm = TRUE),
            .groups = "drop")

#wyniki
list(Zawody = top_zawody, Statystyki = stress_stats)






