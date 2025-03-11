# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")

########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------
str(df)
# Gender - jakosciowe, binarne
# Age - ilosciowe, ilorazowe
# Occupation - jakosciowe, nominalne
# Sleep.Duration - ilosciowe, zliczeniowe
# Quality.of.Sleep - jakosciowe, uporzadkowane
# Physical.Activity.Level - ilosciowe, ilorazowe
# BMI.Category - jakosciowe, uporzadkowane
# Sleep.Disorder - jakosciowe, nominalne


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

Zad2 <- df %>% 
  group_by(BMI.Category) %>% 
  summarise(avg_heart_rate = mean(Heart.Rate, na.rm = TRUE)) %>% 
  arrange(desc(avg_heart_rate))

Zad2
# srednie tetno jest najwieksze dla kategorii "Obese"

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
Zad3 <- df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(avg_sleep_dur = mean(Sleep.Duration, na.rm = TRUE)) %>% 
  arrange(desc(avg_sleep_dur))

Zad3
# najwiecej spia ludzie o zawodzie "Engineer"

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

Zad4 <- df %>% 
  filter(Sleep.Disorder != 'None') %>% 
  group_by(Occupation) %>% 
  summarise(nr = n()) %>% 
  arrange(desc(nr))

Zad4
# w zawodzie "Nurse"

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

Zad5 <- df %>% 
  filter(BMI.Category == 'Obese') %>% 
  group_by(Occupation, Gender) %>%         
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = Gender, values_from = count, values_fill =0) %>% 
  filter(Male > Female)

Zad5

# w zawodach "Doctor", "Sales Representative", "Software Engineer", "Teacher"

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

Zad6 <- df %>%
  mutate(skurczowe = as.numeric(sapply(strsplit(Blood.Pressure, "/"), "[", 1)),
        rozkurczowe = as.numeric(sapply(strsplit(Blood.Pressure, "/"), "[", 2)),
        avg_bp_diff = skurczowe-rozkurczowe) %>% 
  group_by(Occupation) %>% 
  filter(20 < n()) %>%
  summarise(avg = mean(avg_bp_diff, na.rm = TRUE)) %>% 
  arrange(desc(avg)) %>% 
  slice(1:3)
Zad6

Zad6_2 <- df %>% 
  filter(Occupation == "Lawyer") %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(avg_time = mean(Sleep.Duration),
            median_time = median(Sleep.Duration))
Zad6_2

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
Zad7 <- df %>% 
  group_by(Occupation) %>%
  summarise(avg_activity = mean(Physical.Activity.Level)) %>% 
  arrange(desc(avg_activity)) %>% 
  slice(1:3)
Zad7

Zad7_2 <- df %>% 
  filter(Occupation %in% Zad7$Occupation) %>% 
  mutate(age_category = ifelse(Age < 50, 2, 1)) %>%
  group_by(age_category) %>%
  summarise(avg_hr = mean(Heart.Rate)) %>%
  mutate(avg_hr_diff = mean(df$Heart.Rate)-avg_hr)

Zad7_2


########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

Zad8 <- df %>%
  mutate(Age_Bucket = cut(Age, breaks = seq(0, 200, by = 10), right = FALSE, include.lowest = TRUE)) %>% 
  group_by(Age_Bucket,Gender) %>%
  summarise(most_popular  = names(which.max(table(Occupation))),
            avg_stress = mean(Stress.Level),
            med_stress = median(Stress.Level),
            sd_stress = sd(Stress.Level), .groups='drop')
Zad8


  


