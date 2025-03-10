# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")

View(df)

########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa binarna
# Age - ilościowa ilorazowe
# Occupation - jakościowa nominalna
# Sleep.Duration - ilościowa ilorazowa
# Quality.of.Sleep - ilościowa przedziałowa
# Physical.Activity.Level - ilościowa ilorazowa
# BMI.Category - jakościowa uporządkowana
# Sleep.Disorder - jakościowa naminalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  group_by(BMI.Category) %>% 
  summarise(mean.Heart.Rate = mean(Heart.Rate)) %>% 
  arrange(-mean.Heart.Rate) 

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(mean.Sleep.Duration = mean(Sleep.Duration)) %>% 
  arrange(-mean.Sleep.Duration)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>% filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% summarise(number = n()) %>% arrange(-number)

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  group_by(Occupation, Gender, BMI.Category) %>% 
  filter(BMI.Category == "Obese") %>% summarise(n()) %>% ifelse()%>% View()

df %>%
  filter(BMI.Category == "Obese") %>%
  group_by(Occupation, Gender) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = Gender, values_from = count, values_fill = 0) %>%
  filter(Male > Female)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  mutate(BP_Diff = as.numeric(sub("/.*", "", Blood.Pressure)) - 
           as.numeric(sub(".*/", "", Blood.Pressure))) %>% 
  group_by(Occupation) %>% 
  summarise(count = n(), mean.BP = mean(BP_Diff)) %>% 
  filter(count > 20) %>% 
  arrange(-mean.BP) %>% 
  head(3)

second_occ <- df %>% 
  mutate(BP_Diff = as.numeric(sub("/.*", "", Blood.Pressure)) - 
           as.numeric(sub(".*/", "", Blood.Pressure))) %>% 
  group_by(Occupation) %>% 
  summarise(count = n(), mean.BP = mean(BP_Diff)) %>% 
  filter(count > 20) %>% 
  arrange(-mean.BP) %>% 
  slice(2) %>%
  pull(Occupation) 

df %>% 
  filter(Occupation == second_occ) %>% group_by(Quality.of.Sleep) %>% 
  summarise(mean.Sleep.Duration = mean(Sleep.Duration), median.Sleep.Duration = median(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
df %>% group_by(Occupation) %>% 
  summarise(mean.Physical.Activity.Level = mean(Physical.Activity.Level)) %>% 
  arrange(-mean.Physical.Activity.Level) %>% head(3)

top_ocupations <- df %>% group_by(Occupation) %>% 
  summarise(mean.Physical.Activity.Level = mean(Physical.Activity.Level)) %>% 
  arrange(-mean.Physical.Activity.Level) %>% head(3) %>% pull(Occupation)

df %>% 
  filter(Occupation %in% top_ocupations) %>%
  mutate(is.50 = ifelse(Age >= 50, "1", "2")) %>% 
  group_by(Occupation, is.50) %>% 
  summarise(mean.Heart.Rate = mean(Heart.Rate))

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  mutate(Age.Group = cut(Age, breaks = seq(0, 100, 10), right = FALSE)) %>% 
  group_by(Age.Group, Gender, Occupation) %>% 
  summarise(count = n()) %>% 
  arrange(Age.Group, -count) 

top_occupations <- df %>%
  mutate(Age.Group = cut(Age, breaks = seq(0, 100, 10), right = FALSE)) %>% 
  group_by(Age.Group, Gender, Occupation) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(Age.Group, Gender, -count) %>%
  group_by(Age.Group, Gender) %>%
  slice(1) %>% 
  select(-count)

top_occupations %>%
  pivot_wider(names_from = Gender, values_from = Occupation)

stress_stats <- df %>%
  mutate(Age.Group = cut(Age, breaks = seq(0, 100, 10), right = FALSE)) %>% 
  group_by(Age.Group, Gender) %>%
  summarise(
    mean.Stress = mean(Stress.Level, na.rm = TRUE),
    median.Stress = median(Stress.Level, na.rm = TRUE),
    sd.Stress = sd(Stress.Level, na.rm = TRUE),
    .groups = "drop"
  )

left_join(top_occupations, stress_stats, by = c("Age.Group", "Gender"))
