# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa binarna
# Age - ilościowa przedziałowa
# Occupation - jakościowa nominalna
# Sleep.Duration - ilościowa zliczeniowa
# Quality.of.Sleep - jakościowa uporządkowana
# Physical.Activity.Level - ilościowa ilorazowa
# BMI.Category - jakościowa uporządkowana
# Sleep.Disorder - jakościowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(BMI.Category) %>% 
  summarize(avg_HR = mean(Heart.Rate, na.rm=TRUE))

df %>% 
  group_by(BMI.Category) %>% 
  summarize(avg_HR = mean(Heart.Rate, na.rm=TRUE)) %>% 
  arrange(-avg_HR) %>% 
  top_n(1, BMI.Category) %>% 
  pull(BMI.Category)

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(avg_Sleep.Duration = mean(Sleep.Duration, na.rm = TRUE)) %>%
  arrange(-avg_Sleep.Duration) %>% 
  head(1) %>% 
  pull(Occupation)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(!(Sleep.Disorder == "None")) %>% 
  group_by(Occupation) %>%
  summarise(Number_of_insomniacs = n()) %>% 
  arrange(-Number_of_insomniacs) %>% 
  head(1) %>% 
  pull(Occupation)

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

Mezczyzni <- df %>% 
  filter(BMI.Category == "Obese" & Gender == "Male") %>% 
  group_by(Occupation) %>% 
  summarize(Mezczyzni = n())

df %>% 
  filter(BMI.Category == "Obese" & Gender == "Female") %>% 
  group_by(Occupation) %>% 
  summarize(Kobiety = n()) %>% 
  full_join(Mezczyzni, by = join_by(Occupation), values) %>% 
  replace(is.na(.), 0) %>% 
  filter(Mezczyzni > Kobiety) %>% 
  select(Occupation)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

Dziwne_zawody <- df %>% 
  mutate(BP_Diff = as.numeric(sub("/.*", "", Blood.Pressure)) - as.numeric(sub(".*/", "", Blood.Pressure))) %>% 
  group_by(Occupation) %>% 
  filter(n() > 20) %>% 
  summarise(avg_BP_diff = mean(BP_Diff, na.rm = TRUE), .groups = "drop") %>%
  arrange(-avg_BP_diff) %>%
  head(3) %>% 
  select(Occupation)

Dziwne_zawody

Drugi <- Dziwne_zawody %>% 
  slice(2) %>% 
  pull(Occupation)

Drugi

df %>%
  filter(Occupation == Drugi) %>%
  group_by(Quality.of.Sleep) %>%
  summarise(
    avg_Sleep_Duration = mean(Sleep.Duration, na.rm = TRUE),
    median_Sleep_Duration = median(Sleep.Duration, na.rm = TRUE),
    .groups = "drop"
  )

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  summarise(avg_Physical.Activity.Level = mean(Physical.Activity.Level, na.rm = TRUE)) %>% 
  arrange(-avg_Physical.Activity.Level) %>% 
  head(3) %>% 
  select(Occupation)

All_HR <- df %>% 
  mutate(Over_Under_50 = ifelse(Age >= 50, 1, 2)) %>% 
  group_by(Over_Under_50) %>% 
  summarise(avg_HR = mean(Heart.Rate, na.rm= TRUE))

df %>% 
  mutate(Over_Under_50 = ifelse(Age >= 50, 1, 2)) %>%
  filter(Occupation == "Nurse" | Occupation == "Lawyer" | Occupation == "Accountant") %>% 
  group_by(Over_Under_50) %>% 
  summarise(avg_HR_sporty = mean(Heart.Rate, na.rm= TRUE)) %>% 
  inner_join(All_HR, by = join_by(Over_Under_50)) %>% 
  mutate(HR_difference = avg_HR - avg_HR_sporty) %>% 
  select(Over_Under_50, HR_difference)

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  arrange(-Age) %>% 
  select(Age) %>% 
  head(1) %>% 
  pull(Age)

Popularne_zawody <- df %>% 
  mutate(Kubelki = case_when(Age >= 50 ~ "[50, 60)",
                             Age >= 40 ~ "[40, 50)",
                             Age >= 30 ~ "[30, 40)",
                             Age >= 20 ~ "[20, 30)",
                             Age >= 10 ~ "[10, 20)",
                             TRUE ~ "[0,10)")) %>% 
  group_by(Kubelki, Gender, Occupation) %>% 
  summarise(Liczba = n(), .groups = "drop") %>% 
  group_by(Kubelki, Gender) %>%
  slice_max(Liczba, n = 1, with_ties = FALSE) %>%
  select(-Liczba)

Popularne_zawody

df %>%
  mutate(Kubelki = case_when(Age >= 50 ~ "[50, 60)",
                             Age >= 40 ~ "[40, 50)",
                             Age >= 30 ~ "[30, 40)",
                             Age >= 20 ~ "[20, 30)",
                             Age >= 10 ~ "[10, 20)",
                             TRUE ~ "[0,10)")) %>% 
  group_by(Kubelki, Gender) %>%
  summarise(
    avg_Stress = mean(Stress.Level, na.rm = TRUE),
    median_Stress = median(Stress.Level, na.rm = TRUE),
    odchylenie_Stress = sd(Stress.Level, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  full_join(Popularne_zawody, by = join_by(Kubelki, Gender)) %>% 
  relocate(Occupation, .after = Gender)
