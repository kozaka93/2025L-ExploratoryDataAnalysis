# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowe binarne
# Age - ilościowe ilorazowe
# Occupation - jakościowe nominalne
# Sleep.Duration - ilościowe ilorazowe
# Quality.of.Sleep - jakościowe uporządkowane
# Physical.Activity.Level - ilościowe ilorazowe
# BMI.Category - jakościowe uporządkowane
# Sleep.Disorder - jakościowe nominalne

########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(BMI.Category) %>% 
  summarise(mean_pressure = mean(Heart.Rate, na.rm = TRUE)) %>% 
  arrange(desc(mean_pressure))

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  filter(Stress.Level<7) %>% 
  summarise(mean_sleep = mean(Sleep.Duration, na.rm = TRUE)) %>% 
  arrange(desc(mean_sleep)) %>% 
  head(1)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarise(how_many=n()) %>% 
  arrange(desc(how_many))

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(BMI.Category == "Obese") %>% 
  group_by(Occupation) %>% 
  summarise(male = sum(Gender == "Male"),
            female = sum(Gender == "Female")) %>% 
  filter(male > female)
  
########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df%>%
  separate(Blood.Pressure, into = c("up","down"),sep="/",convert=TRUE) %>% 
  mutate(diff = up-down) %>% 
  group_by(Occupation) %>% 
  summarise(liczba = n(), mean_diff = mean(diff)) %>% 
  filter(liczba > 20) %>% 
  arrange(desc(mean_diff)) %>% 
  head(3)

df %>% 
  filter(Occupation == "Lawyer") %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(mean_time = mean(Sleep.Duration), median_time = median(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  summarise(mean_move = mean(Physical.Activity.Level)) %>% 
  arrange(desc(mean_move)) %>% 
  head(3)

mean_heart = mean(df$Heart.Rate)

df %>%
  filter(Occupation == "Nurse" | Occupation == "Lawyer" | Occupation == "Accountant") %>% 
  mutate(isold = case_when(Age >= 50 ~ "group1", TRUE ~ "group2")) %>% 
  group_by(isold) %>% 
  summarise(diff_heart = mean_heart - mean(Heart.Rate))
  
########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  mutate(generacja = case_when(Age < 10 ~ '[0,10)',
                               Age < 20 ~ '[10,20)',
                               Age < 30 ~ '[20,30)',
                               Age < 40 ~ '[30,40)',
                               Age < 50 ~ '[40,50)',
                               Age < 60 ~ '[50,60)')) %>% 
  group_by(generacja, Gender, Occupation) %>% 
  summarise(ilosc = n()) %>% 
  group_by(generacja, Gender) %>% 
  slice_max(order = ilosc)

df %>%
  mutate(generacja = case_when(Age < 10 ~ '[0,10)',
                               Age < 20 ~ '[10,20)',
                               Age < 30 ~ '[20,30)',
                               Age < 40 ~ '[30,40)',
                               Age < 50 ~ '[40,50)',
                               Age < 60 ~ '[50,60)')) %>% 
  group_by(generacja) %>% 
  summarise(mean_stress = mean(Stress.Level), median_stress = median(Stress.Level), os_stress = sd(Stress.Level))
  