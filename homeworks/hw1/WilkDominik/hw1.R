# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")

########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa binarna
# Age - ilościowa ilorazowa
# Occupation - jakościowa nominalna
# Sleep.Duration - ilościowa ilorazowa
# Quality.of.Sleep - jakościowa uporządkowana
# Physical.Activity.Level - ilościowa ilorazowa
# BMI.Category - jakościowa uporządkowana
# Sleep.Disorder - jakościowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(BMI.Category) %>% 
  summarise(avg_heart_rate = mean(Heart.Rate, na.rm= TRUE)) %>% 
  arrange(-avg_heart_rate)

# Najwyższe średnie tętno mają osoby otyłe

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Stress.Level<7) %>% 
  group_by(Occupation) %>% 
  summarise(avg_sleep_duration = mean(Sleep.Duration, na.rm=TRUE)) %>% 
  arrange(-avg_sleep_duration)

#Wśród osób których poziom stresu jest poniżej 7 najwięcej średnio spią inżynierzy

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarise(amount = n()) %>% 
  arrange(-amount)

# Osoby z dowolnym zaburzeniem snu najczęściej pracują jako pielęgniarze/pielęgniarki

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = Gender, values_from = Count, values_fill = 0) %>% 
  filter(Male > Female)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Occupation, Blood.Pressure) %>% 
  separate(Blood.Pressure, into = c("Systolic", "Diastolic"), sep = "/", convert = TRUE) %>%
  mutate(Difference = Systolic - Diastolic) %>% 
  group_by(Occupation) %>% 
  mutate(Count = n()) %>% 
  filter(Count > 20) %>% 
  summarise(Avg_difference = mean(Difference)) %>% 
  arrange(-Avg_difference) %>% 
  top_n(3,Avg_difference)

df %>% 
  filter(Occupation == "Lawyer") %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(Avg_sleep_duration = mean(Sleep.Duration), Median = median(Sleep.Duration))
  

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  summarise(Avg_daily_steps = mean(Daily.Steps)) %>% 
  arrange(-Avg_daily_steps) %>% 
  top_n(3,Avg_daily_steps) 

df1 <- df %>% 
  filter(Occupation %in% c("Nurse", "Lawyer", "Accountant")) %>% 
  mutate(Age_50 = case_when(Age >= 50 ~ "Over or equal 50",
                            Age < 50 ~ "Under 50")) %>% 
  group_by(Age_50) %>% 
  summarise(Avg_heart_rate_selected = mean(Heart.Rate))

df %>% 
  mutate(Age_50 = case_when(Age >= 50 ~ "Over or equal 50",
                            Age < 50 ~ "Under 50")) %>% 
  group_by(Age_50) %>% 
  summarise(Avg_hear_rate_overall = mean(Heart.Rate)) %>% 
  inner_join(df1, by = "Age_50") %>% 
  mutate(Difference = Avg_heart_rate_selected - Avg_hear_rate_overall)
  


########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>%
  mutate(Age_Group = cut(Age, 
                         breaks = seq(0, 100, by = 10),  
                         right = FALSE,  
                         labels = paste0("[", seq(0, 90, by = 10), "-", seq(10, 100, by = 10), ")"))) %>% 
  group_by(Age_Group, Occupation) %>% 
  summarise(Count = n()) %>% 
  slice_max(Count, n = 1)

df  %>% 
  mutate(Age_Group = cut(Age, 
                         breaks = seq(0, 100, by = 10),  
                         right = FALSE,  
                         labels = paste0("[", seq(0, 90, by = 10), "-", seq(10, 100, by = 10), ")"))) %>% 
  group_by(Age_Group) %>% 
  summarise(Avg_stress_lvl = mean(Stress.Level), Median_stress_lvl = median(Stress.Level), Sd_stress_lvl = sd(Stress.Level))
  
  
