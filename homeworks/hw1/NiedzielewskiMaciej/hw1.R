# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, nominalna
# Age - ilościowa, przedziałowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep - porządkowa
# Physical.Activity.Level - ilościowa, ilorazowa
# BMI.Category - porządkowa
# Sleep.Disorder - jakościowa, nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(`BMI Category`,`Heart Rate`) %>% 
  group_by(`BMI Category`) %>% 
  summarise(average_hear_rate = mean(`Heart Rate`)) %>% 
  View()

df %>% 
  select(`BMI Category`,`Heart Rate`) %>% 
  group_by(`BMI Category`) %>% 
  summarise(average_hear_rate = mean(`Heart Rate`)) %>% 
  slice_max(average_hear_rate, n = 1) %>% 
  View()

# Najwyższe tętno występuje przy kategorii BMI "Obese"

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(`Occupation`, `Sleep Duration`, `Stress Level`) %>% 
  filter(`Stress Level` < 7) %>% 
  group_by(Occupation) %>% 
  summarise(average_sleep = mean(`Sleep Duration`)) %>% 
  slice_max(average_sleep, n = 1) %>% 
  View()

# Odpowiedź: Engineer 


########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(Occupation, `Sleep Disorder`) %>% 
  filter(`Sleep Disorder` != "None") %>% 
  group_by(Occupation) %>% 
  summarise(sleep_disorder_count = n()) %>% 
  arrange(desc(sleep_disorder_count)) %>% 
  head(5)

# Occupation      n
# <chr>       <int>
#   1 Nurse          64
# 2 Teacher        31
# 3 Salesperson    30
# 4 Accountant      7
# 5 Doctor          7

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(Occupation, `BMI Category`, Gender) %>% 
  filter(`BMI Category` == "Obese") %>% 
  group_by(Occupation, Gender) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = Gender, values_from = count, values_fill = list(count = 0)) %>% 
  filter(Male > Female) %>% 
  View()

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>%
  group_by(Occupation) %>%
  separate(`Blood Pressure`, into = c("Systolic", "Diastolic"), sep = "/", convert = TRUE) %>%
  summarise(Count = n(), `Mean Difference` = mean(Systolic - Diastolic)) %>% 
  arrange(desc(`Mean Difference`)) %>% 
  filter(Count > 20) %>% 
  head(3) 

df %>% 
  select(Occupation, `Sleep Duration`, `Quality of Sleep`) %>% 
  filter(Occupation == "Lawyer") %>% 
  group_by(`Quality of Sleep`) %>% 
  summarise(`Sleep Mean` = mean(`Sleep Duration`), `Sleep Median` = median(`Sleep Duration`)) %>% 
  View()


########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
jobs <-df %>% 
  group_by(Occupation) %>% 
  summarise(`Mean Movement` = mean(`Physical Activity Level`)) %>% 
  arrange(desc(`Mean Movement`)) %>% 
  head(3) %>% 
  pull(Occupation)

mean_heart_rate <- df %>% 
  summarise(mean_heart_rate = mean(`Heart Rate`, na.rm = TRUE)) %>% 
  pull(mean_heart_rate)

df %>% 
  filter(Occupation %in% jobs) %>% 
  mutate(Age_Group = ifelse(!Age < 50, ">=50", "<50")) %>%
  group_by(Age_Group) %>% 
  summarise(`Mean Heart Rate` = mean(`Heart Rate`),
            Diff_from_overall = `Mean Heart Rate` - mean_heart_rate) %>% 
  View()
  
########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>%
  mutate(Age_Category = cut(Age, breaks = seq(0, 100, by = 10), 
                            right = FALSE, 
                            labels = paste0("[", seq(0, 90, by = 10), ", ", seq(10, 100, by = 10), ")"))) %>% 
  group_by(Age_Category, Gender, Occupation) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(Age_Category, Gender) %>%
  slice_max(Count, n = 1) %>% 
  View()

df %>%
  mutate(Age_Category = cut(Age, breaks = seq(0, 100, by = 10), 
                            right = FALSE, 
                            labels = paste0("[", seq(0, 90, by = 10), ", ", seq(10, 100, by = 10), ")"))) %>% 
  select(Age_Category, `Stress Level`) %>% 
  group_by(Age_Category) %>% 
  summarise(`Mean Stress Level` = mean(`Stress Level`), `Median Stress Level` = median(`Stress Level`), `Standard Deviation` = sd(`Stress Level`)) %>% 
  View()
  

