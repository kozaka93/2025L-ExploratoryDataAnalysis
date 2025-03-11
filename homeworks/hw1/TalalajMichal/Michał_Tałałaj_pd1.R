library(dplyr)
library(tidyr)

df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Gender - Binarne
# Age - Ilorazowe
# Occupation - Nominalne
# Sleep.Duration - Ilorazowe
# Quality.of.Sleep - Uporządkowane*
# Physical.Activity.Level - Uporządkowane*
# * - ewentualnie przedziałowe w zależności czy dopuszczamy odejmowanie tych wartości
# BMI.Category - Uporządkowane
# Sleep.Disorder - Nominalne


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

df %>% group_by(BMI.Category) %>% summarise(mean_heart_rate = mean(Heart.Rate)) %>%
  arrange(-mean_heart_rate)

# Rozwiązanie
# 1 Obese                    84.3
# 2 Normal Weight            71.3
# 3 Overweight               70.9
# 4 Normal                   68.7


########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

df %>% filter(Stress.Level < 7) %>% group_by(Occupation) %>% 
  summarise(mean_sleep_duration = mean(Sleep.Duration)) %>%
  arrange(-mean_sleep_duration)  %>% head(1)
  
# Rozwiązanie
# 1 Engineer - 8.05

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 
df %>% group_by(Occupation) %>% summarise(Sleep_disorder_count = n()) %>% 
  arrange(-Sleep_disorder_count) %>% head(3)

# Rozwiązanie
# 1 Nurse                        73
# 2 Doctor                       71
# 3 Engineer                     63

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

df %>% filter(BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>% summarise(Obese_count =n())%>% 
  summarize(
    Male_Obese = sum(Obese_count[Gender == "Male"], na.rm = TRUE),
    Female_Obese = sum(Obese_count[Gender == "Female"], na.rm = TRUE)
  ) %>% filter(Male_Obese > Female_Obese) %>% select(Occupation)
  
# Rozwiązanie
# 1 Doctor              
# 2 Lawyer              
# 3 Sales Representative
# 4 Salesperson         
# 5 Software Engineer 

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

df1 <- df %>% separate(col = Blood.Pressure, into = c("Systolic", "Diastolic"), sep = "/") %>% 
  group_by(Occupation) %>%  mutate(count = n()) %>% filter(count > 20)

df1 %>% group_by(Occupation) %>%
  mutate(Pressure_diff = as.integer(Systolic) - as.integer(Diastolic)) %>%
  summarise(mean_pressure_diff = mean(Pressure_diff)) %>% arrange(-mean_pressure_diff)

# wyszło, że prawnicy
df %>% filter(Occupation == "Lawyer") %>% group_by(Quality.of.Sleep) %>% 
  summarize(
    Mean_sleep_length = mean(Sleep.Duration),
    Median_sleep_length = median(Sleep.Duration)
  )

# Rozwiązanie - top 3 zawody

# 1 Salesperson               45  
# 2 Lawyer                    45.0
# 3 Nurse                     44.8

# Rozwiązanie - średnia i mediana

# Quality.of.Sleep   Mean_sleep_length     Median_sleep_length
# 7                  7.18                  7.1
# 8                  7.44                  7.3

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2).



top3 <- df %>% group_by(Occupation) %>%
  summarise(mean_physical_activity= mean(Physical.Activity.Level)) %>% 
  arrange(-mean_physical_activity) %>%  head(3)

# Rozwiązanie - 3 zawody
# 1 Nurse                                  78.6
# 2 Lawyer                                 70.4
# 3 Accountant                             58.1

mean_ativity <- mean(df$Physical.Activity.Level)

df %>% mutate(Age_Group = ifelse(Age >= 50, "50+", "<50")) %>%
  group_by(Occupation, Age_Group) %>%
  summarise(new_mean = mean(Physical.Activity.Level))%>% 
  filter(Occupation %in% c("Nurse", "Lawyer", "Accountant"))%>% 
  mutate(physical_activity_diffrence = new_mean - mean_ativity) %>%
  select(-new_mean)

# Rozwiązanie - TE ZADANIE MOZNA INTERPRETOWAC NA 5 ROZNYCH SPOSOBOW
# NIE WIEM O KTORE CHODZI

# Occupation    Age_Group  physical_activity_diffrence
# Accountant    50+        -14.2 
# Accountant    <50         1.47
# Lawyer        <50         11.3 
# Nurse         50+         21.4 
# Nurse         <50         13.0

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

df2 <- df %>% mutate(Age_group = cut(Age, breaks = seq(0, 60, by = 10))) %>% 
  group_by(Occupation, Gender, Age_group) %>% mutate(Count = n()) %>% 
  group_by(Age_group, Gender) %>% slice_max(order_by = Count, n = 1)

df2 %>% select(Age_group, Gender, Occupation, Count) %>% distinct()

# 'W tak utworzonych grupach' czyli w tych końcowych czy w kubełkach???
df2 %>% group_by(Age_group, Gender, Occupation) %>% 
  summarise(
    Stress_mean = mean(Stress.Level),
    Stress_median = median(Stress.Level),
    Stress_deviation = sd(Stress.Level),
  )

# Rozwiązanie - pierwszy kawałek

# Age_group   Gender  Occupation    Count
# 1 (20,30]   Female  Nurse           4
# 2 (20,30]   Male    Doctor         23
# 3 (30,40]   Female  Accountant     30
# 4 (30,40]   Male    Doctor         42
# 5 (40,50]   Female  Teacher        29
# 6 (40,50]   Female  Nurse          29
# 7 (40,50]   Male    Salesperson    32
# 8 (50,60]   Female  Nurse          37

# Rozwiązanie - drugi kawałek

# Age_group   Gender  Occupation    Stress_mean   Stress_median   Stress_deviation
# 1 (20,30]   Female  Nurse              7                7            0    
# 2 (20,30]   Male    Doctor             6.70             6            0.974
# 3 (30,40]   Female  Accountant         4.07             4            0.583
# 4 (30,40]   Male    Doctor             7.19             8            0.994
# 5 (40,50]   Female  Nurse              8                8            0    
# 6 (40,50]   Female  Teacher            4.34             4            0.897
# 7 (40,50]   Male    Salesperson        7                7            0    
# 8 (50,60]   Female  Nurse              3.54             3            1.45 

