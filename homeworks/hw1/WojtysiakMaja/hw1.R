# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
df <- read.csv("/Users/majawojtysiak/Desktop/data.csv")

########### Zad 1 (1pkt) ########### ok
# Określ jakiego typu są poniższe zmienne w zbiorze danych.


# Gender - jakościowa, binarna
# Age - ilościowa, przedziałowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep - jakościowa, uporządkowana
# Physical.Activity.Level - ilościowa, ilorazowa
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder -jakościowa, nominalna


########### Zad 2 (0.5pkt) ########### ok
# Jakie mają średnie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

df %>% 
  group_by(BMI.Category) %>% 
  summarise(mean = mean(Heart.Rate, na.rm = TRUE)) %>%
  arrange(-mean) %>%
  top_n(1, mean)

########### Zad 3 (0.5pkt) ########### ok
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

df %>% 
  filter( Stress.Level < 7) %>% 
  group_by(Occupation) %>%
  summarise(mean_sleep = mean(Sleep.Duration, na.rm=TRUE)) %>%
  arrange(-mean_sleep) %>%
  top_n(1, mean_sleep)

########### Zad 4 (0.5pkt) ########### ok
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

df %>% 
  filter( Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>%
  summarise(n = n()) %>%
  arrange(-n) 

########### Zad 5 (0.5pkt) ########### ok
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?
library(tidyr)
df %>% 
  filter( BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = n, values_fill = 0) %>%
  filter(Male > Female)

########### Zad 6 (1 pkt) ########### ok
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

df %>%
  group_by(Occupation) %>%
  mutate(Blood.Pressure_1 = as.numeric(substring(Blood.Pressure, 1, 3))) %>%
  mutate(Blood.Pressure_2 = as.numeric(substring(Blood.Pressure, 5, 6))) %>%
  mutate(Difference = Blood.Pressure_1 - Blood.Pressure_2) %>%
  summarise(mean = mean(Difference), n = n()) %>%
  filter( n > 20) %>%
  arrange(-mean) %>%
  slice(2)

df %>%
  filter(Occupation == "Lawyer") %>%
  group_by(Quality.of.Sleep) %>%
  summarise(mean = mean(Sleep.Duration), median = median(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

df_7 <- df[, c("Occupation", "Age", "Physical.Activity.Level")]%>%
  group_by(Occupation) %>%
  mutate(mean_activity = mean(Physical.Activity.Level)) %>%
  select(c("Occupation", "mean_activity")) %>%
  arrange(desc(mean_activity)) %>%
  distinct()%>%
  head(3)

job_1 <- df_7$Occupation[1]
job_2 <- df_7$Occupation[2]
job_3 <- df_7$Occupation[3]

heart_rate <- df[, c("Occupation", "Age", "Heart.Rate")] %>%
  filter(Occupation == job_1 |Occupation == job_2 |Occupation == job_3) %>%
  mutate(group = ifelse(Age<50, "<50", "50+")) %>%
  group_by(group) %>%
  summarise(average_heart_rate2 = mean(Heart.Rate))
  
total_heart_rate <- df[, c("Heart.Rate", "Age")] %>%
  mutate(group = ifelse(Age< 50, "<50", "50+")) %>%
  group_by(group) %>%
  summarise(average_heart_rate_all = mean(Heart.Rate))

compare <- left_join(heart_rate, total_heart_rate, by="group" ) %>%
  mutate(difference = abs(average_heart_rate2 - average_heart_rate_all))
########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

df_8 <- df[, c("Age", "Gender", "Occupation", "Stress.Level")]%>%
  mutate(Age_group = case_when(Age < 10 ~ "[0, 10)",
                           Age >= 10 & Age < 20 ~ "[10, 20)", 
                           Age >= 20 & Age < 30 ~ "[20, 30)",
                           Age >= 30 & Age < 40 ~ "[30, 40)",
                           Age >= 40 & Age < 50 ~ "[40, 50)",
                           TRUE ~ "[50, 60)")) %>%
  group_by(Age_group, Gender, Occupation) %>%
  summarise(count = n()) %>%
  arrange(Age_group, Gender, desc(count))%>%
  slice_max(count, n=1) %>%
  select("Age_group", "Gender", "Occupation" ) %>%
  rename(Most_popular_occupation = Occupation)


