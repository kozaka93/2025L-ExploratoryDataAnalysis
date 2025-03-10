# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowe, binarne
# Age - ilościowe, ilorazowe
# Occupation - jakościowe, nominalne
# Sleep.Duration - ilościowe, ilorazowe
# Quality.of.Sleep - jakościowe, uporządkowane
# Physical.Activity.Level - ilościowe, ilorazowe
# BMI.Category - jakościowe, uporządkowane
# Sleep.Disorder - jakościowe, nominalne


########### Zad 2 (0.5pkt) ###########
# Jakie mają średnie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(Heart.Rate, BMI.Category) %>% 
  group_by(BMI.Category) %>% 
  summarise(average_HR = mean(Heart.Rate)) %>% 
  arrange(-average_HR)

# Wybieramy interesujące nas kolumny z df, grupujemy ludzi po kategori BMI, do której należą, następnie dla każdej
# grupy obliczamy średnie tętno za pomocą summarise i sortujemy malejąco według średniego tętna. Najwyższe średnie
# tętno jest dla osób otyłych

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Occupation, Sleep.Duration, Stress.Level) %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(Average_SD = mean(Sleep.Duration)) %>% 
  arrange(-Average_SD) %>% 
  select(Occupation) %>% 
  head(1)

# Najwięcej śpią inżynierzy, jeżeli ich poziom stresu jest poniżej 7

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Occupation, Sleep.Disorder) %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

# Osoby z dowolnym zaburzeniem snu najczęściej pracują jako pielęgniarki, nauczyciele i sprzedawcy

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Occupation, Gender, BMI.Category) %>% 
  filter(BMI.Category == "Obese") %>%
  mutate(is_male = ifelse(Gender == "Male",1,0)) %>% 
  mutate(is_female = ifelse(Gender == "Female",1,0)) %>% 
  group_by(Occupation) %>% 
  summarise(males = sum(is_male), females = sum(is_female)) %>% 
  filter(males > females) %>% 
  select(Occupation)

# Zawody, w których jest więcej otyłych mężczyzn niż otyłych kobiet to Doktor, Przedstawiciel sprzedaży,
# nauczyciel i inżynier oprogramowania

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Occupation, Blood.Pressure) %>% 
  separate(Blood.Pressure, into = c("sk","roz"), sep = "/", convert = TRUE) %>% 
  group_by(Occupation) %>% 
  summarise(count = n(), average_BPD = mean(sk-roz)) %>% 
  filter(count > 20) %>% 
  arrange(-average_BPD)

df %>% 
  select(Occupation, Sleep.Duration, Quality.of.Sleep) %>% 
  filter(Occupation == "Lawyer") %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(average_duration = mean(Sleep.Duration), median_duration = median(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Occupation, Physical.Activity.Level) %>% 
  group_by(Occupation) %>% 
  summarise(average_activity = mean(Physical.Activity.Level)) %>% 
  arrange(-average_activity) %>% 
  head(3)

average_HRD_full = mean(df$Heart.Rate)
df %>% 
  filter(Occupation == "Nurse"|Occupation=="Lawyer"|Occupation=="Accountant") %>% 
  mutate(age_lt50 = ifelse(Age<50, "Grupa 2", "Grupa 1")) %>% 
  group_by(age_lt50) %>% 
  summarise(mean_HR = mean(Heart.Rate)) %>% 
  mutate(diff = average_HRD_full - mean_HR)
  
########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  mutate(age_bracket = cut(Age, breaks = seq(0, 60, by = 10), right = FALSE)) %>% 
  group_by(Gender, age_bracket, Occupation) %>%
  summarise(count = n()) %>% 
  group_by(age_bracket, Gender) %>% 
  slice_max(order = count)

df %>% 
  mutate(age_bracket = cut(Age, breaks = seq(0, 60, by = 10), right = FALSE)) %>% 
  group_by(age_bracket) %>% 
  summarise(mean_sl = mean(Stress.Level), med_sl = median(Stress.Level), sd_sl = sd(Stress.Level))
