# Ładowanie pakietów
library(dplyr)
library(tidyr)


# Ładowanie danych
#getwd()
#setwd("C:/Users/Krystian/Desktop/Programowanie/Eksploracja danych")
df <- read.csv("data.csv")



########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakosciowa binarna 
# Age - ilosciowa ilorazowa 
# Occupation - jakosciowa nominalna
# Sleep.Duration - ilosciowa ilorazowa
# Quality.of.Sleep - ilosciowa ilorazowa
# Physical.Activity.Level - ilosciowa ilorazowa
# BMI.Category - jakosciowa uporzadkowana
# Sleep.Disorder - jakosciowa nominalna

#str(df)
#summary(df)
#df$Blood.Pressure


########### Zad 2 (0.5pkt) ###########
#1.)
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? 
#2.)
#Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
#1.)
df %>% group_by(BMI.Category) %>% summarise(Mean.Heart.Rate = mean(Heart.Rate, na.rm = TRUE)) %>%
  arrange(-Mean.Heart.Rate)

#2.)
df %>% group_by(BMI.Category) %>% summarise(Mean.Heart.Rate = mean(Heart.Rate, na.rm = TRUE)) %>% 
  arrange(-Mean.Heart.Rate) %>% select(BMI.Category) %>% head(1)

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% filter(Stress.Level<7) %>% select(Occupation, Sleep.Duration) %>%
  group_by(Occupation) %>% summarise(Mean.Sleep.Duration = mean(Sleep.Duration, na.rm = TRUE)) %>% 
  arrange(-Mean.Sleep.Duration) %>% select(Occupation) %>% head(1)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>% select(Occupation, Sleep.Disorder) %>% filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% summarise(n = n()) %>% arrange(-n) %>% select(Occupation) %>% head()

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df %>% select(Gender, BMI.Category, Occupation) %>% filter(BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>% summarise(n = n()) %>% 
  pivot_wider(names_from = Gender, values_from = n, values_fill = 0) %>% 
  filter(Male > Female) %>% select(Occupation)

########### Zad 6 (1 pkt) ###########
#1.)
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
#2.)
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
#zawody <- df %>% select(Occupation, Blood.Pressure) %>% group_by(Occupation) %>% 
# summarise(n = n()) %>% filter(n >20) %>% select(Occupation) %>% head(3) %>% pull(Occupation)
#df %>% select(Occupation, Blood.Pressure) %>% 
#  filter(Occupation == zawody[1] | Occupation == zawody[2] |Occupation == zawody[3]) %>% 

#1.)
df %>% select(Occupation, Blood.Pressure) %>% group_by(Occupation) %>%
  filter(n()>20) %>% 
  mutate(skurcz = substr(Blood.Pressure, 1, 3), rozkurcz = substr(Blood.Pressure, 5, 6)) %>% 
  mutate(subs = as.numeric(skurcz) - as.numeric(rozkurcz)) %>% select(Occupation, subs) %>% 
  group_by(Occupation) %>% summarise(mean = mean(subs)) %>% arrange(desc(mean)) %>% head(3) %>% 
  select(Occupation)

#2.)
y <- df %>% select(Occupation, Blood.Pressure) %>% group_by(Occupation) %>%
  filter(n()>20) %>% 
  mutate(skurcz = substr(Blood.Pressure, 1, 3), rozkurcz = substr(Blood.Pressure, 5, 6)) %>% 
  mutate(subs = as.numeric(skurcz) - as.numeric(rozkurcz)) %>% select(Occupation, subs) %>% 
  group_by(Occupation) %>% summarise(mean = mean(subs)) %>% arrange(desc(mean)) %>% 
  select(Occupation) %>% slice(2) %>% pull 
df %>% select(Occupation, Sleep.Duration, Quality.of.Sleep) %>% filter(Occupation == y) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(mediana = median(Sleep.Duration), srednia = mean(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
#1.)
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. 
#2.)
# Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
#1.)
zawody <- df %>% select(Occupation, Physical.Activity.Level) %>% group_by(Occupation) %>%  
  summarise(mean = mean(Physical.Activity.Level)) %>% arrange(desc(mean)) %>% 
  select(Occupation) %>% head(3) %>% pull(Occupation)

#2.)
x <- df %>% summarise(mean = mean(Heart.Rate)) %>% pull
df %>% select(Age, Heart.Rate, Occupation) %>% 
  mutate(age_over_50 = ifelse(Age >= 50, "Yes", "No")) %>% group_by(Occupation, age_over_50) %>% 
  summarise(mean = mean(Heart.Rate)) %>% mutate(subs_Heart_Rate = x - mean) %>% 
  select(Occupation, age_over_50, subs_Heart_Rate) %>% 
  filter(Occupation == zawody[1] | Occupation == zawody[2] |Occupation == zawody[3]) 

########### Zad 8 (1 pkt) ###########
#1.)
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
#2.)
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
#1.)
df %>% select(Age, Occupation, Gender, Stress.Level) %>% 
  mutate(Age_Group = cut(Age, breaks = seq(0, max(Age, na.rm = TRUE) + 10, by = 10), right = FALSE)) %>% 
  group_by(Age_Group, Gender, Occupation) %>% summarise(n = n()) %>%
  group_by(Age_Group, Gender) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>% 
  select(Age_Group, Gender, Occupation)

#2.)
df %>% select(Age, Stress.Level) %>% 
  mutate(Age_Group = cut(Age, breaks = seq(0, max(Age, na.rm = TRUE) + 10, by = 10), right = FALSE)) %>% 
  group_by(Age_Group) %>%
  summarise(
    mean = mean(Stress.Level, na.rm = TRUE),
    median = median(Stress.Level, na.rm = TRUE),
    sd = sd(Stress.Level, na.rm = TRUE)
  )
