# Ładowanie pakietów
library(dplyr)
library(tidyr)
# Ładowanie danych
df <- read.csv("data.csv")
########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, binarna
# Age - ilościowa, ilorazowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep - ilościowa, przedziałowa
# Physical.Activity.Level - iloścciowa, przedziałowa
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder - jakościowa, nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: 

df %>% 
  select(BMI.Category, Heart.Rate) %>% 
  group_by(BMI.Category) %>% 
  summarise(mean_heart_rate = mean(Heart.Rate, na.rm = TRUE))%>%
  arrange(desc(mean_heart_rate))

# Odpowiedź : dla kategorii Obese średnie tętno jest najwyższe.

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: 

 df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(mean_sleep =  mean(Sleep.Duration)) %>% 
  arrange(desc(mean_sleep)) %>% 
  head(1) %>% 
  select(Occupation)

# Odpowiedź : inżynier


########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: 

df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarise(Sleep.Disorder.Number = n()) %>% 
  arrange(desc(Sleep.Disorder.Number)) %>% 
  head(5) %>% 
  select(Occupation)

# Osoby z zaburzeniami snu najczęściej pracują jako pielęgniarz/pielęgniarka, 
# nauczyciel/nauczycielka, sprzedawca/sprzedawczyni, księgowy/księgowa i lekarz/lekarka.

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

df %>% 
  filter(BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>% 
  summarise(Count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = Gender, values_from = Count, values_fill = list(Count = 0)) %>%
  filter(Male > Female) %>%
  select(Occupation)
  
########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: 

zawody <- 
  df %>% 
  mutate(
    S = as.numeric(sub("/.*", "", Blood.Pressure)), 
    RS = as.numeric(sub(".*/", "", Blood.Pressure))) %>%
  mutate(Pressure.Diff = abs(S - RS)) %>%
  group_by(Occupation)%>% 
  summarise(Count = n(), Mean.Diff = mean(Pressure.Diff))%>% 
  filter(Count > 20) %>% 
  arrange(desc(Mean.Diff)) %>% 
  head(3) 

View(zawody)

# Zawody : SalesPerson, Lawyer, Nurse

df %>% 
  filter(Occupation == zawody$Occupation[2]) %>% 
  group_by(Sleep.Disorder) %>% 
  summarise(Mean.Sleep = mean(Sleep.Duration), Mediana.Sleep = median(Sleep.Duration))

# Sleep.Disorder      Mean.Sleep    Mediana.Sleep
# 1 Insomnia             7.4            7.4
# 2 None                 7.42           7.3
# 3 Sleep Apnea          7.3            7.4

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: 

zawody2 <- 
  df %>% 
  group_by(Occupation) %>% 
  summarise(Mean.Activity = mean(Physical.Activity.Level)) %>% 
  arrange(desc(Mean.Activity)) %>%
  head(3) %>% 
  select(Occupation)

View(zawody2)

df %>% 
  mutate(Mean.Heart.Rate.All = mean(Heart.Rate)) %>% 
  filter(Occupation %in% zawody2$Occupation) %>% 
  mutate(Age.Group = ifelse(Age >= 50, "group1", "group2")) %>%
  group_by(Occupation, Age.Group) %>% 
  summarise(Mean.Heart.Rate = mean(Heart.Rate, na.rm = TRUE), 
            Mean.Heart.Rate.All = first(Mean.Heart.Rate.All),
            Heart.Rate.Diff = abs(Mean.Heart.Rate.All - Mean.Heart.Rate), .groups = 'drop') %>% 
  pivot_wider(names_from = Age.Group, 
              values_from = c(Mean.Heart.Rate, Heart.Rate.Diff))

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: 

df %>% 
  mutate(Age.Bracket = case_when(
    Age >= 0 & Age < 10 ~ "[0,10)",
    Age >= 10 & Age < 20 ~ "[10,20)",
    Age >= 20 & Age < 30 ~ "[20,30)",
    Age >= 30 & Age < 40 ~ "[30,40)",
    Age >= 40 & Age < 50 ~ "[40,50)",
    Age >= 50 & Age < 60 ~ "[50,60)",
    Age >= 60 & Age < 70 ~ "[60,70)" )) %>% 
  group_by(Age.Bracket) %>%
  summarise(Mean.Stress.Level = mean(Stress.Level, na.rm = TRUE), Mediana.Stress.Level = median(Stress.Level, na.rm = TRUE),
            SD.Stress.Level = sd(Stress.Level, na.rm = TRUE), .groups = 'drop')
df %>% 
  mutate(Age.Bracket = case_when(
    Age >= 0 & Age < 10 ~ "[0,10)",
    Age >= 10 & Age < 20 ~ "[10,20)",
    Age >= 20 & Age < 30 ~ "[20,30)",
    Age >= 30 & Age < 40 ~ "[30,40)",
    Age >= 40 & Age < 50 ~ "[40,50)",
    Age >= 50 & Age < 60 ~ "[50,60)",
    Age >= 60 & Age < 70 ~ "[60,70)" )) %>% 
  group_by(Gender, Age.Bracket, Occupation) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Gender, Age.Bracket) %>%
  slice_max(order_by = Count, n = 1) %>%  
  select(Gender, Age.Bracket, Most.Popular.Occupation = Occupation) %>% 
  pivot_wider(names_from = Gender,values_from = Most.Popular.Occupation, names_prefix = 'Most.Popular.Occupation.')


