# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

str(df)

# Gender - character
# Age - integer
# Occupation - character
# Sleep.Duration - numeric
# Quality.of.Sleep -integer
# Physical.Activity.Level - integer
# BMI.Category - character
# Sleep.Disorder - character


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>%
  group_by(BMI.Category) %>%
  summarise(Average.Heart.Rate = mean(Heart.Rate)) %>%
  arrange(desc(Average.Heart.Rate)) %>%
  head(1) %>%
  select(BMI.Category)

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?


# Rozwiązanie: ------------------------------------------------------------

df %>%
  filter(Stress.Level < 7) %>%
  group_by(Occupation) %>%
  summarise(Average.Sleep.Duration = mean(Sleep.Duration)) %>%
  arrange(desc(Average.Sleep.Duration)) %>%
  head(1) %>%
  select(Occupation)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>%
  filter(Sleep.Disorder != "None") %>%
  group_by(Occupation) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>%
  group_by(Occupation, Gender)%>%
  summarise(Obese.Count = n()) %>%
  pivot_wider(names_from = Gender, values_from = Obese.Count) %>%
  replace(is.na(.), 0)%>%
  filter(Male > Female)%>%
  select(Occupation)
  

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>%
  mutate(Blood.Pressure1 = substr(Blood.Pressure, 1, 3), Blood.Pressure2 = substr(Blood.Pressure, 5, 6)) %>%
  mutate(Blood.Pressure.dif = as.integer(Blood.Pressure1) - as.integer(Blood.Pressure2))%>%
  group_by(Occupation)%>%
  summarise(Occupation.Count = n(), Av.Blood.Pressure.dif = mean(Blood.Pressure.dif))%>%
  filter(Occupation.Count > 20)%>%
  arrange(desc(Av.Blood.Pressure.dif)) %>%
  head(3)%>%
  select(Occupation)

df %>%
  filter(Occupation == "Lawyer")%>%
  group_by(Quality.of.Sleep)%>%
  summarise(Av.Sleep.Duration = mean(Sleep.Duration), Median.Sleep.Duration = median(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

top3zawody <- df %>%
  group_by(Occupation)%>%
  summarise(Av.Physical.Activity.Level = mean(Physical.Activity.Level))%>%
  arrange(desc(Av.Physical.Activity.Level))%>%
  head(3)%>%
  pull(Occupation)

df.top3zawody <- df %>%
  mutate(Group = case_when(
    Age < 50 ~ 'Group 2',
    TRUE ~ 'Group 1')) %>%
  filter(Occupation %in% top3zawody)%>%
  group_by(Group)%>%
  summarise(Av.Heart.Rate = mean(Heart.Rate))

df%>%
  mutate(Group = case_when(
    Age < 50 ~ 'Group 2',
    TRUE ~ 'Group 1')) %>%
  group_by(Group)%>%
  summarise(Av.Heart.Rate = mean(Heart.Rate))%>%
  inner_join(df.top3zawody, by = "Group")%>%
  mutate(Av.Heart.Rate.dif = abs(Av.Heart.Rate.x - Av.Heart.Rate.y))

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>%
  mutate(Age.Group = case_when(
    Age %in% 0:9 ~ '[0, 10)',
    Age %in% 10:19 ~ '[10, 20)',
    Age %in% 20:29 ~ '[20, 30)',
    Age %in% 30:39 ~ '[30, 40)',
    Age %in% 40:49 ~ '[40, 50)',
    Age %in% 50:59 ~ '[50, 60)',
    Age %in% 60:69 ~ '[60, 70)'))%>%
  group_by(Age.Group, Gender, Occupation)%>%
  summarise(Occupation.Count = n(), Av.Stress.Level = mean(Stress.Level), Med.Stress.Level = median(Stress.Level), Sd.Stress.Level = sd(Stress.Level))%>%
  filter(Occupation.Count == max(Occupation.Count))
