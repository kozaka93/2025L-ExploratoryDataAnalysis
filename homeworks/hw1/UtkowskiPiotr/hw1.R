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
# Quality.of.Sleep - ilościowa ilorazowa
# Physical.Activity.Level - ilościowa ilorazowa
# BMI.Category - jakościowa uporządkowana
# Sleep.Disorder - jakościowa nominalna

########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  group_by(BMI.Category) %>% 
  summarise(srednie_tetno = mean(Heart.Rate, na.rm=T))



df %>% 
  group_by(BMI.Category) %>% 
  summarise(srednie_tetno = mean(Heart.Rate, na.rm=T)) %>% 
  top_n(1, srednie_tetno) %>% 
  select(BMI.Category)



########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(srednia_snu = mean(Sleep.Duration, na.rm = T)) %>% 
  top_n(1, srednia_snu) %>% 
  select(Occupation)



########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(Sleep.Disorder != 'None') %>% 
  group_by(Occupation) %>% 
  summarise(n = n()) %>% 
  top_n(1, n) %>% 
  select(Occupation)

 

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(BMI.Category == 'Obese') %>% 
  group_by(Occupation, Gender) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = Gender, values_from = n, values_fill = 0) %>% 
  filter(Male > Female) %>% 
  select(Occupation)



  
########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  separate(Blood.Pressure, sep = '/', into = c('skur', 'roz')) %>% 
  mutate(roznica = as.integer(skur) - as.integer(roz)) %>% 
  group_by(Occupation) %>% 
  summarise(n = n(), srednia = mean(roznica)) %>% 
  filter(n >20) %>% 
  arrange(desc(srednia)) %>% 
  top_n(3, srednia) %>% 
  select(Occupation)



  
df %>% 
  filter(Occupation == 'Lawyer') %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(srednia_snu = mean(Sleep.Duration), mediana_snu = median(Sleep.Duration))



########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  summarise(srednia_ilosc_krokow = mean(Daily.Steps)) %>% 
  arrange(desc(srednia_ilosc_krokow)) %>% 
  top_n(3, srednia_ilosc_krokow) %>% 
  select(Occupation)



srednie_tetno_ogolem <- mean(df$Heart.Rate)

df %>% 
  mutate(grupa = ifelse(Age >= 50, 'grupa1', 'grupa2')) %>% 
  filter(Occupation == 'Nurse'|Occupation == 'Lawyer'|Occupation ==  'Accountant') %>% 
  group_by(Occupation, grupa) %>% 
  summarise(roznica = srednie_tetno_ogolem - mean(Heart.Rate)) 



########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

#max(df$Age)

df %>% 
  mutate(kubelek = case_when(Age >=0 & Age <10 ~ '[0,10)',
                             Age >=10 & Age <20 ~ '[10,20)',
                             Age >=20 & Age <30 ~ '[20,30)',
                             Age >=30 & Age <40 ~ '[30,40)',
                             Age >=40 & Age <50 ~ '[40,50)',
                             Age >=50 & Age <60 ~ '[50,60)')) %>% 
  group_by(Gender ,kubelek, Occupation) %>% 
  summarise(n = n()) %>% 
  top_n(1, n) %>% 
  select(Gender, kubelek, Occupation)



df %>% 
  mutate(kubelek = case_when(Age >=0 & Age <10 ~ '[0,10)',
                             Age >=10 & Age <20 ~ '[10,20)',
                             Age >=20 & Age <30 ~ '[20,30)',
                             Age >=30 & Age <40 ~ '[30,40)',
                             Age >=40 & Age <50 ~ '[40,50)',
                             Age >=50 & Age <60 ~ '[50,60)')) %>% 
  group_by(kubelek) %>% 
  summarise(srednia = mean(Stress.Level), mediana = median(Stress.Level), odchylenie_standarowe = sd(Stress.Level))




