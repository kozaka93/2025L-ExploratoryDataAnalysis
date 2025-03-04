# Ładowanie pakietów
library(dplyr)


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
# Physical.Activity.Level - ilościowe przedziałowe
# BMI.Category - jakościowe uporządkowane
# Sleep.Disorder - jakościowe nominalne


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(BMI.Category) %>% 
  summarise(srednie_tetno = mean(Heart.Rate))

df %>% 
  group_by(BMI.Category) %>% 
  summarise(srednie_tetno = mean(Heart.Rate)) %>% 
  arrange(-srednie_tetno) %>% 
  head(1)

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(sredni_sen = mean(Sleep.Duration)) %>% 
  arrange(-sredni_sen) %>% 
  head(1)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarise(liczba_chorych = n()) %>% 
  arrange(-liczba_chorych) %>% 
  head(1)

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>% 
  summarise(liczba = n()) %>% 
  summarise(On = sum(liczba[Gender == "Male"]),
            Ona = sum(liczba[Gender == "Female"])) %>% 
  filter(On > Ona) %>% 
  select(Occupation)


########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------


df %>% 
  mutate(cisnienie_skurczowe = substr(df$Blood.Pressure,1,3),
         cisnienie_rozkurczowe = substr(df$Blood.Pressure,5,6)) %>% 
  mutate(roznica_cisnien = as.numeric(cisnienie_skurczowe) - as.numeric(cisnienie_rozkurczowe)) %>% 
  group_by(Occupation) %>% 
  summarise(liczba = n(), srednia_roznica = mean(roznica_cisnien)) %>% 
  filter(liczba >= 20) %>% 
  arrange(-srednia_roznica) %>% 
  head(3) %>% 
  select(c(Occupation, srednia_roznica))

df %>% 
  mutate(cisnienie_skurczowe = substr(df$Blood.Pressure,1,3),
         cisnienie_rozkurczowe = substr(df$Blood.Pressure,5,6)) %>% 
  mutate(roznica_cisnien = as.numeric(cisnienie_skurczowe) - as.numeric(cisnienie_rozkurczowe)) %>% 
  group_by(Occupation) %>% 
  summarise(liczba = n(), srednia_roznica = mean(roznica_cisnien)) %>% 
  filter(liczba >= 20) %>% 
  arrange(-srednia_roznica) %>% 
  head(2) %>% 
  tail(1) %>% 
  select(Occupation) -> szukany_zawod

df %>% 
  filter(Occupation == as.character(szukany_zawod)) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(srednia = mean(Sleep.Duration), mediana = median(Sleep.Duration))
  

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  summarise(sredni_ruch = mean(Physical.Activity.Level)) %>% 
  arrange(-sredni_ruch) %>% 
  head(3) %>% 
  select(Occupation)

mean(df$Heart.Rate) -> srednie_tetno

df %>% 
  filter(Occupation == c("Nurse","Lawyer","Accountant")) %>% 
  mutate(wiek = ifelse(Age >= 50, 'grupa1','grupa2')) %>% 
  group_by(wiek) %>% 
  summarise(roznica_sredniego_tetna = mean(Heart.Rate)-srednie_tetno )
  
########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  mutate(kubelek_min = floor(Age/10)*10,
         kubelek_maks = ceiling(Age/10)*10) %>% 
  mutate(kubelek = ifelse
         (kubelek_maks == kubelek_min, paste0('[',kubelek_min-10,',', kubelek_maks,')'),
           paste0('[',kubelek_min,',', kubelek_maks,')'))) %>% 
  group_by(kubelek,Gender) %>% 
  count(Occupation) %>% 
  top_n(1, n)

df %>% 
  mutate(kubelek_min = floor(Age/10)*10,
         kubelek_maks = ceiling(Age/10)*10) %>% 
  mutate(kubelek = ifelse
         (kubelek_maks == kubelek_min, paste0('[',kubelek_min-10,',', kubelek_maks,')'),
           paste0('[',kubelek_min,',', kubelek_maks,')'))) %>% 
  group_by(kubelek) %>% 
  summarise(srednia = mean(Stress.Level), mediana = median(Stress.Level), odchylenie = sd(Stress.Level))

