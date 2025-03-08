# Ładowanie pakietów
library(dplyr)
library(tidyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

str(df)

df %>%
  filter(Gender != 'Male' & Gender != 'Female')   #tylko dwie możliwości, więc jest to zmienna jakościowa binarna

df %>% 
  filter(Physical.Activity.Level == 0) # brak wartosci zerowych, wiec dzielenie ma sens (?)

# Gender - jakościowa binarna
# Age - ilościowa ilorazowa
# Occupation - jakościowa nominalna
# Sleep.Duration - ilościowa ilorazowa
# Quality.of.Sleep - jakościowa uporządkowana
# Physical.Activity.Level - ilościowa ilorazowa
# BMI.Category - jakościowa uporządkowana
# Sleep.Disorder - jakościowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średnie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>%
  group_by(BMI.Category) %>%
  summarise(mean_hr = mean(Heart.Rate)) # średnie tętno w różnych kategoriach BMI

df %>%
  group_by(BMI.Category) %>%
  summarise(mean_hr = mean(Heart.Rate)) %>%
  top_n(1, mean_hr) %>%
  select(BMI.Category) # kategoria BMI, dla której średnie tętno jest najwyższe

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>%
  filter(Stress.Level < 7) %>%
  group_by(Occupation) %>%
  summarise(mean_sleep = mean(Sleep.Duration)) %>%
  top_n(1, mean_sleep) %>%
  select(Occupation) 

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>%
  filter(Sleep.Disorder != "None") %>%
  group_by(Occupation) %>%
  summarise(n = n()) %>%
  arrange(-n)  # tabela zawodów posortowana ze względu na ilość osób z dowolnym zaburzeniem snu w danym zawodzie

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>%
  filter(BMI.Category == "Obese") %>%
  group_by(Occupation, Gender) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = Gender, values_from = n, values_fill = 0)  %>%
  filter(Male > Female) %>%
  select(Occupation)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

cz1 <- df %>%
  separate(Blood.Pressure, into = c("cis_skur", "cis_rozkur"), sep = "/") %>%
  mutate(cis_rozn = abs(as.numeric(cis_skur) - as.numeric(cis_rozkur))) %>%
  group_by(Occupation) %>%
  summarise(n = n(), mean_rozn = mean(cis_rozn)) %>%
  filter(n > 20) %>%
  arrange(-mean_rozn) %>%
  head(3) %>%
  select(Occupation)  # 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
                      # różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.

df %>% 
  filter(Occupation == cz1$Occupation[2]) %>%
  group_by(Quality.of.Sleep) %>%
  summarise(med_sleep = median(Sleep.Duration), mean_sleep = mean(Sleep.Duration)) # średnia i mediana dla roznych jakosci snu dla zawodu dla
                                                                                   # zawodu drugiego pod wzgledem sredniej roznicy

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

cz12 <- df %>% 
  group_by(Occupation) %>%
  summarise(mean_act = mean(Physical.Activity.Level)) %>%
  top_n(3, mean_act)  # 3 zawody, w których ludzie średnio najbardziej się ruszają

mean_all <- as.numeric(df %>%
  summarise(mean_hr = mean(Heart.Rate))) # srednie tetno z wszystkich danych

df %>% 
  filter(Occupation %in% cz12$Occupation) %>%
  mutate(grupa = case_when((Age < 50) ~ "grupa 1",
                             TRUE ~ "grupa 2")) %>%
  group_by(grupa, Occupation) %>%
  summarise(mean_hr = mean(Heart.Rate)) %>%
  mutate(diff_from_mean = abs(mean_hr - mean_all))  # roznica miedzy srednim tetnem z wszystkich danych a srednim tetnem w tych zawodach grupując
                                                    # dane względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>%
  summarise(max_age = max(Age)) # sprawdzam maksymalny wiek

df %>% 
  mutate(age_interval = case_when((Age >= 0 & Age < 10) ~ "[0, 10)",
                                  (Age >= 10 & Age < 20) ~ "[10, 20)",
                                  (Age >= 20 & Age < 30) ~ "[20, 30)",
                                  (Age >= 30 & Age < 40) ~ "[30, 40)",
                                  (Age >= 40 & Age < 50) ~ "[40, 50)",
                                  TRUE ~ "[50, 60)")) %>%
  group_by(age_interval, Gender, Occupation) %>%
  summarise(n = n()) %>%
  slice_max(n)  # najbardziej popularny zawod dla mezczyczn i kobiet w kazdej z grup (wraz z liczbą obserwacji tego zawodu w danej grupie)

df %>% 
  mutate(age_interval = case_when((Age >= 0 & Age < 10) ~ "[0, 10)",
                                  (Age >= 10 & Age < 20) ~ "[10, 20)",
                                  (Age >= 20 & Age < 30) ~ "[20, 30)",
                                  (Age >= 30 & Age < 40) ~ "[30, 40)",
                                  (Age >= 40 & Age < 50) ~ "[40, 50)",
                                  TRUE ~ "[50, 60)")) %>%
  group_by(age_interval) %>%
  summarise(mean_stress = mean(Stress.Level), med_stress = median(Stress.Level), std_stress = sd(Stress.Level)) 
  # średnia, mediana i odchylenie standardowe dla poziomu stresu w grupach
