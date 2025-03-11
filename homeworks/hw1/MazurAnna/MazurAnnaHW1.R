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
# Physical.Activity.Level - ilościowa, przedziałowa
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder - jakościowa, nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

#zalozylam ze normal i normal weight to ta sama kategoria
zad2 <- df %>% 
  mutate(BMI = ifelse(BMI.Category == "Normal Weight",  "Normal", BMI.Category)) %>% 
  group_by(BMI) %>% 
  summarise(srednie_tetno = mean(Heart.Rate, na.rm = TRUE))

max_mean_pulse <- zad2 %>% 
  top_n(1, BMI) %>% 
  select(BMI)

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
zad3 <- df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(mean_sleep_time = mean(Sleep.Duration, na.rm = TRUE)) %>% 
  top_n(1, mean_sleep_time) %>% 
  select(Occupation)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

zad4 <- df %>% 
  group_by(Sleep.Disorder, Occupation) %>% 
  summarise(n = n()) %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Sleep.Disorder) %>% 
  top_n(1, n)
  

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
zad5 <- df %>% 
  filter(BMI.Category == "Obese") %>% 
  group_by(Gender, Occupation) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = "Gender", values_from = "n", values_fill = 0) %>% 
  group_by(Occupation) %>% 
  filter(Male > Female) %>% 
  select(Occupation)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
zad6 <- df %>% 
  group_by(Occupation) %>% 
  mutate(number_of_people_in_a_given_occupation = n()) %>% 
  filter(number_of_people_in_a_given_occupation > 20) %>% 
  mutate(
    skurczowe = as.numeric(substr(Blood.Pressure, 1, 3)),
    rozkurczowe = as.numeric(substr(Blood.Pressure, 5, 6))
    ) %>% 
  mutate(roznica = skurczowe - rozkurczowe) 
  
zad6_cz1 <- zad6 %>% 
  group_by(Occupation) %>% 
  summarise(srednia_roznica = mean(roznica)) %>% 
  top_n(3, srednia_roznica) %>% 
  arrange(desc(srednia_roznica))

#odczytuje ze tym zawodem jest prawnik (Lawyer)

zad6_cz2 <- zad6 %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(srednia = mean(Sleep.Duration), mediana = median(Sleep.Duration))

  
########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

zad7_cz1 <- df %>% 
  group_by(Occupation) %>% 
  summarise(sredni_ruch = mean(Physical.Activity.Level)) %>% 
  top_n(3, sredni_ruch) %>% 
  arrange(sredni_ruch)

zad7_cz2 <- df %>% 
  mutate(rodzaj_zawodu = ifelse(Occupation == "Accountant" | Occupation == "Lawyer" | Occupation == "Nurse"
                                 , "ruchliwy", "nieruchliwy")) %>% 
  mutate(grupa_wiekowa = ifelse(Age < 50, "ponizej 50 (2)", "50 lub wiecej (1)")) %>% 
  group_by(grupa_wiekowa, rodzaj_zawodu) %>% 
  summarise(srednie_tetno = mean(Heart.Rate)) %>% 
  pivot_wider(names_from = "rodzaj_zawodu", values_from = "srednie_tetno") %>% 
  mutate(roznica = ruchliwy - nieruchliwy)
  

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
zad8_cz1 <- df %>% 
  mutate(przedział_wiekowy = case_when(
    Age >= 0 & Age < 10 ~ "[0,10)",
    Age >= 10 & Age < 20 ~ "[10,20)",
    Age >= 20 & Age < 30 ~ "[20,30)",
    Age >= 30 & Age < 40 ~ "[30,40)",
    Age >= 40 & Age < 50 ~ "[40,50)",
    Age >= 50 & Age < 60 ~ "[50,60)",
    )
  ) 

zad8_cz2 <- zad8_cz1 %>% 
  group_by(przedział_wiekowy, Gender) %>% 
  count(Occupation) %>% 
  group_by(przedział_wiekowy, Gender) %>% 
  filter(n == max(n))
  
zad8_cz3 <- zad8_cz1 %>% 
  group_by(przedział_wiekowy, Gender) %>% 
  summarise(
    srednia_stresu = mean(Stress.Level),
    mediana_stresu = median(Stress.Level),
    odch_stand_stresu = sd(Stress.Level)
  )
  
