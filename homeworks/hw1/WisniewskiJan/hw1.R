# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")
head(df)

########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakosciowa binarna
# Age - ilosciowa ilorazowa
# Occupation - jakosciowa nominalna
# Sleep.Duration - ilosciowa ilorazowa
# Quality.of.Sleep - ilosciowa przedzialowa
# Physical.Activity.Level - ilosciowa przedzialowa
# BMI.Category - jakosciowa uporzadkowana
# Sleep.Disorder - jakosciowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?
df %>%
  group_by(BMI.Category) %>%
  summarise(srednia = mean(Heart.Rate, na.rm=TRUE))%>%
  top_n(1, srednia)
# Rozwiązanie: średnie tętno jest najwyższe dla kategorii: otyłość


########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?
df %>% 
  filter(Stress.Level<7)%>%
  group_by(Occupation)%>%
  summarise(srednia_dlugosc_snu = mean(Sleep.Duration, na.rm=TRUE))%>%
  top_n(1, srednia_dlugosc_snu)
# Rozwiązanie: wsród ludzi, których poziom stresu jest poniżej 7, najwięcej śpią inżynierowie


########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 
df %>%
  filter(Sleep.Disorder != "None") %>%
  group_by(Occupation) %>%
  summarise(ilosc_osob=n()) %>% 
  arrange(-ilosc_osob)
# Rozwiązanie: kolejno jako: pielęgniarka, nauczyciel, sprzedawca, księgowy, lekarz, inżynier, prawnik
# przedstawiciel handlowy, naukowiec, inżynier oprogramowania pracuje najwiecej osob z zaburzeniami snu


########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?
df %>% 
  filter(BMI.Category=="Obese") %>%
  group_by(Occupation, Gender) %>%
  summarise(ilosc_otylych=n()) %>%
  pivot_wider(names_from = Gender, values_from = ilosc_otylych, values_fill = 0) %>%
  mutate(m_f=Male-Female) %>%
  mutate(if_male_more_than_female = ifelse(m_f>0, TRUE, FALSE)) %>% 
  filter(if_male_more_than_female==TRUE)
# Rozwiązanie: jest więcej otyłych mężczyzn, którzy są lekarzami, przedstawicielami handlowych, inżynierami oprogramowania i nauczycielami niż kobiet w tych zawodach


########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.
df <- df %>%
  separate(`Blood.Pressure`, into = c("Systolic", "Diastolic"), sep = "/", convert = TRUE) %>%
  mutate(BloodPressure_Diff = Systolic - Diastolic)

x <- df %>%
  group_by(Occupation) %>%
  filter(n() > 20) %>%  
  summarise(srednia_roznica = mean(BloodPressure_Diff, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(srednia_roznica)) %>% 
  top_n(3, srednia_roznica)
x

df %>% 
  filter(Occupation=="Lawyer") %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(srednia = mean(Sleep.Duration, na.rm=TRUE), mediana = median(Sleep.Duration, na.rm=TRUE))
# Rozwiązanie: 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa to przedstawiciel handlowy, prawnik, pielęgniarka, 
# średnią i medianę czasu snu dla różnych jakości snu prawnikow to odpowiednio 7.18 i 7.1 dla jakosci snu 7 oraz 7.44, 7.3 dla jakosci snu 8


########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 


# Rozwiązanie: ------------------------------------------------------------
df %>%
  group_by(Occupation) %>%
  summarise(srednia_aktywnosc = mean(`Physical.Activity.Level`, na.rm = TRUE)) %>%
  top_n(3, wt = srednia_aktywnosc)

top_3_jobs <- c("Accountant", "Lawyer", "Nurse")

df %>%
  filter(Occupation %in% top_3_jobs) %>%
  mutate(Age_Group = if_else(Age >= 50, "50+", "<50")) %>%
  group_by(Age_Group) %>%
  summarise(srednie_tetno_w_top3_zawodach = mean(`Heart.Rate`, na.rm = TRUE), .groups = "drop") -> tetno_top3_zawody

df %>%
  mutate(Age_Group = if_else(Age >= 50, "50+", "<50")) %>%
  group_by(Age_Group) %>%
  summarise(srednie_tetno_razem = mean(`Heart.Rate`, na.rm = TRUE), .groups = "drop") -> tetno_razem

wynik <- left_join(tetno_top3_zawody, tetno_razem, by = "Age_Group") %>%
  mutate(roznica = srednie_tetno_w_top3_zawodach - srednie_tetno_razem)

wynik

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
df2 <- df %>%
  mutate(Age_Group = case_when(
    Age < 10  ~ "[0,10)",
    Age < 20  ~ "[10,20)",
    Age < 30  ~ "[20,30)",
    Age < 40  ~ "[30,40)",
    Age < 50  ~ "[40,50)",
    Age < 60  ~ "[50,60)",
    Age < 70  ~ "[60,70)",
    Age < 80  ~ "[70,80)",
    Age < 90  ~ "[80,90)",
    Age < 100 ~ "[90,100)",
    TRUE      ~ "[100+]"
  ))

najpopularniejsze <- df2 %>%
  group_by(Age_Group, Gender, Occupation) %>%           
  summarise(count = n(), .groups = "drop_last") %>%       
  top_n(1, count) %>%# 
  ungroup() %>%
  select(Age_Group, Gender, najczestszy_zawod = Occupation)

stres <- df2 %>%
  group_by(Age_Group, Gender) %>%
  summarise(
    srednia = mean(`Stress.Level`, na.rm = TRUE),
    mediana = median(`Stress.Level`, na.rm = TRUE),
    odchylenie = sd(`Stress.Level`, na.rm = TRUE),
    .groups = "drop"
  )


wynik <- left_join(najpopularniejsze, stres, by = c("Age_Group", "Gender"))
wynik
