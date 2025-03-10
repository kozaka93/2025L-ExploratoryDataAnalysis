# Ładowanie pakietów
library(dplyr)
library(tidyr)
library(stringr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------
# Gender - jakościowa binarna
# Age - ilościowa ilorazowa
# Occupation - jakosciowa nominalna
# Sleep.Duration - ilosciowa ilorazowa
# Quality.of.Sleep - jakosciowa uporzadkowana
# Physical.Activity.Level - ilosciowa ilorazowa
# BMI.Category - jakosciowa uporządkowana
# Sleep.Disorder - jakosciowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średnie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(BMI.Category) %>% 
  summarize(srednie.tetno = mean(Heart.Rate, na.rm = T)) -> df_zad2

df_zad2
  
#   BMI.Category      srednie.tetno
#
# 1 Normal            68.7
# 2 Normal Weight     71.3
# 3 Obese             84.3
# 4 Overweight        70.9

df_zad2 %>% 
  top_n(1, srednie.tetno) %>% 
  select(BMI.Category) -> odp_zad2

odp_zad2

#   BMI.Category
#
# 1 Obese  


########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarize(srednia.dlugosc.snu = mean(Sleep.Duration, na.rm = T)) %>% 
  top_n(1, srednia.dlugosc.snu) %>% 
  select(Occupation) -> odp_zad3

odp_zad3

#   Occupation
#
# 1 Engineer

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarize(ilosc = n()) -> df_zad4

df_zad4 %>% 
  top_n(1, ilosc) %>% 
  select(Occupation) -> odp_zad4

odp_zad4
# Nurse

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>% 
  summarize(ilosc = n()) %>% 
  pivot_wider(names_from = Gender, values_from = ilosc) -> df_zad5

df_zad5[is.na(df_zad5)] <- 0 
# w miejsce NA wpisuje 0, poniewaz dzieki temu bede mogl porownac ilosci wystapien
# a NA powstalo w miejscach gdzie tych wystapien nie bylo

df_zad5 %>% 
  filter(Male > Female) %>%
  select(Occupation) -> odp_zad5

odp_zad5

#   Occupation
#
# 1 Doctor              
# 2 Sales Representative
# 3 Software Engineer   
# 4 Teacher  

####################### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  summarize(ilosc = n()) %>% 
  filter(ilosc > 20) -> zawody_zad6

df %>% 
  filter(Occupation %in% zawody_zad6$Occupation) -> df_zad6 

str_split(df_zad6$Blood.Pressure, "/", simplify = T) -> cisnienia_zad6

df_zad6 %>% 
  mutate(cisnienie.skurczowe = as.numeric(cisnienia_zad6[,1]), cisnienie.rozkurczowe = as.numeric(cisnienia_zad6[,2]), 
         roznica.cisnien = cisnienie.skurczowe - cisnienie.rozkurczowe) %>% 
  group_by(Occupation) %>% 
  summarize(srednia.roznica.cisnien = mean(roznica.cisnien)) %>% 
  arrange(desc(srednia.roznica.cisnien)) %>% 
  top_n(3, srednia.roznica.cisnien) %>% 
  select(Occupation) -> final_zawody_zad6

final_zawody_zad6

#   Occupation 
#
# 1 Salesperson
# 2 Lawyer     
# 3 Nurse

df %>% 
  filter(Occupation %in% final_zawody_zad6[2, 1]) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarize(srednia.czas.snu = mean(Sleep.Duration), mediana.czas.snu = median(Sleep.Duration)) -> odp_zad6

odp_zad6  
  
#   Quality.of.Sleep    srednia.czas.snu    mediana.czas.snu
#
# 1 7                   7.18                7.1
# 2 8                   7.44                7.3

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  summarize(srednia.ruch = mean(Physical.Activity.Level)) %>% 
  top_n(3, srednia.ruch) %>% 
  select(Occupation) -> zawody_zad7

zawody_zad7

#   Occupation
#
# 1 Accountant
# 2 Lawyer    
# 3 Nurse    

srednieTetnoWszyscy <- mean(as.numeric(df$Heart.Rate))

df %>% 
  filter(Occupation %in% zawody_zad7$Occupation) %>% 
  mutate(starszy.niz.50 = Age > 50) %>% 
  group_by(starszy.niz.50) %>% 
  summarize(srednie.tetno = mean(Heart.Rate)) %>% 
  mutate(roznica.srednich = srednie.tetno - srednieTetnoWszyscy)-> odp_zad7

odp_zad7

#   starszy.niz.50    srednie.tetno     roznica.srednich
#
# 1 FALSE             71.0              0.852
# 2 TRUE              69.3             -0.840

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

kubelki <- c("[0, 10)", "[10, 20)", "[20, 30)", "[30, 40)", "[40, 50)", "[50, 60)")

df %>% 
  mutate(kubelek = kubelki[floor(Age / 10) + 1]) %>% 
  group_by(kubelek, Gender, Occupation) %>% 
  summarize(ilosc = n()) %>%
  slice_max(ilosc, n = 1, with_ties = T) %>% 
  select(-ilosc) -> zawody_zad8

zawody_zad8

#   kubelek     Gender    Occupation 
#      
# 1 [20, 30)    Female    Nurse      
# 2 [20, 30)    Male      Doctor     
# 3 [30, 40)    Female    Accountant 
# 4 [30, 40)    Male      Doctor     
# 5 [40, 50)    Female    Teacher    
# 6 [40, 50)    Male      Salesperson
# 7 [50, 60)    Female    Nurse  

df %>%
  mutate(kubelek = kubelki[floor(Age / 10) + 1]) %>% 
  group_by(Gender, kubelek) %>% 
  summarize(srednia.poziom.stresu = mean(Stress.Level), mediana.poziom.stresu = median(Stress.Level), 
            odchylenie.poziom.stresu = sd(Stress.Level)) -> odp_zad8

odp_zad8

#   kubelek     srednia.poziom.stresu     mediana.poziom.stresu     odchylenie.poziom.stresu
#
# 1 [20, 30)    7.32                      8                         0.885
# 2 [30, 40)    5.52                      5                         1.50 
# 3 [40, 50)    5.68                      5                         1.38 
# 4 [50, 60)    4.45                      3                         2.19 