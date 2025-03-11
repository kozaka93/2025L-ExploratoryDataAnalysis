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
# Quality.of.Sleep - jakościowa uporządkowana
# Physical.Activity.Level - ilościowa ilorazowa
# BMI.Category - jakościowa uporządkowana
# Sleep.Disorder -jakościowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>% group_by(BMI.Category) %>% summarise(srednie_tetno = mean(Heart.Rate, na.rm = TRUE)) #średnie tętno w różnych kategoriach

df %>% group_by(BMI.Category) %>% summarise(srednie_tetno = mean(Heart.Rate, na.rm = TRUE)) %>% 
  top_n(1, srednie_tetno) %>% select(BMI.Category) #kategoria z najwyższym

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% summarise(sredni_sen = mean(Sleep.Duration, na.rm = TRUE)) %>% 
  top_n(1, sredni_sen) %>% select(Occupation)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% filter(Sleep.Disorder!= "None") %>% 
  group_by(Occupation) %>% 
  summarise(suma = n()) %>% top_n(3,suma) %>% arrange(desc(suma)) #3 najczestsze zawody wraz z liczbą osob z zaburzeniami snu

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% filter(BMI.Category == "Obese") %>% 
  group_by(Occupation) %>% summarise(otyly_m = sum(Gender == "Male"), otyly_k = sum(Gender == "Female")) %>% 
  filter(otyly_m > otyly_k) %>% select(Occupation)


########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df = df %>% separate(Blood.Pressure, into = c("skurczowe", "rozkurczowe"), sep = "/", convert = TRUE) %>% 
  mutate(roznica_cisnienia = skurczowe - rozkurczowe)

top_3_zawody1 = df %>% group_by(Occupation) %>% 
  summarise(liczba_przykladow = n(), srednia_roznica = mean(roznica_cisnienia, na.rm = TRUE)) %>% 
  filter(liczba_przykladow>20) %>% top_n(3,srednia_roznica) %>% arrange(desc(srednia_roznica)) %>% select(Occupation)

top_3_zawody1 #Odpowiedź 1

zawod_drugi = (top_3_zawody1 %>% slice(2))$Occupation

df %>% filter(Occupation == zawod_drugi) %>% group_by(Quality.of.Sleep) %>% 
  summarise(srednia_snu = mean(Sleep.Duration, na.rm = TRUE), mediana_snu = median(Sleep.Duration, na.rm=TRUE)) #Odpowiedź 2

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

top_3_zawody2 = df %>% group_by(Occupation) %>% summarise(ruch = mean(Daily.Steps)) %>% top_n(3,ruch) %>% 
  arrange(desc(ruch)) %>% select(Occupation) 

top_3_zawody2 #Odpowiedź 1

df = df %>% mutate(grupa_wiekowa = if_else(Age >= 50, "Grupa_1", "Grupa_2")) #tworzymy 2 grupy wiekowe

df %>% group_by(grupa_wiekowa) %>% 
  summarise(tetno_calkowite = mean(Heart.Rate), tetno_w_zawodach = mean(Heart.Rate[Occupation %in% top_3_zawody2$Occupation])) %>%
  mutate(roznica = tetno_calkowite - tetno_w_zawodach) %>% select(grupa_wiekowa, roznica) #Odpowiedź 2
                                          

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df = df %>% mutate(podzial_wieku = cut(Age, breaks = seq(0, 100, by = 10), include.lowest = TRUE, right = FALSE)) #grupowanie

df %>% group_by(podzial_wieku, Occupation, Gender) %>% summarise(ilosc_zawodow = n()) %>% 
  group_by(podzial_wieku, Gender) %>% top_n(1,ilosc_zawodow) %>% 
  select(podzial_wieku, Gender, Occupation) %>% arrange(podzial_wieku, Gender) #Odpowiedź 1

df %>% group_by(podzial_wieku) %>%
  summarise(
    srednia_stres = mean(Stress.Level, na.rm = TRUE),
    mediana_stres = median(Stress.Level, na.rm = TRUE),
    odchylenie_stres = sd(Stress.Level, na.rm = TRUE)) #Odpowiedź 2 

