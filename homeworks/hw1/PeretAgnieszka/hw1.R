# Ładowanie pakietów
library(dplyr)
library(tidyr)


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
# Physical.Activity.Level - jakościowe uporządkowane
# BMI.Category - jakościowe uporządkowane
# Sleep.Disorder - jakościowe nominalne


########### Zad 2 (0.5pkt) ###########
# Jakie mają średnie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df%>%
  group_by(BMI.Category)%>%
  summarise(srednie.tetno = mean(Heart.Rate, na.rm = TRUE))%>%
  arrange(desc(srednie.tetno)) 
 
#Odpowiedź:Poniżej podane jest średnie tętno dla każdej z kategorii. 
#Jest ono największe u osób otyłych. 
#  BMI.Category  srednie.tetno
#1 Obese                  84.3
#2 Normal Weight          71.3
#3 Overweight             70.9
#4 Normal                 68.7

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df%>%
  filter(Stress.Level<7)%>%
  group_by(Occupation)%>%
  summarise(sredni.sen = mean(Sleep.Duration, na.rm = TRUE))%>%
  top_n(1,sredni.sen)
  
#Odpowiedź: Inżynierowie

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześciej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df%>%
  filter(Sleep.Disorder!="None")%>%
  group_by(Occupation)%>%
  summarise(n = n())%>%
  top_n(3,n)

#Odpowiedź: zaburzenia snu najczęściej mają pielęgniarki, sprzedawcy i nauczyciele

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df%>%
  filter(BMI.Category=="Obese")%>%
  group_by(Gender, Occupation)%>%
  summarise(n = n())

#Więcej otyłych mężczyzn jest wśród doktorów, przedstawicieli handlowych, nauczycieli
#oraz inżynierów oprogramowania

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df%>%
  separate(Blood.Pressure, into = c("skurczowe", "rozkurczowe"), sep = "/", convert = TRUE)%>%
  mutate(roznica = skurczowe-rozkurczowe)%>%
  group_by(Occupation)%>%
  summarise(n=n(), srednia_roznica = mean(roznica))%>%
  filter(n>20)%>%
  top_n(3,srednia_roznica)

#Odpowiedź: 3 szukane zawody to prawnik, pielęgniarka i sprzedawca. Drugi pod względem
#średniej różnicy jest zawód pielęgniarki, więc dla niego będziemy szukać kolejnych danych

df%>%
  filter(Occupation=="Nurse")%>%
  group_by(Quality.of.Sleep)%>%
  summarise(mediana = median(Sleep.Duration), srednia = mean(Sleep.Duration))

#Odpowiedź:
#Quality.of.Sleep mediana srednia
#1                5    6.45    6.45
#2                6    6.1     6.07
#3                7    7.1     7.1 
#4                8    7.7     7.7 
#5                9    8.1     8.09

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df%>%
  group_by(Occupation)%>%
  summarise(srednia_ruchu = mean(Physical.Activity.Level)) %>% 
  top_n(3, srednia_ruchu)

#Odpowiedź: 3 zawody, w których ludzie są najbardziej aktywni fizycznie to
#księgowy, prawnik i pielęgniarka

df %>% 
  mutate(grupa = ifelse(Age>=50,"grupa1","grupa2")) %>% 
  group_by(grupa) %>% 
  summarise(mean(Heart.Rate))

df %>% 
  mutate(grupa = ifelse(Age>=50,"grupa1","grupa2")) %>%
  filter(Occupation %in% c("Accounant", "Lawyer", "Nurse")) %>% 
  group_by(grupa) %>% 
  summarise(mean(Heart.Rate))

#Odpowiedź:
#Wyniki dla wszystkich danych:
# grupa1               68.9
# grupa2               70.6

#Wyniki dla 3 zawodów:
# grupa1               71.0
# grupa2               71.2

#Różnice są na poziomie 2.1 uderzenia na minutę w grupie 1 i 0.8 uderzenia w grupie 2

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  mutate(kubelek_wieku = case_when(floor(Age/10) == 0 ~ "[0,10)",
                                   floor(Age/10) == 1 ~ "[10,20)",
                                   floor(Age/10) == 2 ~ "[20,30)",
                                   floor(Age/10) == 3 ~ "[30,40)",
                                   floor(Age/10) == 4 ~ "[40,50)",
                                   floor(Age/10) == 5 ~ "[50,60)")) %>% 
  group_by(kubelek_wieku, Gender, Occupation) %>% 
  summarise(n=n()) %>% 
  slice_max(n, n = 1, with_ties = FALSE) 

#Odpowiedź
#kubelek_wieku Gender Occupation      
# [20,30)       Female Nurse           
# [20,30)       Male   Doctor         
# [30,40)       Female Accountant     
# [30,40)       Male   Doctor         
# [40,50)       Female Teacher      
# [40,50)       Male   Salesperson    
# [50,60)       Female Nurse   

df %>% 
  mutate(kubelek_wieku = case_when(floor(Age/10) == 0 ~ "[0,10)",
                                   floor(Age/10) == 1 ~ "[10,20)",
                                   floor(Age/10) == 2 ~ "[20,30)",
                                   floor(Age/10) == 3 ~ "[30,40)",
                                   floor(Age/10) == 4 ~ "[40,50)",
                                   floor(Age/10) == 5 ~ "[50,60)")) %>% 
  group_by(kubelek_wieku) %>% 
  summarise(sredni_stres = mean(Stress.Level), mediana_stres = median(Stress.Level),
            odchylenie_stres = sd(Stress.Level)) %>% 
  View()

#  kubelek_wieku sredni_stres mediana_stres odchylenie_stres
# [20,30)               7.32             8            0.885
# [30,40)               5.52             5            1.50 
# [40,50)               5.68             5            1.38 
# [50,60)               4.45             3            2.19 