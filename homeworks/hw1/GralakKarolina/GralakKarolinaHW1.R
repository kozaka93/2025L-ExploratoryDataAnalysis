install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)

df <- read.csv('C:\\Users\\karol\\Downloads\\data.csv')

########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Gender - jakościowe, binarne
# Age - ilościowe, zliczenia/ ilorazowe
# Occupation - jakościowe, nominalne
# Sleep.Duration - ilościowe, przedziałowe/ ilorazowe
# Quality.of.Sleep - jakościowe, uporządkowane
# Physical.Activity.Level - ilościowe, przedziłowe/ ilorazowe
# BMI.Category - jakościowe, nominalne
# Sleep.Disorder - jakościowe, uporządkowane


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

unique(df$BMI.Category)
df2 <- select(df, Heart.Rate, BMI.Category)
df2 %>% 
  group_by(BMI.Category) %>% 
  summarise(srednia = mean(Heart.Rate, na.rm = TRUE))
# średnie dla poszczególnych kategorii wynoszą: Normal- 68.7, Normal Weight- 71.3, 
#Obese- 84.3, Overweight- 70.9


########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?
df %>% 
  filter(Stress.Level < 7) %>% 
  select(Occupation, Sleep.Duration, Stress.Level) %>% 
  group_by(Occupation) %>% 
  summarise(srednia= mean(Sleep.Duration, na.rm = TRUE)) %>% 
  arrange(-srednia) %>% 
  head(1)
#w zawodzie Engineer


########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 
df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarise(count = n()) %>% 
  arrange(-count)
#Pięć zawodów, w których najczęściej pracują osoby z zaburzeniem snu to: Nurse, Teacher, Salesperson, Accountant, Doctor



########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?
df %>% 
  filter(BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>% 
  summarise(count = n(), .groups = "drop") %>%  
  pivot_wider(names_from = Gender, values_from = count, values_fill = 0) %>% 
  filter(Male > Female)
#więcej otyłych mężczyzn jest wśród zawodów: Doctor, Sales Representative, Software Engineer, Teacher.


########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

df <- df %>%
  separate(Blood.Pressure, into = c("Skurczowe.BP", "Rozkurczowe.BP"), sep = "/", convert = TRUE)
df6 <- df %>% 
  mutate(Difference = Skurczowe.BP - Rozkurczowe.BP) %>% 
  group_by(Occupation) %>% 
  summarise(count= n(), srednia = mean(Difference, na.rm = TRUE)) %>% 
  filter(count > 20) %>% 
  arrange(-srednia) %>% 
  head(3)
df6
#te zawody to: Salesperson, Lawyer, Nurse

df %>% 
  filter(Occupation == "Lawyer") %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(srednia = mean(Sleep.Duration, na.rm = TRUE),
            mediana = median(Sleep.Duration, na.rm = TRUE))
#dla jakości snu równej 7 srednia wynosi 7.18, a mediana 7.1
#dla jakości snu równej 8 średnia wynosi 7.44, a mediana 7.3



########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 
aktywni <- df %>% 
  group_by(Occupation) %>% 
  summarise(ruch = mean(Physical.Activity.Level, na.rm = TRUE)) %>% 
  arrange(-ruch) %>% 
  head(3)
aktywni
#Szukane zawody to: Nurse, lawyer, Accountant

aktywni.BP <- df %>% 
  mutate(grupa = ifelse(Age >= 50, "grupa1", "grupa2")) %>% 
  filter(Occupation == "Nurse" | Occupation == "Lawyer" | Occupation == "Accountant") %>% 
  group_by(grupa) %>% 
  summarise(srednia_aktywni = mean(Heart.Rate, na.rm = TRUE))
aktywni.BP

wszyscy.BP <- df %>% 
  mutate(grupa = ifelse(Age >= 50, "grupa1", "grupa2")) %>% 
  group_by(grupa) %>% 
  summarise(srednia_wszyscy = mean(Heart.Rate, na.rm = TRUE))
wszyscy.BP
  
wynik <- aktywni.BP %>% 
  left_join(wszyscy.BP, by = "grupa") %>% 
  mutate(roznica = srednia_aktywni- srednia_wszyscy)
wynik
#w grupie 1 "aktywne zawody" mają wyższe średnie tętno, a w grupie2- niższe.



########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.
df <- df %>%
  mutate(grupa = cut(Age, breaks = seq(0, 100, by = 10), 
                         right = FALSE, 
                         labels = c("[0,10)", "[10,20)", "[20,30)", "[30,40)", 
                                    "[40,50)", "[50,60)", "[60,70)", "[70,80)", "[80,90)", "[90,100)"))) 

popularny_z <- df %>% 
  group_by(grupa, Gender) %>% 
  count(Occupation) %>% 
  top_n(1, n) %>% 
  select(grupa, Gender, Occupation)
popularny_z 
#wyświetla najbardziej popularne zawody w zależności od grupy wiekowej i płci

stres_p <- df %>% 
  group_by(grupa) %>% 
  summarise(
    srednia = mean(Stress.Level, na.rm = TRUE),
    mediana = median(Stress.Level, na.rm = TRUE),
    odchylenie_st = sd(Stress.Level, na.rm = TRUE)
  )
stres_p 
#wyświetla średnią, mediane i odchylenie standardowe
  
  
  
