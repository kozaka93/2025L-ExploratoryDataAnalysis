# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ########### OK
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowe, binarne
# Age - ilościowe, ilorazowe
# Occupation - jakościowe, nominalne
# Sleep.Duration - ilościowe, ilorazowe
# Quality.of.Sleep - jakościowe, uporządkowane
# Physical.Activity.Level - jakościowe, uporządkowane
# BMI.Category - jakościowe, uporządkowane
# Sleep.Disorder - jakościowe, nominalne


########### Zad 2 (0.5pkt) ########### OK
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(BMI.Category,Heart.Rate) %>% 
  group_by(BMI.Category) %>% 
  summarise(mean_hr = mean(Heart.Rate, na.rm=TRUE)) %>% 
  arrange(-mean_hr)

########### Zad 3 (0.5pkt) ########### OK
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Stress.Level<7) %>% 
  select(Occupation,Sleep.Duration) %>% 
  group_by(Occupation) %>% 
  summarise(mean_s = mean(Sleep.Duration, na.rm = TRUE)) %>% 
  arrange(-mean_s) %>% 
  head(1)

########### Zad 4 (0.5pkt) ########### OK
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Occupation,Sleep.Disorder) %>% 
  group_by(Occupation) %>% 
  filter(Sleep.Disorder!="None") %>% 
  summarise(amount = n()) %>% 
  arrange(-amount)

########### Zad 5 (0.5pkt) ########### OK
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Gender,Occupation,BMI.Category) %>% 
  filter(BMI.Category=="Obese") %>% 
  group_by(Occupation) %>% 
  summarise(male = sum(Gender=="Male"), female = sum(Gender=="Female")) %>% 
  filter(male>female) %>% 
  select(Occupation)

########### Zad 6 (1 pkt) ########### OK
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  filter(n()>20) %>% 
  separate(Blood.Pressure,into = c("sk","roz"),sep = "/", convert = TRUE) %>% 
  summarise(mean_s = mean(sk-roz)) %>% 
  arrange(-mean_s) %>% 
  head(3)

df %>% 
  filter(Occupation=="Lawyer") %>% 
  select(Occupation,Sleep.Duration,Quality.of.Sleep) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(mean_x = mean(Sleep.Duration), med_x = median(Sleep.Duration))

########### Zad 7 (1 pkt) ########### OK
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  summarise(mean_pa = mean(Physical.Activity.Level,na.rm = TRUE)) %>% 
  arrange(-mean_pa) %>% 
  head(3)

a <- mean(df$Heart.Rate,na.rm = TRUE)
df %>% 
  filter(Occupation=="Nurse"|Occupation=="Lawyer"|Occupation=="Accountant") %>% 
  mutate(age_g = ifelse(Age>=50,"50+","<50")) %>% 
  group_by(age_g) %>% 
  summarise(mean_hr = mean(Heart.Rate,na.rm = TRUE)) %>% 
  mutate(diff = mean_hr-a)
  
########### Zad 8 (1 pkt) ########### OK
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

max(df$Age)
df %>%
  mutate(age_b = cut(Age, breaks = seq(0, 60, by = 10), right = FALSE)) %>%
  group_by(Gender,age_b,Occupation) %>% 
  summarise(am = n()) %>% 
  group_by(age_b,Gender) %>% 
  slice_max(order = am)

df %>%
  mutate(age_b = cut(Age, breaks = seq(0, 60, by = 10), right = FALSE)) %>%
  group_by(age_b) %>% 
  summarise(ms = mean(Stress.Level, na.rm = TRUE),meds = median(Stress.Level, na.rm = TRUE),vs = sd(Stress.Level, na.rm = TRUE))
