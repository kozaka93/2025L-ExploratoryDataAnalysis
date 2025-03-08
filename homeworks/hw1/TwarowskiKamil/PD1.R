# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, binarna/nominalna
# Age - ilościowa, zliczenia
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep - ilościowa, przedziałowa
# Physical.Activity.Level - ilościowa, przedziałowa
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder - jakościowa, nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df %>% group_by(BMI.Category) %>% 
  summarise(mean_Heart.Rate = mean(Heart.Rate))

df %>% group_by(BMI.Category) %>% 
  summarise(mean_Heart.Rate = mean(Heart.Rate)) %>% arrange(desc(mean_Heart.Rate)) %>% head(1)
########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% filter(Stress.Level<7) %>% group_by(Occupation) %>% 
  summarise(mean_Sleep.Duration = mean(Sleep.Duration)) %>% arrange(desc(mean_Sleep.Duration)) %>% head(1)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% filter(Sleep.Disorder!="None") %>% group_by(Occupation) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(5)

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
library(tidyr)
df %>% filter(BMI.Category=="Obese") %>% group_by(Occupation, Gender) %>% 
  summarise(n = n()) %>% pivot_wider(names_from = Gender, values_from = n, values_fill = 0) %>% 
  filter(Male>Female) %>% select(Occupation)
  

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>% separate(Blood.Pressure, into = c("sku", "rozk"), sep = "/", convert = TRUE) %>% 
  mutate(dif=sku-rozk) %>% add_count(Occupation, name="count") %>% filter(count>20) %>%
  group_by(Occupation) %>% summarise(mean.blood.dif = mean(dif)) %>% arrange(desc(mean.blood.dif)) %>% head(3)
  
df %>% filter(Occupation=="Lawyer") %>% group_by(Occupation,Quality.of.Sleep) %>% 
  summarise(mean.duration = mean(Sleep.Duration), median.duration = median(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
df %>% group_by(Occupation) %>% summarise(mean.activity =  mean(Physical.Activity.Level)) %>% 
  arrange(desc(mean.activity)) %>% head(3)


df %>%  mutate(mean.heartrate = mean(.$Heart.Rate)) %>% 
  mutate(GrupaWiekowa = ifelse(Age >= 50, "Grupa 1 (≥50)", "Grupa 2 (<50)")) %>%
  filter(Occupation %in% c("Nurse","Lawyer","Accountant")) %>%
  group_by(Occupation, GrupaWiekowa, mean.heartrate) %>% 
  summarise(mean.top.heartrate =  mean(Heart.Rate)) %>% 
  mutate(dif.heartrate=abs(mean.heartrate-mean.top.heartrate)) %>% select(Occupation, GrupaWiekowa,dif.heartrate)

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
df %>%mutate(AgeGroup = case_when(
  Age < 10 ~ "[0,10)",
  Age >= 10 & Age < 20 ~ "[10,20)",
  Age >= 20 & Age < 30 ~ "[20,30)",
  Age >= 30 & Age < 40 ~ "[30,40)",
  Age >= 40 & Age < 50 ~ "[40,50)",
  Age >= 50 & Age < 60 ~ "[50,60)",
  Age >= 60 & Age < 70 ~ "[60,70)",
  Age >= 70 & Age < 80 ~ "[70,80)",
  Age >= 80 & Age < 90 ~ "[80,90)",
  Age >= 90 ~ "[90,100)"
)) %>% group_by(Occupation,AgeGroup,Gender) %>% summarise(n=n()) %>% arrange(AgeGroup, Gender, desc(n)) %>%
  group_by(AgeGroup, Gender) %>%
  slice(1) %>%
  ungroup()

df %>%mutate(AgeGroup = case_when(
  Age < 10 ~ "[0,10)",
  Age >= 10 & Age < 20 ~ "[10,20)",
  Age >= 20 & Age < 30 ~ "[20,30)",
  Age >= 30 & Age < 40 ~ "[30,40)",
  Age >= 40 & Age < 50 ~ "[40,50)",
  Age >= 50 & Age < 60 ~ "[50,60)",
  Age >= 60 & Age < 70 ~ "[60,70)",
  Age >= 70 & Age < 80 ~ "[70,80)",
  Age >= 80 & Age < 90 ~ "[80,90)",
  Age >= 90 ~ "[90,100)"
)) %>%
  group_by(AgeGroup) %>%
  summarise(
    SredniaStresu = mean(Stress.Level, na.rm = TRUE),
    MedianaStresu = median(Stress.Level, na.rm = TRUE),
    OdchylenieStandardowe = sd(Stress.Level, na.rm = TRUE)
  )
