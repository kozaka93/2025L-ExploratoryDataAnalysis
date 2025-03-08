# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakosciowe binarne
# Age - ilosciowe ilorazowe
# Occupation - jakosciowe nominalne
# Sleep.Duration - ilosciowe ilorazowe
# Quality.of.Sleep - jakosciowe uporzadkowane
# Physical.Activity.Level - jakosciowe uporzadkowane
# BMI.Category - jakosciowe uporzadkowane
# Sleep.Disorder - jakosciowe nominalne


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(BMI.Category,Heart.Rate) %>% 
  group_by(BMI.Category) %>% 
  summarise(mhr=mean(Heart.Rate)) %>% 
  arrange(-mhr) #najwieksze tetno ma kategoria w pierwszej kolumnie, a wiec Obese


########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(Stress.Level<7) %>% 
  group_by(Occupation) %>% 
  summarise(msd=mean(Sleep.Duration)) %>% 
  arrange(-msd) %>% 
  head(1)
  

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(Sleep.Disorder,Occupation) %>% 
  filter(Sleep.Disorder!="None") %>% 
  group_by(Occupation) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  head(3)

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(BMI.Category=="Obese") %>% 
  select(Gender,Occupation) 
  #jak widac te zawody to Sales Representative, Software Engineer, Doctor, Teacher

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  group_by(Occupation) %>% 
  filter(n()>20) %>%
  select(Blood.Pressure, Occupation, Sleep.Duration) %>% 
  separate(Blood.Pressure, into = c("sk", "roz"), sep = "/", convert = TRUE) %>%
  mutate(diff = sk - roz) %>% 
  summarise(ww=mean(diff)) %>% 
  arrange(-ww) %>% 
  head(3)
#drugi zawod to Lawyer
df %>% 
  filter(Occupation=="Lawyer") %>% 
  select(Sleep.Duration, Quality.of.Sleep) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(mediana=median(Sleep.Duration), srednia=mean(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
sre<-mean(df$Heart.Rate)

df %>% 
  group_by(Occupation) %>% 
  summarise(minn=mean(Physical.Activity.Level)) %>% 
  arrange(-minn) %>% 
  head(3)   #zawody to nurse lawyer i accountant

#grupa 1
df %>% 
  filter(Age>49, Occupation %in% c("Nurse","Lawyer","Accountant")) %>% 
  group_by(Occupation) %>% 
  summarise(uyk=mean(Physical.Activity.Level)-sre)
  

#grupa 2
df %>% 
  filter(Age<50, Occupation %in% c("Nurse","Lawyer","Accountant")) %>% 
  group_by(Occupation) %>% 
  summarise(uk=mean(Physical.Activity.Level)-sre)
  




########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  mutate(kat = Age %/% 10) %>% 
  group_by(kat, Gender) %>% 
  summarise(zaw = names(sort(table(Occupation), decreasing = TRUE))[1], sred=mean(Stress.Level), med=median(Stress.Level), odch=sd(Stress.Level), .groups = "drop")


  
