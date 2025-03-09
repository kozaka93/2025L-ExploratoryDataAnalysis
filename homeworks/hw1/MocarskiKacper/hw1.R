# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - zmienna jakościowa binarna
# Age - zmienna ilościowa ilorazowa
# Occupation - zmienna jakościowa nominalna
# Sleep.Duration - zmienna ilościowa ilorazowa
# Quality.of.Sleep -zmienna jakościowa uporządkowana
# Physical.Activity.Level - zmienna ilościowa ilorazowa
# BMI.Category - zmienna jakościowa uporządkowana
# Sleep.Disorder - zmienna jakościowa nominalna

########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

tętno<- df %>% 
  select(BMI.Category,Heart.Rate) %>% 
  group_by(BMI.Category) %>% 
  summarise(średnie_tętno=mean(Heart.Rate))

print(tętno) 

tętno %>% 
  top_n(1,średnie_tętno)   

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Occupation,Sleep.Duration,Stress.Level) %>% 
  filter(Stress.Level<7) %>% 
  group_by(Occupation) %>% 
  summarise(średnia_długość_snu=mean(Sleep.Duration)) %>% 
  top_n(1,średnia_długość_snu) 

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Occupation,Sleep.Disorder) %>% 
  filter(Sleep.Disorder!= 'None') %>% 
  count(Occupation) %>% 
  arrange(-n)  #lista zawodów, w których ludzie mają dowolne zaburzenia snu, uporządkowana malejąco

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Gender,BMI.Category,Occupation) %>% 
  filter(BMI.Category=="Obese") %>% 
  group_by(Occupation) %>% 
  summarise(n_m = sum(Gender == "Male"),n_f = sum(Gender == "Female")) %>%
  filter(n_m > n_f) %>% 
  select(Occupation)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

szukane_zawody <- df %>% 
  select(Occupation,Blood.Pressure) %>% 
  group_by(Occupation) %>% 
  filter(n()>20) %>% 
  mutate(
    skurczowe = as.numeric(sapply(strsplit(as.character(Blood.Pressure), "/"), `[`, 1)), 
    rozkurczowe = as.numeric(sapply(strsplit(as.character(Blood.Pressure), "/"), `[`, 2))) %>% 
  summarise(różnica=mean(skurczowe)-mean(rozkurczowe)) %>% 
  arrange(-różnica) %>% 
  head(3) %>% 
  select(Occupation)
print(szukane_zawody)

# tutaj nie jestem pewien jak interpretować drugi pod względem średniej różnicy, ponieważ
# Salesperson i Lawyer mają właściwie tą samą różnice więc powinni być ex aequo pierwsi 
# wtedy powinno się rozpatrywać Nurse, jednak ja przyjmę kolejność sugerowaną przez R

wybrany_zawód <- szukane_zawody[2,1]

df %>% 
  filter(Occupation %in% wybrany_zawód) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(średnia=mean(Sleep.Duration),mediana=median(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

zawody <- df %>% 
  group_by(Occupation) %>% 
  summarise(średnia_aktywność=mean(Physical.Activity.Level)) %>%
  arrange(-średnia_aktywność) %>% 
  select(Occupation) %>% 
  head(3) %>% 
  pull(Occupation)
print(zawody)

df %>% 
  mutate(grupa_wiekowa = ifelse(Age >= 50, "grupa_1", "grupa_2")) %>% 
  group_by(grupa_wiekowa) %>% 
  summarise(
    średnia_ogólna = mean(Heart.Rate),
    średnia_zawody = mean(Heart.Rate[Occupation %in% zawody], na.rm = TRUE),
    różnica = średnia_ogólna - średnia_zawody)




########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  mutate(grupa_wiekowa= case_when(
    Age<10 ~ "[0,10)",
    10<=Age & Age<20 ~ "[10,20)", 
    20<=Age & Age<30 ~ "[20,30)",
    30<=Age & Age<40 ~ "[30,40)",
    40<=Age & Age<50 ~ "[40,50)",
    50<=Age & Age<60 ~ "[50,60)")) %>% 
  group_by(grupa_wiekowa, Gender, Occupation) %>%  
  summarise(liczba_wystapien = n(), średnia_stresu=mean(Stress.Level)
            ,mediana_stresu=median(Stress.Level),odch._stresu=sd(Stress.Level)) %>% 
  group_by(grupa_wiekowa, Gender) %>% 
  arrange(desc(liczba_wystapien)) %>% 
  slice_head(n = 1) %>% 
  select(-liczba_wystapien) #tabela z najpopularniejszymi zawodami
                            #w danych grupach wiekowych z podziałem na płcie

# nie jestem pewien, w których grupach trzeba było policzyć te parametry więc
# powyżej są policzone grupując ze względu na wiek, najpopularniejszy zawód, i płeć,
#a poniżej tylko ze względu na wiek

df %>% 
  mutate(grupa_wiekowa= case_when(
    Age<10 ~ "[0,10)",
    10<=Age & Age<20 ~ "[10,20)", 
    20<=Age & Age<30 ~ "[20,30)",
    30<=Age & Age<40 ~ "[30,40)",
    40<=Age & Age<50 ~ "[40,50)",
    50<=Age & Age<60 ~ "[50,60)")) %>% 
  group_by(grupa_wiekowa) %>% 
  summarise(średnia_stresu=mean(Stress.Level)
            ,mediana_stresu=median(Stress.Level),odch._stresu=sd(Stress.Level))
