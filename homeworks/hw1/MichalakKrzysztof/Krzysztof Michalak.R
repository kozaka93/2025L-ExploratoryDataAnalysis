# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - Jakościowe Binarne
# Age - Ilościowe Ilorazowe
# Occupation - Jakościowe Nominalne
# Sleep.Duration - Ilościowa Ilorazowa
# Quality.of.Sleep -Jakościowa Uporządkowana
# Physical.Activity.Level - Ilościowa Przedziałowa
# BMI.Category - Jakościowa Uporządkowana
# Sleep.Disorder -Jakościowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

Srednie_tetno_od_BMI  <- df %>% 
  group_by(BMI.Category) %>% 
  summarise(srednie_tetno = mean(Heart.Rate, na.rm = TRUE)) %>% 
  arrange(desc(srednie_tetno))


########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?
Najwiecej_snu_jak_stres_ponizej_7 <- df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(sredni_sen = mean(Sleep.Duration, na.rm = TRUE)) %>% 
  arrange(desc(sredni_sen)) %>% 
  head(1)


########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

Zawody_z_zaburzeniami_snu <- df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarise(Problemy_ze_snem = n()) %>% 
  arrange(desc(Problemy_ze_snem))


########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

Zawody_z_otylymi_mezczyznami <-df %>% filter(BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>% 
  summarise(ilosc = n()) %>% 
  summarise(faceci = sum(ilosc[Gender == "Male"]),
            kobiety = sum(ilosc[Gender == "Female"])) %>% 
  filter(faceci > kobiety) %>% 
  select(Occupation)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

Zawody_zad_6.1 <- df %>% 
  select(Occupation,Blood.Pressure) %>% 
  group_by(Occupation) %>% 
  filter(n()>20) %>% 
  mutate(
    skurczowe = as.numeric(sapply(strsplit(as.character(Blood.Pressure), "/"), "[", 1)), 
    rozkurczowe = as.numeric(sapply(strsplit(as.character(Blood.Pressure), "/"), "[", 2))) %>% 
  summarise(różnica=mean(skurczowe)-mean(rozkurczowe)) %>% 
  arrange(-różnica) %>% 
  head(3) %>% 
  select(Occupation)
Zawod_zad_6.2<-df %>% 
  filter(Occupation %in% Zawody_zad_6.1[2,1]) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(średnia=mean(Sleep.Duration),mediana=median(Sleep.Duration))


########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

Zawody_ruszajace_sie <-df %>% 
  group_by(Occupation) %>% 
  summarise(ruch = mean(Physical.Activity.Level)) %>% 
  arrange(-ruch) %>% 
  head(3) %>% 
  select(Occupation)

mean((df%>%filter(Age>=50)) $Heart.Rate) -> srednie_tetno_50_wiecej
mean((df%>%filter(Age<=50)) $Heart.Rate) -> srednie_tetno_50_mniej
#trzymam jako zmienna zeby nie zliczal pare razy program
roznica<-df %>% 
  filter(Occupation %in% c("Nurse", "Lawyer", "Accountant")) %>% 
  group_by(Occupation)%>% 
  summarise(starsi=mean(Heart.Rate[Age>=50]-srednie_tetno_50_wiecej),
            mlodsi=mean(Heart.Rate[Age<50]-srednie_tetno_50_mniej))
  


########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

Pogrupowani_wzgl_wieku <- df %>% 
  mutate(grupa_wiekowa= cut(Age, breaks = seq(0, max(Age) + 10, by = 10), right = FALSE))
Zadanie8.2 <- df %>% 
  mutate(grupa_wiekowa = cut(Age, breaks = seq(0, max(Age) + 10, by = 10), right = FALSE)) %>% 
  group_by(grupa_wiekowa, Gender) %>% 
  summarise(zawody_popularne= names(sort(table(Occupation), decreasing = TRUE)[1]),
            stres_s = mean(Stress.Level, na.rm = TRUE),
            stres_m = median(Stress.Level, na.rm = TRUE),
            stres_otchylenie = sd(Stress.Level, na.rm = TRUE),
            .groups = "drop")

