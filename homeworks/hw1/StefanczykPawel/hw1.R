# Ładowanie pakietów
library(dplyr)
library(tidyr)


# Ładowanie danych
df <- read.csv("C:/Users/Paawel/Desktop/homeworks_data/StefanczykPawel/data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowe(binarne) 
# Age - jakościowe(uporządkowane)
# Occupation - jakościowe(nominalne)
# Sleep.Duration - ilościowe(ilorazowe)
# Quality.of.Sleep - jakościowe(uporządkowane)
# Physical.Activity.Level - ilościowe(ilorazowe)
# BMI.Category - jakościowe(nomianalne)
# Sleep.Disorder - jakościowe(nominalne)


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
unique(df$BMI.Category)

df_separated <- df %>% group_by(BMI.Category) %>% summarise(srednie_tetno = mean(Heart.Rate)) %>%
                arrange(desc(srednie_tetno))

df_separated %>% slice(1)

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df_stress <- df %>% filter(Stress.Level < 7) %>% group_by(Occupation) %>% summarise(sredni_sen = mean(Sleep.Duration, na.rm = TRUE)) %>%
              arrange(desc(sredni_sen))
df_stress %>% slice(1) #Ci najwiecej spią

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
unique(df$Sleep.Disorder)

df_disorders <- df %>% filter(Sleep.Disorder != "None") %>% group_by(Occupation) %>%
                summarise(liczba_zaburzen = n()) %>% arrange(desc(liczba_zaburzen))

df_disorders %>% slice(1)
########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df_obese <- df %>% filter(BMI.Category == "Obese") %>% group_by(Occupation, Gender) %>%
            summarise(liczba_otylych = n(), .groups = "drop") %>%
            pivot_wider(names_from = Gender, values_from = liczba_otylych, values_fill = 0) %>%
            filter(Male > Female)
print(df_obese$Occupation)
            

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
df_20_pressure <- df %>% separate(Blood.Pressure, into = c("Skurcz", "Rozkurcz"), sep = "/", convert = TRUE) %>%
                  mutate(roznica_cisnienia = Skurcz - Rozkurcz) %>% group_by(Occupation) %>%
                  filter(n() > 20) %>% summarise(srednia_roznica = mean(roznica_cisnienia, na.rm = TRUE), liczba_przykladow = n()) %>%
                  arrange(desc(srednia_roznica)) %>% slice_head(n = 3)

drugi_zawod <- df_20_pressure %>% slice(2) %>% pull(Occupation)
df_sleep_quality <- df %>% filter(Occupation == drugi_zawod) %>% group_by(Quality.of.Sleep) %>%
                    summarise(sredni_sen = mean(Sleep.Duration),
                              mediana_snu = median(Sleep.Duration),
                              liczba_przykladow = n()) %>% filter(liczba_przykladow > 20) %>%
                    arrange(desc(Quality.of.Sleep))
print(df_sleep_quality)

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
df <- df %>% mutate(Wiek_grupa = ifelse(Age < 50, "<50", ">=50"))

top_3_occupations <- df %>% group_by(Occupation) %>% summarise(sredni_ruch = mean(Physical.Activity.Level)) %>%
                      arrange(desc(sredni_ruch)) %>% slice_head(n = 3) %>% pull(Occupation)

srednie_tetno_all <- mean(df$Heart.Rate)

srednie_tetno_podzial <- df %>% filter(Occupation %in% top_3_occupations) %>% group_by(Wiek_grupa) %>%
                        summarise(srednie_tetno = mean(Heart.Rate))

roznica_tetn <- srednie_tetno_podzial %>% mutate(roznica = srednie_tetno - srednie_tetno_all)
print(roznica_tetn)

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
df <- df %>%
  mutate(Wiek_Grupa = cut(Age, 
                          breaks = seq(0, 100, by = 10),  
                          right = FALSE,  
                          labels = paste0("[", seq(0, 90, by = 10), ",", seq(10, 100, by = 10), ")")))  
df_popular_jobs <- df %>% group_by(Wiek_Grupa, Gender, Occupation) %>%
                    summarise(liczba_osob = n(), .groups = "drop") %>%
                    arrange(Wiek_Grupa, Gender, desc(liczba_osob)) %>%
                    group_by(Wiek_Grupa, Gender) %>% slice_head(n = 1)   

print(df_popular_jobs)

df_stress_statistics <- df %>% group_by(Wiek_Grupa) %>%
                        summarise(
                        Sredni_Stres = mean(Stress.Level, na.rm = TRUE),
                        Mediana_Stres = median(Stress.Level, na.rm = TRUE),
                        Odchylenie_Stres = sd(Stress.Level, na.rm = TRUE),
                        liczba_osob = n())

print(df_stress_statistics)

df_final <- df_popular_jobs %>%
            pivot_wider(names_from = Gender, values_from = Occupation, names_prefix = "Zawod_") %>%
            inner_join(df_stress_statistics, by = "Wiek_Grupa")  

print(df_final)

