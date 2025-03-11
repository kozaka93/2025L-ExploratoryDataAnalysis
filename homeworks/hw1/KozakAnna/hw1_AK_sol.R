# Ładowanie danych

df <- read.csv("Sleep_health_and_lifestyle_dataset.csv")
library(dplyr)


# Zadanie 1 (1p) ----------------------------------------------------------

# Określ jakiego typu są zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, binarna
# Age - ilościowa, ilorazowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep - jakościowa, uporządkowana
# Physical.Activity.Level - ilościowa, zliczeniowa
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder - jakościowa, nominalna (można się też uprzeć na uporządkowaną)


# Zadannie 2 (0.5p) -------------------------------------------------------

# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(BMI.Category) %>% 
  summarise(srednie_tetno = mean(Heart.Rate))


# Zadanie 3 (0.5p) --------------------------------------------------------

# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(srednia_sen = mean(Sleep.Duration)) %>% 
  top_n(1)


# Zadanie 4 (0.5p) --------------------------------------------------------

# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarise(n = n()) %>% 
  top_n(1)


# Zadanie 5 (0.5p) --------------------------------------------------------

# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(BMI.Category == "Obese") %>% 
  group_by(Gender, Occupation) %>% 
  summarise(n=n())


# Zadanie 6 (1p) ----------------------------------------------------------

# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który ma najmniejszą średnią różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  summarise(n = n()) %>% 
  filter(n > 20) %>% 
  left_join(df) %>% 
  mutate(skurczowe = as.numeric(substring(Blood.Pressure, 1, 3)),
         rozkurczowe = as.numeric(substring(Blood.Pressure, 5, 6)),
         roznica = (skurczowe - rozkurczowe)) %>% 
  group_by(Occupation) %>% 
  summarise(srednie = mean(roznica)) %>% 
  top_n(3) %>% 
  tail(1) %>% 
  left_join(df) %>% 
  group_by(Sleep.Disorder) %>% 
  summarise(srednia = mean(Sleep.Duration),
            mediana = median(Sleep.Duration))


# Zadanie 7 (1p) ----------------------------------------------------------

# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  summarise(sredni_ruch = mean(Daily.Steps)) %>% 
  top_n(3) %>% 
  left_join(df) %>% 
  mutate(lat50 = ifelse(Age >= 50, "grupa 1", "grupa 2")) %>% 
  group_by(Occupation, lat50) %>% 
  summarise(roznica = mean(df$Heart.Rate) - mean(Heart.Rate))


# Zadanie 8 (1p) ----------------------------------------------------------

# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  mutate(kubelki_wiek = cut(Age, breaks = seq(0, 100, 10), right = FALSE)) %>% 
  group_by(kubelki_wiek, Occupation) %>% 
  summarise(n = n()) %>% 
  top_n(1) %>% 
  ungroup() %>% 
  left_join(df, relationship = "many-to-many") %>%
  group_by(kubelki_wiek, Occupation) %>% 
  summarise(srednia = mean(Stress.Level),
            mediana = median(Stress.Level),
            std = sd(Stress.Level))

