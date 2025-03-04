# Ładowanie pakietów
library(dplyr)
library(tidyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa (binarna)
# Age - ilościowa (ilorazowa)
# Occupation - jakościowa (nominalna)
# Sleep.Duration - ilościowa (ilorazowa)
# Quality.of.Sleep - ilościowa (ilorazowa)
# Physical.Activity.Level - ilościowa (ilorazowa)
# BMI.Category - jakościowa (uporządkowana)
# Sleep.Disorder - jakościowa (nominalna)


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
srednie_tetno <- df %>% 
  group_by(BMI.Category) %>% 
  summarise(avg_hr = mean(Heart.Rate)) %>% 
  arrange(-avg_hr)

# Najwyższe tętno jest dla kategorii
head(srednie_tetno, 1)
########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
najwiecej_spiacy <- df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(avg_sleep = mean(Sleep.Duration)) %>% 
  arrange(-avg_sleep) %>% 
  head(1)
najwiecej_spiacy

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 



# Rozwiązanie: ------------------------------------------------------------
zawody <- df %>% 
  mutate(Has.Sleep.Problem = ifelse(Sleep.Disorder == "None", 0, 1)) %>% 
  group_by(Occupation) %>% 
  summarise(Odsetek = sum(Has.Sleep.Problem)/n()) %>% 
  arrange(-Odsetek) %>% 
  head
zawody

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
zawody_wiecej_mezczyzn <- df %>%
  filter(BMI.Category == "Obese") %>%
  group_by(Occupation, Gender) %>%
  summarise(Liczba_otylych = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Gender, values_from = Liczba_otylych, values_fill = list(Liczba_otylych = 0)) %>%
  filter(Male > Female)

print(zawody_wiecej_mezczyzn)
########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
zawody_mniej_20 <- df %>% 
  group_by(Occupation) %>% 
  summarise(number = n()) %>% 
  filter(number <= 20)
zawody <- df %>% 
  filter(!Occupation %in% zawody_mniej_20$Occupation) %>% 
  separate(Blood.Pressure, into = c("skurczowe", "rozkurczowe"), sep = "/", convert = TRUE) %>% 
  mutate(Roznica = skurczowe - rozkurczowe) %>% 
  group_by(Occupation) %>% 
  summarise(srednie_cisnienie = mean(Roznica)) %>% 
  arrange(-srednie_cisnienie) %>% 
  head(3)
drugi <- zawody$Occupation[2]
sen_prawnikow <- df %>% 
  filter(Occupation == drugi) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(srednia = mean(Sleep.Duration), mediana = median(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
zawody <- df %>% 
  group_by(Occupation) %>% 
  summarise(ruch = mean(Daily.Steps)) %>% 
  arrange(-ruch) %>% 
  head(3)

zawody_mniej_50 <- df %>% 
  filter(Occupation %in% zawody$Occupation, Age < 50) %>% 
  summarise(tetno = mean(Heart.Rate))

  

zawody_wiecej_50 <- df %>% 
  filter(Occupation %in% zawody$Occupation, Age >= 50) %>% 
  summarise(tetno = mean(Heart.Rate))

wszystkie_mniej_50 <- df %>% 
  filter(Age < 50) %>% 
  summarise(tetno = mean(Heart.Rate))

wszystkie_wiecej_50 <- df %>% 
  filter(Age >= 50) %>% 
  summarise(tetno = mean(Heart.Rate))

tabela <- data.frame(
  Wiek = c("Mniej niż 50", "Więcej niż 50"),
  Roznica = c(abs(wszystkie_mniej_50$tetno[1] - zawody_mniej_50$tetno[1]), abs(wszystkie_wiecej_50$tetno[1] - zawody_wiecej_50$tetno[1]))
)

tabela
########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
df <- df %>%
  mutate(Age.Group = cut(Age, breaks = seq(0, 100, by = 10), right = FALSE))


grouped_data <- df %>%
  group_by(Age.Group, Gender, Occupation) %>%
  summarise(Count = n(), .groups = 'drop')


most_popular_occupation <- grouped_data %>%
  group_by(Age.Group, Gender) %>%
  slice_max(order_by = Count, n = 1) %>%
  select(Age.Group, Gender, Occupation, Count)


stress_stats <- df %>%
  group_by(Age.Group) %>%
  summarise(
    Mean.Stress.Level = mean(Stress.Level, na.rm = TRUE),
    Median.Stress.Level = median(Stress.Level, na.rm = TRUE),
    SD.Stress.Level = sd(Stress.Level, na.rm = TRUE)
  )


print("Najbardziej popularny zawód w każdej grupie wiekowej:")
print(most_popular_occupation)

print("Statystyki poziomu stresu w każdej grupie wiekowej:")
print(stress_stats)
