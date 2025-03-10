# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

types <- sapply(df, class)
print(types)

# Gender - "character"
# Age - "integer"
# Occupation - "character"
# Sleep.Duration - "numeric"
# Quality.of.Sleep - "integer"
# Physical.Activity.Level - "integer"
# BMI.Category - "character"
# Sleep.Disorder - "character"


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>%
  group_by(BMI.Category) %>%
  summarise(Avg_bp = mean(Heart.Rate, na.rm = TRUE)) %>%
  arrange(desc(Avg_bp))


########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>%
  filter(Stress.Level < 7) %>%
  group_by(Occupation) %>%
  summarise(Sredni_Sen = mean(Sleep.Duration, na.rm = TRUE)) %>%
  arrange(desc(Sredni_Sen)) %>%
  head(1)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>%
  filter(Sleep.Disorder != "None") %>%
  count(Occupation, sort = TRUE)

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

df %>%
  filter(BMI.Category == "Obese") %>%
  group_by(Occupation, Gender) %>%
  tally() %>%
  spread(Gender, n, fill = 0) %>%
  filter(Male > Female) %>% 
  select(Occupation)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

top_op <- df %>%
  separate(Blood.Pressure, into = c("S_bp", "D_bp"), sep = "/", convert = TRUE) %>%
  mutate(Diff_bp = S_bp - D_bp) %>%
  group_by(Occupation) %>%
  filter(n() > 20) %>%  # Tylko zawody z >20 przykładami
  summarise(Srednia_Roznica_BP = mean(Diff_bp, na.rm = TRUE)) %>%
  arrange(desc(Srednia_Roznica_BP)) %>%
  head(3)

top_op

df %>%
  filter(Occupation == top_op$Occupation[2]) %>%
           group_by(Quality.of.Sleep) %>%
           summarise(
             Avg_Sleep = mean(Sleep.Duration, na.rm = TRUE),
             Med_Sleep = median(Sleep.Duration, na.rm = TRUE))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

top_3_ac <- df %>%
  group_by(Occupation) %>%
  summarise(Avg_ac = mean(Physical.Activity.Level, na.rm = TRUE)) %>%
  arrange(desc(Avg_ac)) %>%
  head(3) %>%
  pull(Occupation)

df %>%
  filter(Occupation %in% top_3_ac) %>%
  mutate(Age_Group = ifelse(Age >= 50, "50+", "<50")) %>%
  group_by(Age_Group) %>%
  summarise(Avg_bp= mean(Heart.Rate, na.rm = TRUE))
Avg_bp_gen <- mean(df$Heart.Rate)
Avg_bp_gen
########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
df <- df %>%
  mutate(Age_Bin = cut(Age, breaks = seq(0, 100, by = 10), right = FALSE))

df %>%
  group_by(Age_Bin, Gender, Occupation) %>%
  tally() %>%
  arrange(Age_Bin, Gender, desc(n)) %>%
  group_by(Age_Bin, Gender) %>%
  slice(1)

df %>%
  group_by(Age_Bin) %>%
  summarise(Avg_Stress = mean(Stress.Level, na.rm = TRUE),
            Mediana_Stress = median(Stress.Level, na.rm = TRUE),
            SD_Stress = sd(Stress.Level, na.rm = TRUE))
