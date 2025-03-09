# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")
View(df)

########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa binarna
# Age - ilościowa przedziałowa
# Occupation - jakościowa nominalna
# Sleep.Duration - ilościowa ilorazowa
# Quality.of.Sleep - ilościowa uporządkowana
# Physical.Activity.Level - ilościowa ilorazowa
# BMI.Category - jakościowa nominalna
# Sleep.Disorder - jakościowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  group_by(BMI.Category) %>%
  summarise(mean_heart.rate = mean(Heart.Rate)) %>% 
  arrange(desc(mean_heart.rate))

# Średnie tętno jest najwyższe dla kategorii: Obese.

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(Stress.Level < 7 ) %>% 
  group_by(Occupation) %>% 
  summarise(mean_sleep.duration = mean(Sleep.Duration)) %>% 
  arrange(desc(mean_sleep.duration))

# Ludzie z zawodu engineer średnio najwięcej śpią, jeśli ich poziom stresu jest poniżej 7.

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześciej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(Sleep.Disorder != 'None') %>% 
  group_by(Occupation) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Osoby pracujące jako nurse mają dowolne zaburzenie snu. 

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(BMI.Category == 'Obese') %>%
  group_by(Occupation, Gender) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = Gender, values_from = n, values_fill = c(n = 0)) %>% 
  filter(Male > Female) %>% 
  select(Occupation, Male, Female)

# W zawodach takich jak: Doctor, Sales Representative, Software Engineer, Teacher 
# jest więcej otyłych mężczyzn niż otyłych kobiet.

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  separate(Blood.Pressure, into = c("skurczowe","rozkurczowe"), sep = "/", convert = TRUE) %>% 
  mutate(blood.pressure.diff = skurczowe - rozkurczowe) %>% 
  group_by(Occupation) %>% 
  summarise(n = n(), mean_blood.pressure.diff = mean(blood.pressure.diff)) %>% 
  filter(n > 20) %>% 
  arrange(desc(mean_blood.pressure.diff)) %>% 
  head(3) -> top_3_occupations

chosen_occupation <- top_3_occupations$Occupation[2]

df %>% 
  filter(Occupation == chosen_occupation) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(mean_sleep.duration = mean(Sleep.Duration, na.rm = TRUE),
            median_sleep.duration = median(Sleep.Duration, na.rm = TRUE)) %>% 
  arrange(Quality.of.Sleep)

# Dla snu o jakości równej 7 średni czas to 7.18 godziny, a mediana czasu snu to 7.1 godziny.
# Dla snu o jakości równej 8 średni czas to 7.44 godziny, a mediana czasu snu to 7.3 godziny.

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  group_by(Occupation) %>% 
  summarise(mean_daily.steps = mean(Daily.Steps, na.rm = TRUE)) %>% 
  arrange(desc(mean_daily.steps)) %>% 
  head(3) -> top_3_occup_sports
print(top_3_occup_sports)

# Średnio najbardziej ruszają się osoby z zawodów: Nurse, Lawyer i Accountant.

general_mean_heart.rate = mean(df$Heart.Rate, na.rm = TRUE)

df %>% 
  filter(Occupation == 'Nurse' | Occupation == 'Lawyer' | Occupation == 'Accountant') %>% 
  mutate(Age.Group = ifelse(Age >= 50, '1', '2')) %>% 
  group_by(Age.Group) %>% 
  summarise(mean_heart.rate = mean(Heart.Rate)) -> df_age.group_heart.rate

diff_mean_hr_group_1 <- abs(general_mean_heart.rate - df_age.group_heart.rate[1,2])  
diff_mean_hr_group_2 <- abs(general_mean_heart.rate - df_age.group_heart.rate[2,2])  

print(diff_mean_hr_group_1)
print(diff_mean_hr_group_2)

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
df %>%
  mutate(Age.Bucket = cut(Age, breaks = seq(0, 100, by = 10), include.lowest = TRUE, right = FALSE)) %>% 
  group_by(Age.Bucket, Gender, Occupation) %>% 
  summarise(Count = n()) %>% 
  arrange(Age.Bucket, Gender, desc(Count)) %>% 
  group_by(Age.Bucket, Gender) %>% 
  slice_max(order_by = Count, n = 1) %>% 
  View()

df %>% 
  mutate(Age.Bucket = cut(Age, breaks = seq(0, 100, by = 10), include.lowest = TRUE, right = FALSE)) %>% 
  group_by(Age.Bucket) %>% 
  summarise(mean_stress.level = mean(Stress.Level, na.rm = TRUE),
            median_stress.level = median(Stress.Level, na.rm = TRUE),
            sd_stress.level = sd(Stress.Level, na.rm = TRUE)
            ) %>% 
  View()
