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
# Sleep.Duration - zmienna ilościowa zliczeniowa
# Quality.of.Sleep - zmienna jakościowa uporządkowana
# Physical.Activity.Level - zmienna ilościowa ilorazowa
# BMI.Category - zmienna jakościowa uporządkowana
# Sleep.Disorder - zmienna jakościowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średnie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

zadanko_2 <- df %>% 
  group_by(BMI.Category) %>% 
  summarise(srednie_tetno = mean(Heart.Rate, na.rm = TRUE)) %>% 
  arrange(desc(srednie_tetno))


########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

zadanko_3 <- df %>% 
  filter(Stress.Level < 7) %>% 
  group_by(Occupation) %>% 
  summarise(sredni_sen = mean(Sleep.Duration, na.rm = TRUE)) %>% 
  arrange(desc(sredni_sen)) %>% 
  head(1)


########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześciej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

zadanko_4 <- df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarise(amount_of_sleep_disorders = n()) %>% 
  arrange(desc(amount_of_sleep_disorders))


########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

zadanko_5 <- df %>% 
  filter(BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  mutate(male_count = ifelse(Gender == "Male", count, 0),
         female_count = ifelse(Gender == "Female", count, 0)) %>% 
  group_by(Occupation) %>% 
  summarise(obese_male = sum(male_count), obese_female = sum(female_count)) %>% 
  filter(obese_male > obese_female) %>% 
  select(Occupation)


########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

zadanko_6_1 <- df %>% 
  mutate(systolic = as.numeric(sapply(strsplit(Blood.Pressure, "/"), `[`, 1)),
         diastolic = as.numeric(sapply(strsplit(Blood.Pressure, "/"), `[`, 2))) %>% 
  mutate(pressure_difference = systolic - diastolic) %>% 
  group_by(Occupation) %>% 
  filter(n() > 20) %>% 
  summarise(average_difference = mean(pressure_difference, na.rm = TRUE)) %>% 
  arrange(desc(average_difference)) %>% 
  head(3)

zadanko_6_2 <- df %>% 
  filter(Occupation == zadanko_6_1$Occupation[2]) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(mean_sleep = mean(Sleep.Duration, na.rm = TRUE),
            median_sleep = median(Sleep.Duration, na.rm = TRUE))


########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

zadanko_7_1 <- df %>% 
  group_by(Occupation) %>% 
  summarise(mean_activity = mean(Physical.Activity.Level, na.rm = TRUE)) %>% 
  arrange(desc(mean_activity)) %>% 
  head(3)

mean_heart_rate_all <- df %>% 
  mutate(age_group = ifelse(Age >= 50, "grupa 1 (>=50)", "grupa 2 (<50)")) %>% 
  group_by(age_group) %>% 
  summarise(mean_heart_rate_all = mean(Heart.Rate, na.rm = TRUE))

zadanko_7_2 <- df %>% 
  filter(Occupation %in% zadanko_7_1$Occupation) %>% 
  mutate(age_group = ifelse(Age >= 50, "grupa 1 (>=50)", "grupa 2 (<50)")) %>% 
  group_by(age_group) %>% 
  summarise(mean_heart_rate_selected = mean(Heart.Rate, na.rm = TRUE)) %>% 
  left_join(mean_heart_rate_all, by = "age_group") %>% 
  mutate(difference = mean_heart_rate_all - mean_heart_rate_selected) %>% 
  select(age_group, difference)


########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

zadanko_8_1 <- df %>% 
  mutate(age_group = cut(Age, breaks = seq(0, max(Age) + 10, by = 10), right = FALSE))

zadanko_8_2 <- df %>% 
  mutate(age_group = cut(Age, breaks = seq(0, max(Age) + 10, by = 10), right = FALSE)) %>% 
  group_by(age_group, Gender) %>% 
  summarise(popular_occupation = names(sort(table(Occupation), decreasing = TRUE)[1]),
            stress_mean = mean(Stress.Level, na.rm = TRUE),
            stress_median = median(Stress.Level, na.rm = TRUE),
            stress_sd = sd(Stress.Level, na.rm = TRUE),
            .groups = "drop")

