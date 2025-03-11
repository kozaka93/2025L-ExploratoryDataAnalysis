#Agnieszka Kęska
# Ładowanie pakietów

library(forcats)
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, nominalna
# Age - ilościowa, ilorazowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep -ilościowa, przedziałowa
# Physical.Activity.Level - ilościowa, ilorazowa
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder - jakosciowa, nominalna



########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df1 <- df %>% select(BMI.Category, Heart.Rate) %>% group_by(BMI.Category) %>% summarise(mean_heart_rate = mean(Heart.Rate)) %>% arrange(-mean_heart_rate)
df1
head(df1,1)
#Największe średnie tętno jest dla kategorii otyłyej.


########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% filter(Stress.Level < 7) %>% group_by(Occupation) %>% summarise(mean_sleep_duration =mean(Sleep.Duration)) %>% arrange(-mean_sleep_duration) %>% head(1)
#Średnio najwięcej śpią inżynierowie.

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>% filter(Sleep.Disorder != "None") %>% group_by(Occupation) %>% summarise(n = n())
#Accountant,Doctor, Engineer,Lawyer,Nurse,Sales Representative,Scientist,Software Engineer,Teacher.



########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% select(Gender,Occupation,BMI.Category) %>% filter(BMI.Category == "Obese") %>% 
  group_by(Gender, Occupation) %>% summarise(n = n(),.groups="drop") %>% arrange(-n) %>% 
  pivot_wider(names_from = Gender, values_from = n, values_fill = 0) %>% filter(Male > Female)
#Doctor,Sales Representative,Software Engineer,Teacher.


########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>% select(Occupation, Blood.Pressure) %>% 
  separate(Blood.Pressure, into = c("systolic", "diastolic"), sep = "/", convert = TRUE) %>%
  mutate(diff = systolic - diastolic) %>% group_by(Occupation) %>% summarise(n = n(), mean_diff = mean(diff)) %>% 
  filter(n>20) %>% arrange(-mean_diff) %>% slice(1:3)

df %>% select(Occupation, Sleep.Duration,Quality.of.Sleep) %>% filter(Occupation == "Lawyer") %>%
  group_by(Quality.of.Sleep) %>% summarise(mean_lawyer_sleep_duration = mean(Sleep.Duration), median_lawyer_sleep_duration = median(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

top_occupations <- df %>% select(Occupation, Physical.Activity.Level, Heart.Rate) %>% group_by(Occupation ) %>% summarise(mean_physical_activity = mean(Physical.Activity.Level)) %>%
  arrange(-mean_physical_activity) %>%slice(1:3) %>% pull(Occupation)
df %>% filter(Occupation %in% top_occupations) %>% mutate(Age_Group = ifelse(Age >= 50, "Grupa1", "Grupa2")) %>%
  group_by(Age_Group) %>%
  summarise(mean_heart_rate_top = mean(Heart.Rate)) -> top_heart_rates
df %>%
  mutate(Age_Group = ifelse(Age >= 50, "Grupa1", "Grupa2")) %>%
  group_by(Age_Group) %>%
  summarise(mean_heart_rate_all = mean(Heart.Rate)) -> all_heart_rates
left_join(all_heart_rates, top_heart_rates, by = "Age_Group") %>%
  mutate(diff = mean_heart_rate_all - mean_heart_rate_top)

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------


df %>% mutate(Age_Group = cut(Age, 
                         breaks = seq(0, max(Age, na.rm = TRUE) + 10, by = 10), right = FALSE, include.lowest = TRUE)) %>%
  group_by(Age_Group, Gender) %>% summarise(most_common_occupation = Occupation %>% fct_infreq() %>% levels() %>% .[1],
                                            mean_stress = mean(Stress.Level),median_stress = median(Stress.Level),
                                            sd_stress = sd(Stress.Level),.groups = "drop")
              
              
              
              