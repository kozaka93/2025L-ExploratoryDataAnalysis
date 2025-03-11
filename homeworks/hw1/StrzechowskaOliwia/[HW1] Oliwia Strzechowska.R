# Ładowanie pakietów
library(dplyr)
library(tidyr)


# Ładowanie danych
df <- read.csv("/Users/oliwiastrzechowska/Desktop/Oliwia_Strzechowska/data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, nominalna/binarna
# Age - ilościowa, przedziałowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, przedziałowa
# Quality.of.Sleep - ilościowa, przedziałowa
# Physical.Activity.Level - ilościowa, przedziałowa
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder - jakościowa, nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

avg_hr <- df %>% 
  group_by(BMI.Category) %>% 
  summarise(mean_heart_rate = mean(Heart.Rate, na.rm = TRUE)) %>% 
  arrange(desc(mean_heart_rate))
  

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

avg_sleep <- df %>% 
  group_by(Occupation) %>% 
  filter(Stress.Level < 7) %>% 
  summarise(mean_sleep_duration = mean(Sleep.Duration, na.rm = TRUE)) %>% 
  arrange(desc(mean_sleep_duration)) %>% 
  head(1)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

sleep_dis <- df %>% 
  group_by(Occupation) %>% 
  filter(Sleep.Disorder != "None") %>% 
  count(Occupation) %>% 
  arrange(desc(n)) 
  

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

obese <- df %>% 
  filter(BMI.Category == "Obese") %>% 
  group_by(Occupation, Gender) %>% 
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = count, values_fill = list(count = 0)) %>% 
  filter(Male > Female)
  

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

tab1 <- df %>% 
  separate(Blood.Pressure, into = c("systolic", "diastolic"), sep = "/", convert = TRUE) %>%
  mutate(pressure_difference = systolic - diastolic) %>% 
  group_by(Occupation) %>% 
  summarise(count = n(), mean_pressure = mean(pressure_difference, na.rm = TRUE)) %>% 
  filter(count > 20) %>% 
  arrange(desc(mean_pressure)) %>% 
  head(3)
  
sec_occupation <- tab1 %>% 
  nth(2) %>% 
  pull(Occupation)
    
av_med_sleep <- df %>% 
  filter(Occupation == sec_occupation) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(mean_sleep_dur = mean(Sleep.Duration, na.rm = TRUE), median_sleep_dur = median(Sleep.Duration, na.rm = TRUE))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df1 <- df %>% 
  mutate(
    age_category = ifelse(Age >= 50, "grupa1", "grupa2")
  )
  
th_occupations <- df1 %>% 
  group_by(Occupation) %>% 
  summarise(mean_physical_activity_leveL = mean(Physical.Activity.Level, na.rm = TRUE)) %>% 
  arrange(desc(mean_physical_activity_leveL)) %>% 
  head(3) %>% 
  pull(Occupation)

ov_mean_heart_rate <- df1 %>% 
  group_by(age_category) %>% 
  summarise(mean_ov = mean(Heart.Rate)) 

th_mean_heart_rate<- df1 %>% 
  filter(Occupation %in% th_occupations) %>% 
  group_by(age_category) %>%    
  summarise(mean_th = mean(Heart.Rate))
  
mer <- merge(th_mean_heart_rate, ov_mean_heart_rate, by = "age_category")

roznica <- mer %>% 
  group_by(age_category) %>% 
  summarise(diff = mean_th - mean_ov)


########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df2 <- df %>% 
  mutate(
    age_cat = case_when(
      Age < 10 ~ '[0,10)',
      Age < 20 ~ '[10, 20)',
      Age < 30 ~ '[20, 30)',
      Age < 40 ~ '[30, 40)',
      Age < 50 ~ '[40, 50)',
      TRUE ~ '[50, 60)'
    )
  )

pop_occ <- df2 %>% 
  group_by(age_cat, Gender) %>% 
  count(Occupation) %>%   
  filter(n == max(n)) %>% 
  select(age_cat, Gender, Occupation) %>% 
  rename(TopOccupation = Occupation)
  
mean_med_dev <- df2 %>% 
  group_by(age_cat) %>% 
  summarise(mean_stress = mean(Stress.Level), med_stress = median(Stress.Level), standard_dev_stress = sd(Stress.Level))

  
  









