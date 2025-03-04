# Ładowanie pakietów
library(dplyr)

# Ładowanie danych
df <- read.csv("/Users/misiaduda/Downloads/data.csv")
View(df)

########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, binarna
# Age - ilościowa przedzialowa
# Occupation - jakosciowa nominalna 
# Sleep.Duration - ilosciowa ilorazowa
# Quality.of.Sleep - jakosciowa uporzadkowana
# Physical.Activity.Level - ilosciowa ilorazowa
# BMI.Category - jakosciowa nominalna
# Sleep.Disorder - jakosciowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie:
df %>%
  group_by(BMI.Category) %>%
  summarise(mean_heart.rate = mean(Heart.Rate)) %>% 
  arrange(desc(mean_heart.rate))
#Srednie tetno jest najwyższe dla kategorii Obese.

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>%
  filter(Stress.Level > 7) %>%
  group_by(Occupation) %>% 
  summarise(mean_sleep.duration = mean(Sleep.Duration)) %>% 
  arrange(desc(mean_sleep.duration))
#Ludzie srednio najwiecej spia w zawodzie Nurse jeżeli ich poziom stresu jest poniżej 7.


########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>%
  filter(Sleep.Disorder != 'None' ) %>%
  group_by(Occupation) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))
#Najczesciej osoby z dowolnym zaburzeniem snu pracuja jako Nurse. 

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df %>%
  filter(BMI.Category == 'Obese' ) %>%
  group_by(Occupation, Gender) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = Gender, values_from = Count, values_fill = list(Count = 0)) %>%
  filter(Male > Female) %>%
  select(Occupation, Male, Female) 
#W zawodach Doctor, Sales Representative, Software Engineer oraz Teacher.

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>%
  separate(Blood.Pressure, into = c("Systolic", "Diastolic"), sep = "/", convert = TRUE) %>%
  mutate(blood_pressure_diff = Systolic - Diastolic) %>%
  group_by(Occupation) %>% 
  summarise(Count = n(), mean_pressure_diff = mean(blood_pressure_diff, na.rm = TRUE)) %>% 
  filter(Count > 20) %>% 
  arrange(desc(mean_pressure_diff)) %>%
  head(3) -> top_3_jobs
print(top_3_jobs)

chosen_job <- top_3_jobs$Occupation[2]
df %>%
  filter(Occupation == chosen_job) %>%
  group_by(Quality.of.Sleep) %>%
  summarise(
    Mean_Sleep = mean(Sleep.Duration, na.rm = TRUE),
    Median_Sleep = median(Sleep.Duration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Quality.of.Sleep)

#Dla zawodu Lawyer srednia i mediana czasu snu dla jakości 7 to odpowiednio 7.18 i 7.1 a dla jakosci 8 to 7.33 i 7.3


########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
df %>%
  group_by(Occupation) %>% 
  summarise(mean_steps = mean(Daily.Steps, na.rm = TRUE)) %>% 
  arrange(desc(mean_steps)) %>%
  head(3) -> top_3_jobs_sport 
#Ludzie srednio najbardziej sie ruszaja w zawodach (od najwiekszej ilosci krokow): Nurse, Lawyer, Accountant)

general_mean_heart_rate <-mean(df$Heart.Rate, na.rm = TRUE)

df %>%
  filter(Occupation == "Nurse" | Occupation =="Lawyer" | Occupation =="Accountant" ) %>% 
  mutate(Age.Group = ifelse(Age >= 50, "1", "2")) %>% 
  group_by(Age.Group) %>% 
  summarise(mean_heart_rate = mean(Heart.Rate)) -> df_mean_heart_rate

diff_mean_hr_group1_vs_all<-general_mean_heart_rate - df_mean_heart_rate[1,2]
diff_mean_hr_group2_vs_all<-general_mean_heart_rate - df_mean_heart_rate[2,2]
abs(diff_mean_hr_group1_vs_all)
abs(diff_mean_hr_group2_vs_all)

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
df %>%
  mutate(Age_Bucket = cut(Age, breaks = seq(0, 100, by = 10), right = FALSE, include.lowest = TRUE)) %>% 
  group_by(Age_Bucket, Gender, Occupation) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Age_Bucket, Gender, desc(Count)) %>% 
  group_by(Age_Bucket, Gender) %>%
  slice_max(order_by = Count, n = 1) %>% 
  select(Age_Bucket, Gender, Occupation)
# W przedziale [20,30) najpopularniejszy zawod dla kobiet to Nurse, a dla męzczyzn Doctor. Natomiast w przedziale [30,40) najpopularniejszy 
#zawod wsrod kobiet to Accountant a wsrod mezczyzn Doctor. W przedziale [40,50) najpopularniejszy wsrod mezczyzn to Salesperson a wsrod kobiet Teacher.
#Wprzedziale ostatnim juz tylko kobiety-Nurse.

df %>%
  mutate(Age_Bucket = cut(Age, breaks = seq(0, 100, by = 10), right = FALSE, include.lowest = TRUE)) %>%
  group_by(Age_Bucket) %>%
  summarise(
    Mean_Stress = mean(Stress.Level, na.rm = TRUE),
    Median_Stress = median(Stress.Level, na.rm = TRUE),
    SD_Stress = sd(Stress.Level, na.rm = TRUE),
    .groups = "drop"
  )
