# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowe, binarne
# Age - ilościowe, przedziałowe
# Occupation - jakościowe, nominalne
# Sleep.Duration - ilościowe, ilorazowe
# Quality.of.Sleep - ilościowe, ilorazowe
# Physical.Activity.Level - ilościowe, ilorazowe
# BMI.Category - jakościowe, uporządkowane
# Sleep.Disorder - jakościowe, nominalne


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

df %>% 
  group_by(BMI.Category) %>% 
  summarise(mean_heart_rate = mean(Heart.Rate))

# Rozwiązanie: ------------------------------------------------------------

#BMI.Category  mean_heart_rate
#<chr>                   <dbl>
#  1 Normal                   68.7
#2 Normal Weight            71.3
#3 Obese                    84.3
#4 Overweight               70.9

# Najwyższe średnie tętno jest dla BMI Obese.

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

df %>% 
  group_by(Occupation) %>% 
  filter(Stress.Level < 7) %>% 
  summarise(mean_sleep_duration = mean(Sleep.Duration)) %>% 
  arrange(desc(mean_sleep_duration)) %>% 
  head(1)


# Rozwiązanie: ------------------------------------------------------------

# Occupation        mean_sleep_duration
# <chr>                           <dbl>
#   1 Engineer                         8.05

# Najwięcej śpią inżynierowie.

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(3)

# Rozwiązanie: ------------------------------------------------------------

#    Occupation               n
# <chr>                <int>
#   1 Nurse                   64
# 2 Teacher                 31
# 3 Salesperson             30

# Najczęściej pracują jako pielęgniarki, nauczyciele i w dziale sprzedaży.
  

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

df %>% 
  filter(BMI.Category == 'Obese') %>% 
  group_by(Occupation, Gender) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
  
# Rozwiązanie: ------------------------------------------------------------

# Occupation           Gender     n
# <chr>                <chr>  <int>
#   1 Doctor               Male       4
# 2 Sales Representative Male       2
# 3 Lawyer               Female     1
# 4 Lawyer               Male       1
# 5 Software Engineer    Male       1
# 6 Teacher              Male       1

# W każdym zawodzie oprócz prawników jest więcej otyłych mężczyzn niż kobiet,
# tylko prawników jest równa liczba.


########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

df %>% 
  mutate(
    Systolic = as.numeric(sub("/.*", "", Blood.Pressure)),  
    Diastolic = as.numeric(sub(".*?/", "", Blood.Pressure))  
  ) %>% 
  mutate( difference = Systolic - Diastolic) %>% 
  group_by(Occupation) %>% 
  summarise(
    n = n(),
    avg_difference = mean(difference)
  ) %>%
  filter(n > 20) %>% 
  arrange(desc(avg_difference)) %>% 
  slice(2) %>% 
  inner_join(df, by = 'Occupation') %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(
    mean = mean(Sleep.Duration),
    median = median(Sleep.Duration)
            )

# Rozwiązanie: ------------------------------------------------------------

# Quality.of.Sleep  mean median
# <int> <dbl>  <dbl>
#   1                7  7.18    7.1
# 2                8  7.44    7.3

# Dla jakości snu równej 7, średnia jest równa 7.18 i mediana jest równa 7.1.
# Dla jakości snu równej 8, średnia jest równa 7.44 i mediana jest równa 7.3.

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

df %>% 
  group_by(Occupation) %>% 
  summarise(mean_activity_level = mean(Physical.Activity.Level)) %>% 
  arrange(desc(mean_activity_level)) %>% 
  slice(1:3)

# Occupation mean_activity_level
# <chr>                    <dbl>
#   1 Nurse                     78.6
# 2 Lawyer                    70.4
# 3 Accountant                58.1


df %>%
  select(Occupation, Age, Heart.Rate) %>%
  mutate(
    is_old = ifelse(
      Age > 50, '50+', 'below 50'
    )
  ) %>%
  group_by(is_old) %>%
  summarise(
    mean_heart_rate_top_occupations = mean(Heart.Rate[Occupation %in% c("Nurse", "Lawyer", "Accountant")]),
    mean_heart_rate_all_occupations = mean(Heart.Rate)
  ) %>%
  mutate(difference = abs(mean_heart_rate_top_occupations - mean_heart_rate_all_occupations)) %>% 
  arrange(is_old)

# Rozwiązanie: ------------------------------------------------------------

#3 zawody, w których ludzie średnio najwięcej sie ruszają to:

# Occupation mean_activity_level
# <chr>                    <dbl>
#   1 Nurse                     78.6
# 2 Lawyer                    70.4
# 3 Accountant                58.1

# is_old   mean_heart_rate_top_occupations mean_heart_rate_all_occupations difference
# <chr>                              <dbl>                           <dbl>      <dbl>
#   1 50+                                 69.3                            67.4      1.88 
# 2 below 50                            71.0                            70.9      0.158

# Różnica w grupie 50+ wynosi 1.88, a w grupie poniżej 50 wynosi 0.158.



########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

df %>%
  select('Gender', 'Age', 'Occupation', 'Stress.Level') %>% 
  mutate(age_group = case_when(
    Age < 10 ~ "[0, 10)",
    Age < 20 ~ "[10, 20)",
    Age < 30 ~ "[20, 30)",
    Age < 40 ~ "[30, 40)",
    Age < 50 ~ "[40, 50)",
    Age < 60 ~ "[50, 60)",
    Age < 70 ~ "[60, 70)",
    TRUE ~ "[70, 80)"
  )) %>% 
  group_by(age_group, Gender) %>% 
  summarise(
    most_popular_occupation = names(which.max(table(Occupation))),
    mean_stress = mean(Stress.Level),
    median_stress = median(Stress.Level),
    sd_stress = sd(Stress.Level)
  ) 

# Rozwiązanie: ------------------------------------------------------------

# age_group Gender most_popular_occupation mean_stress median_stress sd_stress
# <chr>     <chr>  <chr>                         <dbl>         <dbl>     <dbl>
#   1 [20, 30)  Female Nurse                          7                7     0    
# 2 [20, 30)  Male   Doctor                         7.35             8     0.931
# 3 [30, 40)  Female Accountant                     4.47             4     1.16 
# 4 [30, 40)  Male   Doctor                         6.01             6     1.38 
# 5 [40, 50)  Female Teacher                        5.31             4     1.73 
# 6 [40, 50)  Male   Salesperson                    5.88             5     1.10 
# 7 [50, 60)  Female Nurse                          4.45             3     2.19


