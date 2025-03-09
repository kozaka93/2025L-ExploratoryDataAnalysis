# Ładowanie pakietów
library(dplyr)
library(tidyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa binarna 
# Age - ilościowa ilorazowa
# Occupation - jakośiowa niebinarna 
# Sleep.Duration - ilościowa ilorazowa
# Quality.of.Sleep - jakościowa porządkowa
# Physical.Activity.Level - ilościowa ilorazowa
# BMI.Category -  jakościowa porządkowa
# Sleep.Disorder -  jakościowa niebinarna 


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  group_by(BMI.Category) %>% 
  summarise(mean_heart_rate = mean(Heart.Rate,na.rm=TRUE)) %>% 
  arrange(desc(mean_heart_rate))

# Obese                    84.3
# Normal Weight            71.3
# Overweight               70.9
# Normal                   68.7

#najwiecej Obese

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(Stress.Level<7) %>% 
  group_by(Occupation) %>% 
  summarise(mean_sleep = mean(Sleep.Duration,na.rm=TRUE)) %>% 
  arrange(desc(mean_sleep)) %>% 
  head(1)

  # Najwiecej spia: Engineer      srednia liczba godzin: 8.05
  
########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(Sleep.Disorder!="None") %>% 
  group_by(Occupation) %>% 
  summarise(count_workers=n()) %>% 
  arrange(desc(count_workers))
  
  # w zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu: 
# 1 Nurse 
# 2 Teacher
# 3 Salesperson


########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(BMI.Category=="Obese") %>% 
  group_by(Occupation,Gender) %>% 
  summarise(count_obese = n()) %>% 
  tidyr::pivot_wider(
    names_from = Gender,
    values_from = count_obese,
    values_fill = 0
  ) %>% 
  filter(Male>Female)


# Doctor
#Sales Representative
#Software Engineer
#Teacher
  

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  separate(Blood.Pressure,into = c("systolic","diastolic"),sep="/",convert = TRUE) %>% 
  group_by(Occupation) %>% 
  filter(n()>20) %>% 
  summarise(mean_diff = mean(systolic-diastolic)) %>% 
  arrange(desc(mean_diff)) %>% 
  head(3)
  # 1. Salesperson
  #2. Lawyer
  #3. Nurse

  ###b) dla Lawyer
  
df %>% 
  filter(Occupation=="Lawyer") %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(
    mean_sleep = mean(Sleep.Duration),
    median_sleep = median(Sleep.Duration)
  )
  # quality of sleep: 7 srednia: 7.18 mediana: 7.1
# quality of sleep: 8 srednia: 7.44 mediana: 7.3

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
top3_active <- df %>% 
  group_by(Occupation) %>% 
  summarise(mean_activity = mean(Physical.Activity.Level)) %>% 
  arrange(desc(mean_activity)) %>% 
  head(3)
#Nurse
#Lawyer
#Accountant
all_hr <- df %>% 
  mutate(age_group = if_else(Age >= 50,"grupa 1", "grupa 2")) %>% 
  group_by(age_group) %>% 
  summarise(mean_HR = mean(Heart.Rate))
hr_top3 <- df %>% 
  filter(Occupation %in% top3_active$Occupation) %>% 
  mutate(age_group = if_else(Age >= 50,"grupa 1", "grupa 2")) %>% 
  group_by(age_group) %>% 
  summarise(mean_HR_top3 = mean(Heart.Rate))
result <- left_join(all_hr,hr_top3, by = "age_group") %>% 
  mutate(
    diff = abs(mean_HR - mean_HR_top3)
  )
result
# grupa1(>=50) roznica : 2,15
# grupa2(<50) roznica : 0.376

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

kubelek_df <- df %>% 
  mutate(age_group = cut(Age,breaks=seq(0,100,by = 10),
                         right = FALSE,
                         include.lowest =TRUE
  ))
#a) najbardziej popularne zawody w grupach
kubelek_df %>%
  group_by(Gender,age_group,Occupation) %>% 
  summarise(count = n()) %>% 
  group_by(age_group, Gender) %>% 
  slice_max(order_by = count,n =1 ) 

#1 Female [20,30)   Nurse           2
#2 Male   [20,30)   Doctor         12
#3 Female [30,40)   Accountant     28
#4 Male   [30,40)   Doctor         53
#5 Female [40,50)   Teacher        29
#6 Male   [40,50)   Salesperson    32
#7 Female [50,60)   Nurse          56


#b)średnią, medianę i odchylenie standardowe dla poziomu stresu

  kubelek_df %>% 
    group_by(age_group) %>% 
    summarise(
      mean_stress = mean(Stress.Level),
      median_stress = median(Stress.Level),
      sd_stress = sd(Stress.Level)
      
    )
#  age_group mean_stress median_stress odchylenie standardowe_stress
#1 [20,30)          7.32             8     0.885
#2 [30,40)          5.52             5     1.50 
#3 [40,50)          5.68             5     1.38 
#4 [50,60)          4.45             3     2.19