# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
df <- read.csv("/Users/pawelmozaryn/Desktop/4 semestr/wstep do explo/data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.


# Gender - jakościowa, nominalna
# Age - ilościowa, przedziałowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep - porządkowa
# Physical.Activity.Level - ilościowa, ilorazowa
# BMI.Category - porządkowas
# Sleep.Disorder - jakościowa, nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?
df %>% group_by(BMI.Category) %>%  summarise(srednie_tetno = mean(Heart.Rate)) %>% top_n(1, srednie_tetno) 
#Srednie tetno jest najwyzsze dla kategorii "Obsese" i wynosi 84.3

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?
df %>% select(Occupation, Sleep.Duration, Stress.Level) %>% filter(Stress.Level < 7)  %>% 
  group_by(Occupation) %>% summarise(Avg_sleep_duration = mean(Sleep.Duration)) %>% top_n(1, Avg_sleep_duration) %>% 
  View()
# Sa to inzynierowie
  


########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 
df %>% filter(Sleep.Disorder != "None") %>% select(Occupation, Person.ID) %>% 
  group_by(Occupation) %>% summarise(ile_osob = length(Person.ID)) %>% top_n(5, ile_osob) %>% 
  arrange(-ile_osob) %>% View()

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?
df5 <- df %>% filter(BMI.Category == "Obese") %>% select(Person.ID, Occupation, Gender) %>% 
  group_by(Occupation, Gender)
df5_men <- df5 %>% filter(Gender == "Male") %>% group_by(Occupation) %>%
  summarise(How_many_men = length(Person.ID))
df5_women <- df5 %>% filter(Gender == "Female") %>% group_by(Occupation) %>%
  summarise(How_many_women = length(Person.ID))
df5 <- merge(df5_men, df5_women, by = "Occupation")
df5[is.na(df5)] <- 0 
df5 %>% filter(How_many_men > How_many_women) %>% View()
rm(df5, df5_men, df5_women)


########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.
v6 <- df %>% group_by(Occupation) %>% summarise(Ile_ludzi_w_zawodzie = length(Person.ID)) %>% 
  filter(Ile_ludzi_w_zawodzie > 20) %>% select(Occupation)
v6 <- v6$Occupation
df6 <- df %>% filter(Occupation %in% v6) %>%
  mutate(bp_difference = as.numeric(substr(Blood.Pressure, 1, 3)) - as.numeric(substr(Blood.Pressure, 5, 6)))
df6 %>% group_by(Occupation) %>% summarise(Avg_bp_diff = mean(bp_difference)) %>% arrange(-Avg_bp_diff) %>% 
  top_n(3, Avg_bp_diff) %>% View() #drugi zawód to Lawyer
df %>% filter(Occupation == "Lawyer") %>% select(Person.ID, Sleep.Duration, Quality.of.Sleep) %>% 
  group_by(Quality.of.Sleep) %>% summarise(Sleep_duration_median = median(Sleep.Duration), Sleep_duration_mean = mean(Sleep.Duration)) %>% 
  View()



########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 
df %>% group_by(Occupation) %>%  summarise(Avg_steps = mean(Daily.Steps)) %>%
  arrange(-Avg_steps) %>% top_n(3, Avg_steps) %>% View()
# Te zawody to Nurse, Lawyer, Accountant
zawody <- c("Nurse", "Lawyer", "Accountant")
total_avg_heartrate <- mean(df$Heart.Rate)
#grupa_1;
df %>% filter(Age >= 50, Occupation %in% zawody) %>%
  group_by(Occupation) %>% summarise(avg_heartrate = mean(Heart.Rate)) %>%
  mutate(hr_deviation_from_avg = avg_heartrate - total_avg_heartrate) %>% View()

#grupa_2
df %>% filter(Age < 50, Occupation %in% zawody) %>%
  group_by(Occupation) %>% summarise(avg_heartrate = mean(Heart.Rate)) %>%
  mutate(hr_deviation_from_avg = avg_heartrate - total_avg_heartrate) %>% View()

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

#stress info
stress_info <- df %>% mutate(age_group = paste0("[",10*(as.numeric(substr(Age, 1, 1))), ",", 10 *(as.numeric(substr(Age, 1, 1))+1), ")" )) %>% 
  select(Person.ID,age_group,Stress.Level) %>% group_by(age_group) %>%
  summarise(stress_mean = mean(Stress.Level), stress_median = median(Stress.Level), stress_std_deviation = sd(Stress.Level))

#men

male_top_occupation <- df %>% mutate(age_group = paste0("[",10*(as.numeric(substr(Age, 1, 1))), ",", 10 *(as.numeric(substr(Age, 1, 1))+1), ")" )) %>% 
  filter(Gender == "Male") %>% 
  group_by(age_group, Occupation) %>% summarise(occupation_popularity = length(Occupation)) %>% 
  group_by(age_group) %>% slice_max(order_by = occupation_popularity, n = 1, with_ties = FALSE) %>% select(-occupation_popularity)

#women

female_top_occupation <- df %>% mutate(age_group = paste0("[",10*(as.numeric(substr(Age, 1, 1))), ",", 10 *(as.numeric(substr(Age, 1, 1))+1), ")" )) %>% 
  filter(Gender == "Female") %>% 
  group_by(age_group, Occupation) %>% summarise(occupation_popularity = length(Occupation)) %>% 
  group_by(age_group) %>% slice_max(order_by = occupation_popularity, n = 1, with_ties = FALSE) %>% select(-occupation_popularity)

#renaming 

colnames(female_top_occupation)[2] <- "top_job_among_females"
colnames(male_top_occupation)[2] <- "top_job_among_males"

#merge 
top_job_info <- merge.data.frame(male_top_occupation, female_top_occupation, by = "age_group", all.y= TRUE)
occupation_and_stress_summary <- merge.data.frame(top_job_info, stress_info, by = "age_group", all.y = TRUE)

#wynik to occupation_and_stress_summary

