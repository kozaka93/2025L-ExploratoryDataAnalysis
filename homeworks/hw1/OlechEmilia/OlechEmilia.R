# Ładowanie pakietów
library(dplyr)

# Ładowanie danych
df <- read.csv("homeworks/hw1/data.csv", sep = ",", header = TRUE)

########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowe, binarne
# Age - ilościowe, ilorazowe
# Occupation - jakościowe, naminalne
# Sleep.Duration - ilościowe, ilorazowe
# Quality.of.Sleep - ilościowe, przedziałowe
# Physical.Activity.Level - ilościowe, ilorazowe
# BMI.Category -jakościowe, uporządkowane
# Sleep.Disorder - jakościowa,  nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df %>%
  group_by(BMI.Category) %>%
  summarise(SrednieTetno = mean(Heart.Rate, na.rm = TRUE)) %>%
  arrange(desc(SrednieTetno))
# ODP: Średnie tętno jest najwyższe dla kategorii BMI obese

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>%
  filter(Stress.Level < 7) %>%  
  group_by(Occupation) %>% 
  summarise(SredniSen = mean(Sleep.Duration, na.rm = TRUE)) %>%  
  arrange(desc(SredniSen))  
# ODP: Ludzie w zawodzie engineer średnio śpią najdłużej

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>%
  filter(Sleep.Disorder != "None") %>%  
  group_by(Occupation) %>% 
  summarise(LiczbaOsob = n()) %>%  
  arrange(desc(LiczbaOsob))  
# ODP: Osoby z zaburzeniem snu najczęściej pracują w zawodach: nurse, teacher i salesperson

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
otyli_mezczyzni <- df %>%
  filter(BMI.Category == "Obese", Gender == "Male") %>%
  group_by(Occupation) %>%
  summarise(LiczbaMezczyzn = n(), .groups = "drop")

otyłe_kobiety <- df %>%
  filter(BMI.Category == "Obese", Gender == "Female") %>%
  group_by(Occupation) %>%
  summarise(LiczbaKobiet = n(), .groups = "drop")
wynik <- left_join(otyli_mezczyzni, otyłe_kobiety, by = "Occupation") %>%
  mutate(LiczbaKobiet = ifelse(is.na(LiczbaKobiet), 0, LiczbaKobiet)) %>%  
  filter(LiczbaMezczyzn > LiczbaKobiet) %>% 
  arrange(desc(LiczbaMezczyzn))  
print(wynik)
# ODP: Są to zawody doctor, sales representative, software engineer oraz teacher

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
df <- df %>%
  mutate(BP_Diff = as.numeric(sub("/.*", "", Blood.Pressure)) - as.numeric(sub(".*/", "", Blood.Pressure)))

top_jobs <- df %>%
  group_by(Occupation) %>%
  summarise(
    Count = n(),
    Mean_BP_Diff = mean(BP_Diff, na.rm = TRUE)
  ) %>%
  filter(Count > 20) %>%
  arrange(desc(Mean_BP_Diff)) %>%
  slice_head(n = 3)
print(top_jobs)
# ODP: te zadowy to: salesperson, lawyer, nurse.

second_job <- top_jobs$Occupation[2]
sleep_stats <- df %>%
  filter(Occupation == second_job) %>%
  group_by(Quality.of.Sleep) %>%
  summarise(
    Mean_Sleep = mean(Sleep.Duration, na.rm = TRUE),
    Median_Sleep = median(Sleep.Duration, na.rm = TRUE)
  ) %>%
  arrange(Quality.of.Sleep)
print(sleep_stats)
# ODP: dla zawodu pielęgniarki mamy: 
#      dla jakości snu 7 - średnią 7.18 i medianę 7.1,
#      dla jakości snu 8 - średnią 7.44 i medianę 7.3

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
top_active_jobs <- df %>%
  group_by(Occupation) %>%
  summarise(Mean_Activity = mean(Physical.Activity.Level, na.rm = TRUE)) %>%
  arrange(desc(Mean_Activity)) %>%
  slice_head(n = 3)
print(top_active_jobs)
# ODP: te trzy zadowy to: nurse, lawyer, accountant

df_active <- df %>%
  filter(Occupation %in% top_active_jobs$Occupation) 

heart_rate_comparison <- df %>%
  mutate(Age_Group = ifelse(Age >= 50, "Grupa 1 (50+)", "Grupa 2 (<50)")) %>%
  group_by(Age_Group) %>%
  summarise(Mean_Heart_Rate_All = mean(Heart.Rate, na.rm = TRUE))

heart_rate_active_jobs <- df_active %>%
  mutate(Age_Group = ifelse(Age >= 50, "Grupa 1 (50+)", "Grupa 2 (<50)")) %>%
  group_by(Age_Group) %>%
  summarise(Mean_Heart_Rate_Active = mean(Heart.Rate, na.rm = TRUE))

result <- left_join(heart_rate_comparison, heart_rate_active_jobs, by = "Age_Group")
print(result)
# ODP: w grupie 1 różnica między średnim tętnem wszytskich danych to 68.9, a w konkretnych zadowach to 71.1,
#      w grupie 2 róznica między średnim tętnem wszystkich danych to 70.6, a w konkretnych zawodach to 70.2

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
df <- df %>%
  mutate(Age_Bucket = cut(Age, breaks = seq(0, 100, by = 10), right = FALSE))

most_common_jobs <- df %>%
  group_by(Age_Bucket, Gender, Occupation) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Age_Bucket, Gender, desc(Count)) %>%
  group_by(Age_Bucket, Gender) %>%
  slice_head(n = 1) %>%
  select(Age_Bucket, Gender, Occupation, Count)
print(most_common_jobs)

stress_stats <- df %>%
  group_by(Age_Bucket) %>%
  summarise(
    Mean_Stress = mean(Stress.Level, na.rm = TRUE),
    Median_Stress = median(Stress.Level, na.rm = TRUE),
    SD_Stress = sd(Stress.Level, na.rm = TRUE)
  )
print(stress_stats)

