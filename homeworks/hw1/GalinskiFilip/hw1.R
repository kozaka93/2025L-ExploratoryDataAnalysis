# Ładowanie pakietów
library(dplyr)
library(data.table)
library(tidyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - character
# Age - numeric
# Occupation - character
# Sleep.Duration - numeric
# Quality.of.Sleep - numeric
# Physical.Activity.Level - numeric
# BMI.Category - character
# Sleep.Disorder - character


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: 

avg_tetno_bmi <- aggregate(Heart.Rate ~ BMI.Category, df, mean)
bmi_max_heart <- avg_tetno_bmi[which.max(avg_tetno_bmi$Heart.Rate), ]


########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
sleep_by_job <- aggregate(Sleep.Duration ~ Occupation, df[df$Stress.Level < 7, ], mean)
best_sleeping_job <- sleep_by_job[which.max(sleep_by_job$Sleep.Duration), ]


########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

jobs_sleep_disorder <- table(df$Occupation[df$Sleep.Disorder != "None"])
most_common_jobs_disorder <- names(sort(jobs_sleep_disorder, decreasing = TRUE))[1:5]


########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
obese_men <- data.table(df)[BMI.Category == "Obese" & Gender == "Male", .N, by = Occupation]
obese_women <- data.table(df)[BMI.Category == "Obese" & Gender == "Female", .N, by = Occupation]
jobs_more_obese_men <- merge(obese_men, obese_women, by = "Occupation", all.x = TRUE, suffixes = c("Male", "Female"))
jobs_more_obese_men$NFemale[is.na(jobs_more_obese_men$NFemale)] <- 0
wynik <- jobs_more_obese_men[NMale > NFemale, Occupation]



########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
df_filtered <- df %>%
  separate(Blood.Pressure, into = c("jeden", "dwa"), sep = "/", convert = TRUE) %>% 
  mutate(Diff = jeden - dwa) %>%     
  group_by(Occupation) %>%         
  filter(n() >= 20) %>%            
  summarise(Avg_Diff = mean(Diff)) %>% 
  arrange(desc(Avg_Diff)) %>%      
  head(3) 

prawozadnysen <- df %>%
  filter(Occupation == "Lawyer") %>%   
  group_by(Quality.of.Sleep) %>%         
  summarise(
    Średnia = mean(Sleep.Duration, na.rm = TRUE), 
    Mediana = median(Sleep.Duration, na.rm = TRUE) 
  )


########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
top_3_active_jobs <- df %>%
  group_by(Occupation) %>%
  summarise(Avg_Sportowosc = mean(Physical.Activity.Level, na.rm = TRUE)) %>%  
  arrange(desc(Avg_Sportowosc)) %>%  
  head(3) 

overall_heart_rate <- df %>%
  mutate(Age_Group = ifelse(Age >= 50, "Grupa 1 (50+)", "Grupa 2 (<50)")) %>%
  group_by(Age_Group) %>%
  summarise(Avg_Heart_Rate = mean(Heart.Rate, na.rm = TRUE))


active_jobs_heart_rate <- df %>%
  filter(Occupation %in% top_3_active_jobs$Occupation) %>%  # Wybór aktywnych zawodów
  mutate(Age_Group = ifelse(Age >= 50, "Grupa 1 (50+)", "Grupa 2 (<50)")) %>%
  group_by(Age_Group) %>%
  summarise(Avg_Heart_Rate = mean(Heart.Rate, na.rm = TRUE))

heart_rate_difference <- overall_heart_rate %>%
  inner_join(active_jobs_heart_rate, by = "Age_Group", suffix = c("_All", "_Active")) %>%
  mutate(Diff = Avg_Heart_Rate_Active - Avg_Heart_Rate_All)


########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
łot <- df %>%
  mutate(Age_Group = cut(Age, breaks = seq(0, max(Age, na.rm = TRUE) + 10, by = 10), 
                         right = FALSE, include.lowest = TRUE))

most_common_occupation <- function(x) {
  if (length(unique(x)) == 1) return(unique(x))
  return(names(which.max(table(x))))
}

result <- łot %>%
  group_by(Age_Group, Gender) %>%
  summarise(
    Most_Common_Occupation = most_common_occupation(Occupation),
    Mean_Stress = mean(Stress.Level, na.rm = TRUE),
    Median_Stress = median(Stress.Level, na.rm = TRUE),
    SD_Stress = sd(Stress.Level, na.rm = TRUE)
  ) %>%
  ungroup()

