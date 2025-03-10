# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakosciowa (binarna)
# Age - ilosciowa (ilorazowa)
# Occupation - jakosciowa (nominalna)
# Sleep.Duration - ilosciowa (ilorazowa)
# Quality.of.Sleep - ilosciowa (przedzialowa)
# Physical.Activity.Level - ilosciowa (ilorazowa)
# BMI.Category - jakosciowa (uporzadkowana)
# Sleep.Disorder - jakosciowa (nominalna)


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(BMI.Category,Heart.Rate) %>% 
  group_by(BMI.Category) %>% 
  summarise(Average_Heart_Rate = mean(Heart.Rate)) %>% 
  View()

# Dla jakiej kategorii BMI średnie tętno jest najwyższe?

df %>% 
  select(BMI.Category,Heart.Rate) %>% 
  group_by(BMI.Category) %>% 
  summarise(Average_Heart_Rate = mean(Heart.Rate)) %>% 
  arrange(desc(Average_Heart_Rate)) %>% 
  View()

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(Stress.Level < 7) %>% 
  select(Occupation,Sleep.Duration) %>% 
  group_by(Occupation) %>% 
  summarise(Average_Sleep_Duration = mean(Sleep.Duration)) %>% 
  arrange(desc(Average_Sleep_Duration)) %>% 
  View()

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Occupation,Sleep.Disorder) %>% 
  group_by(Occupation) %>%
  filter(Sleep.Disorder != 'None') %>% 
  count(Occupation) %>% 
  rename(Number_of_disorders=n) %>% 
  arrange(desc(Number_of_disorders)) %>% 
  View()


########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Gender, Occupation, BMI.Category) %>% 
  filter(BMI.Category == 'Obese') %>% 
  group_by(Gender, Occupation) %>% 
  summarise(count = n()) %>% 
  spread(key = Gender, value = count, fill = 0) %>% 
  filter(Male > Female) %>% 
  View()

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  count(Occupation) %>% 
  arrange(desc(n)) %>% 
  filter(n > 20) %>% 
  left_join(df,by = 'Occupation') %>% 
  separate(Blood.Pressure, into = c('Skurczowe','Rozkurczowe'),sep = '/',convert = TRUE) %>% 
  mutate(Difference = Skurczowe - Rozkurczowe) %>% 
  group_by(Occupation) %>% 
  summarise(Average_Difference = mean(Difference)) %>% 
  arrange(desc(Average_Difference)) %>%
  slice(2) %>% 
  left_join(df,by = 'Occupation') %>% 
  select(Quality.of.Sleep, Sleep.Duration) %>% 
  group_by(Quality.of.Sleep) %>% 
  summarise(Mean = mean(Sleep.Duration), Median = median(Sleep.Duration)) %>% 
  View()

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  select(Occupation,Physical.Activity.Level) %>% 
  group_by(Occupation) %>% 
  summarise(Average.Physical.Activity.Level = mean(Physical.Activity.Level)) %>% 
  arrange(desc(Average.Physical.Activity.Level)) %>% 
  slice(1:3) %>% 
  select(Occupation) %>% 
  left_join(df, by = 'Occupation') %>% 
  select(Occupation, Heart.Rate, Age) %>% 
  mutate(Age_50_and_more = if_else(Age >= 50, Age, NA),
         Age_less_than_50 = if_else(Age < 50, Age, NA)) %>%
  summarise(Average_50plus = mean(Age_50_and_more, na.rm = TRUE),
            Average_50_minus = mean(Age_less_than_50, na.rm = TRUE)) -> smaller_group

df %>% 
  mutate(Age_50_and_more = if_else(Age >= 50, Age, NA),
         Age_less_than_50 = if_else(Age < 50, Age, NA)) %>%
  summarise(Average_50plus = mean(Age_50_and_more, na.rm = TRUE),
            Average_50_minus = mean(Age_less_than_50, na.rm = TRUE)) -> all

x1 <- paste('Roznica w grupie 1: ', abs(all$Average_50plus-smaller_group$Average_50plus))
x2 <- paste('Roznica w grupie 2: ', abs(all$Average_50_minus-smaller_group$Average_50_minus))

print(x1)
print(x2)

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>%
  mutate(Age_Category = cut(Age, breaks = seq(0, 100, by = 10), 
                            right = FALSE, 
                            labels = paste0("[", seq(0, 90, by = 10), ", ", seq(10, 100, by = 10), ")"))) %>% 
  group_by(Age_Category, Gender, Occupation) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(Age_Category, Gender) %>%
  slice_max(Count, n = 1) %>% 
  View()

df %>%
  mutate(Age_Category = cut(Age, breaks = seq(0, 100, by = 10), 
                            right = FALSE, 
                            labels = paste0("[", seq(0, 90, by = 10), ", ", seq(10, 100, by = 10), ")"))) %>% 
  select(Age_Category, Stress.Level) %>% 
  group_by(Age_Category) %>% 
  summarise(Mean_Stress_Level = mean(Stress.Level), Median_Stress_Level = median(Stress.Level), Standard_DEviation = sd(Stress.Level)) %>% 
  View()
