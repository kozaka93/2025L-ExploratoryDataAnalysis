# Ładowanie pakietów
library(dplyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, binarna
# Age - ilościowa, przedziałowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - jakościowa, ilorazowa
# Quality.of.Sleep - jakościowa, uporządkowana
# Physical.Activity.Level - ilościowa, ilorazowa
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder - jakościowa, nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średnie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(BMI.Category) %>% 
  summarize(Mean.Heart.Rate = mean(Heart.Rate, na.rm = TRUE)) 
 
df %>% 
  group_by(BMI.Category) %>% 
  summarize(Mean.Heart.Rate = mean(Heart.Rate, na.rm = TRUE)) %>% 
  arrange(desc(Mean.Heart.Rate)) %>% 
  head(1) %>% 
  select(BMI.Category)

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Stress.Level <7) %>% 
  group_by(Occupation) %>% 
  summarize(Mean.Sleep.Duration = mean(Sleep.Duration, na.rm = TRUE)) %>% 
  arrange(desc(Mean.Sleep.Duration)) %>% 
  head(1) %>% 
  select(Occupation)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześciej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(Sleep.Disorder != "None") %>% 
  group_by(Occupation) %>% 
  summarize(Number = n()) %>% 
  arrange(desc(Number))

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(BMI.Category == "Obese") %>% 
  mutate(Gender = ifelse(Gender == "Female", 1, 0)) %>% 
  group_by(Occupation) %>% 
  summarize(Men = n()-sum(Gender), Women = sum(Gender)) %>% 
  filter(Men > Women) %>% 
  select(Occupation)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

df %>%
  mutate(Blood.Pressure.Difference=as.integer(gsub("/..","",Blood.Pressure))-as.integer(gsub(".../","",Blood.Pressure))) %>% 
  group_by(Occupation) %>% 
  summarize(Number = n(), Blood.Pressure.Difference.Mean=mean(Blood.Pressure.Difference)) %>% 
  filter(Number > 20) %>% 
  arrange(desc(Blood.Pressure.Difference.Mean)) %>% 
  head(3) %>% 
  select(Occupation)

df %>%
  filter(Occupation == "Lawyer") %>%  
  group_by(Quality.of.Sleep) %>% 
  summarize(Sleep.Duration.Mean = mean(Sleep.Duration), Sleep.Duration.Median = median(Sleep.Duration))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(Occupation) %>% 
  summarize(Physical.Activity.Level.Mean = mean(Physical.Activity.Level)) %>% 
  arrange(desc(Physical.Activity.Level.Mean)) %>% 
  head(3) %>% 
  select(Occupation)

df %>% 
  mutate(Age.Group = case_when(Age >= 50 ~ "grupa 1",
                         TRUE ~ "grupa 2"), Heart.Rate.Mean.All = mean(Heart.Rate)) %>% 
  filter(Occupation == "Nurse" | Occupation == "Lawyer" | Occupation == "Accountant") %>% 
  group_by(Age.Group) %>% 
  summarize(Heart.Rate.Mean = mean(Heart.Rate), Heart.Rate.Mean.All = mean(Heart.Rate.Mean.All)) %>% 
  mutate(Heart.Rate.Mean.Difference = Heart.Rate.Mean - Heart.Rate.Mean.All) %>% 
  select(Age.Group, Heart.Rate.Mean.Difference)
  

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  mutate(Age.Group = case_when(Age < 10 ~ "0-10",
                               Age < 20 ~ "10-20",
                               Age < 30 ~ "20-30",
                               Age < 40 ~ "30-40",
                               Age < 50 ~ "40-50",
                               Age < 60 ~ "50-60",
                               TRUE ~ "60+")) %>% 
  group_by(Age.Group, Gender, Occupation) %>% 
  summarize(Number = n(), Stress.Mean = mean(Stress.Level), Stress.Median = median(Stress.Level), Stress.Sd = sd(Stress.Level)) %>% 
  filter(Number == max(Number))
  