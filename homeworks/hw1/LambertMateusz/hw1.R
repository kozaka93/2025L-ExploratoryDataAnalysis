# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - zm. jakościowa binarna
# Age - zm. ilościowa ilorazowa
# Occupation - zm. jakościowa nominalna
# Sleep.Duration - zm. ilościowa zliczeniowa
# Quality.of.Sleep - zm. jakościowa uporządkowana
# Physical.Activity.Level - zm. ilościowa ilorazowa
# BMI.Category - zm. jakościowa uporzadkowana
# Sleep.Disorder - zm. jakościowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

z2 <- df %>%
  group_by(BMI.Category) %>%
  summarise(avg_tetno = mean(Heart.Rate, na.rm = T)) %>%
  arrange(desc(avg_tetno))

z2
# Średnie tetno w roznych kategoriach mozna tu odczytac, najwyzsze srednie tetno jest dla 'Obese'

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

z3 <- df %>%
  filter(Stress.Level < 7) %>%
  group_by(Occupation) %>%
  summarise(avg_sen = mean(Sleep.Duration, na.rm = T)) %>%
  arrange(desc(avg_sen))
z3
# Najwięcej spią 'Engineer'

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------

z4 <- df %>%
  filter(Sleep.Disorder != "None") %>%
  group_by(Occupation) %>%
  summarise(cnt = n()) %>%
  arrange(desc(cnt))
z4
# Najwiecej w 'Nurse'

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% filter(BMI.Category == "Obese", Occupation == "Lawyer")

z5 <- df %>%
  filter(BMI.Category == "Obese") %>%
  group_by(Occupation, Gender) %>%
  summarise(cnt = n(), .groups = 'drop') %>%
  mutate(m_cnt = ifelse(Gender == "Male", cnt, 0), f_cnt = ifelse(Gender == "Female", cnt, 0)) %>%
  group_by(Occupation) %>%
  summarise(m = sum(m_cnt), f = sum(f_cnt)) %>%
  filter(m > f)
z5$Occupation

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

z6 <- df %>%
  mutate(sku = as.numeric(sapply(strsplit(Blood.Pressure, "/"), "[", 1)),
         roz = as.numeric(sapply(strsplit(Blood.Pressure, "/"), "[", 2)),
         diff = sku-roz) %>%
  group_by(Occupation) %>%
  filter(20 < n()) %>%
  summarise(avg = mean(diff, na.rm = T)) %>%
  arrange(desc(avg))
z6 # Drugi zawod to Lawyer

z6_2 <- df %>%
  filter(Occupation == z6$Occupation[2]) %>%
  group_by(Quality.of.Sleep) %>%
  summarise(avg = mean(Sleep.Duration), med = median(Sleep.Duration))
z6_2

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

z7 <- df %>%
  group_by(Occupation) %>%
  summarise(avg = mean(Physical.Activity.Level)) %>%
  arrange(desc(avg))
z7 # zawody to Nurse, Lawyer, Accountant

z7_2 <- df %>%
  filter(Occupation %in% z7$Occupation[1:3]) %>%
  mutate(ag = ifelse(Age < 50, 2, 1)) %>%
  group_by(ag) %>%
  summarise(avg = mean(Heart.Rate)) %>%
  mutate(diff = mean(df$Heart.Rate)-avg)
z7_2 # W grupie 1 roznica to -0.899 a w drugiej -0.0553333333...

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

z8 <- df %>%
  mutate(ag = cut(Age, breaks = seq(0, 301, 10), right=F)) %>% # zakladamy ze nikt nie jest starszy niz 300
  group_by(ag, Gender) %>%
  summarise(most_pop = names(which.max(table(Occupation))),
            avg_stress = mean(Stress.Level),
            med = median(Stress.Level),
            std = sd(Stress.Level))
z8  
  

