# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read_csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakosciowa binarna
# Age - ilosciowa ilorazowa
# Occupation - jakosciowa nominalna
# Sleep.Duration - ilosciowa ilorazowa
# Quality.of.Sleep - jakosciowa uporzadkowana
# Physical.Activity.Level - ilosciowa ilorazowa
# BMI.Category - jakosciowa uporzadkowana
# Sleep.Disorder - jakosciowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  group_by(`BMI Category`)%>% 
  summarise(avg_heart_rate=mean(`Heart Rate`,na.rm=TRUE)) %>% 
  arrange(desc(avg_heart_rate))

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(`Stress Level`<7) %>% 
  group_by(`Occupation`) %>% 
  summarise(avg_sleep_duration=mean(`Sleep Duration`,na.rm=TRUE)) %>% 
  arrange(-avg_sleep_duration)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(`Sleep Disorder`!='None') %>% 
  group_by(`Occupation`) %>% 
  summarise(num_with_sleep_disorder=n()) %>% 
  arrange(-num_with_sleep_disorder)

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------

df %>% 
  filter(`BMI Category`=='Obese')%>% 
  group_by(`Gender`,`Occupation`) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from=Gender,values_from=count,values_fill = 0) %>% 
  filter(Male>Female) %>% 
  pull(Occupation)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
options(pillar.sigfig=4)
df %>% 
  separate(`Blood Pressure`,c("skurczowe", "rozkurczowe"), "/",remove=FALSE) %>% 
  mutate_at(c("skurczowe", "rozkurczowe"),as.numeric) %>% 
  mutate(roznica_cisnien=skurczowe-rozkurczowe) %>% 
  group_by(`Occupation`) %>% 
  filter(n()>20) %>% 
  summarise(avg_roznica_cisnien=mean(roznica_cisnien)) %>% 
  arrange(-avg_roznica_cisnien) %>% 
  head(3)
#Uzyskałem, że drugi pod względem średniej różnicy i mający ponad 20 przykładów jest Lawyer
options(pillar.sigfig=3)
df %>% 
  filter(Occupation=='Lawyer') %>% 
  group_by(`Quality of Sleep`) %>% 
  summarise(avg_sleep_time=mean(`Sleep Duration`,na.rm=TRUE),median_sleep_time=median(`Sleep Duration`,na.rm=TRUE))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
most_active<-df %>% group_by(Occupation) %>% 
  summarise(avg_activity_time=mean(`Physical Activity Level`,na.rm=TRUE)) %>% 
  arrange(-avg_activity_time) %>% head(3) %>% pull(Occupation)
# Te 3 zawody to:Nurse, Lawyer, Accountant
all<-df %>% 
        mutate(age_category=ifelse(Age>=50,">=50","<50")) %>% 
        group_by(age_category) %>% 
        summarise(avg_hr_all=mean(`Heart Rate`,na.rm = TRUE))
most_active<-df %>% 
  mutate(age_category=ifelse(Age>=50,">=50","<50")) %>%  
  filter(Occupation %in% most_active) %>% 
  group_by(age_category) %>% 
  summarise(avg_hr_most_active=mean(`Heart Rate`,na.rm = TRUE))
merge(all,most_active,by='age_category') %>% 
  transform(diff_hr=avg_hr_all-avg_hr_most_active)
########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
min_wiek<-min(df$Age)
max_wiek<-max(df$Age)

df %>% 
  mutate(age_group= case_when(
    Age<30~'[20,30)',
    Age<40~'[30,40)',
    Age<50~'[40,50)',
    TRUE~'[50,60)'
  )) %>% 
  group_by(age_group,Gender,Occupation) %>% 
  summarise(osoby_w_zawodzie=n())  %>% 
  slice(which.max(osoby_w_zawodzie))

df %>% 
  mutate(age_group= case_when(
    Age<30~'[20,30)',
    Age<40~'[30,40)',
    Age<50~'[40,50)',
    TRUE~'[50,60)'
  )) %>% 
  group_by(age_group) %>% 
  summarise(avg_stress_level=mean(`Stress Level`,na.rm=TRUE),
         median_stress_level=median(`Stress Level`,na.rm=TRUE),
         sd_stress_level=sd(`Stress Level`,na.rm=TRUE))
