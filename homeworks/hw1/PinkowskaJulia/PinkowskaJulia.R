# Ładowanie pakietów
library(dplyr)
library(tidyr)

# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, binarna
# Age - ilościowa, ilorazowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep - jakościowa, uporządkowana
# Physical.Activity.Level - ilościowa, ilorazowa
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder - jakościowa, nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------

#Tabela zaweirająca średie tętno ludzie w różnych kategoriach BMI 
mean_heart_rate<- df %>%
  group_by(BMI.Category) %>%
  summarise (mean_heart_rate=mean(Heart.Rate, na.rm=TRUE))%>%
  arrange(desc(mean_heart_rate))

#Dla kategorii 'Obese' średnie tentno jest najwyższe, wynosi 84,3.

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------

#Tabela zawierająca zawod, w których ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7
avg_sleep <- df %>%
  filter(Stress.Level < 7) %>%
  group_by(Occupation)%>%
  summarise(Average_sleep = mean(Sleep.Duration, na.rm=TRUE))%>%
  arrange(desc(Average_sleep))

#Najwięcej śpią Inżynierowie, gdyż ich czas snu wynosi 8,05h.

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
#Tabela zaweirająca zawody, w których najczęściej pracują osoby z dowolnym zaburzeniem snu
sleep_disorder <- df %>%
  filter(Sleep.Disorder !='None')%>%
  group_by(Occupation)%>%
  summarise(Count=n())%>%
  arrange(desc(Count))

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?
# Rozwiązanie: ------------------------------------------------------------

#Tabela zawierająca zawody, w których jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet
df_8 <- df[,c('Gender', 'Occupation','BMI.Category')]%>%
  mutate(BMI.Category = ifelse(BMI.Category=='Obese',1,0))%>%
  group_by(Occupation,Gender)%>%
  summarise(Obesity = sum(BMI.Category))%>%
  pivot_wider(names_from = Gender, values_from = Obesity, values_fill = 0)%>%
  filter(Male>Female)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------

#Tabela zawierająca 3 zawody, w których różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa
df_2<- df[,c("Occupation", "Blood.Pressure", "Sleep.Duration", "Quality.of.Sleep")]%>%
  group_by(Occupation) %>%
  filter((Count=n())>20)%>%
  mutate(
    skurczowa = sub("/.*", "", Blood.Pressure),
    rozkurczowa = sub(".*\\/", "", Blood.Pressure)
  )%>%
  mutate(skurczowa=as.numeric(skurczowa),
         rozkurczowa = as.numeric(rozkurczowa))%>%
  mutate(Roznica= skurczowa - rozkurczowa)%>%
  mutate(Srednia = mean(Roznica, na.rm=TRUE))%>%
  group_by(Occupation)%>%
  summarise(Srednia_roznica = mean(Roznica, na.rm=TRUE))%>%
  arrange(desc(Srednia_roznica))%>%
  head(3)

#Wybranie drugiego zawodu pod względem średniej różnicy
Drugi_zawód <- df_2$Occupation[2]

#Tabela zaweirająca średnią i medianę czasu snu dla różnych jakości snu dla prawników
sleep <- df[, c('Occupation', 'Sleep.Duration', "Quality.of.Sleep")]%>%
  filter(Occupation == Drugi_zawód)%>%
  group_by(Quality.of.Sleep)%>%
  mutate(Mean=mean(Sleep.Duration), Median=median(Sleep.Duration))%>%
  select(c(Mean,Median, Quality.of.Sleep))%>%
  distinct()

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------

#Tabela zaweirająca 3 zawody, w których ludzie średnio najbardziej się ruszają
df_5 <- df[,c ("Occupation", 'Age', "Physical.Activity.Level")]%>%
  group_by(Occupation)%>%
  mutate(mean_activity = mean(Physical.Activity.Level))%>%
  select(c('Occupation', "mean_activity"))%>%
  arrange(desc(mean_activity))%>%
  distinct()%>%
  head(3)

#Wybranie 3zawodów, w których ludzie się najbardziej ruszają
Zawód_1<- df_5$Occupation[1]
Zawód_2<- df_5$Occupation[2]
Zawód_3<- df_5$Occupation[3]

#Tabela zawierająca średnie tętno ze wszystkich danych
All_heart_rate <- df[,c('Heart.Rate', 'Age')]%>%
  mutate(Age_group1 =ifelse(Age>=50, "Grupa 1 (50+)", "Grupa 2( <50)"))%>%
  group_by(Age_group1)%>%
  summarise(average_heart_rate_all = mean(Heart.Rate))

#Tabela zawierająca średnie tętno z 3 zawodów, w których ludzie najbadziej się ruszają  
Heart_Rate <- df[, c("Occupation", 'Age', "Heart.Rate")]%>%
  filter(Occupation == Zawód_1 |Occupation == Zawód_2|Occupation == Zawód_3)%>%
  mutate(Age_group1 =ifelse(Age>=50, "Grupa 1 (50+)", "Grupa 2( <50)"))%>%
  group_by(Age_group1)%>%
  summarise(average_heart_rate2 = mean(Heart.Rate))

#Tabela zawierająca różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w odpowiednich zawodach
Comparison_heart_rate <- left_join(Heart_Rate, All_heart_rate, by='Age_group1' )%>%
  mutate(Roznica = abs(average_heart_rate2 - average_heart_rate_all))%>%
  select(Age_group1, Roznica)


########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------

#Tabela zaweirająca przypisanie do odpowiednich grup wiekowych oraz najpopularniejsze zawody dla kobiet i mężczyzn
Age_1 <- df[,c('Age', 'Gender', 'Occupation', "Stress.Level")]%>%
  mutate(Age_group = case_when(
    Age>=50 & Age <60 ~ '[50,60)',
    Age>=40 & Age <50 ~ '[40,50)',
    Age>=30 & Age <40 ~'[30,40)',
    Age>=20 & Age <30 ~'[20,30)',
    Age>=10 & Age <20 ~'[10,20)',
    Age>=0 & Age <10 ~'[0,10)'
  ))%>%
  group_by(Age_group, Gender,Occupation)%>%
  summarise(liczba=n())%>%
  arrange(Age_group, Gender, desc(liczba))%>%
  slice_max(liczba, n=1)%>%
  select('Age_group', 'Gender', "Occupation")%>%
  rename(Most_popular_occupation = Occupation)

#Tabela zawierająca średnią, medianę i odchylenie standardowe dla poziomu stresu w danych grupach wiekowych 
  df_3 <- df[,c('Age', 'Gender', 'Occupation', "Stress.Level")]%>%
    mutate(Age_group = case_when(
      Age>=50 & Age <60 ~ '[50,60)',
      Age>=40 & Age <50 ~ '[40,50)',
      Age>=30 & Age <40 ~'[30,40)',
      Age>=20 & Age <30 ~'[20,30)',
      Age>=10 & Age <20 ~'[10,20)',
      Age>=0 & Age <10 ~'[0,10)'
    ))%>%
    group_by(Age_group)%>%
    summarise(Mean_stress_level=mean(Stress.Level),
            Median_stress_level=median(Stress.Level),
            Sd_stress_level=sd(Stress.Level))


  
  
  

                        







