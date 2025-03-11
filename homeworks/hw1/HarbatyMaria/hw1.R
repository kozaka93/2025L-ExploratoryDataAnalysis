# Ładowanie pakietów
library(dplyr)


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Gender - jakościowa, binarne/nominalna
# Age - ilościowe, przedziałowe
# Occupation - jakościowa, nominalne
# Sleep.Duration - ilościowe,ilorazowe
# Quality.of.Sleep - jakościowe, uporządkowane
# Physical.Activity.Level - ilościowe, ilorazowe
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder - jakościowa, nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średnie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

data %>% 
  group_by(`BMI Category`) %>% 
  summarise(Median_Heart_rate = mean(`Heart Rate`)) %>% 
  arrange(-Median_Heart_rate)
  
# Największa średnia tętna występuje dla BMI wstazujących otyłość (Obese).

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?


data %>% 
  filter(`Stress Level` < 7) %>% 
  group_by(Occupation) %>% 
  summarise(Mean_sleep = mean(`Sleep Duration`)) %>% 
  arrange(-Mean_sleep) %>% 
  head(1)

# Odpowiedź to engineer

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

data %>% 
  filter(!`Sleep Disorder` == "None") %>% 
  group_by(Occupation) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)
  
# Odpowiedzą jest pielęgniarka

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

df1 <- data %>% 
  filter(`BMI Category` == "Obese")  %>% 
  group_by(Occupation,Gender) %>% 
  summarise(liczba = n())
  
v <- unique(df1$Occupation)
x <- numeric(length(v))
y <- numeric(length(v))
for (i in 1:length(v)) {
  if (any(df1$Occupation == v[i] & df1$Gender == "Female") == TRUE) {
    x[i] = subset(df1,df1$Occupation == v[i] & df1$Gender == "Female" )$liczba
  }
  if (any(df1$Occupation == v[i] & df1$Gender == "Male") == TRUE) {
    y[i] = subset(df1,Occupation == v[i] & df1$Gender == "Male" )$liczba
  }
}

df2 <- data.frame(
  Occupation = unique(df1$Occupation),
  Woman = x,
  Man = y
) 
  
df2 %>% 
  filter(Woman < Man) %>% 
  select(Occupation)

# Odp: Doctor, Sales Representative, Software Engineer, Teacher

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

split_column <- str_split(data$`Blood Pressure`,"/")
skurcz <- sapply(split_column, function(col) col[1])
rozkurcz <- sapply(split_column, function(col) col[2])
skurcz <- sapply(skurcz, as.integer)
rozkurcz <- sapply(rozkurcz, as.integer)

data %>% 
  mutate(roznica = abs(rozkurcz - skurcz)) %>% 
  group_by(Occupation) %>% 
  summarise(mean = mean(roznica, na.rm = TRUE),
            n = n()) %>% 
  filter(n>20) %>% 
  arrange(-mean)

# Czyli biorę pielęgniarkę, ponieważ sprzedawca i prawnik mają taką samą średnią

data %>% 
  filter(Occupation == "Nurse") %>% 
  group_by(`Quality of Sleep`) %>% 
  summarise(med = median(`Sleep Duration`),
            mean = mean(`Sleep Duration`))
  
#  `Quality of Sleep`   med  mean
# <dbl> <dbl> <dbl>
# 1                  5  6.45  6.45
# 2                  6  6.1   6.07
# 3                  7  7.1   7.1 
# 4                  8  7.7   7.7 
# 5                  9  8.1   8.09

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

df0 <- data %>% 
  group_by(Occupation) %>% 
  summarise(mean = mean(`Physical Activity Level`)) %>% 
  arrange(-mean) %>% 
  head(3)

# Otrzymuję pielęgniarkę, prawnika oraz księgowego.

df1 <- data %>% 
  mutate(Group = ifelse(Age < 50, 0,1)) %>% 
  group_by(Occupation,Group) %>% 
  summarise(mean = mean(`Heart Rate`)) %>% 
  filter(Occupation == "Nurse" | Occupation == "Lawyer" | Occupation == "Accountant", Group == 0)

df2 <- data %>% 
  mutate(Group = ifelse(Age < 50, 0,1)) %>% 
  group_by(Occupation,Group) %>% 
  summarise(mean = mean(`Heart Rate`)) %>% 
  filter(Occupation == "Nurse" | Occupation == "Lawyer" | Occupation == "Accountant", Group == 1)

# Teraz dla kolejnych zawodoów będą liczyć różnicę 

# Dla pielęgniarek różnica w 1-szej grupie 
abs(df0[df0$Occupation == "Nurse","mean"] - df1[df1$Occupation == "Nurse","mean"])
# Mamy 3.177276

# Dla pielęgniarek różnica w 2-giej grupie 
abs(df0[df0$Occupation == "Nurse","mean"] - df2[df2$Occupation == "Nurse","mean"])
# Mamy 7.624755

# Dla prawników z 1-szej grupy 
abs(df0[df0$Occupation == "Lawyer","mean"] - df1[df1$Occupation == "Lawyer","mean"])
# Mamy 0.787234

# Nie ma prawników z drugiej grupy wiekowej

# Dla księgowych z 1-szej grupy wiekowej
abs(df0[df0$Occupation == "Accountant","mean"] - df1[df1$Occupation == "Accountant","mean"])
# Mamy 10.14996

# Dla księgowych z 2-giej grupy wiekowej
abs(df0[df0$Occupation == "Accountant","mean"] - df2[df2$Occupation == "Accountant","mean"])
# Mamy 13.89189


########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

min(data$Age) # 27
max(data$Age) # 59

data %>% 
  mutate(Group = case_when(Age >= 20 & Age < 30 ~ 3,
                           Age >= 30 & Age < 40 ~ 4,
                           Age >= 40 & Age < 50 ~ 5,
                           Age >= 50 & Age < 60 ~ 6)) %>% 
  group_by(Group,Gender,Occupation) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  group_by(Gender,Group) %>% 
  filter(n == max(n,na.rm = TRUE)) %>% 
  mutate(Mean = mean(`Stress Level`, na.rm = TRUE),
         Med = median(`Stress Level`, na.rm = TRUE),
         Sd = sd(`Stress Level`, na.rm = TRUE)) %>% 
  distinct(Group, Gender, Occupation, Mean, Med, Sd) 
  
  
# Group Gender Occupation   Mean   Med    Sd
# <dbl> <chr>  <chr>       <dbl> <dbl> <dbl>
# 1     3 Male   Doctor       7.33     8 0.985
# 2     3 Female Nurse        7        7 0    
# 3     4 Male   Doctor       6.94     6 1.01 
# 4     4 Female Accountant   3.93     4 0.262
# 5     5 Male   Salesperson  7        7 0    
# 6     5 Female Teacher      4.34     4 0.897
# 7     6 Female Nurse        5.05     3 2.43 
> 
  
  

                           

