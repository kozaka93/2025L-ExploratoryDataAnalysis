# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
#df <- read.csv("C:\\Users\\marys\\Desktop\\pd\\data.csv")
#file.exists("data.csv")
#"C:\Users\marys\Desktop\pd\data.csv"
#df <- read.csv(file.choose())


getwd()
setwd("C:/Users/marys/Desktop/pd")
df <- read.csv("data.csv")
########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - ilosciowa binarna
# Age - ilosciowa ilorazowa
# Occupation - jakosciowa nominalna
# Sleep.Duration - ilosciowa ilorazwa
# Quality.of.Sleep -ilosciowa ilorazowa
# Physical.Activity.Level - ilosciowa ilorazowa
# BMI.Category - jakosciowa
# Sleep.Disorder - jakosciowa


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
bmi_heart_rate <- aggregate(Heart.Rate ~ BMI.Category, data = df, FUN = mean)
print(bmi_heart_rate)
max_bmi_category <- bmi_heart_rate[which.max(bmi_heart_rate$Heart.Rate), ]
print(max_bmi_category)

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
filtered_data <- df[df$Stress.Level < 7, ]
occupation_sleep <- aggregate(Sleep.Duration ~ Occupation, data = filtered_data, FUN = mean)
max_sleep_occupation <- occupation_sleep[which.max(occupation_sleep$Sleep.Duration), ]
print(max_sleep_occupation)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
sleep_disorder_data <- df[df$Sleep.Disorder != "None", ]
occupation_counts <- table(sleep_disorder_data$Occupation)
sorted_occupations <- sort(occupation_counts, decreasing = TRUE)
print(sorted_occupations)

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
obese_data <- df[df$BMI.Category == "Obese", ]
obese_counts <- table(obese_data$Occupation, obese_data$Gender)
obese_df <- as.data.frame(obese_counts)
obese_wide <- reshape(obese_df, timevar = "Var2", idvar = "Var1", direction = "wide")
colnames(obese_wide) <- c("Occupation", "Female", "Male")
higher_male_obesity <- obese_wide[obese_wide$Male > obese_wide$Female, ]
print(higher_male_obesity)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------


########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------


########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
