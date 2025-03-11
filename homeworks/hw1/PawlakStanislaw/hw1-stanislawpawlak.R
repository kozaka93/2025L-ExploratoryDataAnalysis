# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowa, nominalna
# Age - ilościowa, ilorazowa
# Occupation - jakościowa, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep - jakościowa, porządkowa
# Physical.Activity.Level - jakościowa, porządkowa
# BMI.Category - jakościowa, porządkowa
# Sleep.Disorder - jakościowa, nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df%>%
  group_by(BMI.Category)%>%
  summarise(srednie_tetno=mean(Heart.Rate))%>%
  arrange(desc(srednie_tetno))
#ODP: Dla kategorii Obese


########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?
# Rozwiązanie: ------------------------------------------------------------
df %>%
  filter(Stress.Level < 7) %>%  
  group_by(Occupation) %>%
  summarise(srednia_dlugosc_snu = mean(Sleep.Duration, na.rm = TRUE))%>%
  arrange(desc(srednia_dlugosc_snu))
#ODP:Najwięcej śpia wzawdozie Engineer jeżeli ich poziom stresu jest niższy niż 7

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>%
  group_by(Occupation)%>%
  summarise(ilosc_os=sum(Sleep.Disorder!= "None"))%>%
  arrange(desc(ilosc_os))

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df%>%
  filter(BMI.Category=="Obese")%>%
  group_by(Occupation, Gender)%>%
  summarise(ilosc_otylych = n()) %>%
  group_by(Occupation)%>%
  summarise(
    male_count = sum(ilosc_otylych * (Gender == "Male")),
    female_count = sum(ilosc_otylych * (Gender == "Female"))
  )%>%
  filter(male_count > female_count)
########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
drugi_zawod <- df %>%
  mutate(
   cisnienie_skurczowe = as.numeric(sub("/.*", "", Blood.Pressure)),  
   cisnienie_rozkurczowe = as.numeric(sub(".*/", "", Blood.Pressure))    
  )%>%
  group_by(Occupation) %>%
  filter(n() > 20) %>%  
  summarise(srednia_roznica_BP = mean( cisnienie_skurczowe-cisnienie_rozkurczowe , na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(srednia_roznica_BP)) %>%
  slice_head(n = 3)%>%
  slice(2)%>%
  pull(Occupation)
  
df%>%
  filter(Occupation == drugi_zawod) %>%
  group_by(Quality.of.Sleep) %>%
  summarise(
  sredni_sen = mean(Sleep.Duration),
  mediana_sen = median(Sleep.Duration),
  )

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
zawody<- df%>%
  group_by(Occupation)%>%
  summarise(srednia_aktywnosc = mean(Physical.Activity.Level))%>%
  arrange(desc(srednia_aktywnosc))%>%
  slice_head(n=3)

srednie_tetno_wszystkich_ponad50 <- mean(df$Heart.Rate[df$Age >= 50])
srednie_tetno_wybranych_ponad50 <- mean(df$Heart.Rate[df$Age >= 50 & df$Occupation %in% zawody$Occupation])
roznica1 <- srednie_tetno_wybranych_ponad50 - srednie_tetno_wszystkich_ponad50

srednie_tetno_wszystkich_ponizej50 <- mean(df$Heart.Rate[df$Age < 50])
srednie_tetno_wybranych_ponizej50 <- mean(df$Heart.Rate[df$Age < 50 & df$Occupation %in% zawody$Occupation])
roznica2 <- srednie_tetno_wybranych_ponizej50 - srednie_tetno_wszystkich_ponizej50

srednie_tetno_wszystkich_ponad50
srednie_tetno_wybranych_ponad50
roznica1

srednie_tetno_wszystkich_ponizej50
srednie_tetno_wybranych_ponizej50
roznica2
  
########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
df%>%
  mutate(
    grupa1= Age>=0 & Age<10,
    grupa2= Age>=10 & Age<20,
    grupa3= Age>=20 & Age<30,
    grupa4= Age>=30 & Age<40,
    grupa5= Age>=40 & Age<50,
    grupa6= Age>=50 & Age<60
  )%>%
  filter(grupa1 | grupa2 | grupa3 | grupa4 | grupa5 | grupa6) %>%  
  group_by(grupa1, grupa2, grupa3, grupa4, grupa5, grupa6, Gender) %>%  
    summarise(
      najpopularniejszy_zawod = names(sort(table(Occupation), decreasing = TRUE))[1],  
      sredni_stres = mean(Stress.Level),  
      mediana_stres = median(Stress.Level),  
      odchylenie_stres = sd(Stress.Level),  
    )
