# Ładowanie pakietów
library(dplyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

table(df$Gender) #spr., czy są tylko dwie pucie

# Gender - jakościowa binarna
# Age - ilościowa ilorazowa
# Occupation - jakościowa nominalna
# Sleep.Duration - ilościowa ilorazowa
# Quality.of.Sleep - ilościowa ilorazowa
# Physical.Activity.Level - il. ilorazowa
# BMI.Category - jakościowa uporządkowana
# Sleep.Disorder - jakościowa nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

    
# Rozwiązanie: ------------------------------------------------------------

df %>% 
  group_by(BMI.Category) %>% 
    summarise(sr_tetno = mean(Heart.Rate)) %>% 
      arrange(desc(sr_tetno))

#dla Obese

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  filter(Stress.Level < 7) %>% 
    group_by(Occupation) %>%
      summarise(sr_dl_snu = mean(Sleep.Duration)) %>% 
        arrange(desc(sr_dl_snu)) %>% 
          head(1)

#Engineer

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>%
  filter(Sleep.Disorder != "None") %>% 
    group_by(Occupation) %>% 
      count() %>% 
        arrange(desc(n))

#Nurse, Teacher, Salesperson, itd

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  group_by(Occupation, Gender) %>% 
    filter(BMI.Category == "Obese") %>% 
      count()

#we wszystkich oprócz prawnika (tam jest ich po równo)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  mutate(skurczowe = as.numeric(sub("/.*", "", Blood.Pressure)), rozkurczowe = as.numeric(sub(".*/", "", Blood.Pressure)), 
    roznica = skurczowe - rozkurczowe) %>% 
      group_by(Occupation) %>% 
        summarise(n = n(), sr_roznica = mean(roznica)) %>%
          filter(n > 20) %>%
            arrange(desc(sr_roznica)) 
  
#drugi zawód to Lawyer:

df %>% 
  filter(Occupation == "Lawyer") %>% 
  group_by(Quality.of.Sleep) %>%
    summarise(srednia = mean(Sleep.Duration, na.rm = TRUE), mediana = median(Sleep.Duration, na.rm = TRUE))

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
df %>%
  group_by(Occupation) %>%
    summarise(n = n(), sr_kroki = mean(Daily.Steps, na.rm = TRUE)) %>%
      filter(n > 20) %>%
        arrange(desc(sr_kroki)) 
#Nurse, Lawyer, Accountant

df50 <- df %>% 
  mutate(po_piecdziesiatce = ifelse(Age >= 50, TRUE, FALSE)) %>% 
    group_by(po_piecdziesiatce) %>%
      summarise(sr_tetno = mean(Heart.Rate))

df3 <- df %>% 
  filter(Occupation %in% c("Nurse","Lawyer","Accountant")) %>% 
  mutate(po_piecdziesiatce = ifelse(Age >= 50, TRUE, FALSE)) %>% 
  group_by(po_piecdziesiatce) %>%
  summarise(sr_tetno = mean(Heart.Rate))
  
df50 - df3
#w grupie <=50l. jest różnica 0.3760697, a w grupie >50l. wynosi -2.1478495
#(między wszystkimi danymi, a osobami o tych 3 wybranych zawodach)



########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
kfc <- df %>% 
  mutate(kubelek = paste0("[", floor(Age / 10) * 10, "-", floor(Age / 10) * 10 + 10, ")")) 

kfc2 <- kfc %>% 
  count(kubelek, Gender, Occupation) %>% 
    group_by(kubelek, Gender) %>% 
      slice_max(n, n = 1)
kfc %>% 
  semi_join(kfc2, by = c("kubelek", "Gender", "Occupation")) %>%
    group_by(kubelek, Gender, Occupation) %>% 
      summarize(srednia_stresu = mean(Stress.Level),  mediana_stresu = median(Stress.Level),
                odchylenie_stresu = sd(Stress.Level))



