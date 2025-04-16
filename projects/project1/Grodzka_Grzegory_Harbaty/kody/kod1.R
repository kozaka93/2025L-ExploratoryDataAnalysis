getwd()
df2 <- read.csv("Houses.csv")
setwd("/Users/agataharbaty/Desktop/Files_R/Projekt")
install.packages("ggplot2")
install.packages("ggtext")
install.packages("scales")
library(scales)
library(ggtext)
library(ggplot2)
library(dplyr)
library(tidyverse)
  
df <- df2 %>%
  mutate(city = replace(city, city == "Krak\xf3w", "Kraków"),
         city = replace(city, city == "Pozna\xf1", "Poznań"))

oblicz_zarobki <- function(start_brutto = 10000, wiek_start = 23, wiek_koniec = 60, podwyzka = 0.05) {
  # Składki ZUS
  skladka_emerytalna <- 0.0976
  skladka_rentowa <- 0.015
  skladka_chorobowa <- 0.0245
  skladka_zdrowotna <- 0.09
  
  # Progi podatkowe
  kwota_wolna <- 30000
  prog_podatkowy1 <- 120000
  ulga_dla_mlodych <- 85528
  
  # Stawki PIT
  PIT_12 <- 0.12
  PIT_32 <- 0.32
  
  pensja_brutto <- start_brutto
  zarobki <- data.frame(Wiek = integer(), Brutto = numeric(), Netto = numeric())
  
  for (wiek in wiek_start:wiek_koniec) {
    roczne_brutto <- pensja_brutto * 12
    
    # Obliczenie składek ZUS
    skladki_ZUS <- roczne_brutto * (skladka_emerytalna + skladka_rentowa + skladka_chorobowa)
    podstawa_podatku <- roczne_brutto - skladki_ZUS
    
    # Obliczenie podatku
    if (wiek < 26 & podstawa_podatku <= ulga_dla_mlodych) {
      podatek <- 0  # Ulga dla młodych
    } else {
      do_opodatkowania <- max(0, podstawa_podatku - kwota_wolna)
      
      if (do_opodatkowania <= prog_podatkowy1) {
        podatek <- do_opodatkowania * PIT_12
      } else {
        podatek <- (prog_podatkowy1 * PIT_12) + ((do_opodatkowania - prog_podatkowy1) * PIT_32)
      }
    }
    
    # Obliczenie składki zdrowotnej
    skladka_zdrowotna_kwota <- podstawa_podatku * skladka_zdrowotna
    
    # Obliczenie pensji netto
    pensja_netto <- roczne_brutto - skladki_ZUS - podatek - skladka_zdrowotna_kwota
    
    # Zapis do tabeli
    zarobki <- rbind(zarobki, data.frame(Wiek = wiek, Brutto = roczne_brutto, Netto = pensja_netto))
    
    # Wzrost pensji o 5%
    pensja_brutto <- pensja_brutto * (1 + podwyzka)
  }
  
  return(zarobki)
}

# Teraz będę sprawdzała jak szybko można zaoszczędzić na mieszkanie, w zależności od procentu pieniędzy, które są odkładane


df_oszczedzanie <- function(){
  
df2 <- oblicz_zarobki()

# Tworzę kolumny ukazujące ile można zaoszędzić w zależnosci od 
# procenta odkładanych pieniędzy

df2 <- df2 %>% 
  mutate(ten = Netto*0.1,
         fift = Netto*0.15,
         twen = Netto*0.2)

# Tworzę nowe kolumny ukazujące ile pieniędzy na koniec roku będzie na koncie 
# oszczędnościowym jeżeli oprocentowanie wynosi 4% i okładamy pewną sumę pieniędzy
# co miesiąc i odsetki są naliczane co miesiąc. 

df2$cumsum10 <- numeric(nrow(df2))
df2$cumsum15 <- numeric(nrow(df2))
df2$cumsum20 <- numeric(nrow(df2))
df2$cumsum10[1] = oblicz_odsetki(0,(df2$ten[1])/12)
df2$cumsum15[1] = oblicz_odsetki(0,(df2$fift[1]/12))
df2$cumsum20[1] = oblicz_odsetki(0,(df2$twen[1]/12))

for (i in 2:nrow(df2)){
  df2$cumsum10[i] <- oblicz_odsetki(df2$cumsum10[i-1],df2$ten[i]/12)
  df2$cumsum15[i] <- oblicz_odsetki(df2$cumsum15[i-1],df2$fift[i]/12)
  df2$cumsum20[i] <- oblicz_odsetki(df2$cumsum20[i-1],df2$twen[i]/12)
}

df2 <- df2 %>% 
  pivot_longer(cols = c(cumsum10,cumsum15,cumsum20),
               names_to = "Procent odkładany",
              values_to = "Odłożone pieniadze") %>% 
  filter(Wiek < 50)

return(df2)
}

# Tworzę funkcję ukazującą stan konta oszczędnościowego po roku gdzie odsetki są naliczane co miesiąc o oprocentowaniu 4%

oblicz_odsetki <- function(poczatkowa_kwota,kwota_mies) {
  saldo_po_odsetkach <- poczatkowa_kwota * (1 + 0.04 / 12)^(12)
  saldo_po_odsetkach <- saldo_po_odsetkach + kwota_mies * (((1 + 0.04 / 12)^(12) - 1) / (0.04 / 12))
  return(saldo_po_odsetkach)
}

df <- df_oszczedzanie()

ggplot(df, aes(x = Wiek, y = `Odłożone pieniadze`, fill = `Procent odkładany`)) +
  scale_y_continuous(labels = label_number(scale = 0.000001,suffix = " mln "),
                     limits = c(0,1400000),
                     breaks = seq(0,1400000, by = 200000)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values = c("cumsum10" = "mediumorchid4","cumsum15" = "brown2", "cumsum20" = "yellow"), 
                    labels = c("cumsum10" = "10%", "cumsum15" = "15%", "cumsum20" = "20%"))+
  labs(title = "Ile lat oszczędzania jest potrzebne aby zakupić mieszkanie?",
       subtitle = "Zakładamy, że w każdym miesiącu odkładamy ustalony procent pieniędzy z wypłaty 
na konto oszczędnościowe z oprocentowaniem 4%" ) 

# Robię boxplota z porównaniem ceny za metr a ilością pokojów w mieszkaniu

df2 %>% 
  mutate(rooms1 = case_when(
    rooms <= 1 ~ 1,
    rooms <= 2 ~ 2,
    rooms <= 3 ~ 3, 
    rooms <= 4 ~ 4,
    rooms > 4 ~ 5
  )) %>% 
  mutate(cena_za_m = price / sq) %>% 
  filter(cena_za_m < 30000) %>%
  ggplot(aes(x = as.factor(rooms1), y = cena_za_m)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_number(scale = 0.001,suffix = " tyś "))+
  labs(title = expression("Zależność pomiędzy ilością pokoi w mieszkaniu a ceną za "* m^2),
       y = expression("Cena za " * m^2),
       x = "Liczba pokoi") +
  theme_minimal()
       
       
  
  
typeof(df2$rooms)
  
  
  
