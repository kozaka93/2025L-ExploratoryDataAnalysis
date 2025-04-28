#wykres, który poprawię pochodzi ze strony bezpieczna.um.warszawa.pl
#i przedstawia jaki procent zgłoszeń na numer alarmowy w marcu 2025 
#w Warszawie zostało przekazane do jakiej służby ratunkowej.

#Wykres jest kołowy i jedna część wykresu (w tym przypadku policja) jest dominująca,
#co utrudnia "na pierwszy rzut oka" dostrzeżenie pozostałych kategorii.
#Dodatkowo na brzegach wycinków zastosowano cieniowanie, co zniekształaca
#wykres i utrudnia prawidłowe odczytanie danych.
#Myślę też, że tytuł: "Procentowy rozkład zgłoszeń do służb ratunkowych w Warszawie 
#w marcu 2025 roku." byłby bardziej trafny niż obecny, który słabo wskazuje na to, co dokładnie
#przedstawia wykres.

library(ggplot2)
library(dplyr)
# Tworzenie ramki danych
dane <- data.frame(
  Słuzba = c("Policja", 
             "Państwowe Ratownictwo Medyczne", 
             "Inne służby i podmioty", 
             "Państwowa Straż Pożarna"),
  procent_zdarzen = c(51, 35, 11, 3)
)

dane %>% 
  ggplot(aes(x = Słuzba, y = procent_zdarzen))+
  geom_col(fill = "navy") +
  labs(
    title = "Procentowy rozkład zgłoszeń do służb ratunkowych",
    subtitle = "marzec 2025, Warszawa",
    x = "Służba ratunkowa",
    y = "Procent zdarzeń [%]"
  )+
  theme_minimal()+
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 9, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 9))

#Wykres jest lepszy, ponieważ Duzo czytelniej przedstawia różnice między róznymi
#służbami. Dodatkowo tytuł jest bardziej trafny.
  