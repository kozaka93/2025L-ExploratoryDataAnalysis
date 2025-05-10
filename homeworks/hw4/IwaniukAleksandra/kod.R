library(ggplot2)
library(dplyr)
library(tidyverse)

#https://dashboard.stat.gov.pl - strona z danymi i wykresami

#poprawy wymagają:
# kolory wykresów, które są abolutnie za bardzo zbliżone do siebie co znacznie utrudnia odczytywanie danych
# brak podpisów osi


df <- read.csv('Wskaźnik cen towarów i usług konsumpcyjnych.csv')

dane_long <- df %>%
  pivot_longer(-id_rok, names_to = "Miesiac", values_to = "Wskaznik") %>% 
  rename(Rok = id_rok) %>%
  mutate(Wskaznik = as.numeric(str_replace(Wskaznik, ",", ".")),
         Miesiac = factor(Miesiac, 
                          levels = c("I", "II", "III", "IV", "V", "VI", 
                                     "VII", "VIII", "IX", "X", "XI", "XII"))) %>% 
  drop_na(Wskaznik)


ggplot(dane_long, aes(x = Miesiac, y = Wskaznik, group = Rok, color = factor(Rok)))+
  geom_line(size= 1) +
  geom_point(size = 1.2) +
  labs(
    title = "Wskaźnik cen towarów i usług konsumpcyjnych (okres poprzedni = 100)" ,
    subtitle = "Porównanie lat 2019-2025",
    x = "Miesiąc",
    y = "Wskaźnik",
    color = "Rok") +
  scale_color_manual(values = c(
    "2019" = "#202D5B",
    "2020" = "#5BC7F2",
    "2021" = "#7a5195",
    "2022" = "#bc5090",
    "2023" = "#ef5675",
    "2024" = "#ff764a",
    "2025" = "#ffa600"))+
  scale_y_continuous(
    breaks = c(99, 100, 101, 102, 103, 104),
    limits=c(99 ,104))+ 
  theme_minimal()

# wykres ten jest lepszy, poniewaz znacznie latwiej odczytac ktory wykres opowiada jakiemu okresowi, 
# osie zostały podpisane oraz dodany zostal podtytul
