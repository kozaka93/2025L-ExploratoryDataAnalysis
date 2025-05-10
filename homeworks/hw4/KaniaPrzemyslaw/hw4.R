library(ggplot2)
library(dplyr)
library(tidyverse)

# https://dashboard.stat.gov.pl/ - strona, na której znalezione zostały dane

data <- read.csv("koniunktura_dane.csv")

df <- data %>%
  pivot_longer(cols = -id_rok, names_to  = "miesiac", values_to = "wartosc") %>%
  rename(rok = id_rok) %>%
  mutate(
    miesiac = factor(miesiac,
                     levels = c("I","II","III","IV","V","VI",
                                "VII","VIII","IX","X","XI","XII"),
                     ordered = TRUE),
    wartosc = as.numeric(str_replace(wartosc, ",", ".")))

df %>% 
  ggplot(aes(x = miesiac, y = wartosc, group = rok, color = factor(rok)))+
  geom_line(aes(y = wartosc, group = rok, color = factor(rok)), size = 1)+
  geom_point(aes(y = wartosc, group = rok, color = factor(rok)), size = 2)+
  labs(title = "Wskaźnik ogólnego klimatu koniunktury w budownictwie",
    subtitle = "niewyrównany sezonowo", x = "Miesiąc", y = "Wskaźnik koniunktury", color = "Rok")+
  theme_minimal(base_size = 16)+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"))

# a) obowiązkowo dodane zostały podpisy osi x i y
# b) zmienione zostały odcienie niebieskiego na wykresach na kolorowe

# Uzyskany wykres jest lepszy od oryginalnego przede wszystkim poprzez zamianę kolorów wykresów z różnych odcieni niebiesiego,
# które na pierwszy rzut oka trudno było od siebie rozróżnić. Patrząc na kolorowy wykres takiego problemu już nie ma z uwagi na
# różnorodność kolorów. Dużo łatwiej czyta się także wykresy, które podsiadaja podpisane obie osie, gdyż nie trzeba się wtedy
# domyslać jakie wartości są na nich przedstawione.
  
  
  
  
  