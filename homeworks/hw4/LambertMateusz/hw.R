library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# Dane oraz oryginalny wykres: https://dashboard.stat.gov.pl/

# Oryginalny wykres na stronie jest interaktywny, więc teoretycznie możnaby go
# analizować pod różnymi kątami. Według mnie, jezeli chcielibysmy porownac import
# na przestrzeni lat to tamten wykres jest nieczytelny z dwoch powodow:
# 1. Jest to zły typ wykresu, na wykresie liniowym ciężko jest dostrzec różnice w wartościach w tym samym punkcie
# 2. Paleta kolorów utrudnia rozróżnianie danych. Niektóre odcienie niebieskiego wyglądają niemal identycznie
# i nawet korzystajac z interaktywnych feature'ow nie moglem do konca odczytac co jest czym
# *3. Opis wykresu nie jest zbyt dobry, w tytule jest mowa o zł, jednak wartosci na wykresie to w rzeczywistosci 
# porownanie '?procentowe?', tzn. wartosc 108.2 sugeruje, ze byl wzrost o 8.2% wzgledem poprzedniego roku

# Uwazam, ze moj wykres jest lepszy, poniewaz jest duzo bardziej czytelny.
# Uzycie mocno rozniacych sie od siebie kolorow ulatwia szukanie odpowiednich 'kolumn'
# w celu zobaczenia trendow itp. Dodatkowo dopasowalem zakresy osi Y, tak zeby roznice 
# miedzy wartosciami byly bardziej zauwazalne. To pozwala na latwiejsze zidentyfikowanie
# miejsc, ktore chcemy glebiej przeanalizowac.
# A jezeli chcielibysmy analizowac trend na przestrzeni roku, to tez jest to mozliwe wlasnie 
# z uwagi na to ze kolory znaczaco sie od siebie roznia. 

dane <- read_delim(
  "dane.csv",
  delim = ",",
  quote = '"',
  locale = locale(decimal_mark = ",")
)

dane_long <- dane %>%
  pivot_longer(
    cols = c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII"),
    names_to  = "Miesiac",
    values_to = "Indeks"
  ) %>%
  filter(id_rok != 2019) %>%  # w 2019 nie ma danych
  mutate(
    Miesiac = factor(
      Miesiac,
      levels = c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII")
    ),
    Rok = factor(id_rok)
  )

base_colors <- c("#e60049", "#0bb4ff", "#50e991", "#e6d800", "#9b19f5", "#ffa300", "#dc0ab4", "#b3d4ff", "#00bfa0")

ggplot(dane_long, aes(x = Miesiac, y = Indeks, fill = Rok)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = base_colors) + 
  scale_y_continuous(breaks = c(100, 120, 140)) +
  coord_cartesian(ylim = c(75, 150)) +
  labs(
    title    = "Porównanie importu do Polski w różnych latach",
    subtitle = "Analogiczny okres roku poprzedniego = 100",
    x        = "Miesiąc",
    y        = "Stosunek do roku poprzedniego",
    fill     = "Rok"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title   = element_text(face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1)
  )