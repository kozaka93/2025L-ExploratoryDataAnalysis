#Źródło: https://www.infor.pl/prawo/wybory/prezydenckie/6840011,najnowszy-sondaz-prezydencki-2025-cbos-wybory.html

#na wykresie mamy zbędną legendę, która powiela informację zawartą w tytule, 
#tytuł jest mało precyzyjny,
#brakuje podpisów osi

sondaz <- data.frame(
  Kandydat = c(
    "Rafał Trzaskowski",
    "Karol Nawrocki",
    "Sławomir Mentzen",
    "Szymon Hołownia",
    "Adrian Zandberg",
    "Magdalena Biejat",
    "Marek Jakubiak"
  ),
  Procent = c(34.8, 22.7, 15.8, 6.7, 4.2, 3.4, 2.0)
)

library(dplyr)
library(ggplot2)

sondaz$Kandydat <- factor(sondaz$Kandydat, levels = sondaz$Kandydat)

ggplot(sondaz, aes(x = Kandydat, y = Procent)) +
  geom_col(fill = "dodgerblue") +
  labs(
    title = "Procent poparcia kandydatów na prezydenta",
    subtitle = "Dane z kwietnia 2025 na bazie sondażu Research Partner",
    x = "Kandydat",
    y = "Procent poparcia"
  ) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.major.x = element_blank()) 

#dodano tytuły do osi,
#dodano informacje na temat pochodzenia danych,
#przekrzywiono opisy osi, co ułatwia odczytanie
#usunięto legendę,
#słupki są bardziej spójne
