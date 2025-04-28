#wizualizacja została wzięta z linku:https://grupakety.com/relacje-inwestorskie/grupa-kety-na-gpw/raporty-okresowe-i-prezentacje/
#na tej stronie trzeba wejść w pdf "Prezentacja wynikowa za Q1 2025" (zawarta o tym samym tytule w pliku z rozwiązaniem)
#interesujące nas wykresy znajdują się na 9 stronie tej prezentacji (png o tytule "błędna wizualizacja" w rozwiązaniu)



# Co wymaga poprawy?

#   wykres jest kołowy w 3D co jest błędne(zniekształca wartośći)
#   użycie tych samych kolorów sugeruje jakieś połączenie wartości co jest nieprawdą
#   wartości i odpowiadające im napisy są nieestetycznie wstawione
#   brak temu wykresowi jakiegokolwiek tytułu



library(ggplot2)
library(dplyr)
library(patchwork)

#potrzebne dane do wykresów
dane_waluta <- data.frame(
  kategoria = c("PLN", "EUR", "Pozostałe"),
  procent = c(79, 20, 1))

dane_termin <- data.frame(
  kategoria = c("Krótkoterminowe", "Długoterminowe"),
  procent = c(24, 76))

#wybieramy kolory do wykresów
kolory_waluta <- c("PLN" = "lightgreen", "EUR" = "plum", "Pozostałe" = "red")
kolory_termin <- c("Krótkoterminowe" = "orange", "Długoterminowe" = "lightblue")

#pozycje etykiet
dane_waluta <- dane_waluta %>%
  arrange(desc(kategoria)) %>%
  mutate(pos = cumsum(procent) - procent/2)

dane_termin <- dane_termin %>%
  arrange(desc(kategoria)) %>%
  mutate(pos = cumsum(procent) - procent/2)

#robimy wykres 1 ale poprawnie(na obrazku jest drugi)
wykres_waluta <- ggplot(dane_waluta, aes(x = "", y = procent, fill = kategoria)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(procent, "%"), y = pos), color = "black", size = 4) +
  scale_fill_manual(values = kolory_waluta) +
  labs(title = "Struktura Walutowa ", fill = "Waluta") +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)) +
  labs(fill = NULL)

#to samo co wyżej kolejny wykres
wykres_termin <- ggplot(dane_termin, aes(x = "", y = procent, fill = kategoria)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(procent, "%"), y = pos), color = "black", size = 4) +
  scale_fill_manual(values = kolory_termin) +
  labs(title = "Struktura Terminowa", fill = "Termin") +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)) +
  labs(fill = NULL)

#łączymy wykresy w patchworku
(wykres_termin + wykres_waluta) + 
  plot_annotation(
    title = "Struktura zadłużenia Grupy Kęty – 1 kwartał 2025",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))
#ten wykres jako png "nowa wizualizacja"



#Dlaczego nowy wykres jest lepszy?

#   przede wszystkim jest dalej kołowym wykresem ale w 2D co sprawia, że jest czytelniejszy
#   posiada on tytuł, przez co wiemy co opisuje
#   posiada różne kolory, przez co wiemy, że nie ma powiązania między 1 i 2 wykresem
#   wartości procentowe są w wycinkach kół oraz związane z nimi wartości są wypisane w legendzie( jest czytelniej)