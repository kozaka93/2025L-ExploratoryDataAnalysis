# Źródło wykresu: https://www.infor.pl/prawo/wybory/prezydenckie/6840011,najnowszy-sondaz-prezydencki-2025-nawrocki-trzaskowski-mentzen-wyb.html

# Elementami wymagającymi poprawy na oryginalnym wykresie są tytuł, który nie informuje, 
# co przedstawia wykres oraz legenda, która jest niepotrzebna i nie wprowadza żadnych
# nowych danych. 

library(ggplot2)
library(dplyr)

kandydaci <- c("Rafał Trzaskowski", "Karol Nawrocki", "Sławomir Mentzen", "Szymon Hołownia")
poparcie <- c(35.7, 22.3, 15.0,6.9)

df <- data.frame(
  Kandydat = kandydaci,
  Poparcie = poparcie
)

ggplot(df, aes(x = reorder(Kandydat, -Poparcie), y = Poparcie)) +
  geom_bar(stat = "identity", show.legend = FALSE, fill = 'skyblue4') +
  theme_minimal() +
  labs(title = "Poparcie dla najpopularniejszych kandydatów na prezydenta RP",
       subtitle = "według sondażu IBRiS dla 'Polityki'",
       x = "Kandydat",
       y = "Poparcie (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Na poprawionym wykresie tytuł i podtytuł informują odbiorcę, jakie dane są przedstawione.
# Ponadto nie ma niepotrzebnej legendy. Dodatkowo dodałam oś Y, na której zaznaczone są wartości
# procentowe, aby wykres był bardziej spójny z innymi wykresami w artykule. Dodane są również 
# opisy obu osi.