# źródło - wykres: https://enerad.pl/ure-ceny-energii-w-rozliczeniach-wewnatrz-grup-kapitalowych-spadly-o-20-dane-za-i-kwartal-2025-r/
# źródło - dane: https://www.ure.gov.pl/pl/energia-elektryczna/ceny-wskazniki/11063,Srednia-kwartalna-cena-energii-elektrycznej.html

# Głównym problemem początkowego wykresu jest czerwona linia przebiegająca przez 
# wszystkie słupki. Nie ma żadnej informacji, która pozwoliłaby określić co 
# oznacza ta linia. Można jednak zauważyć, że łączy ona słupki w "takiej samej 
# odległości" od górnych krawędzi słupków. Zatem nie dość, że koduje ona tę samą
# informację co słupki, to jeszcze nie łączy ona samych czubków, co może być 
# mylące przy standardowym założeniu, że cena 0 leży na osi x. Oprócz tego mylące
# mogą być również etykiety na słupkach, które są położone na różnych słupkach
# na różnych wysokościach bez jakiejkolwiek zasady (np. w konkretnej odległości
# pod czerwoną linią albo w ustalonej "części" wysokości słupka). Dodatkowo 
# podpisy z datami na osi x są dość blisko siebie gdzieniegdzie się zlewając oraz
# nie są one wypośrodkowane, co negatywnie wpływa na całą estetykę wykresu. 
# Jeszcze jednym wątpliwym elementem jest użycie niebieskiego koloru słupków na
# niebieskim tle.

library(ggplot2)

# ręczne wprowadzenie danych - jest ich mało, a ze strony nie można ich ładnie pobrać
date <- c("I kwartał 2023", "II kwartał 2023", "III kwartał 2023", "IV kwartał 2023", 
          "I kwartał 2024", "II kwartał 2024", "III kwartał 2024", "IV kwartał 2024", "I kwartał 2025")
price <- c(864.02, 784.02, 748.58, 736.61, 596.59, 570.52, 601.11, 595.05, 475.77)

df <- data.frame(date = date, price = price)

print(df)

ggplot(df, aes(x = factor(date, levels = date), y = price)) +
  geom_bar(stat = "identity", fill = "#c94496") +
  # geom_text(aes(label = price), vjust = -0.5) + # etykiety tekstowe bezpośrednio nad słupkami z konkretną wartością ceny - można je dodać, osobiście preferuję wykres bez nich
  labs(title = "Średnia kwartalna cena energii elektrycznej",
       x = "Czas",
       y = "Cena [zł/MWh]") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     breaks = seq(0, max(price) * 1.1, by = 100))

# Wykres został poprawiony. Usunięta została czerwona linia, która nie wnosiła 
# żadnych informacji, tylko sprawiała, że wykres był zdecydowanie mniej czytelny.
# Dodana została oś y, która pozwala określić poszczególne wartości cen bez
# potrzeby używania wszędzie etykiet. Podpisy z datami na osi x zostały 
# "przekrzywione" - już nie zlewają się ze sobą. Poza tym został zmieniony kolor
# wykresu - różowe słupki na białym tle, co jest prostsze do rozróżnienia niż
# różne odcienie błękitu. Zmieniłam również tytuł wykresu na krótszy i prostszy.
