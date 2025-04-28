library(ggplot2)

### Praca domowa 4 ###
#Autor: Olga Kachniewska
# Źródło wykresu : https://pfr.pl/system/files/image/2025-04/20250409_gospodarka_niemiecka_2.jpg
# [https://pfr.pl/document/2351]
# Tytuł używanego wykresu: Główni Odbiorcy niemieckiego eksportu w dziale samochody i ich części


### Nieprawidłowości w pierwotnym wykresie
#Wykres kołowy dla tak podobnych wartości jest bardzo nieczytealny, dodatkowo są
# one przedstawione w tys euro zamiast procentach (co nie ma sensu na tym typie wykresu)
# nie ma tez roku, z którego pochodzą dane, ale nie byłam w stanie znaleźc tej informacji
#podpisanie warotści przy nazwach jest nieczytelne

### Tworzenie wykresu

# Wektory danych
kraje <- c("USA", "Wielka Brytania", "Chiny", "Francja", "Włochy", "Niderlandy", "Polska", "Belgia", "Hiszpania", "Austria", "Pozostałe")
wartosci <- c(33977576, 20480557, 19523089, 18287138, 13826051, 13276387, 13206691, 9888109, 9800050, 8589851, 101717154)

# Tworzenie ramki danych
dane <- data.frame(Kraj = kraje, Wartosc = wartosci)

# Tworzenie wykresu słupkowego
ggplot(dane, aes(x = reorder(Kraj, -Wartosc), y = Wartosc)) +
  geom_bar(stat = "identity", fill = "blue4") +
  labs(title = "Główni Odbiorcy Niemieckiego Eksportu",
       subtitle = "W dziale samochody i ich części",
       x = "Kraj",
       y = "Wartość (tys. EUR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.grid.major.x = element_blank(),  # Usuwa pionowe linie siatki
        panel.grid.minor.x = element_blank()) +  # Usuwa drobne pionowe linie siatki
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, max(dane$Wartosc), by = 10000000))  # Zagęszcza poziome linie siatki


### Dlaczego poprawiony wykres jest lepszy?
#Wykres słupkowy jest bardziej czytelny, prościej porównać dane i nie ma mylących kolorów
#Kolory nie są mylące (ponieważ kolor nie ma znaczenia i jest jednolity)
#Odpowiednie wartości są na podpisanych osiach a nie porozrzuczane dookoła wykresu
