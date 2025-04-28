library("dplyr")
library("ggplot2")
library(RColorBrewer)

# Praca domowa 4
# Moim zadaniem było znalezienie wadliwego wykresu opublikownaego w mediach 
# społeccznościowych oraz jego poprawienie.Mój wykres znalazłam na fecebooku i zawiera on 
# parę wad:

# 1)
# Poparcia wszystkich kandydatów z wykresu nie sumują się do 100 %. Powinna być inna kategoria
# gdzie jest sklasyfikowane, że na przykład 1% - reszta kandydatów albo ten 1% to są osoby nie
# zdecydowane

# 2)
# Niepotrzebne oraz niepoprawne użycie kolorów

# Różne kolory mają nam prezentowac kolejne dane, jednak tutaj nic nie znaczą. 
# Dodatkowo jeżeli już upieramy się na rcnej kolorystyce to powinny być one w podobnej
# gamie kolorystycznej aby żadna dana nie wyrożniała się bardziej od innych. Tutaj został
# użyty kolor czerwony na drugiego kandydata, który jednak bardziej przykuwa uwagę niż inne
# kolory. Jeżeli już chcielibyśmy dodać nową danę, to możemy pokolorować na 2 różne kolory tych
# kandydatów którym zwiększyło się lub zmniejszyło się poparcie, jeżeli chcielibyśmy żeby wykres
# był bardziej interesujący. 


# Chcę więc zmienić te 2 rzeczy w wykresie. Najpierw zrobię tabelkę z danymi z wykresu.

wyniki <- data.frame(
  Kandydat = c("Rafał Trzaskowski", "Karol Nawrocki", "Sławomir Mentzen",
               "Szymon Hołownia", "Magdalena Biejat", "Adrian Zandberg",
               "Grzegorz Braun", "Krzysztof Stanowski", "Marek Jakubiak"),
  Poparcie = c(34.11, 30.76, 14.32, 5.43, 3.72, 3.37, 3.25, 2.34, 1.34),
  Zmiana = c(-1.4, 1.2, -6.1, 2.0, 1.9, 1.6, -1.4, 1.1, 0.5)
)

sum(wyniki$Poparcie)

#> sum(wyniki$Poparcie)
#[1] 98.64


# Dodam więc nową kolumne, która będzie symbolizowała innych kandydatów.

wyniki <- data.frame(
  Kandydat = c("Rafał Trzaskowski", "Karol Nawrocki", "Sławomir Mentzen",
               "Szymon Hołownia", "Magdalena Biejat", "Adrian Zandberg",
               "Grzegorz Braun", "Krzysztof Stanowski", "Marek Jakubiak","Inni kandydaci"),
  Poparcie = c(34.11, 30.76, 14.32, 5.43, 3.72, 3.37, 3.25, 2.34, 1.34,1.36),
  Zmiana = c(- 1.4, 1.2, - 6.1, 2.0, 1.9, 1.6, -1.4, 1.1, 0.5, 0)
)

wyniki %>% 
    mutate(
      Kandydat = factor(Kandydat, levels = c(setdiff(wyniki$Kandydat, "Inni kandydaci"), "Inni kandydaci")),
      spada_czy_nie = ifelse(Zmiana < 0, "spadek", "wzrost"),
      y_text = Poparcie + 2,
      zmiana_label = ifelse(Zmiana < 0, paste0("↓", abs(round(Zmiana, 1))), paste0("↑", round(Zmiana, 1)))
    ) %>% 
    ggplot(aes(x = Kandydat, y = Poparcie, fill = spada_czy_nie)) +
    geom_col() +
    geom_text(aes(y = y_text, label = zmiana_label, color = spada_czy_nie), size = 4, fontface = "bold") +
    scale_fill_manual(values = c("spadek" = "#3182BD", "wzrost" = "#9ECAE1")) +
    scale_color_manual(values = c("spadek" = "#DE2D26", "wzrost" = "#31A354" )) +
    guides(color = "none") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(
      title = "Sondaż wyborczy z 23-25 kwietnia 2025 roku w porównaniu z badaniem z 9-14 kwietnia 2025 roku",
      subtitle = "Nad słupkami ukazany jest wzrost lub spadek poparcia",
      y = "Poparcie [%]"
    ) +
    ylim(0, 40)  


