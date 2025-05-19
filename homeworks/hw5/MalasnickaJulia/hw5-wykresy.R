library(ggplot2)
library(dplyr)

sport <- c("Piłka nożna", "Koszykówka", "Siatkówka", "Pływanie", "Bieganie")
liczba <- c(24, 22, 21, 18, 15)
data = data.frame(sport, liczba)

ggplot(data, aes(x = sport, y = liczba)) +
  geom_bar(stat = "identity", fill = "#c969a1") +
  labs(title = "Ulubiony sport dzieci w klasach 8", y = "Liczba uczniów", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = "", y = liczba, fill = sport)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(x=1.2, label = sport),
            position = position_stack(vjust = 0.5),
            size = 4) +
  labs(title = "Ulubiony sport dzieci w klasach 8") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_void() +
  theme(legend.position = "none")


poprawnosc <- data.frame(
  wykres = rep(c("Kołowy", "Słupkowy"), each = 2),
  Odpowiedź = rep(c("Poprawna", "Niepoprawna"), times = 2),
  liczba = c(9, 6, 15, 0)
)

ggplot(poprawnosc, aes(x = wykres, y = liczba, fill = Odpowiedź)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Poprawność odpowiedzi dla obu wykresów",
       subtitle = "(na pytanie sprawdzające czytelność wykresów)",
       y = "Liczba odpowiedzi", x = "Poprawność odpowiedzi") +
  theme_minimal()


czytelnosc <- data.frame(
  wykres = c("Kołowy", "Słupkowy"),
  srednia_ocena = c(2.13, 4.87)
)

ggplot(czytelnosc, aes(x = wykres, y = srednia_ocena)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#a39e5f") +
  labs(title = "Srednia ocena czytelności wykresow", y = "Średnia ocena", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

