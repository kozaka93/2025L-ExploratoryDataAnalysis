# hw5.R

library(ggplot2)
library(dplyr)

# ======================================
# 1. DANE DO WIZUALIZACJI
# ======================================

sales_data <- data.frame(
  Produkt = c("A", "B", "C", "D"),
  Sprzedaż = c(40, 40.5, 35, 24.5)
)

# ======================================
# 2. WYKRESY DO EKSPERYMENTU
# ======================================

# Wykres słupkowy (bar chart)
bar_plot <- ggplot(sales_data, aes(x = Produkt, y = Sprzedaż, fill = Produkt)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Sprzedaż produktów (wykres słupkowy)", y = "Sprzedaż", x = "Produkt") +
  theme(legend.position = "none")

# Wykres kołowy (pie chart)
pie_data <- sales_data %>%
  mutate(Procent = Sprzedaż / sum(Sprzedaż) * 100,
         Etykieta = paste0(Produkt, " (", round(Procent), "%)"))

pie_chart <- ggplot(pie_data, aes(x = "", y = Sprzedaż, fill = Produkt)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Sprzedaż produktów (wykres kołowy)") +
  theme_void()
print(bar_plot)
print(pie_chart)
# ======================================
# 3.DANE Z ANKIETY (15 OSÓB)
# ======================================
porownaniekolumnowy <- c("B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
                         "A", "A")


porownaniekolowy  <- c("B","B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
                       "A", "A", "A", "A", "A", "A", "A",
                       "C")


procentkolumnowy <-c(25, 25, 27, 30, 31, 35,25, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
                     36, 39, 40, 40.5, 41, 42)


procentkolowy <- c(15, 18, 22, 22, 23, 23, 24, 24, 25,25, 25, 25, 25, 25, 25,
                   27, 28, 30, 30, 30, 35, 37, 18, 21)


dane <- data.frame( porownaniekolumnowy,porownaniekolowy,procentkolumnowy,procentkolowy)
# ======================================
# 4. Analiza Danych
# ======================================
poprawne <- "B"
poprawna_liczba <- 25

# Obliczenie błędów dla kolumn porownaniekolumnowy i porownaniekolowy
bledy_p1 <- sum(dane$porownaniekolumnowy != poprawne)  
bledy_p3 <- sum(dane$porownaniekolowy != poprawne)     

odchylenie_p2 <- abs(dane$procentkolumnowy - poprawna_liczba)
odchylenie_p4 <- abs(dane$procentkolowy - poprawna_liczba)  

srednie_odchylenie_p2 <- mean(odchylenie_p2)  
srednie_odchylenie_p4 <- mean(odchylenie_p4)  
# ======================================
# 5.WNIOSKI
# ======================================
cat("📊 Pytania wyboru (A/B/C/D):\n")
cat("- Pytanie 1: liczba błędnych odpowiedzi:", bledy_p1, "\n")
cat("- Pytanie 3: liczba błędnych odpowiedzi:", bledy_p3, "\n\n")

cat("📈 Pytania liczbowe:\n")
cat("- Pytanie 2: średnie odchylenie:", round(srednie_odchylenie_p2, 2), "\n")
cat("- Pytanie 4: średnie odchylenie:", round(srednie_odchylenie_p4, 2), "\n\n")

# Porównania
cat("📌 Porównania:\n")

# Pytania 1 vs 3 – wybór
if (bledy_p1 < bledy_p3) {
  cat("- Lepsza trafność dla wykresow kolumnowych (mniej błędów).\n")
} else if (bledy_p1 > bledy_p3) {
  cat("- Lepsza trafność dla wykresow kolowych(mniej błędów).\n")
} else {
  cat("- Taka sama liczba błędów w pytaniach 1 i 3.\n")
}

# Pytania 2 vs 4 – liczby
if (srednie_odchylenie_p2 < srednie_odchylenie_p4) {
  cat("- Dokładniejsze szacowanie dla wykresow kolumnowych (mniejsze odchylenie).\n")
} else if (srednie_odchylenie_p2 > srednie_odchylenie_p4) {
  cat("- Dokładniejsze szacowanie dla wykresow kolowych (mniejsze odchylenie).\n")
} else {
  cat("- Taka sama dokładność w pytaniach 2 i 4.\n")
}

