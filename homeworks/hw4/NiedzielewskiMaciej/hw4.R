# Źródło - https://niezalezna.pl/polityka/najnowszy-sondaz-wyborczy-dla-republiki-duzy-spadek-poparcia-dla-trzaskowskiego-i-mentzena/541887

library(ggplot2)
library(tidyr)
library(dplyr)
# Przepisuję dane do wektorów 
kandydaci <- c("Biejat", "Zandberg", "Trzaskowski", "Hołownia", "Stanowski", "Nawrocki", "Jakubiak", "Mentzen", "Braun", "inni")
procenty <- c(3, 5, 33, 8, 3, 25, 1, 15, 5, 2)

# Tworzę z nich ramkę danych
dane_sondaz <- data.frame(Kandydat = kandydaci, Procent = procenty)

# Są cztery duże problemy z tym wykresem:
# 1. Wyniki kandydatów nie są posegregowane (np. malejąco).
# 2. Wynik każdego kandydata jest niepotrzebnie pokolorowany osobnym kolorem.
# 3. W przypadku kandydatów z bardzo niskim poparciem dokłade procenty są ucięte przy osi poziomej.
# 4. Nie ma nazw osi.

# Sortuję wyniki malejąco 
dane_sondaz <- dane_sondaz %>%
  arrange(desc(Procent))

# Tworzę poprawiony wykres słupkowy
wykres <- ggplot(dane_sondaz, aes(x = reorder(Kandydat, -Procent), y = Procent)) +
  geom_bar(stat = "identity", fill = "steelblue") + # Używam jednego koloru
  geom_text(aes(label = Procent), vjust = -0.3, size = 4) + # Dodaję etykiety z wartościami
  labs(title = "Sondaż prezydencki",
       subtitle = "Badanie Pollster dla Republiki | 17-18.04.2025",
       x = "Kandydat",
       y = "Procent poparcia") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"), 
    plot.title = element_text(face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(hjust = 0.5) 
  )

ggsave('/Users/maciej/Desktop/Eksplo/NiedzielewskiMaciej/poprawiony_wykres.png', wykres)

# Dlaczego mój wykres jest lepszy:
# 1. Procent poparcia kandydatów jest posegregowany malejąco co znacznie zwiększa czytelność wykresu
# 2. Etykiety z dokładnymi % są wszędzie dobrze widoczne
# 3. Osie są elegancko podpisane
# 4. Zamiast osobnych kolorów dla każdego kandydata, co jest zbędne dla wykresu kolumnowego bez 
# dodatkowych cech, zastosowałem jeden. To również poprawia czytelność wykresu.
