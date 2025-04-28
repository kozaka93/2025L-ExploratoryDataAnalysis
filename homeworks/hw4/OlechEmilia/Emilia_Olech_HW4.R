
library(ggplot2)

# Źródło : https://tvn24.pl/polska/sondaz-partyjny-cbos-kwiecien-2025-konfederacja-na-podium-trzecia-droga-pod-progiem-st8405143?utm_source=chatgpt.com

# 1. Brak pełnej osi Y z wartościami, na wykresie nie ma oznaczonych wartości osi Y (są tylko linie siatki).
# 2. Nadmiar grafik (loga partii), mogą wprowadzać chaos wizualny — lepsza byłaby zwykła legenda lub podpisy osi X.

partie <- c("Koalicja Obywatelska", "Prawo i Sprawiedliwość", "Konfederacja", 
            "Lewica", "Polska 2050", "PSL", "Razem", "Inna Partia", "Trudno powiedzieć")
poparcie <- c(33, 26, 16, 6, 5, 4, 2, 2, 9)
sondaz <- data.frame(Partia = partie, Poparcie = poparcie)

ggplot(sondaz, aes(x = reorder(Partia, -Poparcie), y = Poparcie)) +
  geom_col(fill = "slateblue4") +
  geom_text(aes(label = paste0(Poparcie, "%")), vjust = -0.5, size = 4, family = "sans") +
  labs(
    title = "Poparcie dla partii politycznych",
    subtitle = "Źródło: CBOS, kwiecień 2025",
    x = "Partie polityczne",
    y = "Poparcie (%)"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Mój wykres jest bardziej czytelny dzięki poprawnemu podpisaniu osi, wyśrodkowaniu tytułu, 
# usunięciu zbędnych elementów graficznych oraz czytelnemu przedstawieniu wartości procentowych nad słupkami.


