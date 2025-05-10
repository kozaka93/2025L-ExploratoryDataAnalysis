# oryginalny wykres: https://x.com/DawidStats/status/1914994417384989163
# wykres umieszczam w oddzielnym pliku 
#
# problemy:
#   
# 1. wielokolorowe słupki są zbytecznym wymiarem informacji i utrudniają
# odczytanie poparcia
# 
# 2. ciemne tło zmniejsza czytelność wykresu
# 
# 3. pionowe słupki wymuszają nazwiska o małej czcionce,
# co zmniejsza czytelność
# 
# 4.dużo miejsca zmarnowane przez panel po lewej stronie wykresu
# 
# Utworzyłem prostszy wykres, który przekazuje te same informacje, na którym
# lepiej są widoczne nazwiska kandydatów dzięki większej czcionce i kontrastowi
# czarno na białym oraz łatwiej przeczytać poparcie i zmianę poparcia dzięki 
# słupkom i napisom o tym samym kolorze i większym kontraście między napisami 
# a tłem.

library(ggplot2)
df <- data.frame(
  kandydat = c("Rafał Trzaskowski", "Karol Nawrocki", "Sławomir Mentzen", 
               "Szymon Hołownia", "Magdalena Biejat", "Adrian Zandberg",
               "Krzysztof Stanowski", "Grzegorz Braun", "Marek Jakubiak",
               "Joanna Senyszyn", "Artur Bartoszewicz", "Maciej Maciak", 
               "Marek Woch"),
  poparcie_procent = c(34.2, 28.0, 14.5, 8.2, 4.8, 3.7, 2.3, 1.5, 1.0, 1.0, 0.5, 0.2, 0.0),
  zmiana_poparcia = c("-4,7", "+3,7", "-1,7", "+0,7", "-1,0", 
                      "+1,1", "+0,8", "-0,4", "-0,3", 
                      "nowy", "nowy", "nowy", "nowy")
  
)

etykieta_poparcie = paste0(df$poparcie_procent, "%")


ggplot(data = df, aes(x = reorder(kandydat, poparcie_procent), y = poparcie_procent)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(x = "Kandydat", y = "Poparcie (%)", title = "Sondaż IBRIS z dni 16-17.04.2025 wśród zdecydowanych wyborców") +
  geom_text(
    aes(label = df$zmiana_poparcia,
        hjust = -0.7,
        color = "black",
        size = 1)
  ) +
geom_text(
  aes(label = poparcie_procent, x = kandydat, y = 0),
  size = 3.5,
  hjust = -0.2
) +
  scale_y_continuous(limits = c(0, 40)) +
  theme_minimal() +
  theme(legend.position = "none") 

 
