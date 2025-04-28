library(ggplot2)
library(tidyr)
library(dplyr)


# Odtwarzam dane

dane <- data.frame(
  Kwartał = factor(c(
    "2021-Q1", "2021-Q2", "2021-Q3", "2021-Q4",
    "2022-Q1", "2022-Q2", "2022-Q3", "2022-Q4",
    "2023-Q1", "2023-Q2", "2023-Q3", "2023-Q4",
    "2024-Q1", "2024-Q2", "2024-Q3", "2024-Q4"
  ), levels = c(
    "2021-Q1", "2021-Q2", "2021-Q3", "2021-Q4",
    "2022-Q1", "2022-Q2", "2022-Q3", "2022-Q4",
    "2023-Q1", "2023-Q2", "2023-Q3", "2023-Q4",
    "2024-Q1", "2024-Q2", "2024-Q3", "2024-Q4"
  )),
  "Ogółem" = c(5,10,11,9,7,5,8,6,10,12,11,9,-5,-8,-6,-3),
  "Prywatny krajowy" = c(8,12,11,10,8,6,8,7,10,15,18,13,-10,-12,-8,-5),
  "Publiczny" = c(-10,15,5,0,0,-5,0,-2,5,10,15,10,-20,-15,-10,-8),
  "Prywatny zagraniczny" = c(20,10,15,8,5,2,3,1,3,5,6,4,-8,-10,-8,-6)
)

dane_long <- pivot_longer(dane, cols = -Kwartał, names_to = "Sektor", values_to = "Wartosc")

# Przygotowanie etykiet na osi X - tylko pierwsze kwartały
etykiety_x <- dane_long %>%
  filter(grepl("Q1", Kwartał)) %>%
  pull(Kwartał) %>%
  gsub("-Q1", "", .)


# rysowanie wykresu

ggplot(dane_long, aes(x = Kwartał, y = Wartosc, color = Sektor, group = Sektor)) +
  geom_line(size = 3,alpha=0.5) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_x_discrete(
    breaks = dane_long$Kwartał[grepl("Q1", dane_long$Kwartał)],
    labels = etykiety_x
  ) +
  scale_y_continuous(breaks = seq(-25, 25, by = 5)) +
  labs(
    title = "Inwestycje przedsiębiorstw 50+",
    subtitle = "Źródło: PONT Info, Credit Agricole",
    x = "Rok",
    y = "Dynamika inwestycji (% r/r, w ujęciu realnym)",
    color = "Sektor"
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = c(0.89, 0.85),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_text(size=10),
    legend.text = element_text(size=8),
plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),
    axis.title.x = element_text(margin = margin(t = 5)),
    axis.title.y = element_text(margin = margin(r = 5)) 
  )

