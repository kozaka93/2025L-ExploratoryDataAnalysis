library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(readr)
df <- read_csv("C:/RStudio/2025L-ExploratoryDataAnalysis/homeworks/KO_1919-09-06_2025-04-06.csv")

#atakuje https://www.google.com/finance/quote/KO:NYSE?window=MAX uważam, że w pionie nic nie widać i trzeba 
#zgadywać/traktować ekran linijką, pominięta jest kwestia jakiegokolwiek trend no i jak na taki przedział 
# czasowy wykres jest mało "gładki" co utrudnia interpretacje



#roczna
df_yearly <- df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(close = mean(close, na.rm = TRUE))

#Combo chart: słupki + linia trendu
ggplot(df_yearly, aes(x = year, y = close)) +
  geom_col(fill = "steelblue", width = 0.7, alpha = 0.8) +
  geom_line(aes(group = 1), color = "darkred", size = 1.2) +
  scale_x_continuous(breaks = seq(min(df_yearly$year), max(df_yearly$year), by = 5)) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title    = "Średnia roczna cena zamknięcia akcji Coca-Cola (1962–2025)",
    subtitle = "Combo chart: kolumny + linia trendu, co 5 lat na osi X",
    x        = "Rok",
    y        = "Cena zamknięcia (USD)",
    caption  = "Źródło: dane giełdowe Coca-Cola (KO)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

#wydaje mi się, że widać wszystko lepiej na tej wizualizacji co jest dziwne bo chyba kto jak kto ale google
# to mógłby robić troche dokładniej. w moim wykresie nie ma zdezorientowania na osi pionowej widać trend
# i jest bardziej czytelny na jeszcze większym przedziale czasowym
