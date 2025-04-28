# źródło wykresu: https://independenttrader.pl/najwazniejsze-wydarzenia-minionych-tygodni-kwiecien-2025/
# wykres bezpośrednio: https://independenttrader.pl/wp-content/uploads/2025/04/GLD.png
# źródło danych: https://finance.yahoo.com/quote/GLD/history/?guce_referrer=aHR0cHM6Ly9jaGF0Z3B0LmNvbS8&guce_referrer_sig=AQAAADXzNYuil88sXET-_MqSfAUMorVu7axux_-1vmCJ7dmsv3jLj-UKOw_vGh_Gbbizs8N4v4TWo0rPNrwQgtIA1uf-Yk2SWQBUZuRixNvrGr-EuySnwBV3Aol2DUgkyVgQieOntZ53xFyib5KYjM914r58SK8capbfZvesslyteQa4&period1=1587600000&period2=1745366400&frequency=1d

# Piewszym elementem wizualizacji wymagającym poprawy to opis osi. Oś X na wyresie źródlowym jest nieczytelnie podpisana, są tam podane tylko pierwsze litery kolejnych miesięcy i trzeba się domyślać od którego miesiąca wykres się zaczyna. Nie ma podanych jednostek ceny złota w która jest podana na osi Y. Ponadto brakuje opisu całego wykresu, bez wczytywania się w tekst artykułu nie wiemy co przedstawia wykres.

library(dplyr)
library(ggplot2)
library(readxl)

zloto <- read_excel("gld_us_d.xlsx")
zloto <- as.data.frame(zloto)
zloto$Date <- as.Date(zloto$Date)

ggplot(zloto, aes(x = Date, y = Open)) +
  geom_line(color = "navy") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%Y-%m",
               expand = c(0, 0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  labs(title = "Cena złota w ostatnich 5 latach (stan na kwiecień 2025)",
       x = "Data",
       y = "Cena [USD/ETF]") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Nowy wykres jest lepszy od oryginalnego, bo posiada dokładne opisy i widać na nim co przedstawia.