library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)

# https://www.rmf24.pl/raporty/raport-wybory-prezydenckie-2025/news-wyborczy-zwrot-akcji-mentzen-traci-poparcie-sondaz-opinia24-,nId,7947412
# Wykres "Na którego kandydata zagłosuje Pan/Pani w tych wyborach?" ma nakładające się etykiety, przez co jest nieczytelny. Poza tym niektóre opcje mają bardzo podobne kolory (zwłaszcza odcienie szarości - Grzegorz Braun, "inny kandydat" i "nie wiem").

partia <- rep(c("Prawo i Sprawiedliwość", "Trzecia Droga tj. Polska 2050 Szymona Hołowni i PSL", "Koalicja Obywatelska", "Konfederacja"), each = 11)
kandydat <- c("Rafał Trzaskowski", "Karol Nawrocki", "Szymon Hołownia", "Sławomir Mentzen", "Magdalena Biejat", "Marek Jakubiak", "Grzegorz Braun", "Adrian Zandberg", "Krzysztof Stanowski", "inny kandydat*", "nie wiem, trudno powiedzieć")
procent <- c(1, 68, 1, 3, 0, 4, 2, 0, 1, 3, 17,
             19, 0, 58, 2, 0, 0, 0, 0, 7, 0, 14,
             87, 1, 2, 2, 2, 0, 0, 2, 2, 2, 4,
             0, 6, 0, 73, 0, 0, 9, 0, 7, 1, 4)
data <- as.tibble(data.frame(partia, kandydat, procent))
data

PIS <- data %>% 
  filter(partia == "Prawo i Sprawiedliwość") %>% 
  mutate(kandydat = fct_relevel(kandydat, c("Rafał Trzaskowski", "Karol Nawrocki", "Szymon Hołownia", "Sławomir Mentzen", "Magdalena Biejat", "Marek Jakubiak", "Grzegorz Braun", "Adrian Zandberg", "Krzysztof Stanowski", "inny kandydat*", "nie wiem, trudno powiedzieć"))) %>% 
  ggplot(aes(x = kandydat, y = procent, label = paste(procent, "%")), show.legend = FALSE) +
  geom_bar(stat = "identity", position="dodge", fill = "darkcyan") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
  labs(
    title = "Prawo i Sprawiedliwość",
    subtitle = "",
  ) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0)) + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3)
Trzecia.Droga <- data %>% 
  filter(partia == "Trzecia Droga tj. Polska 2050 Szymona Hołowni i PSL") %>% 
  mutate(kandydat = fct_relevel(kandydat, c("Rafał Trzaskowski", "Karol Nawrocki", "Szymon Hołownia", "Sławomir Mentzen", "Magdalena Biejat", "Marek Jakubiak", "Grzegorz Braun", "Adrian Zandberg", "Krzysztof Stanowski", "inny kandydat*", "nie wiem, trudno powiedzieć"))) %>% 
  ggplot(aes(x = kandydat, y = procent, label = paste(procent, "%")), show.legend = FALSE) +
  geom_bar(stat = "identity", position="dodge", fill = "darkcyan") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
  labs(
    title = "Trzecia Droga",
    subtitle = "tj. Polska 2050 Szymona Hołowni i PSL",
    y = ""
  ) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0)) + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3)
KO <- data %>% 
  filter(partia == "Koalicja Obywatelska") %>% 
  mutate(kandydat = fct_relevel(kandydat, c("Rafał Trzaskowski", "Karol Nawrocki", "Szymon Hołownia", "Sławomir Mentzen", "Magdalena Biejat", "Marek Jakubiak", "Grzegorz Braun", "Adrian Zandberg", "Krzysztof Stanowski", "inny kandydat*", "nie wiem, trudno powiedzieć"))) %>% 
  ggplot(aes(x = kandydat, y = procent, label = paste(procent, "%")), show.legend = FALSE) +
  geom_bar(stat = "identity", position="dodge", fill = "darkcyan") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
  labs(
    title = "Koalicja Obywatelska",
    subtitle = "",
    y = ""
  ) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0)) + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3)
Konfederacja <- data %>% 
  filter(partia == "Konfederacja") %>% 
  mutate(kandydat = fct_relevel(kandydat, c("Rafał Trzaskowski", "Karol Nawrocki", "Szymon Hołownia", "Sławomir Mentzen", "Magdalena Biejat", "Marek Jakubiak", "Grzegorz Braun", "Adrian Zandberg", "Krzysztof Stanowski", "inny kandydat*", "nie wiem, trudno powiedzieć"))) %>% 
  ggplot(aes(x = kandydat, y = procent, label = paste(procent, "%"))) +
  geom_bar(stat = "identity", position="dodge", fill = "darkcyan") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
  labs(
    title = "Konfederacja",
    subtitle = "",
    y = ""
  ) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0)) + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3)

grid.arrange(PIS, Trzecia.Droga, KO, Konfederacja, nrow = 1,
             bottom = textGrob(
               "* pozostali kandydaci zostali uwzględnieni w kategorii 'inny kandydat'",
               gp = gpar(fontsize = 9),
               hjust = 1,
               x = 1),
              top = textGrob("Na którego kandydata zagłosuje Pan/Pani w tych wyborach? (z podziałem na preferencje partyjne)",
                gp = gpar(fontsize = 15)))

# Etykiety się nie pokrywają. Wyraźnie widać, który % dotyczy którego kandydata.

