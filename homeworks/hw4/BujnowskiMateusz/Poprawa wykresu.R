# Źródło: https://stat.gov.pl/obszary-tematyczne/rynek-pracy/pracujacy-zatrudnieni-wynagrodzenia-koszty-pracy/przecietne-zatrudnienie-i-wynagrodzenie-w-sektorze-przedsiebiorstw-w-marcu-2025-r-,3,160.html
# 
# Wykres (pierwszy w tym dokumencie) przedstawia przeciętne zatrudnienie w sektorze przedsiębiorstw.
# Znajdują się tam dwie osie, lewa prezentuje przeciętne zatrudnienie w tysiącach etatów, natomiast
# prawa dynamikę zatrudnienia, w porównaniu do analogicznego miesiąca poprzedniego roku.
# Wykres wymaga poprawy, ponieważ wartości dla lewej osi w postaci wykresu słupkowego są mało wyraźne w miejscu, gdzie nachodzą
# na siebie wartości dla obu osi. Ponadto brakuje informacji przy prawej osi w jakiej wartości jest przedstawiana.
# Podpisy osi poziomej wyglądają jakby nie należały do wykresu, lecz stanowiły odrębną tabelkę, przy czym jest zostawiony 
# duży odstęp od etykiet osi poziomej, co zwiększa ten efekt.


library(readxl)
library(zoo)
library(dplyr)
library(ggplot2)

dane <- read_excel("przecietne_zatrudnienie_i_wynagrodzenie_w_sektorze_przedsiebiorstw_w_marcu_2025_r._dane.xlsx", sheet = "Wykres 1") 
dane <- dane[-1, ] 
colnames(dane) <- as.character(dane[1, ])
dane <- dane[-1, ]
dane[[1]] <- na.locf(dane[[1]])
dane <- dane[, -c(5, 6)]
colnames(dane)[2] <- "Miesiąc"
colnames(dane)[1] <- "Rok"
dane <- dane %>%
  mutate(`Data` = paste(dane[[2]], dane[[1]], sep = "-")) %>%
  select(c(5, 3, 4))
dane <- dane %>%
  mutate(`Wartosc 1` = as.double(`W tys. etatów`),
         `Wartosc 2` = as.double(`Analogiczny okres roku\r\npoprzedniego=100`)) %>% 
  select(`Data`, `Wartosc 1`, `Wartosc 2`)
dane <- dane %>%
  mutate(Data = factor(Data, levels = unique(Data)))

wykres <- ggplot(dane, aes(x = Data)) +
  geom_point(aes(y = `Wartosc 1`, color = "Przeciętne zatrudnienie (w tys. etatów)"), size = 3) + 
  geom_line( aes(y = `Wartosc 1`, color = "Przeciętne zatrudnienie (w tys. etatów)", group = 1),
             linewidth = 1.5) +
  geom_point(aes(y = (`Wartosc 2` - 96) * 850, color = "Dynamika, analogiczny miesiąc poprzedniego roku = 100"), size = 3) +
  geom_line( aes(y = (`Wartosc 2` - 96) * 850, color = "Dynamika, analogiczny miesiąc poprzedniego roku = 100", group = 1),
             linewidth = 1.5) +
  scale_y_continuous(
    expand   = c(0, 0),
    name     = "Przeciętne zatrudnienie (w tys. etatów)", 
    limits   = c(0, 6800),
    breaks   = seq(0, 6800, 400),
    sec.axis = sec_axis(
      ~ . * (104 - 96) / 6800 + 96,
      name   = "Dynamika, analogiczny miesiąc poprzedniego roku = 100",
      breaks = seq(96, 104, by = 1)
    ))+
  scale_color_manual(
    name   = "Legenda",
    values = c("Przeciętne zatrudnienie (w tys. etatów)" = "blue", "Dynamika, analogiczny miesiąc poprzedniego roku = 100" = "red")) +
  scale_x_discrete(
    expand = c(0.02, 0),
    breaks = levels(dane$Data)[seq(1, length(levels(dane$Data)), by = 2)]) +
  labs(
    title = "Przeciętne zatrudnienie w sektorze przedsiębiorstw",
    x     = "Data (miesiąc-rok)") +
  guides(
    fill  = guide_legend(nrow = 3, byrow = TRUE),
    color = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_line(color = "gray90", size = 0.5),
    panel.grid.major.y = element_line(color = "gray90", size = 0.5)) +
  theme(
    plot.margin = margin(t = 15, r = 10, b = -20, l = 10),
    plot.title         = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x       = element_text(margin = margin(t = 5), size = 14, color = 'black'),
    axis.title.y       = element_text(margin = margin(r = 14), size = 13, color = 'black'),
    axis.title.y.right = element_text(margin = margin(l = 14), size = 13, color = 'black'),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 12, color = 'black'),
    axis.text.y        = element_text(size = 12, color = 'black'),
    axis.text.y.right  = element_text(size = 12, color = 'black'),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.title       = element_text(size = 14, face = "bold", margin = margin(b = 25)),  # tytuł legendy
    legend.text        = element_text(size = 13))+ 
  geom_hline(
  yintercept = 3400, 
  color      = "gray30", 
  linewidth  = 0.5)
wykres

ggsave("Przeciętne zatrudnienie w sektorze przedsiębiorstw.jpg",
       plot   = wykres,
       device = "jpeg",
       width  = 12,    # szerokość w calach
       height = 6,     # wysokość w calach
       dpi    = 300)

# Podsumowanie.
# Na przygotowanym wykresie bardzo wyraźnie widać obydwie linie, nie zlewają się tak jak poprzednio
# oraz nie przecinają się, co sprawia, że wykres jest bardziej czytelny.
# Każda z osi jest podpisana, etykiety osi pizomej są lepiej widoczne.
# Wykres teraz jest bardziej w zamkniętej strukturze, nie ma tak dużego pustego pola pod liniami
# dla wykresó liniowych, co sprawia, że wykres jest bardziej zwarty. Zostały również dodane
# linie siatki, co ułatwia odczytanie wartości. Wykres liniowy dla prawej osi y został podniesiony, 
# żeby zwiększyćczytelność, nie zwiększać liczby arguemtów na osiach, co spowodowałoby zmniejszenie
# widoczności zmian zachodzących na wykresie. Żeby lepiej było widać miejsce przejścia przez 
# 100, cyzli tak samo jak w poprzednim roku została dodana ciemniejsza linia siatki na tym poziomie.




