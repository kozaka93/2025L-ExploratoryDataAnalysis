library(dplyr)
library(ggplot2)
# Poprawiona wizualizacja wyników sondażu prezydenckiego 2025

# Problem 1 
#Brak etykiet z imionami i nazwiskami – widzimy tylko portrety, co utrudnia identyfikację.
# Szczegolnie gdy bedziemy chcieli uwzglednic wszystkich kandydatów
# Wtedy najslabsze ogniwa tego prezydenckiego wyscigu bedą wrecz nie do rozpoznania
#zachowując proporcje słupków


# Problem 2 
#Brak skali i wartości liczbowych – nie wiemy dokładnie, 
#ile procent zebrał każdy z kandydatów.
#Możemy ustalić tylko kto jest na ktorym miejscu
#ale zeby dostrzec róznice w punktach procentowych jest bardzo ciezkie
#wielkosci portretow slabo obrazuja tą róznice




#https://wszystkoconajwazniejsze.pl/pepites/najnowsze-sondaze-wyborcze-prezydenckie-wybory-2025/
# Po kolejnosci kandytatow mozna sie domyslec ze dane sa z:
#IBRIS dla tygodnika „Polityka”
#16-17 kwietnia 2025 r. 
# Wiec z nich bedziemy korzystać
)

df <- data.frame(
  kandydat = c("Rafał Trzaskowski", "Karol Nawrocki", "Sławomir Mentzen", 
               "Szymon Hołownia", "Magdalena Biejat", "Adrian Zandberg", 
               "Krzysztof Stanowski", "Grzegorz Braun", "Marek Jakubiak", 
               "Joanna Senyszyn", "Artur Bartoszewicz", "Maciej Maciak"),
  procent   = c(30.4, 24.9, 12.9, 7.3, 4.3, 3.3, 2.0, 1.3, 0.9, 0.9, 0.4, 0.2)
) %>%
  arrange(procent) %>%
  mutate(kandydat = factor(kandydat, levels = kandydat))

y_max <- ceiling(max(df$procent) * 1.1)


ggplot(df, aes(x = kandydat, y = procent)) +
  geom_col(fill = "#4C72B0", width = 0.7) + 
  geom_text(aes(label = paste0(procent, "%")),
            hjust = -0.1, size = 4, fontface = 'bold') +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.02), limits = c(0, y_max)) +
  coord_flip() +
  labs(
    title = "Wyniki sondażu kandydatów na Prezydenta RP (kwiecień 2025)",
    x = "Kandydat", y = "Procent głosów",
    caption = "Źródło: IBRIS dla tygodnika „Polityka” 16-17 kwietnia 2025 r."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    aspect.ratio       = 1,
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, margin = margin(b = 10)),
    plot.caption     = element_text(size = 8, hjust = 1, margin = margin(t = 10)),
    axis.text.y      = element_text(size = 11, color = "black"),
    axis.title.y     = element_blank(),
    axis.title.x     = element_text(margin = margin(t = 10)),
    panel.grid.major.y = element_blank()
  )


#Czytelność: imiona i nazwiska wyświetlone bezpośrednio mozemy poznać kazdego kandytata bez pominiecia ostatnich.
#Precyzja:  liczby procentowe doklejone do słupków, czytelnikowi na dokladna analize
#Estetyka: Wykres jest prosty ale czytelny, bez zbednych dodatków
# Uwazam ze pod tymi 3 wzgledami moj wykres jest lepszy
