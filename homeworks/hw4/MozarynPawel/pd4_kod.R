

library(ggplot2)
library(dplyr)
library(patchwork)

##### WYKRES GORA
a <- data.frame(
  id = 1:5,
  procent = c(0.7, 13.9, 39.1, 41.4, 4.9),
  kategoria = c(
    "ochrona i przywrócenie wartości, ochrona wód podziemnych oraz powierzchniowych",
    "pozostała działalność",
    "ochrona powietrza atmosferycznego i klimatu",
    "gospodarka ściekowa i ochrona wód",
    "gospodarka odpadami"
  )
)


a <- a %>%
  arrange(desc(kategoria)) %>%
  mutate(
    ymax = cumsum(procent),
    ymin = c(0, head(ymax, n = -1)),
    label_pos = (ymin + ymax) / 2,
    label = paste0(procent, "%")
  )


wyk1 <- ggplot(a, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2, fill = kategoria)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  theme_void() +
  geom_text(aes(x = 3, y = label_pos, label = label), size = 3, color = "white") +  # percent labels
  annotate("text", x = 1, y = sum(a$procent)/2, label = "767.9 mln zł", size = 3, fontface = "bold") +  # corrected center label
  labs(fill = "Kategoria", title = "Udział procentowy wydatków", subtitle = "wg kierunków inwestowania") +
  theme(legend.position = "right")

##### WYKRES DOL

b <- data.frame(id = 1:7,
                   procent = c(4.4, 3, 54.2, 24.6, 0.1, 10, 3.7),
                   kategoria = c("kredyty i pożyczki",
                                 "inne środki",
                                 "środki własne",
                                 "środki z budżetu centralnego",
                                 "środki z budżetu województwa",
                                 "środki z zagranicy",
                                 "fundusze ekologiczne"))
a <- b 

a <- a %>%
  arrange(desc(kategoria)) %>%
  mutate(
    ymax = cumsum(procent),
    ymin = c(0, head(ymax, n = -1)),
    label_pos = (ymin + ymax) / 2,
    label = paste0(procent, "%")
  )


wyk2 <- ggplot(a, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2, fill = kategoria)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  theme_void() +
  geom_text(aes(x = 3.5, y = label_pos, label = label), size = 3, color = "white") +  # percent labels
  annotate("text", x = 1, y = sum(a$procent)/2, label = "767.9 mln zł", size = 3, fontface = "bold") +  # corrected center label
  labs(fill = "Kategoria", title = "Udział procentowy wydatków", subtitle = "wg kierunków finansowania") +
  theme(legend.position = "right")

wykres_final <- wyk1 / wyk2
