install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("scales")
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

trends <- read.csv('googletrends1.csv', header = FALSE)

colnames(trends) <- c('tydzień', 'horrory', 'filmy_świąteczne', 'filmy_romantyczne', 'filmy_familijne')
trends <- trends %>%  filter(!row_number() %in% c(1, 2)) %>%
  select(tydzień, horrory, filmy_świąteczne, filmy_romantyczne)

trends$horrory <- as.numeric(trends$horrory)
trends$filmy_świąteczne <- as.numeric(trends$filmy_świąteczne)
trends$filmy_romantyczne <- as.numeric(trends$filmy_romantyczne)
#trends$filmy_familijne <- as.numeric(trends$filmy_familijne)
trends$tydzień <- as.Date(trends$tydzień)

trends2 <- trends %>% pivot_longer(cols = c(horrory, filmy_świąteczne, filmy_romantyczne),
                                   names_to = "Gatunek", values_to = "wartość")
trends2$tydzień[trends2$tydzień == as.Date("2023-12-31")] <- as.Date("2024-01-01")

png("trends_transparent2.png", width = 800, height = 600, bg = "transparent")

ggplot(trends2, aes(x=tydzień, y=wartość, fill=Gatunek)) + 
  geom_area(alpha=0.4, position="identity") +
  geom_line(aes(color=Gatunek), size=1) +
  scale_fill_manual(values = c(
    "horrory" = "#c79f59",
    "filmy_świąteczne" = "#617983",
    "filmy_romantyczne" = "#8a2b0d"),
    labels = c(
      "horrory" = "Horrory",
      "filmy_świąteczne" = "Filmy Świąteczne",
      "filmy_romantyczne" = "Filmy Romantyczne"
    )) +
  scale_color_manual(values = c(
    "horrory" = "#c79f59",
    "filmy_świąteczne" = "#617983",
    "filmy_romantyczne" = "#8a2b0d"),
    labels = c(
      'horrory' = 'Horrory',
      "filmy_świąteczne" = "Filmy Świąteczne",
      "filmy_romantyczne" = "Filmy Romantyczne")) +
  scale_x_date(
    breaks = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "1 month"),
    date_labels = "%b %Y",
    limits = c(as.Date("2024-01-01"), as.Date("2024-12-31")),
    expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1, size = 19),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_line(color = "gray95", size = 0.3),
    axis.line = element_line(size = 0.7, color="#3a1302")) +
  labs(x='Czas', y='Popularność',  title='')

dev.off()

