library(ggplot2)
library(dplyr)
library(sf)
library(tidyr)

options(scipen = 12)

granice_wojewodztw <-  st_read("kody/maps/A01_Granice_wojewodztw.shp", quiet = TRUE)

dane <- read.csv("kody/LUDN_1342_CTAB_20250401175332.csv", sep = ";", dec = ",")

dane <- dane %>% 
  mutate(Nazwa = tolower(Nazwa), 
         stosunek_2003 = w.wieku.poprodukcyjnym.ogółem.2003..osoba./ogółem.ogółem.2003..osoba.*100,
         stosunek_2023 = w.wieku.poprodukcyjnym.ogółem.2023..osoba./ogółem.ogółem.2023..osoba.*100,
         roznica = stosunek_2023 - stosunek_2003)

common_limits <- range(c(dane$roznica, dane$roznica), na.rm = TRUE)

granice_wojewodztw %>%
  left_join(dane, by = join_by(JPT_NAZWA_ == Nazwa)) %>% 
  ggplot() +
  geom_sf(aes(fill = roznica), color = "grey") +
  theme_void() +
  scale_fill_fermenter(palette = 1, trans = "log10", direction = 1, limits = common_limits, n.breaks = 8) +
  labs(title = "Wzrost odsetka osób w wieku poprodukcyjnym względem",
       subtitle = "całej ludności województw między rokiem 2003 a 2023",
       fill = "Wzrost") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 16),
        legend.position = "bottom",
        plot.subtitle = element_text(hjust = 0.5))

