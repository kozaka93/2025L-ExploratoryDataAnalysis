library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(mapdata)
library(sf)
library(grid)
library(patchwork)


setwd("D:/Studia/4 semest/Eksploracja/Projekt")
frekwencja_22_23 <- read.csv("Frekwencja_22_23.csv")
frekwencja_23_24 <- read.csv("Frekwencja_23_24.csv")

frekwencja_22_23 <- frekwencja_22_23 %>% 
  select(Drużyny, średnia.liczba.widzów.we.własnej.hali, Miasto) %>% 
  rename(Druzyna = Drużyny, srednia_liczba_widzow_22_23 = średnia.liczba.widzów.we.własnej.hali)

frekwencja_23_24 <- frekwencja_23_24 %>% 
  select(Drużyny, średnia.liczba.widzów.we.własnej.hali, Miasto) %>% 
  rename(Druzyna = Drużyny, srednia_liczba_widzow_23_24 = średnia.liczba.widzów.we.własnej.hali)

frekwencja_porównanie <- frekwencja_22_23 %>% 
  inner_join(frekwencja_23_24, by=c("Druzyna","Miasto"))


### mapa 

Sys.setenv(SHAPE_RESTORE_SHX = "YES")
granice_wojewodztw <- st_read("A01_Granice_wojewodztw.shp", quiet = TRUE)


granice_wojewodztw <- st_set_crs(granice_wojewodztw, 4326)

mapa_polski <- ggplot(data = granice_wojewodztw) + 
  geom_sf(color = "#c29306", fill = "#ffedab") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#4a13e7", color = NA),
    panel.background = element_rect(fill = "#4a13e7", color = NA)
  )

miasta <- read.csv("miasta_wspolrzedne.csv")
colnames(miasta) <- c("Miasto","long","lat")

druzyny <- frekwencja_porównanie %>% 
  inner_join(miasta, by = "Miasto")

rysuj_wykres <- function(widzowie_22_23, widzowie_23_24, nazwa_druzyny) {
  df <- data.frame(
    sezon = c("22/23", "23/24"),
    liczba_widzow = c(widzowie_22_23, widzowie_23_24)
  )
  
  p <- ggplot(df, aes(x = sezon, y = liczba_widzow, fill = sezon)) +
    geom_col(width = 1) +
    geom_label(aes(label = liczba_widzow, y = liczba_widzow/2), 
              size = 2.5,      
              color = "white",
              fontface = "bold",
              label.size = 0,  
              label.padding = unit(0.15, "lines")) +
    scale_fill_manual(values = c("#f7c548", "#4a13e7")) +
    theme_void() +
    labs(title = paste(strwrap(nazwa_druzyny, width = 15), collapse = "\n")) +
    theme(legend.position = "None", axis.title.y = element_blank(),
          plot.title = element_text(size = 8, hjust = 0.5, vjust = -32, face = "bold"))
   
  return(ggplotGrob(p))  
}

mapa_z_wykresami <- mapa_polski +
  lapply(1:nrow(druzyny), function(i) {
    annotation_custom(
      rysuj_wykres(druzyny$srednia_liczba_widzow_22_23[i], druzyny$srednia_liczba_widzow_23_24[i], druzyny$Druzyna[i]),
      ymin = druzyny$long[i] - 0.5, ymax = druzyny$long[i] + 0.5,
      xmin = druzyny$lat[i] - 0.6, xmax = druzyny$lat[i] + 0.6
    )
  }) +
  labs(title = "Frekwencja na meczach PlusLigi w sezonach 22/23 i 23/24",
       subtitle = "Średnia frekwencja kibiców na wszystkich meczach sezonowych") +
  theme_void() +
  theme(
    legend.position = "right",  
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10)
  )

legenda <- ggplot(data.frame(sezon = c("22/23", "23/24")), 
                  aes(x = sezon, y = 1, fill = sezon)) +
  geom_tile(width = 0.8, height = 0.8) +
  geom_text(aes(label = sezon),size = 3, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("22/23" = "#f7c548", "23/24" = "#4a13e7")) +
  annotate("text", x = 1.5, y = 1.8, label = "Sezon", 
           fontface = "bold", size = 4) +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed() +
  ylim(0.5, 2)

finalna_mapa <- (
  mapa_z_wykresami + legenda +   
    plot_layout(widths = c(6, 1))  
) +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "#4d68f7", color = NA)
  ))

