#Oryginalny wykres prezentuje informacje na temat wskaźnika wypadkowości za pomocą 
#koloru i liczby umieszczonej na mapie. W takim przypadku jest to niepotrzebne i można
#zastosować tylko jeden ze sposobów reprezentacji danych. Dobrana skala barw nie jest najlepsza,
#ponieważ każdy kolor zawiera dosyć duży przedział wartości, przez co nie da się odróżnić
#"na pierwszy rzut oka" skrajnych wartości. Przykładowo 6.17 i 6.53 są przedstawione w tej 
#samej barwie, a są to niezbyt bliskie liczby patrząc na to, że rozpatrujemy wypadki na 
#1000 pracujących.
#Link do źródła:
#https://stat.gov.pl/obszary-tematyczne/rynek-pracy/warunki-pracy-wypadki-przy-pracy/wypadki-przy-pracy-w-2024-r-dane-wstepne,3,58.html


library(sf)
library(dplyr)
library(ggplot2)

wojewodztwa <- st_read("mapy/A01_Granice_wojewodztw.shp", quiet = TRUE)

wypadki <- data.frame(
  nazwa = tolower(c("dolnośląskie", "kujawsko-pomorskie", "lubelskie", "lubuskie", 
                    "łódzkie", "małopolskie", "mazowieckie", "opolskie", 
                    "podkarpackie", "podlaskie", "pomorskie", "śląskie", 
                    "świętokrzyskie", "warmińsko-mazurskie", "wielkopolskie", "zachodniopomorskie")),
  wypadek_wsk = c(5.44, 5.61, 4.98, 4.88, 5.04, 3.62, 3.22, 6.17,
                    4.49, 5.26, 4.65, 6.53, 5.20, 5.61, 5.09, 5.88)
)

mapa_wypadki <- wojewodztwa %>%
  left_join(wypadki, by = join_by(JPT_NAZWA_ == nazwa))

ggplot(mapa_wypadki) +
  geom_sf(aes(fill = wypadek_wsk), color = "white", size = 0.2) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    guide = guide_colorbar(
      barwidth = 0.7,
      barheight = 10
    )
  ) +
  labs(fill = "Wskaźnik wypadkowości",
       title = "Poszkodowani w wypadkach przy pracy na 1000 pracujących w 2024 r.",
       subtitle = "bez gospodarstw indywidualnych w rolnictwie")+
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
        legend.position = "right",
        legend.title = element_text(hjust = 0.5,size = 12),
        legend.title.position = "top",
        ) 

#Utworzony przeze mnie wykres zawiera lepszą skalę barw, legenda jest sformatowana w ładniejszy 
#sposób. Mapa stała się bardziej przejrzysta i brak na niej natłoku informacji.

