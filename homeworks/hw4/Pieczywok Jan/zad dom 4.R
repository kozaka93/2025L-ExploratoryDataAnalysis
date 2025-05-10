library(dplyr)
library(ggplot2)
library(scales) 

# 1) Aktualna kolorystyka oraz zastosowana czcionka wykresu znacząco utrudniają odczyt. 
# Wartości na osi Y są przedstawione w zielonym kolorze, co zmniejsza ich czytelność. 
# Dodatkowo, liczby naniesione bezpośrednio przy każdym słupku są zbędne — przy odpowiednio zaprojektowanym 
# wykresie użytkownik powinien móc samodzielnie i intuicyjnie odczytać wartości.

# 2) Typ wykresu, biorąc pod uwagę tytuł, nie ułatwia interpretacji danych. Brakuje także legendy, 
# przez co nie wiadomo, co przedstawiają poszczególne słupki i wartości obok nich. Może to prowadzić do 
# błędnej interpretacji (np. mylne założenie, że wykres pokazuje temperatury dzienne i nocne, co jest często 
# przedstawiane w prognozach pogody). Wykres powinien "sam mówić za siebie" co jest na nim przedstawione, a nie że
# użytkownik musi się domyślać.

#http://www.pogoda-dlugoterminowa.info.pl/moduly,aktualnosci-1,87.html



najnizsza_temperatura = c(5, 3, 4, 6, 4, 6, 9, 11, 9, 8, 7, 8, 9, 10, 11, 11)
najwyzsza_temperatura = c(16, 15, 14, 20, 13, 14, 15, 20, 21, 18, 17, 18, 19, 21, 21, 20)
daty = seq(as.Date("2025-03-31"), as.Date("2025-04-15"), by = "day")

prognoza_pogody = data.frame(dzien = daty,
           najnizsza_temperatura = najnizsza_temperatura,
           najwyzsza_temperatura = najwyzsza_temperatura)


prognoza_pogody %>% 
  ggplot(aes(x = dzien)) +
  geom_line(aes(y = najwyzsza_temperatura, color = "Najwyższa temperatura"), size = 0.9) +
  geom_point(aes(y = najwyzsza_temperatura, color = "Najwyższa temperatura"), size = 2.2) +
  geom_line(aes(y = najnizsza_temperatura, color = "Najniższa temperatura"), size = 0.9) +
  geom_point(aes(y = najnizsza_temperatura, color = "Najniższa temperatura"), size = 2.2) +
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%m") +
  scale_color_manual(
    values = c(
      "Najniższa temperatura" = "blue",
      "Najwyższa temperatura" = "red"
       )) +
  labs(title = "Prognoza temperatur od 31 marca do 15 kwietnia 2025",
    x = "Data",
    y = "Temperatura (°C)",
    color = "Legenda") +
  theme_light() +  
  theme(
    panel.background = element_rect(fill = "#e6f2ff"),   
    plot.background = element_rect(fill = "#f5f5f5"),
    panel.grid.major = element_line(color = "gray80", size = 0.5),
    panel.grid.minor = element_line(color = "gray90", size = 0.25),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5))


#Wykres jest lepszy od poprzedniego, ponieważ dzięki ujednoliconej czcionce i minimalistycznej kolorystyce jest graficznie łatwiejszy do odczytu.
#Użytkownik już na pierwszy rzut oka rozumie przekaz wykresu i może szybko ocenić, jak będą zmieniać się najwyższe oraz najniższe temperatury w 
#ciągu kolejnych 16 dni dzięki przedstawieniu wykresu w postaci liniowego.






