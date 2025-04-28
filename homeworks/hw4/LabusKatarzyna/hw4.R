library(ggplot2)
library(dplyr)
library(tidyr)

#źródło wykresu:
#https://www.wnp.pl/polityka-i-sondaze/sondaze/sondaz-cbos-holownia-liderem-rankingu-zaufania-kaczynski-nieufnosci,1610.html
# dane wykorzystane do anaizy zostały odczytene z artykułu i wykresu z naszego źródła

#Wykres dotyczy zaufania Polaków do polityków, badanie przeprowadzono
#w dniach od 3 do 13 kwietnia, wykres opublikowany 25 kwietnia 2025

#Co jest źle?
#1) Brak tytułu i legenda, która nie ma sensu, bo mówi o 5 odpowiedziach
#do wyboru w przeprowadzanej ankiecie, podczs gdy na wykesie mamy tylko
#dwie skrajne odpowiedzi "Zdecydowanie ufa", "Zdecydowanie nie ufa"

#2) Brak podpisów osi, zdjęcia polityków zamiast nazwisk co utrudinia
#identyfikację, podobne kolory utrudniające czytanie i procenty 
#jeden pod drugim pod słupkami przez co ciężej określić, który 
#wynik dotyczy której kategorii

dane <- data.frame("nazwiska" = c("Szymon \nHołownia", "Rafał \nTrzaskowski", 
                                 "Andrzej \nDuda", "Radosław \nSikorski", 
                                 "Władysław \nKosiniak-Kamysz", "Donald \nTusk", 
                                 "Krzysztof \nBosak", "Adam \nBodnar", 
                                 "Mateusz \nMorawiecki", "Magdalena \nBiejat", 
                                 "Mariusz \nBłaszczak", "Jarosław \nKaczyński"), 
                   "procent_ufa" = c(45, 44, 43, 41, 40, 39, 34, 33, 29, 28, 28, 25), 
                    "procent_nie_ufa" = c(35, 40, 42, 31, 28, 45, 33, 27, 55, 32, 42, 60))

View(dane)


dane_long <- dane %>% pivot_longer(cols = c(procent_ufa, procent_nie_ufa), 
                      names_to = "kategoria", values_to = "procent") 

kolejnosc <- dane_long %>%
  filter(kategoria == "procent_ufa") %>%
  arrange(desc(procent)) %>%
  pull(nazwiska)

dane_long$nazwiska <- factor(dane_long$nazwiska, levels = kolejnosc)

dane_long$kategoria <- factor(dane_long$kategoria, levels = c("procent_ufa", "procent_nie_ufa"))


dane_long %>% 
  ggplot(aes(nazwiska, procent, fill = kategoria)) + 
  geom_col(position = position_dodge()) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 8)) +
  labs(title = "Poziom zaufania Polaków do polityków",
       subtitle = "kwiecień 2025",
       x = NULL, y = "Procent", fill = "Kategoria") +
  scale_fill_manual(name = "Kategoria", 
                    values = c("procent_ufa" = "#1f77b4", "procent_nie_ufa" = "#ff7f0e"),
                     labels = c("procent_nie_ufa" = "Zdecydowanie nie ufa", "procent_ufa" = "Zdecydowanie ufa")) +
  geom_text(aes(label = paste0(procent, "%")), 
            position = position_dodge(width = 0.9), vjust = 1.5,  size = 3) +
  scale_y_continuous(expand = c(0,0)) 


#Mój wykres jest lepszy ponieważ został dodany jasny tytuł i podtytuł
#czyli od razu wiadomo czego dotyczy wykres. Poprawiłam różnież legendę,
#usunęłam zbędne kategorie, które nie zostały uwzglęnione na wykresie.
#Procent zaufania bądź jego braku jest na słupku a nie pod nim 
#co ułatwia szybkie odczytanie wyników badania. Zmieniłam kolory 
#na bardziej kontrastowe niż czerwony i pomorańczowy, które mogą się
#zlewać. Dodatkowo niebieski kojarzy się z pozytywną reakcją a pomorańczowy
#z negatywną. Osie zostały podpisane, a zdjęcia zmienione na podpisy
#imieniem i nazwiskiem