library(ggplot2)
library(tidyr)

##link do wykresu pierwotnego
# https://dashboard.stat.gov.pl/ i dalej zakładka rynek pracy a w niej
#Stopa bezrobocia rejestrowanego (stan w końcu okresu) w(%)

dane<-read.csv("C:/Users/MSI_GL72/Desktop/dane.csv")
dane
dane_long <- pivot_longer(dane, 
                          cols = -id_rok, 
                          names_to = "Miesiąc", 
                          values_to = "Wartość")

#Ustawianie miesięcy
dane_long$Miesiąc <- factor(dane_long$Miesiąc,
                            levels = c("I", "II", "III", "IV", "V", "VI", 
                                       "VII", "VIII", "IX", "X", "XI", "XII"))

#Tworzenie wykresu
ggplot(dane_long, aes(x = Miesiąc, y = Wartość, color = as.factor(id_rok), group = id_rok)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1", name = "Rok") +
  labs(title = "Stopa bezrobocia rejestrowanego (stan w końcu okresu) w(%)",
       x = "Miesiąc",
       y = "Wartość") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

## ELEMENTY WIZUALIZACJI WYMAGAJĄCE POPRAWY##
#Po pierwsze, poprawy wymagają linie; na pierowtnym
#wykresie, wszystkie z nich są tego samego koloru,
#przez co wykres jes mało czytelny i wszystkie lata zlewają się, co więcej
#skala podziałki jest źle dobrana, przez co nie widać jakie są faktyczne wartości w 
#danych latach i miesiącah. Po drugie należy dodać opisy osi, by osoba oglądająca 
#mogła łatwiej zrozumieć o co chodzi

##
#W moim wykresie porawiłem czytelność lini przez nadanie im różnych kolorów,
#a także zmieniłem skalę, dzięki czemu dane są czytelne. Ponadto uwidoczniłem 
#tytuł oraz nadałem nazwy osiom.
