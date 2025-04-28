library("ggplot2")
library("dplyr")
library("tidyverse")

#przygotowanie danych na wygodny sposob (dane ze strony https://dashboard.stat.gov.pl/)

df<-read.csv("C:/Users/filip/Downloads/Przewozy pasażerów transportem kolejowym (tys.)[bezwzgledna].csv")
df <- df %>%
  pivot_longer(cols = -id_rok, names_to  = "miesiac", values_to = "wartosc") %>%
  rename(rok = id_rok) %>%
  mutate(
    miesiac = factor(miesiac,
                     levels = c("I","II","III","IV","V","VI",
                                "VII","VIII","IX","X","XI","XII"),
                     ordered = TRUE),
    wartosc = as.numeric(wartosc))

#ponizej generuje wykres

ggplot(df, aes(x = miesiac, y = wartosc, fill = factor(rok))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Przewozy pasażerów transportem kolejowym",
    x = "Miesiąc",
    y = "Wartość (mln)",
    fill = "Rok"
  ) +
  theme_dark(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "transparent")
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = function(x) x / 1000)

#pierwotny wykres jest malo estetyczny, kolory sie zlewaja i trzeba sie dwa razy
#upewnic, ze patrzymy na odpowiedni rok. do tego nigdy nie uwazalem linie trendu
#za konieczna, wiec postanowilem, ze sie jej pozbede oraz zrobie porzadek 
#z kolorami. w moim wykresie postawilem na slupki, co uwazam za poprawna decyzje
# i wybralem taka palete kolorow i motyw, ktore przejrzyscie zwizualizuja dane - 
# motyw dark i paleta Set3. kolory sie roznia i dobrze kontrastuja z ciemnym
#tlem. na koniec jeszcze postanowilem, ze troche bez sensu jest pokazywac tak 
#duze wartosci w tysiacach - wiele wiecej sensu ma to w milionach, wiec to tez
#zmienilem. sadze, ze moj wykres jest dobry, a na pewno lepszy od pierwotnego.

