library(ggplot2)
library(dplyr)

#Dane ze strony https://www.jsw.pl/fileadmin/user_files_ri/raporty-okresowe/pl/2024/1y/jednostkowy/SZZD_GK_i_JSW_2024_07042025_OST.xhtml
# SPRAWOZDANIE ZARZĄDU Z DZIAŁALNOŚCI JASTRZĘBSKIEJ SPÓŁKI WĘGLOWEJ S.A.  ORAZ GRUPY KAPITAŁOWEJ JASTRZĘBSKIEJ SPÓŁKI WĘGLOWEJ S.A. ZA ROK OBROTOWY ZAKOŃCZONY 31 GRUDNIA 2024 ROKU

# Co zostało poprawione:
# 1) Dodano oś Y, któej skala została ustawiona od zera, 
# co eliminuje złudzenie przesadnych różnic między wartościami.
# 2) Zmieniono kolory na bardziej wyróżniające się i łatwiejsze do rozróżnienia.
# 3) Usunięto powtarzające się dane, które pojawiały się jako tytuł i pod słupkami.


# Dane
dane <- data.frame(
  Produkt = c("HRC", "HRC", "Pręty", "Pręty"),
  Rok = c("2023", "2024", "2023", "2024"),
  Cena = c(711.5, 626.0, 638.3, 612.8)
)

# Wykres
dane %>% 
  ggplot(aes(x = Produkt, y = Cena, fill = Rok)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", Cena)), 
            position = position_dodge(width = 0.7), vjust = -0.5) +
  scale_fill_manual(values = c("2023" = "hotpink", "2024" = "navyblue")) +
  labs(
    title = "Notowania cen stali na rynku europejskim",
    subtitle = "Średnie ceny HRC i Prętów",
    x = "Produkt",
    y = "Cena (EUR/t)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(),
    legend.position = "top"
  )

# Dlaczego wykres jest lepszy:
# Ten wykres jest bardziej przejrzysty i czytelny, prawidłowo ustawiona skala 
# nie wprowadza odbiorcy w błąd co do wielkości różnic. 
# Wyraziste kolory i brak powtarzających się danych oznaczenia sprawiają, 
# że dane są czytelne i łatwo porównywalne nawet dla osób z zaburzeniami widzenia kolorów.

