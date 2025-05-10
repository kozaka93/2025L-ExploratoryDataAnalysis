# Praca domowa 4

# źrodło wykresu:
# https://www.bankier.pl/wiadomosc/Statystyki-pokazuja-ze-prezydent-Trump-mogl-zostac-oklamany-w-sprawie-cel-Najgorsze-ze-wciaz-moze-w-to-klamstwo-wierzyc-8926655.html

# Niepoprawy jest typ wykresu, 
# ponieważ ludzie nie radzą sobie najlepiej z wykresami kołowymi.
# Dwa wykresy kołowe zawierają dane, które łatwo można 
# przedstawić na jednym wykresie, bez pominięcia żadnych danych, 
# ani zmniejszenia czytelonści wykresu.
# Liczby w wewnętrznym wykresie nie pokrywają się z podanymi kątami.


library(ggplot2)

budzet <- data.frame(
  kategoria = c("Przychody","Wydatki obowiązkowe","Wydatki uznaniowe","Obsługa długu"),
  wartosc = c(4.92, 4.1, 1.8 ,0.9),
  typ = c("Przychody","Wydatki", "Wydatki", "Wydatki"))

ggplot(budzet, aes(x = typ, y = wartosc, fill = kategoria)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c(
    "Przychody" = "forestgreen",
    "Wydatki obowiązkowe" = "#e63946",
    "Wydatki uznaniowe" = "darkorange",
    "Obsługa długu" = "#ff7b7b"), 
    breaks = c("Wydatki obowiązkowe", "Wydatki uznaniowe", "Obsługa długu")) +
  geom_text(aes(label = paste0(wartosc, " bln USD")), 
            position = position_stack(vjust = 0.5),
            size = 4, color = "white") +
  labs(
    title = "Budżet federalny Stanów Zjednoczonych w roku fiskalnym 2024",
    x = "",
    y = "Biliony dolarów (bln USD)") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


# Podany wykres jest lepszy ze względu na lepszy typ wykresu oraz
# bez straty informacji udało się dwa wykresy przedstawić jako jeden.
