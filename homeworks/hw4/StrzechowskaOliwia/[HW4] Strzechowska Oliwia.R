library(ggplot2)


dane <- data.frame(
  miesiac = c("04.2024", "05.2024", "06.2024", "07.2024", "08.2024",
              "09.2024", "10.2024", "11.2024", "12.2024", "01.2025",
              "02.2025", "03.2025", "04.2025"),
  cena_PLN = c(6.70, 6.50, 6.35, 6.20, 6.15, 
               6.00, 6.05, 6.10, 6.10, 6.30,
               6.25, 6.10, 6.00)
)

dane$miesiac <- factor(dane$miesiac, levels = dane$miesiac)

ggplot(dane, aes(x = miesiac, y = cena_PLN, fill = cena_PLN)) +
  geom_col() +                                
  geom_text(aes(label = sprintf("%.2f", cena_PLN)), 
            vjust = -0.5, 
            size = 4) +                        
  scale_fill_gradient(
    low = "#56B1F7",  
    high = "#132B43"  
  ) +
  labs(
    title = "Średnie ceny oleju napędowego (ON)",
    subtitle = "Dane z ostatnich 12 miesięcy (04.2024 - 04.2025)",
    x = "Miesiąc",
    y = "Cena (PLN za litr)",
    fill = "Cena (PLN)",        
    caption = "Źródło: autocentrum.pl"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 10),
    legend.position = "right"   
  ) +
  scale_y_continuous(limits = c(0, 7))          

#Początkowo wykres z paru powodów był nieczytelny, w szczególności oś OX - 
#daty były zapisane w formacie x.xxxx (miesiąc.rok) a oś nie była podpisana, 
#zatem w pierwszej kolejności można było odnieść wrażenie, że jest to zbiór 
#liczb losowych z separatorem dziesiętnym ".". W ramach rozwiązania tego 
#problemu oś podpisano, a numery miesięcy od 1 do 9 opatrzono zerem z przodu.
#
#Oś OY początkowo także nie prezentowała się należycie - wiadomym było, że 
#ukazane są na niej średnie ceny oleju napędowego, nieznana była jednak 
#waluta. Tutaj z pomocą przyszła strona, będąca źródłem danych. Wówczas 
#właściwą walutę naniesiono na tytuł osi, ponadto rozpoczęto oś OY od 0 a nie od
#6, aby pokazać rzeczywistą zależność między spadkami cen w danym miesiącu, a 
#nie, jak poprzednio, tę sztucznie podkreśloną.
#
#Ponadto na początkowym wykresie legenda była niepotrzebna - wykres przedstawia
#jeden rodzaj paliwa, nie trzy. Aktualna legenda opisuje natężenie koloru w 
#stosunku do wysokości wartości średniej ceny oleju napędowego. Takie 
#rozwiązanie umożliwia porównanie danych między sobą.

#Źródła:
#wykres początkowy: https://www.fxmag.pl/energetyka/prognoza-cen-paliw-benzyna-gaz-i-olej-napedowy-na-kwiecien-2025-czy-zobaczymy-jeszcze-obnizki-na-stacjach-przed-wielkanoca
#dane: https://www.autocentrum.pl/paliwa/ceny-paliw/