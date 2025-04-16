# wczytywanie bibliotek
library(haven)
library(dplyr)
library(ggplot2)
library(tidyverse)

# wczytywanie danych
dane <- read_sav("BN_2023.sav")
dane <- data.frame(dane)

# "Wiek, a liczba przeczytanych książek w ciągu 12 miesięcy (listopad 2023)"

mozliwosci_ksiazki <- dane %>% select(ANKIETER,BNa_P2a,L3BNa_P1, EDUK5, WIEK,L1BNa_P20,L5BNa_P1,BNa_P13b,BNa_P17)
colnames(mozliwosci_ksiazki) <- c("ankieter","przeczytane_ksiazki","odczucie_mozliwosci","wyksztalcenie","wiek","netflix_itp","sens","korzystasz_z_biblio","ksiazki_w_domu")

# wykres violin liczba książek vs wiek

mozliwosci_ksiazki %>% filter(przeczytane_ksiazki != 7) %>% ggplot(aes(x = as.factor(przeczytane_ksiazki), y = wiek)) +
  geom_violin(trim = FALSE, alpha = 0.8, fill = "#dbd6c1") +
  labs(x = "Wiek", y = "Wartość", fill = "Wiek") +
  theme_minimal() +
  labs(x = "Liczba przeczytanych książek", y ="Wiek", title = "", subtitle = "Listopad 2023") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#001267"),
        plot.subtitle = element_text(color = "white", size = 12 ),
        axis.text = element_text(color = "#dbd6c1", size = 12),
        axis.title = element_text(color = "white", size = 12),
        panel.grid.major = element_line(color = "cornflowerblue"),
        panel.grid.minor = element_line(color = "cornflowerblue")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_x_discrete(labels = c("1" = "0", "2" = "1-2", "3" = "3-6", "4" = "7-11", "5" = "12-23", "6"= "24+"))+
  theme(panel.grid.minor.x = element_blank())  +
  theme(axis.title.x = element_text(margin = margin(t = 20)))+
  theme(axis.title.y = element_text(margin = margin(r = 20)))

# "Liczba książek w podziale na źródła rekomendacji (listopad 2023)"

f <- function(arg) {
  ifelse(is.na(arg), as.integer(0), as.integer(1))
}

pom <- dane %>% 
  mutate(INTNR = as.integer(str_sub(INTNR, 4, 20)), L1BNa_P2b8i1 = f(L1BNa_P2b8i1), L1BNa_P2b8i2 = f(L1BNa_P2b8i2), L1BNa_P2b8i3 = f(L1BNa_P2b8i3), L1BNa_P2b8i4 = f(L1BNa_P2b8i4), L1BNa_P2b8i5 = f(L1BNa_P2b8i5), L1BNa_P2b8i6 = f(L1BNa_P2b8i6), L1BNa_P2b8i7 = f(L1BNa_P2b8i7), L1BNa_P2b8i8 = f(L1BNa_P2b8i8), L1BNa_P2b8i9 = f(L1BNa_P2b8i9), L1BNa_P2b8i10 = f(L1BNa_P2b8i10), L1BNa_P2b8i11 = f(L1BNa_P2b8i11)) %>% 
  select(INTNR, L1BNa_P2b8i1, L1BNa_P2b8i2, L1BNa_P2b8i3, L1BNa_P2b8i4, L1BNa_P2b8i5, L1BNa_P2b8i6, L1BNa_P2b8i7, L1BNa_P2b8i8, L1BNa_P2b8i9, L1BNa_P2b8i10, L1BNa_P2b8i11)

ret <- pom %>% 
  mutate(rodzina = sum(L1BNa_P2b8i1), znajomi = sum(L1BNa_P2b8i2), bibliotekarz = sum(L1BNa_P2b8i3), księgarz = sum(L1BNa_P2b8i4), inna.osoba = sum(L1BNa_P2b8i5), media.społecznościowe = sum(L1BNa_P2b8i6), strony.internetowe = sum(L1BNa_P2b8i7), inne.media = sum(L1BNa_P2b8i8), samodzielnie = sum(L1BNa_P2b8i9), autor = sum(L1BNa_P2b8i10), inaczej = sum(L1BNa_P2b8i11)) %>% 
  select(INTNR, rodzina, znajomi, bibliotekarz, księgarz, inna.osoba, strony.internetowe, inne.media, samodzielnie, autor, inaczej) %>% 
  head(1)

# wykres słupkowy liczba książek

ret %>% 
  pivot_longer(-INTNR, names_to = "źródło", values_to = "liczba") %>% 
  mutate(źródło = fct_relevel(źródło, 
                              "rodzina", "znajomi", "bibliotekarz", "księgarz", "inna.osoba", "strony.internetowe", "inne.media", "samodzielnie", "autor", "inaczej")) %>% 
  ggplot(aes(x = źródło, y=liczba)) +
  geom_bar(stat = "identity", position = "dodge", fill="darkblue", color=NA) +
  scale_y_continuous(limits = c(0, 250), expand = c(0,0)) +
  labs(
    title = "Liczba książek w podziale na źródła rekomendacji",
    subtitle = "listopad 2023",
    x = "źródło rekomendacji",
    y = "liczba książek"
  ) + 
  scale_x_discrete(labels = c("rodzina", "znajomi", "bibliotekarz", "księgarz", "inna osoba", "strona internetowa", "inne media", "samodzielnie", "autor", "inaczej")) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle = -50, hjust = 0, vjust = 0), axis.line = element_line(colour = "black"), panel.border = element_rect(colour = NA, fill=NA))

# źródło rekomendacji: inaczej

funfact <- dane %>% 
  mutate(INTNR = as.integer(str_sub(INTNR, 4, 20)), L1BNa_P2b8i1 = f(L1BNa_P2b8i1), L1BNa_P2b8i2 = f(L1BNa_P2b8i2), L1BNa_P2b8i3 = f(L1BNa_P2b8i3), L1BNa_P2b8i4 = f(L1BNa_P2b8i4), L1BNa_P2b8i5 = f(L1BNa_P2b8i5), L1BNa_P2b8i6 = f(L1BNa_P2b8i6), L1BNa_P2b8i7 = f(L1BNa_P2b8i7), L1BNa_P2b8i8 = f(L1BNa_P2b8i8), L1BNa_P2b8i9 = f(L1BNa_P2b8i9), L1BNa_P2b8i10 = f(L1BNa_P2b8i10), L1BNa_P2b8i11 = f(L1BNa_P2b8i11)) %>% 
  filter(L1BNa_P2b8i11 == 1) %>% 
  select(INTNR, L1BNa_P2b8t1)

# "Liczba posiadanych książek w domu, a korzystanie z bibliotek publicznych"

dane_zliczone <- as.data.frame(table(mozliwosci_ksiazki$korzystasz_z_biblio, mozliwosci_ksiazki$ksiazki_w_domu))
colnames(dane_zliczone) <- c("korzystasz_z_biblio", "ksiazki_w_domu", "liczba_osob")
dane_zliczone1 <- dane_zliczone %>% filter(korzystasz_z_biblio != 3) %>% filter(ksiazki_w_domu != 8)

# mapa ciepła liczba posiadanych książek

ggplot(dane_zliczone1,aes(x = as.numeric(korzystasz_z_biblio), y = as.numeric(ksiazki_w_domu), fill = liczba_osob)) +
  geom_tile() +
  scale_fill_gradient(low = "#8d9eef", high = "#15298a") +  # Kolor od małej do dużej liczby osób
  labs(x = "Korzystanie z bibliotek",
       y = "Liczba książek w domu",
       fill = "Liczba osób") + scale_x_discrete(limits = c("1" = "Tak", "2" = "Nie"))+ theme(panel.grid = element_blank())+ scale_y_continuous(
         breaks = seq(1, 7, by = 1),  # Przedziały na osi Y
         labels = c("1" = "1-10 ", "2" = "11-50 ", "3" = "51-100 ", "4" = "101-200 ", "5" = "201-500 ", "6" = "501-1000 ", "7" = "1000+")
       ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#fffbe8"),
        plot.subtitle = element_text(color = "#382e04", size = 12 ),
        axis.text = element_text(color = "#382e04", size = 12),
        axis.title = element_text(color = "#382e04", size = 12),
        panel.grid.major = element_line(color = "#857224"),
        panel.grid.minor = element_line(color = "#857224"),
        legend.text = element_text(size = 12))

# "Poczucie sensu życia, a liczba przeczytanych książek w ciągu 12 miesięcy (listopad 2023)"
# wykres słupkowy liczba obserwacji w podziale na przeczytane książki i ocenę poczucia sensu życia

mozliwosci_ksiazki%>% filter(przeczytane_ksiazki != 7) %>% ggplot( aes(x = as.factor(przeczytane_ksiazki), fill = as.factor(sens))) +
  geom_bar(stat = "count", position = "stack") +  # Tworzymy wykres słupkowy
  labs(x = "Przeczytane książki", y = "Liczba obserwacji", fill = "sens", title = "") +
  theme_minimal()  +scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(color = "#dbd6c1", size = 12 ),
        legend.title = element_text(color = "#dbd6c1", size = 12 ),
        plot.background = element_rect(fill = "#001267"),
        plot.subtitle = element_text(color = "#dbd6c1", size = 12 ),
        axis.text = element_text(color = "#dbd6c1", size = 12),
        axis.title = element_text(color = "white", size = 12),
        panel.grid.major = element_line(color = "#857224"),
        panel.grid.minor = element_line(color = "#857224"),
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank())+ 
  ylim(0, 1200) +
  scale_x_discrete(labels = c("1" = "0", "2" = "1-2", "3" = "3-6", "4" = "7-11", "5" = "12-23", "6"= "24+"))+
  labs(fill = "Jak często czujesz, \n że życie ma sens?") + 
  scale_fill_manual(values = c("lightblue", "#8d9eef", "cornflowerblue", "#072ce3"),labels = c("1" = "Często", "2" = "Czasami", "3" = "Rzadko", "4" = "Nigdy"))

