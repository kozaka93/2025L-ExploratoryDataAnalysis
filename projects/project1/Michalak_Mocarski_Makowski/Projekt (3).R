
library(dplyr)

library(ggplot2)
library(tidyr)

tabelka <- wskaznikicen %>% 
  filter(Rok>=2005 & `Sposób prezentacji`=="Okres poprzedni = 100") %>% 
  mutate(Roczna_wartość=ifelse(Kwartał=="II półrocze",(Rok+0.5)*10,Rok*10))
  
 


ggplot(tabelka, aes(x = Roczna_wartość, y = Wartość)) +
  geom_line(color = "blue") +  # Linia
  geom_point(color = "red") +  # Punkty
  labs(title = "Zmiany wskaźnika w czasie", x = "Rok", y = "Wskaźnik") +
  theme_minimal()

wskazniki_na_półrocze <- tabelka %>%
  arrange(Rok) %>%  # Sortowanie po roku
  mutate(Roznica = Wartość - lag(Wartość, 1))

średnia <- wskazniki_na_półrocze%>% 
  group_by(Kwartał) %>% 
  summarise(średnia=mean(Roznica,na.rm=TRUE),suma=sum(Roznica>0,na.rm=TRUE)/n())

xd <- wskazniki_na_półrocze %>% 
  group_by(Rok) %>% 
  summarise(kolumna=sum(ifelse(Kwartał=="I półrocze",Roznica,-Roznica))) %>% 
  filter(Rok>2005)


#######
wynagrodzenie <- Przeciętne_wynagrodzenie %>% 
  select("Lata",`Przeciętne wynagrodzenie w zł`) %>% 
  filter(Lata>=2001)
minim <- minimalna %>% 
  filter(Rok>=2001 & Rok<2023)

wsk_inflacji <- rocznewskaznikicentowarowiuslugkonsumpcyjnychod1950roku %>% 
  select("Rok","Wartość") %>% 
  filter(Rok>=2001 & Rok<2023)
dane_1 <- data.frame(wsk_inflacji$Rok,wsk_inflacji$Wartość,wynagrodzenie$`Przeciętne wynagrodzenie w zł`)
colnames(dane_1) <- c("Rok", "Inflacja", "Wynagrodzenie")
dane_1$Inflacja <- dane_1$Inflacja -100
dane_1$Wynagrodzenie <- as.numeric(gsub(",", ".", dane_1$Wynagrodzenie))
dane_1$Wynagrodzenie <- replace(dane_1$Wynagrodzenie, is.na(dane_1$Wynagrodzenie), 76153.80)
dane_1$Wynagrodzenie <- dane_1$Wynagrodzenie/12

ggplot(dane_1, aes(x = Rok)) +
  geom_line(aes(y = Inflacja, color = "Inflacja (%)")) +
  geom_line(aes(y = Wynagrodzenie / 1000, color = "Wynagrodzenie (tys. PLN)")) +
  scale_y_continuous(
    name = "Inflacja (%)",
    sec.axis = sec_axis(~.*1000, name = "Wynagrodzenie (PLN)")
  ) +
  labs(title = "Trend inflacji i przeciętnego wynagrodzenia w Polsce (2020–2025)",
       x = "Rok") +
  scale_color_manual(values = c("Inflacja (%)" = "red", "Wynagrodzenie (tys. PLN)" = "blue")) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )


#####
library(ggplot2)
library(dplyr)

# Zidentyfikowanie największych i najmniejszych wartości inflacji i wynagrodzenia
max_inflacja <- dane_1[which.max(dane_1$Inflacja),]
min_inflacja <- dane_1[which.min(dane_1$Inflacja),]

max_wynagrodzenie <- dane_1[which.max(dane_1$Wynagrodzenie),]
min_wynagrodzenie <- dane_1[which.min(dane_1$Wynagrodzenie),]

# Tworzenie wykresu
ggplot(dane_1, aes(x = Rok)) +
  geom_line(aes(y = Inflacja, color = "Inflacja (%)"), size = 1.2) +
  geom_line(aes(y = Wynagrodzenie / 1000, color = "Wynagrodzenie (tys. PLN)"), size = 1.2) +
  
  # Zaznaczenie punktów max i min dla inflacji
  geom_point(data = max_inflacja, aes(x = Rok, y = Inflacja), color = "black", size = 3) +
  geom_point(data = min_inflacja, aes(x = Rok, y = Inflacja), color = "black", size = 3) +
  
  # Zaznaczenie punktów max i min dla wynagrodzenia
  geom_point(data = max_wynagrodzenie, aes(x = Rok, y = Wynagrodzenie / 1000), color = "blue", size = 3) +
  geom_point(data = min_wynagrodzenie, aes(x = Rok, y = Wynagrodzenie / 1000), color = "blue", size = 3) +
  
  # Etykiety do punktów max i min z wartościami
  geom_text(data = max_inflacja, aes(x = Rok, y = Inflacja, label = paste(round(Inflacja, 2), "%")), 
            vjust = 0, color = "black", size = 4, fontface = "bold", hjust = 1) +
  geom_text(data = min_inflacja, aes(x = Rok, y = Inflacja, label = paste(round(Inflacja, 2), "%")), 
            vjust = 0, color = "black", size = 4, fontface = "bold", hjust = 1) +
  
  geom_text(data = max_wynagrodzenie, aes(x = Rok, y = Wynagrodzenie / 1000, label = paste(round(Wynagrodzenie / 1000, 2), "tys. PLN")), 
            vjust = -1, color = "blue", size = 4, fontface = "bold", hjust = 1) +
  geom_text(data = min_wynagrodzenie, aes(x = Rok, y = Wynagrodzenie / 1000, label = paste(round(Wynagrodzenie / 1000, 2), "tys. PLN")), 
            vjust = 1.5, color = "blue", size = 4, fontface = "bold", hjust = 0) +
  
  # Skala osi Y
  scale_y_continuous(
    name = "Inflacja (%)",
    sec.axis = sec_axis(~.*1000, name = "Wynagrodzenie (PLN)")
  ) +
  
  # Tytuł i etykiety
  labs(
    title = "Trend inflacji i przeciętnego wynagrodzenia w Polsce (2001–2022)",
    x = "Rok",
    color = "Wskaźnik"
  ) +
  
  # Kolory dla linii
  scale_color_manual(values = c("Inflacja (%)" = "black", "Wynagrodzenie (tys. PLN)" = "blue")) +
  
  # Stylizacja wykresu
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 10, 10) # Daje więcej miejsca na etykiety
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )
########
library(ggplot2)
library(dplyr)

minim <- minimalna %>% 
  filter(Rok >= 2001 & Rok < 2023) %>% 
  arrange(Rok)

# Odwrócenie kolejności minimalnego wynagrodzenia tak, aby pasowało do kolejności w dane_1
minim <- minim[order(minim$Rok),]
dane_1 <- dane_1[order(dane_1$Rok),]

# Dodanie minimalnego wynagrodzenia do głównych danych
dane_1$Stawka <- minim$Stawka / 1000

# Zidentyfikowanie największych i najmniejszych wartości inflacji i wynagrodzenia
max_inflacja <- dane_1[which.max(dane_1$Inflacja),]
min_inflacja <- dane_1[which.min(dane_1$Inflacja),]

max_wynagrodzenie <- dane_1[which.max(dane_1$Wynagrodzenie),]
min_wynagrodzenie <- dane_1[which.min(dane_1$Wynagrodzenie),]

max_min_wynagrodzenie <- dane_1[which.max(dane_1$Stawka),]
min_min_wynagrodzenie <- dane_1[which.min(dane_1$Stawka),]

# Tworzenie wykresu
ggplot(dane_1, aes(x = Rok)) +
  geom_line(aes(y = Inflacja, color = "Inflacja (%)"), size = 1.2) +
  geom_line(aes(y = Wynagrodzenie / 1000, color = "Wynagrodzenie (tys. PLN)"), size = 1.2) +
  geom_line(aes(y = Stawka, color = "Minimalne wynagrodzenie (tys. PLN)"), size = 1.2, linetype = "dashed") +
  
  # Zaznaczenie punktów max i min dla inflacji
  geom_point(data = max_inflacja, aes(x = Rok, y = Inflacja), color = "black", size = 3) +
  geom_point(data = min_inflacja, aes(x = Rok, y = Inflacja), color = "black", size = 3) +
  
  # Zaznaczenie punktów max i min dla wynagrodzenia
  geom_point(data = max_wynagrodzenie, aes(x = Rok, y = Wynagrodzenie / 1000), color = "red", size = 3) +
  geom_point(data = min_wynagrodzenie, aes(x = Rok, y = Wynagrodzenie / 1000), color = "red", size = 3) +
  
  # Zaznaczenie punktów max i min dla minimalnego wynagrodzenia
  geom_point(data = max_min_wynagrodzenie, aes(x = Rok, y = Stawka), color = "#E69F00", size = 3) +
  geom_point(data = min_min_wynagrodzenie, aes(x = Rok, y = Stawka), color = "#E69F00", size = 3) +
  
  # Etykiety do punktów max i min
  geom_text(data = max_inflacja, aes(x = Rok, y = Inflacja, label = paste(round(Inflacja, 2), "%")), 
            vjust = 0, color = "black", size = 3, fontface = "bold", hjust = 1) +
  geom_text(data = min_inflacja, aes(x = Rok, y = Inflacja, label = paste(round(Inflacja, 2), "%")), 
            vjust = 0, color = "black", size = 3, fontface = "bold", hjust = 1) +
  
  geom_text(data = max_wynagrodzenie, aes(x = Rok, y = Wynagrodzenie / 1000, label = paste(round(Wynagrodzenie / 1000, 2), "tys. PLN")), 
            vjust = -1, color = "red", size = 3, fontface = "bold", hjust = 1) +
  geom_text(data = min_wynagrodzenie, aes(x = Rok, y = Wynagrodzenie / 1000, label = paste(round(Wynagrodzenie / 1000, 2), "tys. PLN")), 
            vjust = 1.5, color = "red", size = 3, fontface = "bold", hjust = 0) +
  
  geom_text(data = max_min_wynagrodzenie, aes(x = Rok, y = Stawka, label = paste(round(Stawka, 2), "tys. PLN")), 
            vjust = -1, color = "#E69F00", size = 3, fontface = "bold", hjust = 1) +
  geom_text(data = min_min_wynagrodzenie, aes(x = Rok, y = Stawka, label = paste(round(Stawka, 2), "tys. PLN")), 
            vjust = 1.5, color = "#E69F00", size = 3, fontface = "bold", hjust = 0) +
  
  # Skala osi Y
  scale_y_continuous(
    name = "Inflacja (%)",
    sec.axis = sec_axis(~.*1000, name = "Wynagrodzenie (PLN)")
  ) +
  
  # Tytuł i etykiety
  labs(
    title = "Trend inflacji, przeciętnego i minimalnego wynagrodzenia w Polsce (2001–2022)",
    x = "Rok",
    color = "Wskaźnik"
  ) +
  
  # Kolory dla linii
  scale_color_manual(values = c("Inflacja (%)" = "black", "Wynagrodzenie (tys. PLN)" = "red", "Minimalne wynagrodzenie (tys. PLN)" = "#E69F00")) +
  
  # Stylizacja wykresu
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 10, 10) 
  )+ theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )



#####

Wynagrodzenia_styczeń_2025[] <- lapply(Wynagrodzenia_styczeń_2025, function(x) gsub(" PLN", "", x)) # Usunięcie PLN
Wynagrodzenia_styczeń_2025[] <- lapply(Wynagrodzenia_styczeń_2025, function(x) gsub(" ", "", x))     # Usunięcie spacji
Wynagrodzenia_styczeń_2025[, -1] <- lapply(Wynagrodzenia_styczeń_2025[, -1], as.numeric)  # Zamiana danych na liczby

dane_long <- Wynagrodzenia_styczeń_2025 %>%
  pivot_longer(cols = -1, names_to = "Sektor", values_to = "Wynagrodzenie") %>%
  rename(Wojewodztwo = 1)

ggplot(dane_long, aes(x = Wojewodztwo, y = Wynagrodzenie, fill = Sektor)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Wynagrodzenia w różnych sektorach w Polsce",
       x = "Województwo", y = "Wynagrodzenie (PLN)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


dane_całaPolska <- Wynagrodzenia_styczeń_2025 %>%
  filter(...1 == "całaPolska") %>%
  pivot_longer(cols = -1, names_to = "Sektor", values_to = "Wynagrodzenie")




ggplot(dane_całaPolska, aes(x = Sektor, y = Wynagrodzenie)) +
  geom_bar(stat = "identity", fill = "#E69F00", color = "black", size = 0.5) +  # Niebieskie słupki z czarnym konturem
  geom_text(aes(label = scales::comma(Wynagrodzenie)), vjust = -0.3, size = 5, fontface = "bold") +  # Etykiety z wartościami
  labs(title = "Średnie wynagrodzenie w różnych sektorach w Polsce",
       x = "Sektor", y = "Wynagrodzenie (PLN)") +
  theme_minimal(base_size = 16) +  # Zwiększamy rozmiar czcionek
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotacja i rozmiar etykiet na osi X
    axis.text.y = element_text(size = 12),  # Rozmiar etykiet na osi Y
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Styl tytułu
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Dodatkowa siatka
    panel.grid.minor = element_blank()  # Usunięcie drobnej siatki
  )

#####
dane_srednie <- dane_long %>%
  group_by(Wojewodztwo) %>%
  summarise(Srednie_wynagrodzenie = mean(Wynagrodzenie, na.rm = TRUE))

library(sf)

wojewodztwa_sf <- st_read("C:\\Users\\LENOVO\\Downloads\\wojewodztwa.shp")
wojewodztwa_sf <- wojewodztwa_sf %>%
  left_join(dane_srednie, by = c("JPT_NAZWA_" = "Wojewodztwo"))
library(tmap)

tm_shape(wojewodztwa_sf) +
  tm_borders() +
  tm_polygons(
    fill = "Srednie_wynagrodzenie",  # Kolumna z wynagrodzeniami
    palette = "YlOrRd",  # Paleta kolorów
    fill.scale = tm_scale_intervals(  # Definicja przedziałów wartości
      style = "quantile",  # Wybór stylu skali (np. kwantyle)
      n = 5  # Liczba przedziałów
    ),
    fill.legend = tm_legend(title = "Wynagrodzenie"),  # Tytuł legendy
    title = "Średnie wynagrodzenie w Polsce"  # Tytuł mapy
  ) +
  tm_layout(
    legend.position = c("left", "bottom")  # Pozycja legendy
  )


tm_shape(wojewodztwa_sf) +
  tm_borders() +
  tm_polygons(
    fill = "Srednie_wynagrodzenie",
    palette = "YlOrRd",
    style = "quantile",  # bez tm_scale_intervals — styl można ustawić bezpośrednio
    n = 5,
    title = "Średnie wynagrodzenie"
  ) +
  tm_layout(
    legend.outside = TRUE,                 # Legenda poza mapą
    legend.outside.position = "right",     # Po prawej stronie (możesz też dać: "left", "bottom", "top")
    legend.title.size = 1.2,
    legend.text.size = 0.9,
    title.size = 1.4
  )

ggplot(inflacja_2, aes(x = Roznica, fill = miesiac2)) +
  geom_histogram(binwidth = 0.2, alpha = 1,  position = "stack") +
  labs(title = "Wspólny histogram danych z różnych miesięcy",
       x = "Wartości", y = "Częstość") +
  theme_minimal()



# Nowa kolumna: tylko "styczeń" i "inne"
inflacja_2$miesiac_grupa <- ifelse(inflacja_2$miesiac == 1, "styczeń", "inne")

# Histogram
ggplot(inflacja_2, aes(x = Roznica, fill = miesiac_grupa)) +
  geom_histogram(binwidth = 0.5, alpha = 1, position = "stack") +
  scale_fill_manual(values = c("styczeń" = "blue", "inne" = "lightblue")) +  # Twoje kolory
  labs(
    title = "Wspólny histogram danych z różnych miesięcy",
    x = "Różnica względem poprzedniego miesiąca", 
    y = "Ilość występowań",
    fill = "Miesiąc"
  ) +
  theme_minimal()


minima <- inflacja_2 %>%
  filter(miesiac == 1) %>%              # tylko styczeń
  arrange(Roznica) %>%                  # sortuj rosnąco
  slice_head(n = 3)                     # weź 3 najmniejsze

library(ggplot2)
library(dplyr)

# Wybierz 3 najniższe wartości stycznia i dodaj etykiety
outliery <- inflacja_2 %>%
  filter(miesiac == 1) %>%
  arrange(Roznica) %>%
  slice_head(n = 3) %>%
  mutate(label = c("2009", "2013", "2018"))

# Histogram z adnotacjami
ggplot(inflacja_2, aes(x = Roznica, fill = miesiac_grupa)) +
  geom_histogram(binwidth = 0.5, alpha = 1, position = "stack") +
  scale_fill_manual(values = c("styczeń" = "blue", "inne" = "lightblue")) +
  labs(
    title = "Wspólny histogram danych z różnych miesięcy",
    x = "Różnica względem poprzedniego miesiąca", 
    y = "Ilość występowań",
    fill = "Miesiąc"
  ) +
  geom_text(
    data = outliery,
    aes(x = Roznica, y = 1, label = label),
    inherit.aes = FALSE,
    vjust = -1,
    color = "black",
    size = 4
  ) +
  theme_minimal() +theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

library(dplyr)
library(ggplot2)
rok_filtrowany=2010
wskaznikicen <- read_excel("wskaznikicen.xlsx")
dane2<-read_excel("wskazniki_cen_produkcji_sprzedanej_przemyslu_w_latach_1996-2021.xlsx")
tabelka <- wskaznikicen %>% 
  filter(Rok>=rok_filtrowany & `Sposób prezentacji`=="Okres poprzedni = 100") %>% 
  mutate(Rok_niecałkowity=ifelse(Kwartał=="II półrocze",(Rok+0.5),Rok))




ggplot(tabelka, aes(x = Rok_niecałkowity, y = Wartość)) +
  geom_line(color = "blue") +  # Linia
  geom_point(color = "black") +  # Punkty
  labs(title = "Zmiany wskaźnika w czasie", x = "Rok", y = "Wskaźnik") +
  theme_minimal()+ theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )


wskazniki_na_półrocze <- tabelka %>%
  arrange(Rok_niecałkowity) %>%  # Sortowanie po roku
  mutate(Roznica = Wartość - lag(Wartość, 1))

średnia <- wskazniki_na_półrocze%>% 
  group_by(Kwartał) %>% 
  summarise(średnia_roznica=mean(Roznica,na.rm=TRUE),suma=sum(Roznica>0,na.rm=TRUE)/n())

xd <- wskazniki_na_półrocze %>% 
  group_by(Rok) %>% 
  summarise(roznica_wzrostu_inflacji=sum(ifelse(Kwartał=="I półrocze",Roznica,-Roznica))) %>% 
  filter(Rok>rok_filtrowany)



inflacja_2<-dane2 %>% 
  filter(rok>=rok_filtrowany) %>% 
  mutate(Rok_niecałkowity=rok+miesiac/12) %>% 
  arrange(Rok_niecałkowity) %>% 
  mutate(Roznica=wartosc - lag(wartosc, 1)) 



inflacja_na_miesiac<-inflacja_2%>% 
  group_by(miesiac) %>% 
  summarise(sredni_wskaźnik=mean(wartosc),srednia_roznica=mean(Roznica,na.rm=TRUE))




inflacja_na_miesiac$miesiac <- factor(inflacja_na_miesiac$miesiac,
                                      levels = 1:12,
                                      labels = c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec",
                                                 "lipiec", "sierpień", "wrzesień", "październik", "listopad", "grudzień"))

# Wykres
ggplot(inflacja_na_miesiac, aes(x = miesiac, y = sredni_wskaźnik, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "black") +
  labs(title = "Wskaźnik inflacji w podziale na miesiące", x = "Miesiąc", y = "Średni wskaźnik", caption = "Dane od 2005r") +
  theme_minimal() +theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )



ggplot(inflacja_na_miesiac, aes(x = miesiac, y = srednia_roznica)) +
  geom_col(fill="lightblue",color="blue",size=0.5) +  # Punkty
  labs(title = "Różnica wskaźnika inflacji na miesiąc", x = "Miesiąc", y = "Średnia różnica względem poprzedniego miesiąca", caption="Dane od 2005r") +
  theme_minimal()+ theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )



inflacja_2 <- inflacja_2 %>%
  mutate(miesiac2 = factor(miesiac, levels = 1:12, labels = month.abb))  # Skróty miesięcy


ggplot(inflacja_2, aes(x = Roznica, fill = miesiac2)) +
  geom_boxplot() +
  labs(title = "Wspólny histogram danych z różnych miesięcy",
       x = "Wartości", y = "Częstość") +
  theme_minimal()

ggplot(inflacja_2, aes(x = miesiac2, y = Roznica, fill = miesiac2)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2,) +
  labs(
    title = "Rozkład wartości różnic w różnych miesiącach",
    x = "Miesiąc", 
    y = "Różnica"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Ładne kolory z palety RColorBrewer
  theme_minimal(base_size = 14) +        # Ustaw większy rozmiar czcionki
  theme(
    legend.position = "none",            # Ukrycie legendy, jeśli kolory powtarzają nazwy miesięcy
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Wyśrodkowanie i pogrubienie tytułu
    axis.text.x = element_text(angle = 45, hjust = 1)       # Obrót etykiet na osi X
  )


# Ustawienie kolejności miesięcy, by styczeń był na górze
inflacja_2$miesiac2 <- factor(inflacja_2$miesiac2, levels = rev(unique(inflacja_2$miesiac2)))

# Wykres z poziomymi boxplotami i styczniem na górze
ggplot(inflacja_2, aes(x = Roznica, y = miesiac2, fill = miesiac2)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2) +
  labs(
    title = "Rozkład wartości różnic w różnych miesiącach",
    x = "Różnica", 
    y = "Miesiąc"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Paleta kolorów
  theme_minimal(base_size = 14) +        # Większa czcionka
  theme(
    legend.position = "none",            # Ukrycie legendy
    plot.title = element_text(hjust = 0.5, face = "bold")  # Wyśrodkowanie tytułu
  )


ggplot(inflacja_2, aes(x = Roznica, y = miesiac2)) +
  geom_boxplot(fill = "lightblue", outlier.shape = 16, outlier.size = 2) +
  labs(
    title = "Rozkład wartości różnic wskaźnika inflacji w różnych miesiącach",
    x = "Różnica", 
    y = "Miesiąc", caption = "Dane od 2010r"
  ) +
  scale_y_discrete(position = "right") +     # Oś Y po prawej
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )




