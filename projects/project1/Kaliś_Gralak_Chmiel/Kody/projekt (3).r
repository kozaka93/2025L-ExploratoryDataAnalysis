#----------------------------------------------
#PRZYDATNE BIBLIOTEKI
#----------------------------------------------
#install.packages("tidyverse")
#install.packages("viridis")
#install.packages("patchwork")
#install.packages("hrbrthemes")
#install.packages("fmsb")
#install.packages("colormap")
#install.packages("sf")
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("ggrepel")
#install.packages("devtools")
#devtools::install_github("ropensci/rnaturalearthhires")
#install.packages("bdl")
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(fmsb)
library(colormap)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
#library(reshape2)
#library(httr)
#library(jsonlite)
library(stringr)
#library(RColorBrewer)
#library(bdl)

getwd()
setwd("C:/Users/Krystian/Desktop/Programowanie/Eksploracja danych/Projekt1")

#----------------------------------------------
#ANALIZA DANYCH
#----------------------------------------------

#-------------
#DANE DOTYCZĄCE OPIEKI PSYCHIATRYCZNEJ -> wykres pajęczynowy oraz słupkowy
#-------------
#Źródło: https://dane.gov.pl/pl/dataset/3523
#Opis danych: Informacje na temat liczby pacjentów w zależności od grupy 
#wiekowej, płci i roku, z którego pochodzą dane. Lata gromadzenia danych:
#2016-2023

#Zadanie 1: Porównanie korzystania z opieki psychiatrycznej przez kobiety i mężczyzn
#w zależności od grupy wiekowej 

#Zadanie 2: Porównanie korzystania z opieki psychiatrycznej przez kobiety i mężczyzn
#w zależności od roku wizyty, zgromadzono dane z lat 2016-2023.

opieka <- read.csv("Liczba_pacjentów_opieka_psychiatryczna.csv", 
               header = TRUE, 
               sep = ",", 
               fileEncoding = "UTF-8-BOM")
str(opieka)
unique(opieka$Rok)

#---
#Zadanie 1
#---

opieka1 <- opieka %>% filter(Grupa.wiekowa != "BRAK") %>% group_by(Płeć, Grupa.wiekowa) %>% summarise(n = sum(as.numeric(Liczba.pacjentów))) %>% ungroup()

kobiety <- opieka1 %>% filter(Płeć == "Kobieta") %>% select(Grupa.wiekowa, n)
mezczyzni <- opieka1 %>% filter(Płeć == "Mężczyzna") %>% select(Grupa.wiekowa, n)

kobiety <- as.data.frame(t(kobiety))
colnames(kobiety) <-  c("0-13", "14-18", "19-25", "26-39", "40-54", "55-69", "70-84", "85+")
kobiety <- kobiety[-1, ]
kobiety <- rbind(rep(as.numeric(apply(kobiety[1, ], 1, max, na.rm = TRUE)),8) , rep(0,8) , kobiety)
typeof(kobiety[1,1])
typeof(kobiety[2,1])
typeof(kobiety[3,1])
kobiety[1,] <- as.numeric(kobiety[1,]) 
kobiety[2,] <- as.numeric(kobiety[2,])
kobiety[3,] <- as.numeric(kobiety[3,])
rownames(kobiety) <- c("1", "2", "3")
str(kobiety)
kobiety <- as.data.frame(sapply(kobiety, as.numeric))

#teraz to samo tylko dla mezczyzn
mezczyzni <- as.data.frame(t(mezczyzni))
colnames(mezczyzni) <-  c("0-13", "14-18", "19-25", "26-39", "40-54", "55-69", "70-84", "85+")
mezczyzni <- mezczyzni[-1, ]
mezczyzni <- rbind(rep(as.numeric(apply(mezczyzni[1, ], 1, max, na.rm = TRUE)),8) , rep(0,8) , mezczyzni)
rownames(mezczyzni) <- c("1", "2", "3")
mezczyzni <- as.data.frame(sapply(mezczyzni, as.numeric))


#Dla obu wykresów razem
opieka2 <- rbind(kobiety, mezczyzni)
opieka2 <- opieka2[-c(4, 5), ]
opieka2 <- as.data.frame(sapply(opieka2, as.numeric))
rownames(opieka2) <- c("1", "2", "Kobiety", "Mężczyźni")

radar <- function() {
radarchart(opieka2, axistype=1,
           pcol= c(rgb(173/255, 216/255, 230/255, alpha = 0.9), rgb(8/255, 88/255, 158/255, alpha = 0.9)), 
           pfcol= c(rgb(173/255, 216/255, 230/255, alpha = 0.5), rgb(8/255, 88/255, 158/255, alpha = 0.3)) , plwd=4 , plty=1,
            cglcol="black", cglty=2, axislabcol="black", caxislabels = seq(0, 4, 1), cglwd=0.8,
            vlcex=3.4)
legend(x=0.7, y=1.6, legend = rownames(opieka2[-c(1,2),]), bty = "n", pch=20 , col=c(rgb(173/255, 216/255, 230/255, alpha = 0.4), rgb(8/255, 88/255, 158/255, alpha = 0.9)),
       text.col = "black", cex=1.6, pt.cex=4.5)
title("Liczba pacjentów w setkach tysięcy w zależności od płci i grupy wiekowej w latach 2016-2023", cex.main = 3.5, col.main = "black")
}
#---
#Zadanie 2
#---

wyk_1 <- opieka %>% 
  mutate(Liczba_pacjentów = case_when(
    Liczba.pacjentów == "<5"~ NA,
    TRUE ~ as.double(Liczba.pacjentów))) %>% 
  filter(!is.na(Liczba_pacjentów)) %>% 
  select(-Liczba.pacjentów) %>% 
  group_by(Płeć, Rok) %>% 
  summarise(n = sum(Liczba_pacjentów)) %>% mutate(n = n/100000)


plot <- wyk_1 %>% 
  ggplot(mapping = aes(
    x = as.factor(Rok),     # Rok jako kategoria (żeby nie sklejały się słupki)
    y = n,
    fill = Płeć)) +         # kolor w zależności od płci
  geom_col(position = "dodge",
           alpha = 0.6) + 
  labs(title = "Liczba osób chodzących do psychiatrów",
       x = "Rok",
       y = "Liczba osób w setkach tysięcy") +
  scale_fill_manual(values = c(
    "Kobieta" = "#b5d2ed",
    "Mężczyzna" = "#08589e"
  )) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(
      hjust = 0.5,           # wyśrodkowanie
      size = 30,             # większy tytuł
      family = "sans",      # czcionka – np. Times
      face = "bold"          # pogrubiony
      ),
    axis.title = element_text(
      size = 25,
      family = "serif"),
    axis.text = element_text(
      size = 25,
      family = "serif"),
    legend.title = element_text(
      size = 25,
      family = "serif"),
    legend.text = element_text(
      size = 25,
      family = "serif"))
plot

#-------------
#DANE DOTYCZĄCE PRZYJĘĆ NA ODDZIAŁY PSYCHIATRYCZNE -> wykres
#-------------
#Źródło: https://dane.gov.pl/pl/dataset/3055/resource/45650,informacje-o-liczbie-przyjec-na-stacjonarne-oddziay-psychiatryczne-i-oddziay-leczenia-uzaleznien/table
#Opis danych: Informacje na temat ilości przyjęć na oddziały psychiatryczne w zależności
#od trybu przyjęcia na przestrzeni lat 2016-2020.

#Zadanie: Porównanie ilości przyjętych osób w zależności od trybu przyjęcia na 
#przestrzeni lat 2016-2020. 

oddzialy <- read.csv("oddzialy_psychiatryczne.csv", 
                   header = TRUE, 
                   sep = ",", 
                   fileEncoding = "UTF-8-BOM")

df <- oddzialy
colnames(df)[2] <- "Nazwa" 

df_long <- df %>%
  pivot_longer(
    cols = -c(Nazwa, Kod.trybu.przyjęcia),   # kolumny do "stopienia"
    names_to = "variable",                   # nazwa nowej kolumny z nazwami kolumn
    values_to = "value"                      # nazwa nowej kolumny z wartościami
  ) %>%
  mutate(Nazwa = str_wrap(Nazwa, width = 30)) %>% 
  mutate(variable = substr(variable, 2, 5)) %>% 
  mutate(value = value/1000)

min_max_labels <- df_long %>% 
  filter(Kod.trybu.przyjęcia %in% c(2, 3, 6, 10)) %>% 
  group_by(Nazwa) %>% 
  filter(value == max(value) | value == min(value)) %>%
  ungroup()

p <- df_long %>% 
  filter(Kod.trybu.przyjęcia %in% c(2, 3, 6, 10)) %>% 
  ggplot(mapping = aes(
    x = variable,
    y = value,
    color = Nazwa,
    group = Nazwa
  )) +
  geom_line(size = 2) +
  geom_point(size = 3) +
  labs(
    title = "Liczba przyjęć według trybu",
    subtitle = "Dane z lat 2016-2020",
    x = "Rok",
    y = "Liczba osób w tysiącach",
    color = "Tryb przyjęcia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    plot.subtitle = element_text(size = 13)
  )

kolory <- c("blue", "lightblue", "navy", "steelblue")


p <- p +
  scale_y_continuous(limits = c(0, 129), expand = c(0, 0)) +
  geom_text(
    data = min_max_labels,
    aes(
      x = variable,
      y = value,
      label = value,
      color = Nazwa
    ),
    vjust = -1,
    size = 3.5,
    show.legend = FALSE
  ) + 
  scale_color_manual(values = kolory) + theme(legend.position = "bottom")
p

#-------------
#DANE DOTYCZĄCE LiCZBY OSÓB LECZONYCH W PORADNIACH DLA OSÓB Z ZABURZENIAMI PSYCHICZNYMI -> mapa
#-------------
#Źródło: 
#1.) https://bdl.stat.gov.pl/bdl/dane/podgrup/tablica
#Opis danych: Zgromadzone są dane pogrupowane rocznie dotyczące liczby leczonych w latach 2005-2022 
#przy podziale na województwa.
#2.) https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/powierzchnia-i-ludnosc-w-przekroju-terytorialnym-w-2022-roku,7,19.html
#Opis danych: Liczba mieszkańców w danym województwie w roku 2022.
#3.) https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/ludnosc-i-ruch-naturalny-w-2016-r-,30,1.html
#Opis danych: Liczba mieszkańców w danym województwie w roku 2016.

#Zadanie: Ile pacjentow leczyło się w latach 2011-2016, a ile w latach 2017-2022 w zależności 
#od województwa znormalizowane względem ilości mieszkańców w danym województwie na podstawie danych
#dotyczących liczby mieszkańców w danym województwie z roku 2016 i 2022. 

pacjenci <- read.csv("pacjenci.csv", 
                   sep = ";",
                   header = TRUE,   
                   stringsAsFactors = FALSE,  
                   fileEncoding = "UTF-8")   

#zaludnienie w roku 2022:
ludnosc <- read.csv("Zaludnienie.csv", 
                     sep = ";",
                     header = TRUE,   
                     stringsAsFactors = FALSE,  
                     fileEncoding = "UTF-8") 

#zaludnienie w roku 2016:
ludnosc2 <- data.frame(
  Województwo = c(
      "DOLNOŚLĄSKIE",
      "KUJAWSKO-POMORSKIE",
      "LUBELSKIE",
      "LUBUSKIE",
      "ŁÓDZKIE",
      "MAŁOPOLSKIE",
      "MAZOWIECKIE",
      "OPOLSKIE",
      "PODKARPACKIE",
      "PODLASKIE",
      "POMORSKIE",
      "ŚLĄSKIE",
      "ŚWIĘTOKRZYSKIE",
      "WARMIŃSKO-MAZURSKIE",
      "WIELKOPOLSKIE",
      "ZACHODNIOPOMORSKIE"),
  Liczba = c(2903710, 2083927, 2133340, 1017376, 2485323, 3382260, 5365898, 993036, 
             2127656, 1186625, 2315611, 4559164, 1252900, 1436367, 3481625, 1708174)
)

calkowite_zaludnienie <- ludnosc %>% summarise(x = sum(as.numeric(Zaludnienie))) %>% pull(1)
#ludnosc <- ludnosc %>% mutate(procent = Zaludnienie/calkowite_zaludnienie) %>% 
#  select(Nazwa, procent)

colnames(pacjenci)[3:20] <- 
  c("rok2005", "rok2006", "rok2007", "rok2008", "rok2009", "rok2010", "rok2011", "rok2012", "rok2013", "rok2014", "rok2015", "rok2016", "rok2017", "rok2018", "rok2019", "rok2020", "rok2021", "rok2022")
pacjenci <- left_join(pacjenci, ludnosc, by = "Nazwa") 
pacjenci <- left_join(pacjenci, ludnosc2, by = c("Nazwa" = "Województwo"))

pacjenci1 <- pacjenci %>% select(-c(23, 22, 21)) %>% 
  mutate(lata = rok2011 + rok2012 + rok2013 + rok2014 + rok2015 +rok2016) %>% 
  group_by(Nazwa) %>% summarise(lata = lata/Liczba) %>% select(Nazwa, lata)

pacjenci2 <- pacjenci %>% select(-c(23, 22, 21)) %>% 
  mutate(lata = rok2017 + rok2018 + rok2019 + rok2020 + rok2021 +rok2022) %>% 
  group_by(Nazwa) %>% summarise(lata = lata/Zaludnienie) %>% select(Nazwa, lata)


polska <- ne_states(country = "Poland", returnclass = "sf")


pacjenci1 <- pacjenci1 %>% mutate(wojewodztwa =  c(
  "Dolnoslaskie", 
  "Kujawsko-Pomorskie", 
  "Lubelskie", 
  "Lubuskie", 
  "Mazowieckie",
  "Malopolskie", 
  "Opolskie", 
  "Podkarpackie", 
  "Podlaskie", 
  "Pomorskie", 
  "Warminsko-Mazurskie", 
  "Wielkopolskie", 
  "Zachodniopomorskie",
  "Lódzkie",
  "Slaskie", 
  "Swietokrzyskie")) %>% select(wojewodztwa, lata)

mapa_dane1 <- left_join(polska, pacjenci1, by = c("name_alt" = "wojewodztwa")) %>% 
  mutate(Procent = factor((case_when(lata < 0.15 ~ "[0; 0,15)",
                                     lata >= 0.15 & lata < 0.2 ~ "[0,15; 0,2)",
                                     lata >= 0.2 & lata < 0.25 ~ "[0,2; 0,25)",
                                     lata >= 0.25 & lata < 0.3 ~ "[0,25; 0,3)",
                                     lata >= 0.3 ~ "0,3+")),
                          ordered = TRUE,
                          levels = c("[0; 0,15)", 
                                     "[0,15; 0,2)", 
                                     "[0,2; 0,25)", 
                                     "[0,25; 0,3)",
                                     "0,3+")))
mapa_dane1$name_alt
mapa_dane1$lata
mapa_dane1$Procent

pacjenci2 <- pacjenci2 %>% mutate(wojewodztwa =  c(
  "Dolnoslaskie", 
  "Kujawsko-Pomorskie", 
  "Lubelskie", 
  "Lubuskie", 
  "Mazowieckie",
  "Malopolskie", 
  "Opolskie", 
  "Podkarpackie", 
  "Podlaskie", 
  "Pomorskie", 
  "Warminsko-Mazurskie", 
  "Wielkopolskie", 
  "Zachodniopomorskie",
  "Lódzkie",
  "Slaskie", 
  "Swietokrzyskie")) %>% select(wojewodztwa, lata)

mapa_dane2 <- left_join(polska, pacjenci2, by = c("name_alt" = "wojewodztwa")) %>% 
  mutate(Procent = factor((case_when(lata < 0.15 ~ "[0; 0,15)",
                                     lata >= 0.15 & lata < 0.2 ~ "[0,15; 0,2)",
                                     lata >= 0.2 & lata < 0.25 ~ "[0,2; 0,25)",
                                     lata >= 0.25 & lata < 0.3 ~ "[0,25; 0,3)",
                                     lata >= 0.3 ~ "0,3+")),
                          ordered = TRUE,
                          levels = c("[0; 0,15)", 
                                     "[0,15; 0,2)", 
                                     "[0,2; 0,25)", 
                                     "[0,25; 0,3)",
                                     "0,3+")))
mapa_dane2$name_alt
mapa_dane2$lata
mapa_dane2$Procent

# Rysowanie mapy z kolorami odpowiadającymi wartościom, legendę wyrysowujemy oddzielnie
kolory <- c("aliceblue", "#b5d2ed", "#8bbce9", "#2b8cbe", "#08589e")
legend_levels <- c("[0; 0,15)", "[0,15; 0,2)", "[0,2; 0,25)", "[0,25; 0,3)", "0,3+")
legend_data <- data.frame(Procent = factor(legend_levels, 
                                           levels = c("[0; 0,15)", "[0,15; 0,2)", "[0,2; 0,25)", "[0,25; 0,3)", "0,3+")))

legenda <- ggplot(legend_data, aes(x = 1, y = Procent, fill = Procent)) +
  geom_tile(show.legend = TRUE, alpha = 0) +  # przezroczysty kafelek, by wymusić legendę
  scale_fill_manual(values = kolory, name = "Wskaźnik", drop = FALSE) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

p1 <- ggplot(mapa_dane1) +
  geom_sf(aes(fill = Procent), color = "black") +
  scale_fill_manual(values = kolory, drop = FALSE)+  
  theme_minimal() +
  labs(subtitle = "Dane z lat 2011-2016", fill = "Wskaźnik") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) + 
  theme(legend.position = "none", plot.title.position = "plot")

p2 <-ggplot(mapa_dane2) +
  geom_sf(aes(fill = Procent), color = "black") + 
  scale_fill_manual(values = kolory, drop = FALSE)+ 
  theme_minimal() +
  labs(subtitle = "Dane z lat 2017-2022", fill = "Wskaźnik") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())+
  theme(legend.position = "none")

figure <- (p1 + p2 + legenda) + plot_layout(widths = c(1, 1, 0.3)) + 
  plot_annotation(
    title = "Wskaźnik ilości osób z zaburzeniami psychicznymi leczonych w poradniach względem populacji",
    theme = theme(plot.title = element_text(size = 10)) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA))
)
figure

#----------------------------------------------
#POBIERANIE DANYCH 
#----------------------------------------------


ggsave("wykres1.png", plot = figure, bg = "transparent", width = 10, height = 6)
ggsave("wykres2.png", plot = p, bg = "transparent", width = 8, height = 6)
ggsave("wykres3.png", plot = plot, bg = "transparent", width = 10, height = 6)
png("wykres4.png", width=2500, height=800, bg="transparent")
radar()
dev.off()

png("wykres5.png", width=400, height=400, bg="transparent")
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1))
legend(x=0, y=0.7, legend = rownames(opieka2[-c(1,2),]), bty = "n", pch=20 , col=c(rgb(173/255, 216/255, 230/255, alpha = 0.4), rgb(8/255, 88/255, 158/255, alpha = 0.9)),
              text.col = "black", cex=1.6, pt.cex=4.5)
dev.off()
