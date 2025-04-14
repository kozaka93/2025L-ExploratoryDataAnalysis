library(sf)
library(ggplot2)
library(dplyr)
library(patchwork)
install.packages("patchwork")

meskie_pierwsze_2020_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2020_męskie_imię_pierwsze.csv")
meskie_pierwsze_2021_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2021_męskie_imię_pierwsze.csv")
meskie_pierwsze_2022_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2022_męskie_imię_pierwsze.csv")
meskie_pierwsze_2023_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2023_męskie_imię_pierwsze.csv")
meskie_pierwsze_2024_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2024_męskie_imię_pierwsze.csv")
zenskie_pierwsze_2020_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2020_żeńskie_imię_pierwsze.csv")
zenskie_pierwsze_2021_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2021_żeńskie_imię_pierwsze.csv")
zenskie_pierwsze_2022_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2022_żeńskie_imię_pierwsze.csv")
zenskie_pierwsze_2023_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2023_żeńskie_imię_pierwsze.csv")
zenskie_pierwsze_2024_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2024_żeńskie_imię_pierwsze.csv")

colnames(meskie_pierwsze_2022_woj) = c("WOJ","WOJEWÓDZTWO","IMIĘ_PIERWSZE","PŁEĆ","LICZBA_WYSTĄPIEŃ")
colnames(zenskie_pierwsze_2022_woj) = c("WOJ","WOJEWÓDZTWO","IMIĘ_PIERWSZE","PŁEĆ","LICZBA_WYSTĄPIEŃ")

imiona_2019_2024 <- bind_rows(meskie_pierwsze_2020_woj, meskie_pierwsze_2021_woj, meskie_pierwsze_2022_woj, meskie_pierwsze_2023_woj, meskie_pierwsze_2024_woj, zenskie_pierwsze_2020_woj, zenskie_pierwsze_2021_woj, zenskie_pierwsze_2022_woj, zenskie_pierwsze_2023_woj, zenskie_pierwsze_2024_woj)

granice_wojewodztw <-  st_read("A01_Granice_wojewodztw.shp", quiet = TRUE)


imiona_2019_2024$WOJEWÓDZTWO <- tolower(imiona_2019_2024$WOJEWÓDZTWO)

imiona_wojewodztwa_m <- imiona_2019_2024 %>% 
  filter(PŁEĆ == 'MĘŻCZYZNA') %>% 
  group_by(WOJEWÓDZTWO, IMIĘ_PIERWSZE) %>% 
  summarise(liczba = sum(LICZBA_WYSTĄPIEŃ)) 

Liczba_urodzen_woj_m <- imiona_wojewodztwa_m %>% 
  group_by(WOJEWÓDZTWO) %>% 
  summarise(suma_urodzen = sum(liczba))

imiona_wojewodztwa_k <- imiona_2019_2024 %>% 
  filter(PŁEĆ == 'KOBIETA') %>% 
  group_by(WOJEWÓDZTWO, IMIĘ_PIERWSZE) %>% 
  summarise(liczba = sum(LICZBA_WYSTĄPIEŃ)) 

Liczba_urodzen_woj_k <- imiona_wojewodztwa_k %>% 
  group_by(WOJEWÓDZTWO) %>% 
  summarise(suma_urodzen = sum(liczba))

imiona_wojewodztwa_m <- imiona_wojewodztwa_m %>% 
  left_join(Liczba_urodzen_woj_m, by = join_by(WOJEWÓDZTWO)) %>% 
  mutate(wskaznik = liczba/suma_urodzen*100)

mapa_imiona <- granice_wojewodztw %>% 
  left_join(imiona_wojewodztwa_m, by = join_by(JPT_NAZWA_ == WOJEWÓDZTWO)) %>% 
  filter(IMIĘ_PIERWSZE == "JAKUB") %>% 
  ggplot()+
  geom_sf(aes(fill=wskaznik))+
  scale_fill_gradient(low = "lightblue", high = "navy")+
  ggtitle("Częstotliwość nadawania imienia Jan", subtitle = "w latach 2019-2024")+
  labs(fill = "Procent dzieci")
  
imiona_wojewodztwa_k <- imiona_wojewodztwa_k %>% 
  left_join(Liczba_urodzen_woj_k, by = join_by(WOJEWÓDZTWO)) %>% 
  mutate(wskaznik = liczba/suma_urodzen*100)

mapa_imiona <- granice_wojewodztw %>% 
  left_join(imiona_wojewodztwa_k, by = join_by(JPT_NAZWA_ == WOJEWÓDZTWO)) %>% 
  filter(IMIĘ_PIERWSZE == "AGNIESZKA") %>% 
  ggplot()+
  geom_sf(aes(fill=wskaznik))+
  scale_fill_gradient(low = "lavenderblush", high = "hotpink")+
  ggtitle("Częstotliwość nadawania imienia Julia", subtitle = "w latach 2019-2024")+
  labs(fill = "Procent dzieci")

mapa_imiona

imiona_ranking_m <- imiona_2019_2024 %>%
  filter(PŁEĆ == 'MĘŻCZYZNA') %>% 
  group_by(IMIĘ_PIERWSZE) %>% 
  summarise(suma = sum(LICZBA_WYSTĄPIEŃ)) %>% 
  arrange(desc(suma)) %>% 
  top_n(5)

imiona_ranking_k <- imiona_2019_2024 %>%
  filter(PŁEĆ == 'KOBIETA') %>% 
  group_by(IMIĘ_PIERWSZE) %>% 
  summarise(suma = sum(LICZBA_WYSTĄPIEŃ)) %>% 
  arrange(desc(suma)) %>% 
  top_n(5)

pop_m <- imiona_wojewodztwa_m %>% 
  filter(IMIĘ_PIERWSZE %in% c("ANTONI", "JAN", "ALEKSANDER", "FRANCISZEK", "NIKODEM")) %>%  
  group_by(WOJEWÓDZTWO) %>% 
  summarise(suma = sum(liczba)) %>% 
  left_join(Liczba_urodzen_woj_m, by = "WOJEWÓDZTWO") %>% 
  mutate(wskaznik = (suma / suma_urodzen) * 100) 
  

mapa_pop_m <- granice_wojewodztw %>% 
  left_join(pop_m, by = join_by(JPT_NAZWA_ == WOJEWÓDZTWO)) %>% 
  ggplot()+
  geom_sf(aes(fill=wskaznik))+
  scale_fill_gradient(low = "lightblue", high = "navy")+
  ggtitle("Odsetek 5 najpopularniejszych imion męskich")+
  labs(fill = "Wartość procentowa", subtitle = "nadanych w latach 2019-2024")+
  theme_void()+
  theme(
    plot.title = element_text(size = 24, color = "white", face = "bold"), 
    legend.title = element_text(size = 18, color = "white", face = "bold"),
    legend.text = element_text(size = 16, color = "white",),
    plot.subtitle = element_text(size = 18, color = "white"),
    axis.text = element_blank())  


pop_k <- imiona_wojewodztwa_k %>% 
  filter(IMIĘ_PIERWSZE %in% c("ZOFIA", "ZUZANNA", "HANNA", "MAJA", "JULIA")) %>%  
  group_by(WOJEWÓDZTWO) %>% 
  summarise(suma = sum(liczba)) %>% 
  left_join(Liczba_urodzen_woj_m, by = "WOJEWÓDZTWO") %>% 
  mutate(wskaznik = (suma / suma_urodzen) * 100) 


mapa_pop_k <- granice_wojewodztw %>% 
  left_join(pop_k, by = join_by(JPT_NAZWA_ == WOJEWÓDZTWO)) %>% 
  ggplot()+
  geom_sf(aes(fill=wskaznik))+
  scale_fill_gradient(low = "pink", high = "violetred3")+
  ggtitle("Odsetek 5 najpopularniejszych imion żeńskich")+
  labs(fill = "Wartość procentowa", subtitle = "nadanych w latach 2019-2024")+
  theme_void()+
  theme(
    plot.title = element_text(size = 24, color = "white", face = "bold"), 
    legend.title = element_text(size = 18, color = "white", face = "bold"),
    legend.text = element_text(size = 16, color = "white",),
    plot.subtitle = element_text(size = 18, color = "white"),
    axis.text = element_blank())  
  

ggsave("mapkaK.png", plot = last_plot(), bg = "transparent", width = 8, height = 6, dpi = 300)
