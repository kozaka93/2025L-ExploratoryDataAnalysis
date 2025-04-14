library(dplyr)
library(ggplot2)

imiona2000_2019 <- read.csv("eksploracja danych projekt 1/imiona_projket/Imiona_nadane_wPolsce_w_latach_2000-2019.csv")
View(imiona2000_2019)

pochodzenie_imion <- read.csv("eksploracja danych projekt 1/imiona_projket/Pochodzenie_imion2.csv", sep = ";")
View(pochodzenie_imion)

imiona2000_2019
imiona2000_2019 %>% filter(Płeć == "M", Imię == "JAKUB") %>% 
  ggplot(aes(x = Rok, y = Liczba)) + geom_point(color = "skyblue") + 
  geom_line(color = "skyblue") +
  labs(title = "Wykres popularności imienia JAKUB na przestrzeni lat", 
       subtitle = "2000 - 2019", 
       x = "Rok", 
       y = "Liczba wystąpień") 

meskie_pierwsze_2020_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2020_męskie_imię_pierwsze.csv")
View(meskie_pierwsze_2020_woj)
meskie_pierwsze_2020 <- meskie_pierwsze_2020_woj %>% 
  group_by(Imię = IMIĘ_PIERWSZE) %>% 
  summarise(Liczba = sum(LICZBA_WYSTĄPIEŃ)) %>% 
  mutate(Rok = 2020, Płeć = "M") %>% 
  relocate(Rok, .before = everything())
bind_rows(imiona2000_2019, meskie_pierwsze_2020)

meskie_pierwsze_2021_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2021_męskie_imię_pierwsze.csv")
meskie_pierwsze_2021 <- meskie_pierwsze_2021_woj %>% 
  group_by(Imię = IMIĘ_PIERWSZE) %>% 
  summarise(Liczba = sum(LICZBA_WYSTĄPIEŃ)) %>% 
  mutate(Rok = 2021, Płeć = "M") %>% 
  relocate(Rok, .before = everything())
bind_rows(imiona2000_2019, meskie_pierwsze_2020, meskie_pierwsze_2021)

meskie_pierwsze_2022_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2022_męskie_imię_pierwsze.csv")
meskie_pierwsze_2022 <- meskie_pierwsze_2022_woj %>% 
  group_by(Imię = IMIĘ.PIERWSZE) %>% 
  summarise(Liczba = sum(LICZBA.WYSTĄPIEŃ)) %>% 
  mutate(Rok = 2022, Płeć = "M") %>% 
  relocate(Rok, .before = everything())
bind_rows(imiona2000_2019, meskie_pierwsze_2020, meskie_pierwsze_2021, meskie_pierwsze_2022)

meskie_pierwsze_2023_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2023_męskie_imię_pierwsze.csv")
meskie_pierwsze_2023 <- meskie_pierwsze_2023_woj %>% 
  group_by(Imię = IMIĘ_PIERWSZE) %>% 
  summarise(Liczba = sum(LICZBA_WYSTĄPIEŃ)) %>% 
  mutate(Rok = 2023, Płeć = "M") %>% 
  relocate(Rok, .before = everything())
bind_rows(imiona2000_2019, meskie_pierwsze_2020, meskie_pierwsze_2021, meskie_pierwsze_2022, meskie_pierwsze_2023)

meskie_pierwsze_2024_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2024_męskie_imię_pierwsze.csv")
meskie_pierwsze_2024 <- meskie_pierwsze_2024_woj %>% 
  group_by(Imię = IMIĘ_PIERWSZE) %>% 
  summarise(Liczba = sum(LICZBA_WYSTĄPIEŃ)) %>% 
  mutate(Rok = 2024, Płeć = "M") %>% 
  relocate(Rok, .before = everything())
imiona_2000_2024 <- bind_rows(imiona2000_2019, meskie_pierwsze_2020, meskie_pierwsze_2021, meskie_pierwsze_2022, meskie_pierwsze_2023, meskie_pierwsze_2024)

zenskie_pierwsze_2020_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2020_żeńskie_imię_pierwsze.csv")
View(zenskie_pierwsze_2020_woj)
zenskie_pierwsze_2020 <- zenskie_pierwsze_2020_woj %>% 
  group_by(Imię = IMIĘ_PIERWSZE) %>% 
  summarise(Liczba = sum(LICZBA_WYSTĄPIEŃ)) %>% 
  mutate(Rok = 2020, Płeć = "K") %>% 
  relocate(Rok, .before = everything())
bind_rows(imiona_2000_2024, zenskie_pierwsze_2020)

zenskie_pierwsze_2021_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2021_żeńskie_imię_pierwsze.csv")
zenskie_pierwsze_2021 <- zenskie_pierwsze_2021_woj %>% 
  group_by(Imię = IMIĘ_PIERWSZE) %>% 
  summarise(Liczba = sum(LICZBA_WYSTĄPIEŃ)) %>% 
  mutate(Rok = 2021, Płeć = "K") %>% 
  relocate(Rok, .before = everything())
bind_rows(imiona_2000_2024, zenskie_pierwsze_2020, zenskie_pierwsze_2021)

zenskie_pierwsze_2022_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2022_żeńskie_imię_pierwsze.csv")
zenskie_pierwsze_2022 <- zenskie_pierwsze_2022_woj %>% 
  group_by(Imię = IMIĘ.PIERWSZE) %>% 
  summarise(Liczba = sum(LICZBA.WYSTĄPIEŃ)) %>% 
  mutate(Rok = 2022, Płeć = "K") %>% 
  relocate(Rok, .before = everything())
bind_rows(imiona_2000_2024, zenskie_pierwsze_2020, zenskie_pierwsze_2021, zenskie_pierwsze_2022)

zenskie_pierwsze_2023_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2023_żeńskie_imię_pierwsze.csv")
zenskie_pierwsze_2023 <- zenskie_pierwsze_2023_woj %>% 
  group_by(Imię = IMIĘ_PIERWSZE) %>% 
  summarise(Liczba = sum(LICZBA_WYSTĄPIEŃ)) %>% 
  mutate(Rok = 2023, Płeć = "K") %>% 
  relocate(Rok, .before = everything())
bind_rows(imiona_2000_2024, zenskie_pierwsze_2020, zenskie_pierwsze_2021, zenskie_pierwsze_2022, zenskie_pierwsze_2023)

zenskie_pierwsze_2024_woj <- read.csv("eksploracja danych projekt 1/imiona_projket/2024_żeńskie_imię_pierwsze.csv")
zenskie_pierwsze_2024 <- zenskie_pierwsze_2024_woj %>% 
  group_by(Imię = IMIĘ_PIERWSZE) %>% 
  summarise(Liczba = sum(LICZBA_WYSTĄPIEŃ)) %>% 
  mutate(Rok = 2024, Płeć = "K") %>% 
  relocate(Rok, .before = everything())
imiona_2000_2024 <- bind_rows(imiona_2000_2024, zenskie_pierwsze_2020, zenskie_pierwsze_2021, zenskie_pierwsze_2022, zenskie_pierwsze_2023, zenskie_pierwsze_2024)

View(imiona_2000_2024)

imiona_2000_2024 %>% filter(Płeć == "M", Imię == "JAKUB") %>% 
  ggplot(aes(x = Rok, y = Liczba)) + geom_point(color = "skyblue") + 
  geom_line(color = "skyblue") +
  labs(title = "Wykres popularności imienia JAKUB na przestrzeni lat", 
       subtitle = "2000 - 2024", 
       x = "Rok", 
       y = "Liczba wystąpień") 

imiona_2000_2024 %>% filter(Płeć == "K", Rok == 2024) %>% arrange(-Liczba) %>% 
  head(10)
imiona_2000_2024 %>% filter(Płeć == "K", Rok == 2000) %>% arrange(-Liczba) %>% 
  head(10)
imiona_2000_2024 %>% filter(Płeć == "K", Rok == 2004) %>% arrange(-Liczba) %>% 
  head(10)

imiona_2000_2024 %>% filter(Płeć == "K", Imię == "MAJA") %>% 
  ggplot(aes(x = Rok, y = Liczba)) + geom_point(color = "hotpink") + 
  geom_line(color = "hotpink") +
  labs(title = "Wykres popularności imienia MAJA na przestrzeni lat", 
       subtitle = "2000 - 2024", 
       x = "Rok", 
       y = "Liczba wystąpień") 
imiona_2000_2024 %>% filter(Płeć == "K", Imię == "NATALIA") %>% 
  ggplot(aes(x = Rok, y = Liczba)) + geom_point(color = "hotpink") + 
  geom_line(color = "hotpink") +
  labs(title = "Wykres popularności imienia NATALIA na przestrzeni lat", 
       subtitle = "2000 - 2024", 
       x = "Rok", 
       y = "Liczba wystąpień")
imiona_2000_2024 %>% filter(Płeć == "K", Imię == "JULIA") %>% 
  ggplot(aes(x = Rok, y = Liczba)) + geom_point(color = "hotpink") + 
  geom_line(color = "hotpink") +
  labs(title = "Wykres popularności imienia JULIA na przestrzeni lat", 
       subtitle = "2000 - 2024", 
       x = "Rok", 
       y = "Liczba wystąpień") 

?ggplot

Liczba_urodzen_w_roku <- imiona_2000_2024  %>% group_by(Rok, Płeć) %>% 
  summarise(Liczba_urodzen_w_roku = sum(Liczba))
Liczba_urodzen_w_roku %>% ggplot(aes(Rok, Liczba_urodzen_w_roku, color = Płeć)) + 
  geom_point() + 
  labs(title = "Liczba urodzeń w Polsce (2000-2024)",
       x = "Rok", y = "Liczba urodzeń") +
  scale_color_manual(values = c("M" = "skyblue", "K" = "hotpink"))


inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>% 
  filter(Imię %in% c("IGA", "FILIP")) %>% ggplot(aes(Rok, udzial, color = Płeć)) + 
  geom_line() +
  geom_point() + labs(title = "Popularność imion", 
                      subtitle = "2000-2024", 
                      x = "Rok", 
                      y = "Procentowy udział") +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(name = "Imię",
                     values = c("M" = "skyblue", "K" = "hotpink"),
                     labels = c("M" = "Hubert", "K" = "Iga"))

inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>% arrange(-udzial)



inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>% 
  filter(Imię %in% c("KLARA", "LAURA")) %>% ggplot(aes(Rok, udzial, color = Imię)) + 
  geom_line() +
  geom_point() + labs(title = "Popularność imion", 
                      subtitle = "2000-2024", 
                      x = "Rok", 
                      y = "Procentowy udział") +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(name = "Imię",
                     values = c("KLARA" = "skyblue", "LAURA" = "forestgreen"))

library(tidyr)

#bump_chart


ranking_dziewczynki <- inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>% 
  group_by(Rok, Imię) %>%
  arrange(Rok, desc(Liczba)) %>%  
  group_by(Rok, Płeć) %>%
  slice_max(Liczba, n = 5) %>% filter(Płeć == "K") %>% 
  mutate(Rank = row_number()) %>% 
  select(-c(Liczba, Liczba_urodzen_w_roku, udzial, Płeć)) %>% 
  pivot_wider(names_from = Rank, values_from = Imię)

install.packages("ggbump")
library(ggbump)

rank_k_pomoc <- inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>% 
  group_by(Rok, Imię) %>%
  arrange(Rok, desc(Liczba)) %>%  
  group_by(Rok, Płeć) %>%
  slice_max(Liczba, n = 10) %>% filter(Płeć == "K") %>% 
  mutate(Rank = row_number()) %>% 
  mutate(color = case_when(Imię == "JULIA" ~ 'deeppink2',
                           Imię == "NATALIA" ~ 'maroon',
                            Imię == "ZOFIA" ~ "azure4",
                           TRUE ~ "#E7E7E7"
                           )) %>% 
  filter(Imię != "KATARZYNA" & Imię != "PATRYCJA" & Imię != "PAULINA" 
         & Imię != "KLAUDIA" & Imię != "MARIA" & Imię != "ALICJA"
         & Imię != "POLA" & Imię != "OLIWIA" & Imię != "MARTYNA"
         & Imię != "AMELIA" & Imię != "NIKOLA")
  #select(-c(Liczba, Liczba_urodzen_w_roku, udzial, Płeć)) 

rank_k <- rank_k_pomoc %>% 
  ggplot(aes(x = Rok, y = Rank, group = Imię, color = I(color))) + 
  geom_bump(size = 0.5) +
  theme_minimal()+
  theme(panel.grid=element_blank()) +
  geom_text(data = rank_k_pomoc %>% group_by(Imię) %>% filter(Rok == min(Rok)), aes(label = Imię), nudge_x = -0.5, nudge_y = 0.2, fontface = "bold", size=2, color = "mistyrose4") +
  geom_text(data = rank_k_pomoc %>% group_by(Imię) %>% filter(Rok == max(Rok)), aes(label = Imię), nudge_x = 0.5, nudge_y = 0.2, fontface = "bold", size=2, color = "mistyrose4") +
  theme(legend.position = "none") %>% 
  labs(title = "Wykres popularności imion żeńskich na przestrzeni lat", 
       subtitle = "2000 - 2024", 
       x = "Rok", 
       y = "Miejsce w rankingu") +
  scale_y_reverse(breaks = 1:10) +
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020, 2024))
  
ggsave("rank_k.png", plot = rank_k, bg = "transparent", width = 8, height = 6, dpi = 300)

install.packages("gridExtra")
library(gridExtra)
library(grid)

png("ranking_dziewczynki.png", width = 2200, height = 2200, res = 300)
grid.table(ranking_dziewczynki)
dev.off()

rank_m_pomoc <- inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>% 
  group_by(Rok, Imię) %>%
  arrange(Rok, desc(Liczba)) %>%  
  group_by(Rok, Płeć) %>%
  slice_max(Liczba, n = 10) %>% filter(Płeć == "M") %>% 
  mutate(Rank = row_number()) %>% 
  group_by(Imię) %>% 
  mutate(progres = max(Rank) - min(Rank)) %>% 
  mutate(color = case_when(Imię == "JAKUB" ~ 'royalblue3',
                           Imię == "JAN" ~ 'blue4',
                           Imię == "KACPER" ~ "azure4",
                           Imię == "NIKODEM" ~ "deepskyblue3",
                           TRUE ~ "#E7E7E7"
  )) %>% 
  filter(Imię != "ADAM" & Imię != "MACIEJ" & Imię != "WOJCIECH" &
           Imię != "WIKTOR", Imię != "PAWEŁ", 
         Imię != "PIOTR", Imię != "STANISŁAW", Imię != "MIKOŁAJ", 
         Imię != "IGNACY", Imię != "KAMIL", Imię != "BARTOSZ", 
         Imię != "SZYMON", Imię != "FILIP")

#select(-c(Liczba, Liczba_urodzen_w_roku, udzial, Płeć)) 
#mutate(color = case_when(progres >= 8 ~ '#0057B8',
#                        progres == 7 ~ '#FDBE11',
#                       TRUE ~ "#E7E7E7"
#)) %>% 

rank_m <- rank_m_pomoc %>% ggplot(aes(x = Rok, y = Rank, group = Imię, color = I(color))) + 
  geom_bump(size = 0.5) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  geom_text(data = rank_m_pomoc %>% group_by(Imię) %>% filter(Rok == min(Rok)), aes(label = Imię), nudge_x = -0.5, nudge_y = 0.2, fontface = "bold", size=2, color = "lightsteelblue4") +
  geom_text(data = rank_m_pomoc %>% group_by(Imię) %>% filter(Rok == max(Rok)), aes(label = Imię), nudge_x = 0.5, nudge_y = 0.2, fontface = "bold", size=2, color = "lightsteelblue4") +
  theme(legend.position = "none") +
  labs(title = "Wykres popularności imion męskich na przestrzeni lat", 
       subtitle = "2000 - 2024", 
       x = "Rok", 
       y = "Miejsce w rankingu") +
  scale_y_reverse(breaks = 1:10) +
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020, 2024))
#theme(panel.grid=element_blank()) +

ggsave("rank_m.png", plot = rank_m, bg = "transparent", width = 8, height = 6, dpi = 300)

print( 
  inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
    mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>% 
    group_by(Rok, Imię) %>%
    arrange(Rok, Liczba) %>%  
    group_by(Rok, Płeć) %>%
    slice_min(Liczba, n = 10) %>% filter(Płeć == "K") %>% 
    mutate(Rank = row_number()) %>% 
    select(-c(Liczba_urodzen_w_roku, udzial, Płeć)) %>% 
    filter(Rok == 2004), 
  #pivot_wider(names_from = Rank, values_from = Imię)
  n = 52
)

inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>%
  filter(Płeć == "K", grepl("KARYNA", Imię) == TRUE) %>% 
  ggplot(aes(y = Imię, x = Liczba)) + geom_col()

inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>%
  filter(Imię == "DOBROSŁAWA") %>% 
  ggplot(aes(y = Liczba, x = Rok)) + geom_col()

inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>%
  filter(Płeć == "K", grepl("SŁAWA", Imię) == TRUE) %>% 
  group_by(Rok) %>% 
  summarise(Suma = sum(Liczba)) %>% 
  ggplot(aes(Rok, Suma)) + geom_line() + geom_point()

inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>%
  filter(Płeć == "M", grepl("SŁAW", Imię) == TRUE) %>% 
  group_by(Rok) %>% 
  summarise(Suma = sum(Liczba)) %>% 
  ggplot(aes(Rok, Suma)) + geom_line() + geom_point()

inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>%
  filter(Płeć == "M", grepl("DONALD", Imię) == TRUE) %>% 
  ggplot(aes(y = Imię, x = Liczba)) + geom_col()

inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>%
  filter(Płeć == "M", grepl("STANISŁAW", Imię) == TRUE) %>% 
  group_by(Rok) %>% 
  summarise(Suma = sum(Liczba)) %>% 
  ggplot(aes(Rok, Suma)) + geom_line() + geom_point()

inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>%
  filter(Płeć == "M", Imię %in% c("PRZEMYSŁAW", "RADOSŁAW")) %>% 
  group_by(Rok) %>% 
  summarise(Suma = sum(Liczba)) %>% 
  ggplot(aes(Rok, Suma)) + geom_line() + geom_point()

liczba_chlopcow_w_xxi <- imiona_2000_2024 %>% 
  filter(Płeć == "M") %>% summarise(sum(Liczba))
imiona_2000_2024 %>% 
  filter(Płeć == "M") %>% 
  group_by(Imię) %>% 
  summarise(imię_suma = sum(Liczba), procent = imię_suma/liczba_chlopcow_w_xxi*100) %>% 
  filter(procent>2) %>% 
  arrange(-procent) %>% ggplot(aes(y = Imię, x = procent)) + geom_col()

print(
  pochodzenie_imion %>% group_by(Pochodzenie) %>% summarise(Imię = n()),
  n = 65)

pochodzenie_imion %>% filter(Pochodzenie == "  polskie")

print(
  inner_join(imiona_2000_2024, pochodzenie_imion, by = "Imię") %>% 
    group_by(Pochodzenie) %>% summarise(Liczba = sum(Liczba)) %>% 
    arrange(-Liczba), 
  n = 60)

pop_6_imion <- inner_join(imiona_2000_2024, Liczba_urodzen_w_roku) %>% 
  mutate(udzial = Liczba/Liczba_urodzen_w_roku*100) %>% 
  filter(Imię %in% c("JAN", "KLARA", "LAURA", "PAWEŁ", "KAROL")) %>% 
  ggplot(aes(Rok, udzial, color = Imię)) + 
  geom_line() +
  geom_point() + 
  theme_minimal() +
  labs(title = "Popularność imion", 
                      subtitle = "2000-2024", 
                      x = "Rok", 
                      y = "Procentowy udział") +
  scale_y_continuous(limits = c(0, 5),  expand = c(0,0)) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020, 2024), expand = c(0,0.1)) +
  scale_color_manual(name = "Imię",
                     values = c("JAN" = "royalblue3", "KLARA" = "deeppink2", 
                                "LAURA" = "maroon", 
                                "PAWEŁ" = "navyblue", "KAROL" = "deepskyblue3")) 

ggsave("pop_6_imion.png", plot = pop_6_imion, bg = "transparent", width = 8, height = 6, dpi = 300)
