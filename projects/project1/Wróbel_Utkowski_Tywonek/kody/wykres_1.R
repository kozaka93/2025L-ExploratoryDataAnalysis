library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
options(scipen=999)

ludnosc_grupy_2021 <- read.csv("kody/ludn_gr_2021.csv", sep=';')
ludnosc_grupy_2002 <- read.csv("kody/ludn_gr_2002.csv", sep=';')

ludnosc_grupy_2021 <- ludnosc_grupy_2021 %>% 
  select(!c(1, 3:22, mężczyźni.ogółem.2021..osoba., kobiety.ogółem.2021..osoba.)) %>%
  pivot_longer(!Nazwa, names_to = "przedzial", values_to = "licznosc") %>%
  mutate(plec = as.numeric((str_extract(przedzial, "^[^.]+")) == "mężczyźni")) %>%
  filter(!is.na(licznosc))

ludnosc_grupy_2021 <- ludnosc_grupy_2021 %>%
  mutate(dolna_granica_wieku = (ludnosc_grupy_2021$przedzial %>% str_extract("\\d+")))

suma_85_90 <- ludnosc_grupy_2021 %>%
  filter(dolna_granica_wieku %in% c(85, 90)) %>%
  group_by(plec) %>%
  summarise(licznosc = sum(licznosc)) %>%
  mutate(dolna_granica_wieku = c("85", "85"))

ludnosc_grupy_2021 <- ludnosc_grupy_2021 %>%
  filter(dolna_granica_wieku != "90") %>%
  mutate(licznosc = case_when((plec == 0 & dolna_granica_wieku == "85") ~ 578591,
                              (plec == 1 & dolna_granica_wieku == "85") ~ 222470,
                              TRUE ~ licznosc))



ludnosc_grupy_2002 <- ludnosc_grupy_2002 %>%
  select(!c(1, 3:90, kobiety.ogółem.2002..osoba.)) %>%
  pivot_longer(!Nazwa, names_to = "wiek", values_to = "licznosc") %>%
  mutate(dolna_granica_wieku = as.numeric(str_extract(wiek, "\\d+"))) %>%
  mutate(plec = as.numeric((str_extract(wiek, "^[^.]+")) == "mężczyźni")) %>%
  filter(!is.na(dolna_granica_wieku))

ludnosc_grupy_2002 <- ludnosc_grupy_2002 %>%
  mutate(dolna_granica_przedzialu = floor(dolna_granica_wieku / 5) * 5)

ludnosc_grupy_2002 <-  ludnosc_grupy_2002 %>%
  group_by(plec, dolna_granica_przedzialu) %>%
  summarise(licznosc = sum(licznosc)) %>%
  mutate(dolna_granica_przedzialu = as.character(dolna_granica_przedzialu))

ludnosc_2002_2021 <- ludnosc_grupy_2002 %>%
  full_join(ludnosc_grupy_2021, by = c("dolna_granica_przedzialu" = "dolna_granica_wieku", "plec" = "plec"))

ludnosc_2002_2021 %>%
  group_by(dolna_granica_przedzialu) %>%
  summarise(licznosc.x = sum(licznosc.x), licznosc.y = sum(licznosc.y)) %>%
  mutate(dolna_granica_przedzialu = factor(as.numeric(dolna_granica_przedzialu))) %>%
  ggplot() +
  geom_segment( aes(x=dolna_granica_przedzialu, xend=dolna_granica_przedzialu, y=licznosc.x, yend=licznosc.y), color="black", size=1) +
  geom_point(aes(x=dolna_granica_przedzialu, y=licznosc.x, color="2002"), size=3.5) +
  geom_point(aes(x=dolna_granica_przedzialu, y=licznosc.y, color="2021"), size=3.5) +
  labs(color = element_blank(),
       y = "liczba ludności",
       x = "dolna granica wieku",
       title = "Zmiana struktury wieku ludności w Polsce",
       subtitle = "pomiędzy 2002 a 2021 rokiem") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = c(0.93, 0.92)) +
  scale_color_manual(values = c("2002" = "navy", "2021" = "deepskyblue2"))

