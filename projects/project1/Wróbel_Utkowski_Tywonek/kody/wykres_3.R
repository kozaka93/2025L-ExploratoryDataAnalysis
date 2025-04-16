library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
options(scipen=999)

ur_zg <- read.csv("kody/ur_zg_pn.csv", sep=';')

ur_zg <- ur_zg %>%
  select(!Kod) %>%
  pivot_longer(!Nazwa, names_to = "kategoria", values_to = "licznosc") %>%
  select(!Nazwa)

ur <- ur_zg[1:22, ] %>%
  separate(kategoria, sep="1000", into = c("nic", "rok")) %>%
  select(!nic) %>%
  mutate(rok = as.numeric(str_extract(rok, "\\d+"))) %>%
  rename(urodzenia = licznosc)

zg <- ur_zg[23:44, ] %>%
  separate(kategoria, sep="1000", into = c("nic", "rok")) %>%
  select(!nic) %>%
  mutate(rok = as.numeric(str_extract(rok, "\\d+"))) %>%
  rename(zgony = licznosc)

pn <- ur_zg[45:66, ] %>%
  separate(kategoria, sep="1000", into = c("nic", "rok")) %>%
  select(!nic) %>%
  mutate(rok = as.numeric(str_extract(rok, "\\d+"))) %>%
  rename(przyrost_naturalny = licznosc)

ur_zg_pn <- ur %>%
  left_join(zg, by = ("rok" = "rok")) %>%
  left_join(pn, by = ("rok" = "rok")) %>%
  mutate(urodzenia = as.numeric(gsub(",", ".", urodzenia)),
         zgony = as.numeric(gsub(",", ".", zgony)),
         przyrost_naturalny = as.numeric(gsub(",", ".", przyrost_naturalny)))

malzenstwa <- read.csv("kody/malze_na_tys.csv", sep=';') %>%
  pivot_longer(!c(Kod, Nazwa), names_to = "rok", values_to = "malzenstwa") %>%
  select(!c(Kod, Nazwa)) %>%
  filter(!is.na(malzenstwa)) %>%
  mutate(rok = as.numeric(str_extract(rok, "\\d+")), malzenstwa = as.numeric(gsub(",", ".", malzenstwa)))

ur_zg_pn_malz <- ur_zg_pn %>%
  left_join(malzenstwa, by = ("rok" = "rok"))

ur_zg_pn_malz %>%
  select(!przyrost_naturalny, !malzenstwa) %>%
  pivot_longer(cols = c(urodzenia, zgony), 
               names_to = "category", 
               values_to = "value") %>%
  ggplot(aes(x=rok, y=value, color=category)) +
  geom_point(size=2) +
  geom_line(size=0.5) +
  labs(x = "rok",
       y = "wartość wskaźnika",
       title = "Wskaźnik urodzeń i zgonów na 1000 mieszkańców w Polsce",
       subtitle = "na przestrzeni lat 2002-2023") +
  scale_x_discrete(limits = c(2002, 2005, 2010, 2015, 2020, 2023)) +
  scale_color_manual(values = c("urodzenia" = "deepskyblue2", "zgony" = "navy")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = c(0.15, 0.9)) +
  geom_vline(xintercept = 2020, linetype = "dashed", linewidth = 0.5, color = "darkgrey")

