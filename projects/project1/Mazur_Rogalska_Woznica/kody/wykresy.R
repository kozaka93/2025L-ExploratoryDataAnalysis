library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ggpattern)
library(showtext)
library(ggrepel)
library(tidyverse)


alkohol <- read.csv2("alkohol.csv")
zbiornik <- read.csv2('rodzaj-zbiornika-wodnego.csv')
wiek <- read.csv2('wiek.csv')
ogolne <- read.csv2('ogolne.csv')
okolicznosc <- read.csv2('okoliczność wypadku.csv')

alkohol$Liczba.osób.które.spożyły.alkohol <- as.numeric(alkohol$Liczba.osób.które.spożyły.alkohol)


####### wykres1 #######

#słupkowy -- liczba utoniec + odsetek pijanych

ogolne_i_alkohol <- alkohol %>% 
  left_join(ogolne) %>%
  mutate(trzezwi = Utonięcia - Liczba.osób.które.spożyły.alkohol) %>% 
  select(-c(3,4,5,6,7,8))

ogolne_i_alkohol_long <- ogolne_i_alkohol %>%
  pivot_longer(cols = c(trzezwi, Liczba.osób.które.spożyły.alkohol), 
               names_to = "Condition", 
               values_to = "Count") %>%
  mutate(Condition = recode(Condition, 
                            "trzezwi" = "trzezwi",
                            "Liczba.osób.które.spożyły.alkohol" = "pijani" 
                            ))
  

ogolne_i_alkohol_long %>%
  mutate(Condition = factor(Condition, levels = c("trzezwi", "pijani"))) %>%
  ggplot(aes(x = as.character(Rok), y = Count, 
             pattern = Condition, 
             fill = Condition)) +
  geom_col_pattern(position = 'stack',
                   pattern_fill = "#006666",  
                   pattern_angle = 45,         
                   pattern_density = 0.05,       
                   pattern_spacing = 0.01,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(
    name = "",
    values = c("trzezwi" = "none", "pijani" = "stripe"),
    labels = c("trzezwi" = "", "pijani" = "w tym pod wpływem alkoholu")) +
  scale_fill_manual(
    name = "",
    values = c("trzezwi" = '#88e1f0',"pijani" = '#88e1f0'),
    labels = c("trzezwi" = "", "pijani" = "w tym pod wpływem alkoholu"),
    guide = "none"
    ) +
  guides(
    fill = guide_legend(override.aes = list(fill = "white"))
  )+
  labs(x = "Rok", 
       y = "Liczba utonięć") +
  theme_minimal()

#### płeć ####

#stosunek utopionych kobiet do utopionych mezczyzn

plec <- ogolne %>% 
  filter(Rok != "2013") %>% 
  mutate(odsetek_kobiet_topielców = as.numeric(Kobiety.wśród.ofiar) / Utonięcia) %>% 
  select(c(Rok, odsetek_kobiet_topielców))

mean(plec$odsetek_kobiet_topielców)

####### wykres 2 #######

#kołowy -- utonięcia wg zbiornika w latach 2013-2023

nowy_zbiornik <- zbiornik %>% 
  group_by(Zbiornik.wodny) %>% 
  summarise(Liczba.utonięć = sum(Liczba.utonięć))


df2 <- nowy_zbiornik %>% 
  mutate(procent_utoniec = round(
    Liczba.utonięć * 100 / sum(Liczba.utonięć), 1)) %>% 
  mutate(csum = rev(cumsum(rev(procent_utoniec))), 
         pos = procent_utoniec/2 + lead(csum, 1),
         pos = if_else(is.na(pos), procent_utoniec/2, pos))
  

ggplot(df2, aes(x = "" , y = procent_utoniec, fill = fct_inorder(Zbiornik.wodny))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values=c('#4957d7', '#4982e9',  '#66bdea', '#88e1f0', '#c7ffff'))+
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(procent_utoniec, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Zbiornik wodny")) +
  theme_void() 

##### wykres 3 #####

#przyczyny utonięć -- dwa wykresy do wyboru

okolicznosc[39,2] <- "Nieostrożność w czasie przebywania nad wodą"
okolicznosc[43,2] <- "Nieostrożność w czasie przebywania nad wodą"
  
#liniowy
ggplot(okolicznosc, aes(x = as.character(Rok), y = Liczba.utonięć, group = Okoliczność))+
  geom_line(aes(color=Okoliczność))+
  geom_point(aes(color=Okoliczność))+
  labs(x = "Rok", y = "Liczba utonięć")+
  scale_color_manual(labels = c(
    "Kąpiel w miejscu niestrzeżonym lecz nie zabronionym" =
    "Kąpiel w miejscu niestrzeżonym, lecz nie zabronionym"),
    values=c('#4982e9',  '#66bdea', '#88e1f0', '#c7ffff'))+
  theme_minimal()

#area plot?
ggplot(okolicznosc, aes(x = as.character(Rok), y = Liczba.utonięć, group = Okoliczność))+
  geom_area(aes(color=Okoliczność, fill = Okoliczność))+
  scale_color_manual(labels = c(
    "Kąpiel w miejscu niestrzeżonym lecz nie zabronionym" =
      "Kąpiel w miejscu niestrzeżonym, lecz nie zabronionym"),
    values=c('#4982e9',  '#66bdea', '#88e1f0', '#c7ffff'))+
  scale_fill_manual(labels = c(
    "Kąpiel w miejscu niestrzeżonym lecz nie zabronionym" =
      "Kąpiel w miejscu niestrzeżonym, lecz nie zabronionym"),
    values=c('#4982e9',  '#66bdea', '#88e1f0', '#c7ffff'))+
  labs(x = "Rok", y = "Liczba utonięć")+
  theme_minimal()


##### wykres 4 #####

wiek <- wiek %>% 
  mutate(
    Przedział.wiekowy = gsub("-", "–", Przedział.wiekowy),
    Przedział.wiekowy = gsub(" – ", "–", Przedział.wiekowy),
    Przedział.wiekowy = factor(Przedział.wiekowy, 
                               levels = c("0–7 lat", "8–14 lat", "15–18 lat", "19–30 lat", "31–50 lat", "Powyżej 50 lat")
                               ))

#area plot -- czyli extended liniowy
ggplot(wiek, aes(x = as.character(Rok), y = Liczba.utonięć, group = Przedział.wiekowy))+
  geom_area(aes(color=Przedział.wiekowy, fill = Przedział.wiekowy))+
  scale_color_manual('Przedział wiekowy',values=c('navy',  '#66bdea', '#88e1f0', '#c7ffff', "blue", '#4982e9'))+
  scale_fill_manual('Przedział wiekowy', values=c('navy',  '#66bdea', '#88e1f0', '#c7ffff', "blue", '#4982e9'))+
  labs(x = "Rok", y = "Liczba utonięć")+
  theme_minimal()


#liniowy
ggplot(wiek, aes(x = as.character(Rok), y = Liczba.utonięć, group = Przedział.wiekowy))+
  geom_line(aes(color=Przedział.wiekowy))+
  geom_point(aes(color=Przedział.wiekowy))+
  scale_color_manual('Przedział wiekowy',values=c('#4982e9',  '#66bdea', '#88e1f0', '#c7ffff', "blue", "navy"))+
  labs(x = "Rok", y = "Liczba utonięć")+
  theme_minimal()

