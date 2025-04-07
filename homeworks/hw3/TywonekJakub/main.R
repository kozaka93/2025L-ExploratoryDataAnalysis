library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(tidyr)

df <- read.csv("ACSDP5Y2021.DP05-2025-04-07T105344.csv")
df$Label..Grouping.
df[c(2, 10:14),] %>% 
  pivot_longer(-Label..Grouping. ,names_to = "Stan", values_to = "Populacja") %>% 
  mutate(Stan = tolower(gsub("\\.", " ", stringr::str_extract(Stan, ".*(?=\\.\\.)"))),
         Label..Grouping. = stringr::str_trim(Label..Grouping.)) %>% 
  rename(Rodzaj = Label..Grouping.) %>% 
  filter(Stan %in% unique(states$region)) %>% 
  mutate(Populacja = as.numeric(gsub(",", "", Populacja))) %>% 
  pivot_wider(values_from ="Populacja", names_from = "Rodzaj") %>%
  mutate(PopPrac = `20 to 24 years`+`25 to 34 years`+`35 to 44 years`+`45 to 54 years`+`55 to 59 years`, proc = PopPrac/`Total population`*100) %>% 
  select(c(1, 9)) -> df_1

states <- map_data("state")
states %>% 
  left_join(df_1, by = c("region" = "Stan")) -> to_map

ggplot(data = to_map, mapping = aes(x = long, y = lat, group = group)) + 
  coord_map("albers", 25, 50) +
  geom_polygon(color = "black", fill = "gray") +
  theme_minimal() -> map_base

map_base + 
  geom_polygon(data = to_map, aes(fill = proc), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_void() +
  scale_fill_fermenter(palette = 6, trans = "log10", direction = 1, n.breaks = 8) +
  labs(title = "Procent populacji w wieku od 20-60 w poszczególnych stanach",
       subtitle = "w stosunku do całej populacji danego stanu w roku 2021",
       fill = "%") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm") )




