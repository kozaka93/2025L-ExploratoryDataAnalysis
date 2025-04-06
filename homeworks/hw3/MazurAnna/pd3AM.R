library(sf)
library(spData)
library(ggplot2)
library(dplyr)
library(stringr)

data("world")

life_exp <- read.csv('life_exp.csv') %>%
  select(1, 2, 5) %>%
  rename(
    name = 1,
    year = 2,
    life_expectancy_at_birth = 3
  ) %>% 
  filter(year == ' 2019') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(name = case_when(
    str_detect(name, "Bolivia") ~ "Bolivia",
    str_detect(name, "Venezuela") ~ "Venezuela",
    TRUE ~ name))

south_am_fr <- world %>% 
  filter(continent == "South America" | iso_a2 == "FR") %>% 
  left_join(life_exp, by = join_by(name_long == name)) 
  
ggplot(data = south_am_fr) +
  geom_sf(aes(fill = lifeExp, colour = "")) +
  scale_fill_fermenter(palette = 1,
                       trans = "log", 
                       direction = 1,
                       na.value = "gray")+
  scale_colour_manual(values = "black")+
  guides(colour=guide_legend("No data", override.aes=list(colour="gray")))+
  theme_minimal() +
  labs(fill = "Life Expectancy (in years)")+
  coord_sf(xlim = c(-85, -35), ylim = c(-55, 12))+
  ggtitle("Life expectancy for women at birth in 2019") 
  

