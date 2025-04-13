library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

crimes_1979_2019 <- read.csv("crimes_1979_2019.csv")


#filtruje ramke danychh(pokazuje ona ile jakich przestępstw popełniono w danym roku w danym stanie)

crimes <- crimes_1979_2019 %>%
  select(year, state_name,population, motor_vehicle_theft) %>% 
  filter(year >= 2010, year <= 2019, !is.na(motor_vehicle_theft), !is.na(population)) %>%
  # dopasujemy do nazwy tam jest z małej
  mutate(state_name = tolower(state_name)) %>%
  #biore średnią z tych lat kradzieży pojazdów na stan
  group_by(state_name) %>%          
    summarise(kradzieże_pojazdów = sum(motor_vehicle_theft) / sum(population) * 100000) %>% 
  #zaokrąglam w dół
  mutate(kradzieże_pojazdów = floor(kradzieże_pojazdów))      


#biore mapkę i joinuje
usa_map <- map_data("state")
crimes_2 <- usa_map %>%
  left_join(crimes, by = c("region" = "state_name"))


#wyznaczam max i min
max_state <- crimes %>% filter(kradzieże_pojazdów == max(kradzieże_pojazdów))
min_state <- crimes %>% filter(kradzieże_pojazdów == min(kradzieże_pojazdów))

wybrane_states <- crimes_2 %>%
  filter(region %in% c(max_state$state_name, min_state$state_name))

#robie wykres

ggplot() +
  geom_polygon(data = crimes_2, aes(x = long, y = lat, group = group, fill = kradzieże_pojazdów), color = "white") +
  geom_polygon(data = wybrane_states, aes(x = long, y = lat, group = group), 
               fill = NA, color = "red", size = 1.2) +
  
  scale_fill_gradient2(
    low = "lightcyan",
    mid = "RoyalBlue",
    high = "navy",
    midpoint = 300) +
  coord_fixed(1.35) +
  theme_minimal() +
  labs(
    title = "Skradzione pojazdy w USA",
    subtitle = "Średnia z lat 2010–2019 na 100 tys. mieszkańców",
    fill = "",
    x = "", y = "") +
  theme(
    plot.title = element_text(color = "navy", hjust = 0.5, size = 20),
    plot.subtitle = element_text(color = "navy", hjust = 0.5, size = 14),
    plot.background = element_rect(fill = "LavenderBlush", color = NA),
    panel.background = element_rect(fill = "LavenderBlush", color = NA),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey85")) +
  
  #robie podpis w rogu max i min
  annotate("text", x = -125, y = 26,
           label = paste0("Najwięcej: ", toupper(max_state$state_name), 
                          "\n", max_state$kradzieże_pojazdów, " / 100 tys."),
           hjust = 0, size = 4, color = "navy") +
  
  annotate("text", x = -125, y = 29,
           label = paste0("Najmniej: ", toupper(min_state$state_name), 
                          "\n", min_state$kradzieże_pojazdów, " / 100 tys."),
           hjust = 0, size = 4, color = "navy")
