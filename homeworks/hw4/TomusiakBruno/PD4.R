



# UWAGA: program, którego użyłem do zczytania nazw krajów z oryginalnej mapy nie zczytał jej w 100% poprawnie.
# Większośc ważnych dla nas krajów została zczytana dobrze, pełna zgodność to kwestia ręcznego przepisania każdego kraju z osobna.
# Nie wpływa to na samą merytorykę uzasadnienia nieprawidłowości, oraz ich poprawek.
# Dlatego jest pewna rozbieżność pomiędzy obiema mapami.




# Oryginalny wykres - źródło: https://www.reddit.com/r/MapPorn/comments/1k48gp1/the_135_cardinal_electors_who_will_participate_in/

# Wykres ma kilka nieprawidłowości. Nazwy wszystkich krajów są zbędne. Można się ograniczyć do krajów, których
# dotyczą wybrane przez nas dane. Podanie nazw krajów, w których nie ma żadnych kardynałów nie ma żadnej merytorycznej wartości.
# Drugą nieprawidłowością jest paleta barw wybrana do wizualizacji wyników. Jest ona zbyt jaskrawa, nieprzyjamna dla oka.
# Powinno się wybrać kolory, na które przyjemnie się patrzy, bardziej stonowane.

library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(gridExtra)
library(tidyr)
library(RColorBrewer)

cols_greens <- colorRampPalette(brewer.pal(9, "Greens"))(17)

df <- read.csv('C:/Users/tomus/Desktop/Conclave.csv') %>% 
  rename(region = 'country')

df <- df %>%
  mutate(region = recode(region,
                          "United States" = "USA",
                          "United Kingdom" = "UK",
                        
  )) %>% 
  select(value, region)
examples <- df %>% 
  filter(value >= 5)
table <- tableGrob(examples)

w2hr <- map_data("world")

w2hr %>% 
  select(region) %>% 
  distinct(region)


data_conclave <- left_join(w2hr, df, by = c('region')) %>% 
  group_by(region)
  

conclave_map <- ggplot(data_conclave, aes(x = long, y = lat, group = group, fill = value))+
  geom_polygon(color = "black", size = 0.2)+
  coord_fixed(1.3)+
  labs(title = 'Conclave 2025 - electing new pope',
       subtitle = 'The 135 cardinal electors who will participate in the 2025 papal conclave,
       to elect a new pope to succeed Francis')+
  theme_void()+
  scale_fill_gradientn(colours = cols_greens,
                       na.value = 'lightgrey',
                       name = 'Number of electors'
                       )+
  theme(legend.position = "right",
        panel.background = element_rect(fill = '#E6E6FA', color = '#E6E6FA'),
        plot.background = element_rect(fill = "#E6E6FA"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12,face = 'italic', hjust = 0.5),
        legend.title = element_text(size = 10, face = "bold")
  )
conclave_map

# W poprawionym wykresie nie zdecydowałem się na nazwy krajów, co w moim odczuciu wychodzi wizualnie na plus.
# Na samej merytoryce nic nie straciliśmy, bo większośc obecnych tutaj krajów jest ludziom bardzo dobrze znana,
# a znajomość statystyki dla krajów mniej znanych nie jest czymś ważnym. Mapa ma pokazać jej globalny rozkład.
# Przykładowo - użytkownika nie będzie interesować to, czy 1 kardynała ma Gibon, czy jednak może Nigeria.
# Bardziej go zainteresuje, że 1 kardynała ma kraj w afryce środkowej.
# Zamieniłem paletę barw na bardziej stonowaną, która jest przyjemniejsza dla oka, z takiej mapy dużo lepiej się użytkuje.











