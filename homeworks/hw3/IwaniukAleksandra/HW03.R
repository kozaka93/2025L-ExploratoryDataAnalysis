library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(jsonlite)

df <- read.csv("https://ourworldindata.org/grapher/average-height-of-women.csv?v=1&csvType=full&useColumnShortNames=true")
View(df)


filtered_df <- df %>% 
  filter(Year == 1977)

filtered_df <- filtered_df %>% 
  rename(Mean_height = Mean.female.height..cm.)

world <- map_data('world')

filtered_df <- filtered_df %>%
  mutate(Entity = recode(Entity,
                         "United States" = "USA",
                         "United Kingdom"="UK",
                         "Czechia"="Czech Republic",  
                         "Cote d'Ivoire"="Ivory Coast" , 
                         "East Timor" = "Timor-Leste",  
                         "Eswatini" = "Swaziland",  
                         "Democratic Republic of Congo" = "Democratic Republic of the Congo",
                         "Congo"="Congo"
                         ))

map_data_joined <- left_join(world, filtered_df, by = c("region" = "Entity"))



ggplot() +
  geom_polygon(data=map_data_joined, aes(x = long, y = lat, group = group, fill = Mean_height),color = "white", linewidth=0.3) +
  scale_fill_viridis_c(option = "magma",direction = -1, na.value = "grey90") +
  theme_minimal() +
  labs(title = "Średni wzrost kobiet urodzonych w 1977 roku",
       subtitle="Pomiar wykonywany w roku w którym kobiety skończyły 18 lat",
       fill = "Wzrost (cm)") +
  theme_void()+ 
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),  
    plot.subtitle = element_text(size = 10, hjust = 0.5)  
  ) +
  coord_fixed(1.1) 

