library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(tidyr)
library(sf)

df <- read.csv('C:/Users/tomus/Desktop/IMDb/imdb.csv')

df %>% 
  separate_rows(Country, sep = '/') %>% 
  group_by(Country) %>% 
  summarise(avg_rating = mean(Rating)) -> df_avg

df_avg <- df_avg %>%
  mutate(Country = recode(Country,
                          "United States" = "USA",
                          "United Kingdom" = "UK"
                          ))


w2hr <- map_data("world")

data_world <- left_join(w2hr, df_avg, by = c('region'='Country')) %>% 
  group_by(region)

world_map <- ggplot(data_world, aes(x = long, y = lat, group = group, fill = avg_rating)) +
  geom_polygon(color = "black", size = 0.2) +
  coord_quickmap(xlim = c(-20, 45), ylim = c(35, 72)) +
  labs(fill = "Avg. Rating [1-10]",
       title = "Europe: Average Imdb Movie Rating",
       caption = 'Source: Imdb.com') +
  theme_void() +
  scale_fill_gradient2(low = "skyblue",
                       mid = "white",
                       high = "firebrick",
                       midpoint = 6.5)+
  theme(legend.position = "right",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 10, face = "bold")
       )


world_map



