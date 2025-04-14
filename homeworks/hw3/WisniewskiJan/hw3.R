library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(SmarterPoland)
install.packages("patchwork")
install.packages("grid")
install.packages("gridExtra")
library(patchwork)



covid <- read.csv("covid.csv")
unique(covid$Date) # liczba zmarlych od 22 stycznia 2020 roku do 16 kwietnia 2022 roku
deaths <- covid %>% 
  group_by(Country) %>% 
  summarise(n = sum(Deaths))
  

df1 <- map_data("world")
df <- df1 %>% 
  left_join(countries, by=c("region"= "country")) %>% 
  select(long, lat, group, order,region, population, continent)

Europe <- df %>% 
  filter(continent=="Europe") %>% 
  filter(long < 180)


Europe2 <- Europe %>%
  left_join(deaths, by = c("region" = "Country")) %>% 
  mutate(deaths = n/10000) %>% 
  select(long,lat,group, order, region, deaths)

w1_deaths <- ggplot(Europe2, aes(x = long, y = lat, group = group, fill = deaths)) +
  geom_polygon(color = "black") +
  theme_void() +  
  coord_map("mollweide") +  
  scale_fill_gradient2(
    low = "white", high = "red", mid = "yellow",  
    midpoint = median(Europe2$deaths, na.rm = TRUE),  
    na.value = "gray"  
  ) +
  labs(title = "Rozkład zgonów w Europie", fill = "Liczba zgonów (log10)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),  
    legend.position = "bottom" 
  )


w1 <- ggplot(Europe, aes(x = long, y = lat, group = group, fill=population/1000)) +
  geom_polygon(color = "black") +
  theme_void()+
  coord_map("mollweide")+
  scale_fill_gradient2(
    low = "white", high = "navyblue", mid = "lightblue",
    midpoint = 1, trans="log2")+
  labs(title = "Rozkład ludności w poszczególnych krajach Europy")+
  theme(plot.title = element_text(hjust = 0.5, size = 14))

kolejnosc <- Europe %>%
  distinct(region, .keep_all = TRUE) %>%
  arrange(desc(population)) %>% 
  mutate(n = row_number())

w1 + geom_point(data = kolejnosc, aes(x = long, y = lat), color = "black", size = 5) +
  geom_text(data = kolejnosc, aes(x = long, y = lat, label = n), color = "yellow")

centroids <- Europe %>%
  group_by(region) %>%
  summarise(
    long = mean(long, na.rm = TRUE),  
    lat = mean(lat, na.rm = TRUE),    
    population = first(population)    
  ) %>%
  arrange(desc(population)) %>%  
  mutate(n = row_number()) %>% 
  as.data.frame() 


centroids_full <- Europe %>%
  select(region, group, order) %>%  
  distinct(region, .keep_all = TRUE) %>%  
  left_join(centroids, by = "region") %>% 
  arrange(desc(n)) %>% 
  select(long, lat, group, order, region, population, n)
  

w2 <- w1 + geom_point(data = centroids_full, aes(x = long, y = lat), color = "black", size = 5) +
  geom_text(data = centroids_full, aes(x = long, y = lat, label = n), color = "yellow")
w3 <- w2+ labs(subtitle="Państwa w kolejności malejącej pod względem liczby mieszkańców") + 
  theme(plot.subtitle = element_text(hjust = 0.5))
w3 <- w2 + 
  labs(fill = "log2(population[mln]")
w3 + w1_deaths
w3 + w1_deaths + plot_layout(widths = c(1.75, 1))

