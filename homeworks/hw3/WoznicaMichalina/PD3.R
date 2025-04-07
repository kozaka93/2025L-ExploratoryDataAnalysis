library(dplyr)
library(tidyr)
library(ggplot2)
library(SmarterPoland)
library(maps)
library(mapdata)
library(scales)
library(gridExtra)
library(grid)

capitals_long_lat<- read.csv(file = "https://gist.githubusercontent.com/ofou/df09a6834a8421b4f376c875194915c9/raw/355eb56e164ddc3cd1a9467c524422cb674e71a9/country-capital-lat-long-population.csv", 
                             encoding = "UTF-8")
df<- read.csv(file = "https://raw.githubusercontent.com/KajetanParzyszek/Eurostat_cars_project_PowerBI/refs/heads/main/CSV%20files%20for%20visualization/new_cars.csv", 
              encoding = "UTF-8")
population <- read.csv(file = "https://gist.githubusercontent.com/alex4321/0a2da1d87205a6c29f0d4235e9523565/raw/ce9fa5637a670e846a37d9a3d9bf36aea547314d/world-population.csv", 
                       encoding = "UTF-8")

df <- df %>%
  mutate(Country = case_when(
    Country == "Czechia" ~ "Czech Republic",
    Country == "Turkiye" ~ "Turkey",
    TRUE ~ Country 
  ))

w1 <- map_data("world")

eu_countries <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", 
                  "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", 
                  "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", 
                  "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
                  "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", 
                  "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", 
                  "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", 
                  "Poland", "Portugal", "Romania", "Russia", "San Marino", 
                  "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", 
                  "Turkey", "Ukraine", "United Kingdom", "Vatican City")
capitals_long_lat <- capitals_long_lat %>% 
  rename(Capital = Capital.City, lat = Latitude, long  = Longitude) %>% 
  select(Capital, Country, lat, long) %>% 
  add_row(Capital = "Pristina", Country = "Kosovo", lat = 42.67, long = 21.17) %>% 
  add_row(Capital = "Prague", Country = "Czech Republic", lat = 50.0755, long = 14.4378) %>% 
  add_row(Capital = "Chisinau", Country = "Moldova", lat = 47.0105, long = 28.8638) %>% 
  add_row(Capital = "Skopje", Country = "North Macedonia", lat = 41.9981, long = 21.4254) %>% 
  add_row(Capital = "Moscow", Country = "Russia", lat = 55.7558, long = 37.6173) %>% 
  add_row(Capital = "Vatican City", Country = "Vatican City", lat = 41.9029, long = 12.4534) %>% 
  filter(Country %in% eu_countries)

population <- population %>% 
  rename(Country = Country.Territory, Population = X2022.Population) %>% 
  select(Population, Country, Capital) %>% 
  add_row(Capital = "Pristina", Country = "Kosovo", Population = 1800000) %>% 
  filter(Country %in% eu_countries) 

df_new <- df %>% 
  filter(Year == 2022) %>% 
  left_join(population, by = "Country")

df2 <- df %>% 
  group_by(Country) %>% 
  filter(Year == 2022) %>%
  summarise(sum(New.cars)) %>% 
  rename(sum = 2) %>% 
  left_join(df_new, by = "Country") %>% 
  mutate(percentage = (New.cars/sum)*100) %>% 
  select(Country, Motor.energy, percentage)

eu_data <- w1 %>% 
  filter(region %in% eu_countries) %>% 
  rename(Country =region) %>% 
  left_join(population, by = "Country") %>% 
  mutate(log_population = 
          log(Population)) %>%  
  ggplot()+
  geom_polygon(aes(x = long, y = lat, group = group, fill = log_population), color = "black") +
  theme_void() + 
  coord_cartesian(xlim = c(-25, 45), ylim = c(35, 72)) +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       midpoint = log(5000000))+
  labs(fill = "Log population")+
  theme(legend.position = "bottom")

options(scipen = 12)
eu_data

df3 <- df_new %>% 
  filter(Motor.energy == 'Diesel') %>%
  rename(region = Country) %>% 
  left_join(w1, by = "region") %>% 
  mutate(logs = log(New.cars))

plot <- ggplot(df3, aes(x = long, y = lat, group = group, fill = logs)) +
  geom_polygon(color = "white",  linewidth = 0.1) +
  coord_fixed(ratio = 1.5) + 
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),  
        legend.title = element_text(size = 10),  
        plot.title = element_text(size = 10),  
        strip.text = element_text(size = 8), 
        plot.margin = unit(c(0, 0, 0, 0), "cm") ) +
  scale_fill_viridis_c(option = "D", na.value = "black") + 
  theme(legend.position = "bottom")+
  labs(title = NULL, fill = "Log cars")
plot
grid.arrange(
  top = textGrob("EU Diesel Cars Distribution in 2022", gp = gpar(fontsize = 16, fontface = "bold")),  
  plot, 
  eu_data, 
  ncol = 2, 
  nrow = 1)









pie <- df %>% 
  filter(Year == 2022) %>%
  group_by(Motor.energy) %>% 
  summarise(suma = sum(New.cars)) %>% 
  mutate(new = (suma/sum(suma))*100)

pie_chart <- ggplot(pie, aes(x = "", y = new, fill = Motor.energy)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  theme_void() + 
  scale_fill_brewer(palette = "Set2") +
  guides(fill = guide_legend(title = "Type"))+
  theme(legend.position = "bottom")

grid.arrange(
  top = textGrob("EU population and Car Energy Distribution in 2022", gp = gpar(fontsize = 16, fontface = "bold")), 
  eu_data, 
  ncol = 2, 
  nrow = 1,
  widths = c(1, 3)
)

