library(dplyr)
library(ggplot2)
library(maps)
library(tidyr)

data <- read.csv("WHO-COVID-19-global-daily-data.csv") #https://data.who.int/dashboards/covid19/data
population <- read.csv("current-world-population_-8,005,176,000.csv") #https://worldpopulationreview.com/
codes <- read.csv("country-codes.csv")
w <- map_data("world")

data2 <- data %>% 
  group_by(Country) %>% 
  summarise(Cumulative_deaths = max(Cumulative_deaths)) %>%
  left_join(population, by = c("Country" = "country")) %>% 
  mutate(Deaths_per_km2 = Cumulative_deaths/area) %>% 
  select(c("Country", "Deaths_per_km2"))

ret <- w %>% left_join(data2, by = c("region" = "Country"))

ret %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Deaths_per_km2), color = "black") +
  coord_fixed(1.3) + 
  scale_fill_fermenter(palette = "YlGnBu", trans="log2", direction = 1) +
  labs(title = "Gęstość śmierci na COVID-19 (kilometry kwadratowe)",
       subtitle = "w skali logarytmicznej (log2)",
       fill = "gęstość [os/mi^2]")
