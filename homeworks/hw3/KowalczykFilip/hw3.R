wrld<-read.csv("C:/Users/filip/Downloads/2015.csv")

library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(patchwork)
library(tidyr)

europ<-wrld %>% filter(Region %in% c("Western Europe","Central and Eastern Europe"))%>% rename(region=Country)

europe_map <- map_data("worldHires") %>%
  filter(long > -30, long < 50, lat > 30, lat < 75)

europe_map <- europe_map %>%
  left_join(europ, by = "region")

europe_map<-europe_map %>% filter(region %in% c("Poland","Germany","Sweden","Finland","France","Italy","Spain","Portugal","Austria","Switzerland","Hungary","Romania","Bulgaria","Greece","Albania"))

plocina<-ggplot(europe_map, aes(x = long, y = lat, group = group, fill = Happiness.Score)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "red", high = "green", name = "Wartość") +
  theme_minimal() +
  coord_fixed(1.3) +
  labs(title = "Średni poziom szczęścia\ndla wybranych państw europejskich\n(w skali od 0 do 10)") + theme(plot.title = element_text(face = "bold", size = 14))

minifrytkens<-ggplot(europe_map,aes(x %in% c("Economy..GDP.per.Capita.","Family","Health..Life.Expectancy.","Freedom","Trust..Government.Corruption.","Generosity","Dystopia.Residual")))+geom_bar()+facet_wrap(~region)

testing<-europe_map %>% group_by(region) %>% reframe(Economy..GDP.per.Capita.,Family,Health..Life.Expectancy.,Freedom,Trust..Government.Corruption.,Generosity,Dystopia.Residual) %>% distinct()

df_long <- testing %>%
  pivot_longer(cols = -region, names_to = "Składowe", values_to = "Wartość")

minifrytkenz <- ggplot(df_long, aes(x = Składowe, y = Wartość, fill = Składowe)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~region, nrow = 3, ncol = 5) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +  
  labs(title = "Porównanie wartości wskaźników dla krajów (eng)",
       x = "Wskaźniki", y = "Wartość")

plocina+minifrytkenz
