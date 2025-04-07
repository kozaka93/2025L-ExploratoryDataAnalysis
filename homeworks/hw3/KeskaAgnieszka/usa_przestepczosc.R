library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
df <- read.csv("state_crime.csv")
usa <- map_data("usa")

df1 <- df %>% filter(Year<= 2019 & Year >= 2015) %>% 
  select(State,Year,Data.Population,Data.Totals.Violent.All,Data.Totals.Property.All) %>%
  mutate(Totals.Crimes.All = Data.Totals.Property.All + Data.Totals.Violent.All) %>%
  group_by(State) %>% 
  summarise(Mean.Population = mean(Data.Population),Mean.Totals.Crimes.All = mean(Totals.Crimes.All)) %>% 
  data.frame() %>% mutate(Rate.Crimes = Mean.Totals.Crimes.All*1000/Mean.Population)%>% mutate(region  = tolower(State))


states <- map_data("state")
ca<- left_join(states,df1,"region")
ggplot(data = ca) +
  geom_polygon(aes(x = long, y = lat, fill = Rate.Crimes, group = group), color = "white") +
  coord_map("albers", 25, 50) + labs(x =NULL ,y = NULL) +
  theme(legend.position = "right") +
  labs(title = "Średnia liczba przestępstw na 1000 mieszkańców w USA",
       subtitle = "2015-2019",
       fill = "Przestępstwa") +
  scale_fill_gradient(low = "lightblue", high = "navyblue") +theme_minimal()


