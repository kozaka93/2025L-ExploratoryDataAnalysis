library(tmap)
library(dplyr)
library(readr)
library(RColorBrewer)
library(maps)
library(mapdata)
library(ggplot2)
library(xlsx)


GDP_data <- read.csv("/Users/misiaduda/Downloads/gdp-per-capita-worldbank/gdp-per-capita-worldbank.csv")
Women_parliament_data <- read.csv("/Users/misiaduda/Downloads/share-of-women-in-parliament/share-of-women-in-parliament.csv")

GDP_data<-GDP_data %>%
  filter(Year == 2023)

Women_parliament_data<-Women_parliament_data %>%
  filter(Year == 2023)



world_map <- map_data("world")
europe_map <- world_map %>%
  filter(region %in% c("Austria", "Belgium", "Bulgaria", "Croatia", "Czech Republic", 
                       "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                       "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", 
                       "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", 
                       "Romania", "Slovakia", "Slovenia", "Spain", "Sweden" 
                    ))
europe_mapa <- ggplot() + 
  geom_polygon(data = europe_map, aes(x = long, y = lat, group = group))

combined_data <- left_join(GDP_data, Women_parliament_data, by = "Entity")

europe_data <- europe_map %>%
  left_join(combined_data, by = c("region" = "Entity"))

colnames(europe_data)


labels <- europe_data %>%
  group_by(region) %>%
  summarize(long = mean(long), lat = mean(lat),
            women_parliament = first(Lower.chamber.female.legislators..aggregate..average.))


ggplot(europe_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = GDP.per.capita..PPP..constant.2021.international...), color = "black") +
  geom_label(
    data = labels,
    aes(x = long, y = lat, label = paste0(round(women_parliament, 1))),
    fill = "darkblue", color = "white", size = 2.5, inherit.aes = FALSE
  ) +
  scale_fill_gradient(low = "#fcb4b3", high = "#990000", name = "GDP per capita ($)") +
  coord_fixed(ratio = 1.5, xlim = c(-25, 40), ylim = c(35, 70)) +
  theme_minimal() +
  labs(title = "Unia Europejska: PKB per capita oraz liczba kobiet w parlamencie (%) w roku 2023",
       caption = "Źródło: World Bank") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_blank(),  
    axis.title = element_blank(), 
    plot.margin = margin(0, 0, 0, 0) 
    
  )
  )













