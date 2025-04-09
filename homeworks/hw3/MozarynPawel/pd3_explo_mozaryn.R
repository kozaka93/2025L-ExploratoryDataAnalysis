library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(jsonlite)


#### filtruję mape swiata pod kraje europejskie


kraje_europy <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                  "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
                  "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", 
                  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
                  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
                  "Spain","Belarus", "Sweden", "United Kingdom",
                  "Bosnia and Herzegovina", "Norway","Moldova","Serbia",
                  "Albania","Russia","Montenegro","North Macedonia", "Switzerland", "UK", "Ukraine","Kosovo", "Liechtenstein"
)

eur <- map_data("world") %>% filter(region %in% kraje_europy, lat < 75, long < 48)


##### dane na temat otyłości w europie


# KOMENTARZ:
#dane pobierane są ze strony ourworldindata
df <- read.csv("https://ourworldindata.org/grapher/share-of-adults-defined-as-obese.csv?v=1&csvType=full&useColumnShortNames=true")

# ramka daych z mapą świata używa innych nazw na wielkiej brytanii oraz czech, trzeba je ujednolicic
df$Entity <- case_when(df$Entity == "United Kingdom" ~ "UK",
                       df$Entity == "Czechia" ~ "Czech Republic",
                       TRUE ~ df$Entity)
#interesuje nas tylko rok 2016, jest to najpóźniejszy z lat obecnych w ramce danych
df <- df %>% filter(as.numeric(Year) == 2016, Entity %in% kraje_europy ) %>% select(Entity, prevalence_of_obesity_among_adults__bmi__gt__30__crude_estimate__pct__sex_both_sexes__age_group_18plus__years)
colnames(df) <- c("region", "Obesity")
#do ramki danych z mapą dodaje info o otyłości
eur <- left_join(eur, df, by = "region")
#### wykres


eur_map <- ggplot() + 
  geom_polygon(data = eur, aes(x = long, y = lat, group = group, fill = as.numeric(Obesity)), color = "black") +
  coord_fixed(1.3) +
  labs(
    title = "Procent ludzi otyłych w krajach Europy",
    subtitle = "Dane zebrane w roku 2016",
    fill = "Procent dorosłych osób otyłych 
            (BMI >= 30)"
  ) +
  scale_fill_gradient2(na.value = "grey80",high = "#d73027", mid = "#ffffbf", low = "#1a9850", midpoint = 26) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      family = "serif",
      face = "plain",
      color = "darkgreen",
      size = 18
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "serif",
      face = "plain",
      color = "darkgreen",
      size = 14
    ),
    legend.title = element_text(
      family = "serif",
      face = "plain",
      color = "darkgreen",
      size = 12
    ),
    legend.text = element_text(
      family = "serif",
      color = "darkgreen",
      size = 10
    )
  )

eur_map








