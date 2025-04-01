library(maps)
library(mapdata)
library(ggplot2)
library(dplyr)
library(stringr)

fertility <- read.csv("children-per-woman-un.csv")

fertility <- fertility %>% rename(Births_per_woman = Fertility.rate...Sex..all...Age..all...Variant..estimates)

fertility_2023 <- fertility %>% filter(Year == 2023)

# dopasowujemy niektóre nazwy państw do tych, które znajdują się w fertility

africa_map <- map_data("world") %>%
  mutate(region = case_when(
    region == "Republic of Congo" ~ "Congo",
    region == "Democratic Republic of the Congo" ~ "Democratic Republic of Congo",
    region == "Ivory Coast" ~ "Cote d'Ivoire",
    region == "Swaziland" ~ "Eswatini",
    TRUE ~ region 
  )) %>%
  filter(region %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
                       "Burundi", "Cape Verde", "Cameroon", "Central African Republic", 
                       "Chad", "Comoros", "Congo", "Democratic Republic of Congo", "Djibouti", 
                       "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", 
                       "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Cote d'Ivoire", 
                       "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
                       "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", 
                       "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
                       "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
                       "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", 
                       "Zambia", "Zimbabwe",  "Western Sahara"))

# sprawdzamy czy wszystkie panstwa z africa_map sa w fertility

setdiff(africa_map$region, fertility_2023$Entity)

africa_map <- africa_map %>%
  left_join(fertility_2023, by = c("region" = "Entity"))

ggplot() +
  geom_polygon(data = africa_map, aes(x = long, y = lat, group = group, fill = Births_per_woman),
               color = "black", size = 0.3) +
  scale_fill_gradient2(low = "#deebf7", mid = "#9e9ac8", high = "#54278f",
                       midpoint = 4, na.value = "grey80",
                       name = str_wrap("Liczba dzieci urodzonych na kobietę", width = 20)
  ) +
  theme_minimal() +
  labs(
    title = "Średnia liczba urodzonych dzieci przypadająca na kobietę w krajach Afryki (2023)",
    subtitle = "Dane przedstawiają średnią liczbę dzieci, które rodzi kobieta w danym państwie Afryki w 2023 roku.",
    caption = "Źródło: https://ourworldindata.org/grapher/children-per-woman-un?region=Africa"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.6),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    plot.caption = element_text(size = 13, hjust = 1),
    legend.title = element_text(size = 18, hjust = 0.5, vjust = 0),
    legend.box.just = "center",
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(0.7, "cm"),
    legend.box.margin = margin(0, 0, 0, 0)
  ) +
  coord_map("ortho", orientation = c(5, 20, 0))

ggsave("wykres_urodzen_Afryka.pdf", width = 17, height = 8, device = cairo_pdf)


