library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthhires)
library(readr)

#znalazłem też inny pakiet, a w poleceniu było powiedziane że można wykorzystywać dowolne narzędzie
#z R'a więc stwierdziłem że go użyję
#remotes::install_github("ropensci/rnaturalearthhires")



##dane o przyroście populacji na podstawie wikipedii https://en.wikipedia.org/wiki/List_of_European_countries_by_population_growth_rate
k <- read.csv("wzr.csv", sep = ';')
colnames(k) <- c("region", "growth")


europe_sf <- ne_countries(scale = "large", continent = "Europe", returnclass = "sf")
europe_sf <- europe_sf %>% 
  filter(name != "Russia")
europe_sf$name[europe_sf$name == "Bosnia and Herz."] <- "Bosnia and Herzegovina"
europe_data <- europe_sf %>%
  left_join(k, by = c("name" = "region"))

europe_data <- europe_data %>% 
  filter(growth != (!is.na(europe_data$growth)))

leaflet(europe_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~colorNumeric(palette = "RdYlBu", domain = europe_data$growth, reverse = T)(growth),
    color = "black",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.8,
    label = ~paste0(name, ": ", growth, "%"),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "white",
      bringToFront = TRUE
    )
  ) %>%
  addLegend("bottomright", 
            pal = colorNumeric("RdYlBu", domain = europe_data$growth, reverse = T),
            values = europe_data$growth,
            title = "Przyrost ludności w %") %>% 
  addControl("Przyrost ludności w Europie w 2023", position = "topleft")






