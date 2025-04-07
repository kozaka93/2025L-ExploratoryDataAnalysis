library(ggplot2)
library(dplyr)
library(maps)
library(geojsonio)

df <-read.csv("GlobalLandTemperaturesByCountry.csv")

df_2012 <- df[substr(df$dt, 1, 4) == "2012", ]

df_2012 <- df_2012 %>% group_by(Country) %>% 
  summarise(AvgYearTemp = mean(AverageTemperature))



world <- geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")

world@data <- world@data %>%
  left_join(df_2012, by = c("name" = "Country"))

m <- leaflet(world) %>%
  setView(lng = 0, lat = 20, zoom = 1) %>%
  addTiles()

pal <- colorNumeric(
  palette = c("blue", "white", "yellow", "red"),
  domain = world$AvgYearTemp
)

labels <- sprintf(
  "<strong>%s</strong><br/>Yearly Average Temperature is %g°C",
  world$name, world$AvgYearTemp
) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
  fillColor = ~pal(AvgYearTemp),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlightOptions = highlightOptions(
    weight = 3,
    color = "black",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))
m

m %>% addLegend(pal = pal, values = ~AvgYearTemp, opacity = 0.7, title = NULL,
                position = "bottomright")

