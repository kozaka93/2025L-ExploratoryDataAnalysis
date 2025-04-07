#Link do danych: https://ec.europa.eu/eurostat/databrowser/view/tran_sf_roadus/default/table?lang=en&category=tran_sf.tran_sf_road
#Pobieranie pakietów
install.packages(c("ggplot2","countrycode", "sf", "dplyr", "rnaturalearth", "rnaturalearthdata", "tidyr","leaflet","htmlwidgets"))
library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(leaflet)
library(htmlwidgets)
library(tidyr)
library(countrycode)

#Uporządkowanie kolumn,pobranie danych
df <- read.csv("Wypadki_drogowe.csv", sep=';')
dane <- separate(df, col = 1, into = c("Time_frequency", "Sex", "Age", "Unit", 'Category_of_Pearsons',
                                       "Geo"), sep = ",")

#Zmiana nazw państwa na pełne
dane <- dane %>%
  mutate(country_name = case_when(
    Geo == "EL" ~ "Greece",
    Geo == "EU27_2020" ~ "European Union",
    Geo == "UK" ~ "United Kingdom",
    TRUE ~ countrycode(Geo, origin = "iso2c", destination = "country.name")
  ))%>%
  rename_with(~ gsub("^X", "", .x))



#Filtorwanie danych
dane <- dane %>%
  filter (Age=='TOTAL', Category_of_Pearsons=='TOTAL', Sex=='T', Unit=='P_MHAB')%>%
  filter(Geo != "EU27_2020")%>%
  select('1999':last_col())

#Utworzenie nowej tabeli
dane2 <- dane %>%
  pivot_longer(cols = -country_name, names_to = "year", values_to = "accidents") %>%
  mutate(year = as.integer(year))

#Pobranie mapy Europy
europa <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_un == "Europe")

#Uporządkowywanie danych
europa1 <- europa %>%
  filter(region_un == "Europe") %>% 
  group_by(sovereignt) %>%
  filter(if_else(sovereignt == "United Kingdom", admin == "United Kingdom", TRUE)) %>%
  ungroup()%>%
  distinct(sovereignt, .keep_all = TRUE)

#Połączenie dwóch tabel: mapy Europy i danych o wypadkach
mapa_dane <- dane2 %>%
  left_join(europa1, by = c("country_name" = "sovereignt"))

mapa_dane <- mapa_dane %>%
  st_as_sf()

mapa_dane$accidents <- as.numeric(mapa_dane$accidents)

#Utworzenie mapy
mapa <- leaflet(mapa_dane) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Warstwy dla różnych lat
  addPolygons(
    data = mapa_dane[mapa_dane$year == 2018, ], # Warstwa dla 2018
    fillColor = ~colorNumeric(c("yellow", "red", "purple"), accidents, n = 9)(accidents),
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    weight = 0.5,
    popup = ~paste(country_name, "<br>Liczba zabitych osób: ", accidents),
    group = "2018"
  ) %>%
  
  addPolygons(
    data = mapa_dane[mapa_dane$year == 2019, ], # Warstwa dla 2019
    fillColor = ~colorNumeric(c("yellow", "red", "purple"), accidents, n = 9)(accidents),
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    weight = 0.5,
    popup = ~paste(country_name, "<br>Liczba zabitych osób: ", accidents),
    group = "2019"
  ) %>%
  
  addPolygons(
    data = mapa_dane[mapa_dane$year == 2020, ], # Warstwa dla 2020
    fillColor = ~colorNumeric(c("yellow", "red", "purple"), accidents, n = 9)(accidents),
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    weight = 0.5,
    popup = ~paste(country_name, "<br>Liczba zabitych osób: ", accidents),
    group = "2020"
  ) %>%
  addPolygons(
    data = mapa_dane[mapa_dane$year == 2021, ], # Warstwa dla 2021
    fillColor = ~colorNumeric(c("yellow", "red", "purple"), accidents, n = 9)(accidents),
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    weight = 0.5,
    popup = ~paste(country_name, "<br>Liczba zabitych osób: ", accidents),
    group = "2021"
  ) %>%
  addPolygons(
    data = mapa_dane[mapa_dane$year == 2022, ], # Warstwa dla 2022
    fillColor = ~colorNumeric(c("yellow", "red", "purple"), accidents, n = 9)(accidents),
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    weight = 0.5,
    popup = ~paste(country_name, "<br>Liczba zabitych osób: ", accidents),
    group = "2022"
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric(c("yellow", "red", "purple"), mapa_dane$accidents, n = 9),
    values = mapa_dane$accidents, 
    title = "Liczba osób zabitych",
    opacity = 1,
    labFormat = labelFormat(suffix = ""),
    na.label=""
  ) %>%
  addLayersControl(
    overlayGroups = c("2018", "2019", "2020","2021", "2022"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  setView(lng = 13.4, lat = 52.5, zoom = 4) %>%
  addControl(
    html = "<h3 style='font-weight:bold;'>Liczba zabitych osób w wypadkach drogowych w Europie w latach 2018-2022 </h3>",
    position = "topright"
  )

# Wyświetlenie mapy
mapa

#Zapisanie mapy
saveWidget(mapa, "C:/Users/julia/Documents/mapa_wypadkow_drogowych.html")


