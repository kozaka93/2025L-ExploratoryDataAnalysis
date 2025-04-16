#install.packages("dplyr")
#install.packages("ggplot2")
install.packages("ggmap")
install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("geosjonsf")
install.packages("sf")

library(dplyr)
library(ggplot2)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(geosjonsf)   
library(sf)

install.packages("leaflet")
install.packages("mapview")
library(leaflet)
library(mapview)
install.packages("webshot")
library(webshot)
webshot::install_phantomjs()


install.packages("htmlwidgets")

setwd("/Users/agataharbaty/Desktop/Files_R/Projekt")

      #ramka danych nazwa Houses (biore sb z folderu ręcznie)
      #zmiana ? na polskie w nazwach miast i dodatnie d(mini) i ceny za sqm

Houses <- read.csv("Houses.csv")

Houses2 <- Houses %>% 
  mutate(city = replace(city, city == "Krak\xf3w", "Kraków"),
         city = replace(city, city == "Pozna\xf1", "Poznań")) %>% 
  mutate(cena_za_metr = round(price / sq, 0)) %>% 
  mutate(MiNI=round((((cos(latitude*pi/180)*(longitude-21.00694))^2+(latitude-52.22194)^2)^0.5*111.32),3))

#tu wybieramy co i jak chcemy z mapki

df_powt <- Houses2 %>% 
  filter(year > 1959 & year < 2024, city == "Warszawa",
         cena_za_metr > 4900 & cena_za_metr < 27000,
         sq <= 60 & sq >= 50) %>% 
  select(latitude, longitude, cena_za_metr, MiNI) %>%
  arrange(cena_za_metr)

#bez dupikatów(np. ten sam deweloper 2 identyczne mieszkania)


df1 <- df_powt[!duplicated(df_powt), ]




      #mapka tylko z punktami co chcemy(bez tła/kółek itd.)(może zmiana kółek?)


leaflet(df1) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    color = ~colorRampPalette(c("yellow", "red", "black"))(10)[
      as.numeric(cut(df1$cena_za_metr, breaks = 10))
    ],
    radius = 6,  #radius kółek
    fillOpacity = 0.7,  #przeźroczystść
    stroke = FALSE  #t/f czy obramowanie
  ) %>%
  setView(lng = 21.0118, lat = 52.2298, zoom = 11)  #view na srodek wwa



      #Tu już z tłem z geosjona


#wczytanie podziału wwa
warsaw_districts <- geojsonio::geojson_read("warszawa-dzielnice.geojson", what = "sp")



#sama mapka jak działa
leaflet(data = warsaw_districts) %>%
  addTiles() %>%
  addPolygons(fillColor = topo.colors(5, alpha = NULL), stroke = FALSE)

      #tworzymy już gościa

#kolory i ile bracketów(do zmiany/ustalenia)
pal_points <- colorNumeric(palette = colorRampPalette(c("yellow","red", "navy"))(10), 
                           domain = df1$cena_za_metr)


mapa1 <- leaflet(df1) %>%
  addTiles() %>%
  
  #tu co chcemy w jakiej kolejności zasłaniania
  
  addMapPane("polygons", zIndex = 400) %>%
  addMapPane("markers", zIndex = 500) %>%
  addMapPane("circles", zIndex = 450) %>%
  addMapPane("back", zIndex = 350) %>%
  addMapPane("mini", zIndex = 550) %>% 
  
  #warstwa z tłem co i jak
  addCircles(
    lng = 21.00694, lat = 52.22194,
    radius = 40000,#promienie
    color = "white", weight = 1, opacity = 1,  #parametry(ust.)   #tu sie da kolor co tło całego plakatu
    fillColor = "white", fillOpacity = 1,  #czy wypelniamy?(ust.)
    options = pathOptions(pane = "back")
  ) %>%
  
  
  
  #warstwa z dzielnicami co i jak
    addPolygons(
    data = warsaw_districts,
    fillColor = "yellow",  #kolor(do ustalenia)
    color = "black", weight = 1, opacity = 1,  #białe obramówki(do ust.)
    fillOpacity = 0.1,  #przezroczystość warstwy dzielnic(do ust.)
    options = pathOptions(pane = "polygons")
  ) %>%
  
  
  #warstwa z mini co i jak
  
  addCircleMarkers(
    lng = 21.00694, lat = 52.22194,
    color = "blue",
    radius = 6,  #radius punktów(do ust.)
    fillOpacity = 1,  #przezrocz. punktow(do ust.)
    stroke = FALSE, weight = 0,  #czy obwódka i grubość(do ust.)
    options = pathOptions(pane = "mini")
  ) %>%
  #warstwa z kółkami co i jak
  
  addCircles(
    lng = 21.00694, lat = 52.22194,
    radius = c(2500, 5000, 7500),#promienie
    color = "navy", weight = 3, opacity = 1,  #parametry(ust.)
    fillColor = "blue", fillOpacity = 0,  #czy wypelniamy?(ust.)
    options = pathOptions(pane = "circles"),
  ) %>%
  
  #warstwa z mieszkankami co i jak
  
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    color = ~pal_points(cena_za_metr),
    radius = 4.5,  #radius punktów(do ust.)
    fillOpacity = 1,  #przezrocz. punktow(do ust.)
    stroke = FALSE, weight = 0,  #czy obwódka i grubość(do ust.)
    options = pathOptions(pane = "markers")
  ) %>%
  
  #legenda co i jak
  
  addLegend(
    position = "bottomright",#gdzie dajemy ziuta(do ust.)
    pal = pal_points,
    values = df1$cena_za_metr,
    title = "Cena w zł za m² ",#tytul(ust.)
    opacity = 1 #przezrocz. punktow(ust.)
  ) %>%
  
  #view na wwa
  
  setView(lng = 21.0118, lat = 52.2298, zoom = 11)


mapshot(mapa1, file = "leaflet_map.png", vwidth = 900, vheight = 800)



