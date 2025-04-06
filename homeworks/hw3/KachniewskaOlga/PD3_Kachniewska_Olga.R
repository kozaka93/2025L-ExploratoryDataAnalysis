



library(htmlwidgets)
library(rnaturalearth)
library(leaflet)
library(dplyr)
library(shiny)

#Źdródło danych
#Dane pochodzą ze strony: https://ourworldindata.org/grapher/coffee-bean-production?tab=table


# Stworzenie aplikacji Shiny
ui <- fluidPage(
  titlePanel("Produkcja zielonych ziaren kawy na świecie"),
  
  p("Wybierz rok z suwaka, aby zobaczyć globalną produkcję kawy w tonach."),
  
  # Suwak do wyboru roku
  sliderInput("year", "Wybierz rok:",
              min = 1961, max = 2023, value = 2020, step = 1, animate = TRUE,
              width = "100%", sep = ""),
  
  # Wyświetlanie wybranego roku
  textOutput("year_display"),
  
  # Mapa
  leafletOutput("map"),
  
  p("Ciekawostka: na mapie zostały zaznaczone zwrotniki, 
    które stanowią granice tak zwanego pasa kawowego, \n 
    czyli obszaru, w którym naturalnie panują dobre warunki do uprawy drzew kawowych.")
)

server <- function(input, output, session) {
  
  output$year_display <- renderText({
    as.character(input$year)
  })
  
  # Zwrotniki
  longitudes <- seq(-180, 180, length.out = 100)
  tropic_cancer <- data.frame(lng = longitudes, lat = rep(23.43722, 100))
  tropic_capricorn <- data.frame(lng = longitudes, lat = rep(-23.43722, 100))
  
  # Pobranie danych o krajach
  kraje <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Wczytanie danych o produkcji kawy
  dane_kraje <- read.csv("coffee-bean-production.csv")
  
  # Zmiana nazw kolumn w danych
  dane_kraje <- dane_kraje %>%
    rename(iso_a3 = Code, CoffeeProduction = Coffee..green...00000656....Production...005510....tonnes)
  
  # Funkcja generująca mapę w zależności od wybranego roku
  output$map <- renderLeaflet({
    
    # Filtrowanie danych na podstawie wybranego roku
    dane_rok <- dane_kraje %>%
      filter(Year == input$year)
    
    # Łączenie danych o produkcji kawy z danymi o krajach
    dane_mapowe <- kraje %>%
      left_join(dane_rok, by = "iso_a3")
    
    # Tworzenie kubełków do klasyfikacji produkcji kawy
    bins <- c(0, 50000, 100000, 200000, 250000, 500000, 1000000, 2000000, 5000000)
    paleta_koloru <- colorBin("YlGnBu", domain = dane_mapowe$CoffeeProduction, bins = bins, na.color = "lightgray")
    
    # Tworzenie mapy
    leaflet(data = dane_mapowe) %>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      
      # Dodawanie zwrotników
      addPolylines(data = tropic_cancer, lng = ~lng, lat = ~lat, color = "#664221", weight = 2,
                   label = "Zwrotnik Raka") %>%
      addPolylines(data = tropic_capricorn, lng = ~lng, lat = ~lat, color = "#664221", weight = 2,
                   label = "Zwrotnik Koziorożca") %>%
      
      # Dodawanie krajów
      addPolygons(
        fillColor = ~paleta_koloru(CoffeeProduction),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "1",
        fillOpacity = 0.7,
        popup = ~paste("Kraj: ", name, "<br>Produkcja kawy: ", CoffeeProduction, " ton")
      ) %>%
      
      # Dodawanie legendy
      addLegend("bottomright", pal = paleta_koloru, values = ~CoffeeProduction,
                title = "Produkcja kawy (tony)",
                labFormat = labelFormat(suffix = " ton"),
                opacity = 1)
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)

app <- shinyApp(ui = ui, server = server)
saveWidget(app, "coffee_world.html")


#pojedyncza mapa do zapisania jako jpg ponieważ nic nie działa jak chciałam zapisać całość jako html
# a po straceniu ponad godziny na próby nie zamierzam ich kontynuować


kraje <- ne_countries(scale = "medium", returnclass = "sf")

# Wczytanie danych o produkcji kawy
dane_kraje <- read.csv("coffee-bean-production.csv")

# Zmiana nazw kolumn w danych
dane_kraje <- dane_kraje %>%
  rename(iso_a3 = Code, CoffeeProduction = Coffee..green...00000656....Production...005510....tonnes)

# Filtrowanie danych na podstawie roku 2020
dane_2020 <- dane_kraje %>%
  filter(Year == 2020)

# Łączenie danych o produkcji kawy z danymi o krajach
dane_mapowe <- kraje %>%
  left_join(dane_2020, by = "iso_a3")

# Tworzenie kubełków do klasyfikacji produkcji kawy
bins <- c(0, 50000, 100000, 200000, 250000, 500000, 1000000, 2000000, 5000000)
paleta_koloru <- colorBin("YlGnBu", domain = dane_mapowe$CoffeeProduction, bins = bins, na.color = "lightgray")

# Tworzenie mapy
mapa <- leaflet(data = dane_mapowe) %>%
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  
  # Dodawanie krajów
  addPolygons(
    fillColor = ~paleta_koloru(CoffeeProduction),
    weight = 1,
    opacity = 1,
    color = "black",
    dashArray = "1",
    fillOpacity = 0.7,
    popup = ~paste("Kraj: ", name, "<br>Produkcja kawy: ", CoffeeProduction, " ton")
  ) %>%
  
  # Dodawanie legendy
  addLegend("bottomright", pal = paleta_koloru, values = ~CoffeeProduction,
            title = "Produkcja kawy (tony)",
            labFormat = labelFormat(suffix = " ton"),
            opacity = 1) %>%
  
  # Dodanie tytułu
  addControl("<strong>Produkcja zielonych ziaren kawy na świecie w 2020 roku</strong>", position = "topright")

# Zapisanie mapy do pliku PNG/JPG
library(htmlwidgets)
saveWidget(mapa, "mapa_kawy_2020.html", selfcontained = TRUE)