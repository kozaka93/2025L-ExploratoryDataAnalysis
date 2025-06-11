library(htmlwidgets)
library(rnaturalearth)
library(leaflet)
library(dplyr)
library(shiny)
library(ggplot2)
library(scales)
library(plotly)
library(readr)
library(shinyWidgets)

#### Dane i wstępne przetworzenie ####
# Wczytanie danych
coffee_data <- read.csv("coffee-bean-production.csv")
health_data <- read.csv("all_cause.csv")
world_coffee_data <- read_csv("worldwide_coffee_habits.csv")

# Przetwarzanie danych zdrowotnych
health_data <- health_data %>%
  mutate(
    Caffeine_cat = case_when(
      Caffeine == 0 ~ "brak",
      Caffeine <= 1 ~ "niskie (1–100 mg)",
      Caffeine <= 3 ~ "umiarkowane (101–300 mg)",
      Caffeine > 3 ~ "wysokie (>300 mg)"
    ),
    Caffeine_cat = factor(Caffeine_cat, 
                          levels = c("brak", 
                                     "niskie (1–100 mg)", 
                                     "umiarkowane (101–300 mg)", 
                                     "wysokie (>300 mg)")),
    Gender_label = factor(Gender, levels = c(1, 2), labels = c("Mężczyzna", "Kobieta"))
  )

# Definicja zmiennych zdrowotnych
health_vars <- c(
  "Wskaźnik masy ciała (BMI)" = "BMI",
  "Nowotwór" = "Cancer",
  "Cukrzyca" = "Diabetes",
  "Udar" = "Stroke",
  "Choroba wieńcowa serca" = "Coronary.heart.disease",
  "Cholesterol" = "Cholesterol" 
)
binary_vars <- c("Cancer", "Diabetes", "Stroke", "Coronary.heart.disease")

# Przetwarzanie danych o konsumpcji kawy
world_coffee_data <- world_coffee_data %>%
  mutate(
    Country = as.factor(Country),
    `Type of Coffee Consumed` = as.factor(`Type of Coffee Consumed`),
    Year = as.integer(Year)
  ) %>%
  filter(!is.na(`Coffee Consumption (kg per capita per year)`) & 
           !is.na(`Average Coffee Price (USD per kg)`) & 
           !is.na(`Type of Coffee Consumed`)) %>%
  group_by(Country, Year, `Type of Coffee Consumed`) %>%
  summarise(
    `Coffee Consumption (kg per capita per year)` = mean(`Coffee Consumption (kg per capita per year)`),
    `Average Coffee Price (USD per kg)` = mean(`Average Coffee Price (USD per kg)`),
    .groups = "drop"
  )

# Mapowanie krajów
country_mapping <- data.frame(
  Country = paste0("Country_", 1:50),
  Real_Country = c(
    "Finlandia", "Japonia", "Brazylia", "Niemcy", "Włochy", "Francja", "Norwegia", "Stany Zjednoczone",
    "Szwecja", "Kanada", "Holandia", "Dania", "Szwajcaria", "Australia", "Wielka Brytania",
    "Austria", "Hiszpania", "Belgia", "Kolumbia", "Meksyk", "Korea Południowa", "Nowa Zelandia",
    "Portugalia", "Rosja", "Polska", "Grecja", "Turcja", "Argentyna", "Etiopia", "Indonezja",
    "Wietnam", "Indie", "RPA", "Chile", "Filipiny", "Tajlandia", "Malezja", "Peru",
    "Kostaryka", "Ekwador", "Irlandia", "Czechy", "Węgry", "Słowacja", "Chorwacja",
    "Serbia", "Słowenia", "Litwa", "Łotwa", "Estonia"
  )
)

world_coffee_data <- world_coffee_data %>%
  left_join(country_mapping, by = "Country") %>%
  mutate(Country = Real_Country) %>%
  select(-Real_Country)

# Lista unikalnych typów kawy
coffee_types <- levels(world_coffee_data$`Type of Coffee Consumed`)

#### ui ####
# Stworzenie aplikacji Shiny
ui <- fluidPage(
  # Dodanie CSS dla tła dashboardu, zakładek i białego tekstu
  tags$head(
    tags$style(HTML("
      body {
        background-color: #38270a !important;
        color: white !important;
      }
      .tab-mapa {
        background-image: url('mapa.jpg') !important;
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        min-height: 100vh;
      }
      .tab-trendy {
        background-image: url('trendy.jpg') !important;
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        min-height: 100vh;
      }
      .tab-kofeina {
        background-image: url('kofeina.jpg') !important;
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        min-height: 100vh;
      }
      .well, .tab-content {
        background-color: rgba(56, 39, 10, 0.8);
        border-radius: 1px;
        padding: 15px;
      }
      h1, h2, h3, h4, h5, h6, p, .shiny-text-output, .control-label, label {
        color: white !important;
      }
      .shiny-input-container, .shiny-html-output {
        color: black !important;
      }
      .slider-animate-container, .irs, .irs-line, .irs-bar, .irs-bar-edge, .irs-slider {
        background-color: rgba(255, 255, 255, 0.8) !important;
        border: 1px solid white !important;
      }
      .coffee-button {
        display: inline-block;
        margin: 5px;
        padding: 5px;
        border: 2px solid transparent;
        background-color: rgba(255, 255, 255, 0.2);
        text-align: center;
        cursor: pointer;
      }
      .coffee-button.selected {
        border: 2px solid #c5af8a !important;
        background-color: rgba(197, 175, 138, 0.5);
      }
      .coffee-button img {
        width: 50px;
        height: 50px;
        display: block;
        margin: 0 auto;
      }
      .coffee-button span {
        color:white;
        font-size: 12px;
      }
      .leaflet-control.legend {
        background-color: rgba(255, 255, 255, 0.8) !important;
        padding: 3px;
        border-radius: 3px;
      }
      .irs-slider:focus {
        border-color: #664221 !important;
        box-shadow: 0 0 5px rgba(102, 66, 33, 0.5) !important;
      }
      .irs-bar {
        background-color: #664221 !important;
        border-color: #664221 !important;
      }
      
      .nav-tabs > li > a {
        color: #e2d0b2 !important;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        color: black !important;
      }
    "))
  ),
  titlePanel("Świat kawy"),
  # Zakładki
  tabsetPanel(
    # Zakładka 1: Mapa produkcji kawy
    tabPanel(
      "Mapa produkcji kawy",
      class = "tab-mapa",
      sidebarPanel(
        p("Wybierz rok z suwaka, aby zobaczyć globalną produkcję kawy w tonach."),
        textOutput("conclusions_map"),
        p(),
        img(src = "plant.jpg", width = "100%"),
        p(),
        p("Kwiaty i owoce drzewa kawowego")
      ),
      mainPanel(
        sliderInput("year", "Wybierz rok:",
                    min = 1961, max = 2023, value = 2020, step = 1, animate = TRUE,
                    width = "100%", sep = ""),
        textOutput("year_display"),
      leafletOutput("map"),
      )
      
    ),
    # Zakładka 2: Trendy konsumpcji kawy
    tabPanel(
      "Trendy konsumpcji kawy",
      class = "tab-trendy",
      p("Wybierz kraje, zakres lat i typy kawy, aby zobaczyć trendy konsumpcji kawy na mieszkańca."),
      sidebarLayout(
        sidebarPanel(
          pickerInput(
            inputId = "countries",
            label = "Wybierz Kraje:",
            choices = unique(world_coffee_data$Country),
            multiple = TRUE,
            selected = c("Finlandia"),
            options = list(
              "actions-box" = TRUE,
              "live-search" = TRUE
            )
          ),
          sliderInput("year_range", "Wybierz Zakres Lat:", 
                      min = min(world_coffee_data$Year, na.rm = TRUE), 
                      max = max(world_coffee_data$Year, na.rm = TRUE), 
                      value = c(min(world_coffee_data$Year, na.rm = TRUE), max(world_coffee_data$Year, na.rm = TRUE)), 
                      step = 1, sep = ""),
          uiOutput("coffee_type_buttons"),
          textOutput("conclusions_trend")
        ),
        mainPanel(
          plotlyOutput("trend_plot", height = "600px"),
          textOutput("no_data_message")
        )
      )
    ),
    # Zakładka 3: Analiza spożycia kofeiny
    tabPanel(
      "Analiza spożycia kofeiny",
      class = "tab-kofeina",
      p("Wybierz płeć i zmienną zdrowotną, aby zobaczyć zależności między spożyciem kofeiny a zdrowiem."),
      sidebarLayout(
        sidebarPanel(
          selectInput("health_var", "Wybierz wskaźnik zdrowotny:", choices = health_vars),
          checkboxGroupInput("gender", "Płeć:",
                             choices = c("Mężczyzna", "Kobieta"),
                             selected = c("Mężczyzna", "Kobieta")),
          textOutput("conclusions_health"),
        ),
        mainPanel(
          plotOutput("healthPlot", height = "500px")
        )
      )
    )
  )
)

#### server ####
server <- function(input, output, session) {
  
  # Logika dla zakładki z mapą
  output$year_display <- renderText({
    as.character(input$year)
  })
  
  output$conclusions_map <- renderText({
    "Największa produkcja ziaren kawy od lat odbywa się w Brazylii, jednocześnie z roku na rok rośnie ogólna produkcja kawy.
      Na mapie zostały zaznaczone zwrotniki, które stanowią granice tak zwanego pasa kawowego,
      czyli obszaru, w którym naturalnie panują dobre warunki do uprawy drzew kawowych. Zgodnie z tym wyróżnia się
    trzy główne regiony upraw: Amerykę Łacińską, Afrykę oraz Azję i Pacyfik- każdy charakteryzujący się innymi nutami smakowymi."
  })
  
  # Zwrotniki
  longitudes <- seq(-180, 180, length.out = 100)
  tropic_cancer <- data.frame(lng = longitudes, lat = rep(23.43722, 100))
  tropic_capricorn <- data.frame(lng = longitudes, lat = rep(-23.43722, 100))
  
  # Pobranie danych o krajach
  kraje <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Zmiana nazw kolumn w danych o kawie
  coffee_data <- coffee_data %>%
    rename(iso_a3 = Code, CoffeeProduction = Coffee..green...00000656....Production...005510....tonnes)
  
  # Funkcja generująca mapę w zależności od wybranego roku
  output$map <- renderLeaflet({
    dane_rok <- coffee_data %>%
      filter(Year == input$year)
    
    dane_mapowe <- kraje %>%
      left_join(dane_rok, by = "iso_a3")
    
    bins <- c(0, 50000, 100000, 200000, 250000, 500000, 1000000, 2000000, 5000000)
    paleta_koloru <- colorBin("YlGnBu", domain = dane_mapowe$CoffeeProduction, bins = bins, na.color = "lightgray")
    
    leaflet(data = dane_mapowe) %>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addPolylines(data = tropic_cancer, lng = ~lng, lat = ~lat, color = "#664221", weight = 2,
                   label = "Zwrotnik Raka",
                   labelOptions = labelOptions(style = list("background-color" = "rgba(255,255,255,0.8)", "padding" = "3px"))) %>%
      addPolylines(data = tropic_capricorn, lng = ~lng, lat = ~lat, color = "#664221", weight = 2,
                   label = "Zwrotnik Koziorożca",
                   labelOptions = labelOptions(style = list("background-color" = "rgba(255,255,255,0.8)", "padding" = "3px"))) %>%
      addPolygons(
        fillColor = ~paleta_koloru(CoffeeProduction),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "1",
        fillOpacity = 0.7,
        popup = ~paste("<div style='background-color: rgba(255,255,255,0.8); padding: 5px;'>Kraj: ", name, "<br>Produkcja kawy: ", CoffeeProduction, " ton</div>")
      ) %>%
      addLegend("bottomright", pal = paleta_koloru, values = ~CoffeeProduction,
                title = "Produkcja kawy (tony)",
                labFormat = labelFormat(suffix = " ton"),
                opacity = 1,
                layerId = "legend")
  })
  
  # Logika dla przycisków typów kawy
  selected_coffee_types <- reactiveVal(coffee_types)
  
  output$coffee_type_buttons <- renderUI({
    lapply(seq_along(coffee_types), function(i) {
      type <- coffee_types[i]
      actionButton(
        inputId = paste0("coffee_type_", i),
        label = div(
          img(src = paste0("coffee_type", i, ".jpg"), width = "50px", height = "50px"),
          span(type)
        ),
        class = ifelse(type %in% selected_coffee_types(), "coffee-button selected", "coffee-button")
      )
    })
  })
  
  lapply(seq_along(coffee_types), function(i) {
    observeEvent(input[[paste0("coffee_type_", i)]], {
      current_types <- selected_coffee_types()
      type <- coffee_types[i]
      if (type %in% current_types) {
        selected_coffee_types(setdiff(current_types, type))
      } else {
        selected_coffee_types(c(current_types, type))
      }
    })
  })
  
  # Logika dla zakładki z trendami konsumpcji kawy
  filtered_coffee <- reactive({
    req(input$countries)
    
    data <- world_coffee_data %>%
      filter(Year >= input$year_range[1], Year <= input$year_range[2]) %>%
      filter(Country %in% input$countries)
    
    if (!is.null(selected_coffee_types()) && length(selected_coffee_types()) > 0) {
      data <- data %>% filter(`Type of Coffee Consumed` %in% selected_coffee_types())
    } else {
      data <- data[0,]
    }
    
    data
  })
  
  output$no_data_message <- renderText({
    if (is.null(filtered_coffee()) || nrow(filtered_coffee()) == 0) {
      "Brak danych dla wybranych kryteriów. Spróbuj zmienić kraje, zakres lat lub typ kawy."
    } else {
      ""
    }
  })
  
  output$conclusions_trend <- renderText({
    "Średnie spożycie kawy na przestrzeni lat jest większe w krajach, w których uprawiana jest kawa,
    takich jak: Brazylia, Wietnam, Kolumbia,
    a także w krajach lepiej rozwiniętych, o wyższym PKB. Za najpopularniejszy typ kawy 
    można uznać ecpresso, a w szerszym postrzeganiu kawy czarne (espresso i americano).
    "
  })
  
  output$trend_plot <- renderPlotly({
    data <- filtered_coffee()
    req(nrow(data) > 0)
    
    line_styles <- c("solid", "dash", "dot", "longdash", "dashdot")
    coffee_types_unique <- unique(data$`Type of Coffee Consumed`)
    line_map <- setNames(rep(line_styles, length.out = length(coffee_types_unique)), coffee_types_unique)
    
    if (length(input$countries) > 1 && length(selected_coffee_types()) > 1) {
      # Tworzenie subplotów dla każdego typu kawy
      plots <- lapply(coffee_types_unique, function(coffee_type) {
        subset_data <- data %>% filter(`Type of Coffee Consumed` == coffee_type)
        if (nrow(subset_data) == 0) return(NULL)
        
        p <- plot_ly()
        for (country in unique(subset_data$Country)) {
          country_data <- subset_data %>% filter(Country == country)
          p <- add_trace(
            p,
            data = country_data,
            x = ~Year,
            y = ~`Coffee Consumption (kg per capita per year)`,
            type = "scatter",
            mode = "lines+markers",
            name = country,
            line = list(dash = line_map[[as.character(coffee_type)]], width = 2),
            text = ~paste(
              "Kraj:", Country,
              "<br>Rok:", Year,
              "<br>Konsumpcja:", round(`Coffee Consumption (kg per capita per year)`, 2), " kg/osoba",
              "<br>Typ kawy:", `Type of Coffee Consumed`
            ),
            hoverinfo = "text"
          )
        }
        p %>% layout(
          title = list(text = paste("Typ kawy:", coffee_type), font = list(color = "black")),
          xaxis = list(title = "Rok", tickformat = ".0f", titlefont = list(color = "black"), tickfont = list(color = "black")),
          yaxis = list(title = "(kg/osoba)", titlefont = list(color = "black"), tickfont = list(color = "black")),
          showlegend = TRUE,
          paper_bgcolor = "black",
          plot_bgcolor = "black"
        )
      })
      
      plots <- plots[!sapply(plots, is.null)]
      subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE) %>%
        layout(
          title = list(text = "Konsumpcja Kawy na Mieszkańca w Czasie", font = list(color = "black")),
          paper_bgcolor = "white",
          plot_bgcolor = "white"
        )
    } else {
      # Pojedynczy wykres
      p <- plot_ly()
      for (country in unique(data$Country)) {
        for (coffee_type in unique(data$`Type of Coffee Consumed`)) {
          subset_data <- data %>% 
            filter(Country == country, `Type of Coffee Consumed` == coffee_type)
          
          if (nrow(subset_data) == 0) next
          
          p <- add_trace(
            p,
            data = subset_data,
            x = ~Year,
            y = ~`Coffee Consumption (kg per capita per year)`,
            type = "scatter",
            mode = "lines+markers",
            name = paste(country, "-", coffee_type),
            line = list(dash = line_map[[as.character(coffee_type)]], width = 2),
            text = ~paste(
              "Kraj:", Country,
              "<br>Rok:", Year,
              "<br>Konsumpcja:", round(`Coffee Consumption (kg per capita per year)`, 2), " kg/osoba",
              "<br>Typ kawy:", `Type of Coffee Consumed`
            ),
            hoverinfo = "text",
            showlegend = TRUE
          )
        }
      }
      
      p %>% layout(
        title = list(text = "Konsumpcja Kawy na Mieszkańca w Czasie", font = list(color = "black")),
        xaxis = list(title = "Rok", tickformat = ".0f", titlefont = list(color = "black"), tickfont = list(color = "black")),
        yaxis = list(title = "Konsumpcja Kawy (kg na mieszkańca rocznie)", titlefont = list(color = "black"), tickfont = list(color = "black")),
        legend = list(title = list(text = "<b>Kraje i Typy Kawy</b>", font = list(color = "black")), font = list(color = "black")),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      )
    }
  })
  
  # Logika dla zakładki z wykresem spożycia kofeiny
  filtered_health <- reactive({
    health_data %>%
      filter(Gender_label %in% input$gender)
  })
  
  output$conclusions_health <- renderText({
    "Wraz ze wzrostem codziennej dawki kofeiny wzrasta również procent badanych z chorobą nowotworową, jak i chorobą wieńcową serca. Ilość spożywanej kofeiny może mieć negatywny wpływ na cholesterol; jego średni poziom rośnie wraz ze wzrostem spożywanej kofeiny.
Zauważalny jest natomiast niski odsetek cukrzyków wśród badanych, spożywających powyżej 300 mg kofeiny na dobę; może to znaczyć o pozytywnych efektach kofeiny na poziom cukru w organizmie. Płeć nie ma większego znaczenia w wynikach; zarówno u kobiet, jak i u mężczyzn wszystkie odpowiedzi są podobne."
  })
  
  output$healthPlot <- renderPlot({
    df <- filtered_health()
    var <- input$health_var
    if (var %in% binary_vars) {
      df <- df %>%
        mutate(
          HealthStatus = factor(as.character(.data[[var]]),
                                levels = c("1", "2"),
                                labels = c("Tak", "Nie"))
        )
      ggplot(df, aes(x = Caffeine_cat, fill = HealthStatus)) +
        geom_bar(position = "fill") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                           limits = c(0, 1), # Ustawia oś Y od 0 do 1
                           expand = c(0, 0)) + # Usuwa odstęp od osi
        scale_fill_manual(values = c("Tak" = "#604720", "Nie" = "#ccb38b")) +
        labs(
          title = paste("Czy występuje:", names(health_vars[health_vars == var])),
          x = "Poziom spożycia kofeiny",
          y = "Procent badanych",
          fill = "Występowanie"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          text = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black"),
          plot.title = element_text(color = "black"),
          legend.text = element_text(color = "black"),
          legend.title = element_text(color = "black"),
          plot.background = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent")
        )
    } else {
      ggplot(df, aes(x = Caffeine_cat, y = .data[[var]])) +
        geom_boxplot(fill = "#ccb38b", outlier.color = "#d4870b", alpha = 0.7) +
        labs(
          title = paste("Kofeina a", names(health_vars[health_vars == var])),
          x = "Poziom spożycia kofeiny",
          y = names(health_vars[health_vars == var])
        ) +
        theme_minimal(base_size = 14) +
        theme(
          text = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black"),
          plot.title = element_text(color = "black"),
          legend.text = element_text(color = "black"),
          legend.title = element_text(color = "black"),
          plot.background = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent")
        )
    }
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)