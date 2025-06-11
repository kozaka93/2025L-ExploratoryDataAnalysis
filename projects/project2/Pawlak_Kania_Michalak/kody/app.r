library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(fastmap)
library(shinythemes)
library(shinyWidgets)
library(scales)
library(ggtext)
library(shinycssloaders)
library(countrycode)
library(SmarterPoland)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(readxl)

################################################################################

piwo_1 <- read.csv("beer-consumption-per-person.csv")
piwo_1 <- piwo_1 %>%
  rename_with(~ "Liters", .cols = 4) %>% 
  rename_with(~ "Country", .cols = 1)
piwo_1$Liters[is.na(piwo_1$Liters)] <- 0

wino_1 <- read.csv("wine-consumption-per-person.csv")
wino_1 <- wino_1 %>%
  rename_with(~ "Liters", .cols = 4) %>% 
  rename_with(~ "Country", .cols = 1)
wino_1$Liters[is.na(wino_1$Liters)] <- 0

wodka_1 <- read.csv("spirits-consumption-per-person.csv")
wodka_1 <- wodka_1 %>%
  rename_with(~ "Liters", .cols = 4) %>% 
  rename_with(~ "Country", .cols = 1)
wodka_1$Liters[is.na(wodka_1$Liters)] <- 0

world1 <- countryname_dict
deaths <- read.csv("alcohol-deaths-amount.csv")
deaths$location_name[deaths$location_name=="Stany Zjednoczone Ameryki"] <- "Stany Zjednoczone" 
deaths$location_name[deaths$location_name=="Birma (Mjanma)"] <- "Birma"
countries$country[countries$country=="Russian Federation"] <- "Russia"
deaths <- deaths %>%
  inner_join(world1, by = c("location_name" = "country.name.alt")) %>% 
  inner_join(countries, by = c("country.name.en" = "country")) %>% 
  select("country.name.en", "year", "metric_name", "val", "continent") %>% 
  filter(metric_name %in% c("Liczba", "Współczynnik")) %>% 
  rename(Country = country.name.en, Year = year, Metric = metric_name, Amount = val, Continent = continent) %>% 
  mutate(Rounded = ifelse(Metric == "Liczba", round(Amount), round(Amount, 2)))
deaths$Amount[is.na(deaths$Amount)] <- 0

p1 <- piwo_1 %>% 
  filter(Year == 2019) %>% 
  rename(Liters_piwo = Liters)
w1 <- wino_1 %>% 
  filter(Year == 2019) %>% 
  rename(Liters_wino = Liters)
aw1 <- wodka_1 %>% 
  filter(Year == 2019) %>% 
  rename(Liters_aw = Liters)
drinks <- p1 %>% 
  inner_join(w1, by = c("Country" = "Country")) %>% 
  inner_join(aw1, by = c("Country" = "Country")) %>% 
  select("Country", "Code", "Year", "Liters_piwo", "Liters_wino", "Liters_aw") %>% 
  rowwise() %>% 
  mutate(temp = list(sort(c(
    "PIWO" = Liters_piwo, "WINO" = Liters_wino, "ALKOHOL WYSOKOPROCENTOWY" = Liters_aw), decreasing = TRUE)),
    first = names(temp)[1], second = names(temp)[2], third = names(temp)[3])

world2 <- ne_countries(scale = "medium", returnclass = "sf")
drinks <- drinks %>%
  mutate(iso_a3 = countrycode(Country, "country.name", "iso3c"))
world2 <- world2 %>%
  mutate(iso_a3 = countrycode(name, "country.name", "iso3c"))
map_data <- world2 %>%
  left_join(drinks, by = "iso_a3")
colnames(world2)
religion <- read_excel("Religious_Composition_by_Country_2010-2050.xlsx")
religion <- religion %>% 
  select(-c("row_number", "level", "Year", "Nation_fk", "Region")) %>% 
  filter(Country != "All Countries") %>% 
  mutate(across(everything(), ~ ifelse(. == "<10,000", "99,999", .))) %>% 
  mutate(across(-Country, ~ as.numeric(gsub(",", "", .)))) %>% 
  rowwise() %>%
  mutate(Dominant = names(across(c("Christians", "Muslims", "Unaffiliated", "Hindus","Buddhists", "Folk Religions", "Other Religions", "Jews")))[
        which.max(c_across(c("Christians", "Muslims", "Unaffiliated", "Hindus","Buddhists", "Folk Religions", "Other Religions", "Jews")))]) %>%
  ungroup() %>% 
  slice(1:230)

rel <- piwo_1 %>% 
  left_join(wino_1, by = c("Year", "Country")) %>% 
  left_join(wodka_1, by = c("Year", "Country")) %>% 
  inner_join(religion, by = c("Country" = "Country"))

prod <- read.csv("wine-production.csv")
prod <- prod %>% 
  rename_with(~ "Production", .cols = 4) %>% 
  filter(Code != "") %>% 
  mutate(iso_a3 = countrycode(Entity, "country.name", "iso3c"))
world_simple <- world2 %>%
  select(name, iso_a3, geometry) %>%
  mutate(iso_a3 = countrycode(name, "country.name", "iso3c")) %>%
  distinct(iso_a3, .keep_all = TRUE)
  
################################################################################

ui1 <- fluidPage(style = "padding: 15px 60px 60px 40px;",
                 div(style = "text-align: center;", titlePanel("Z procentami po świecie - alkoholowe gusta świata")),
                 div(style = "padding: 20px 10px 40px 10px;",
                     shiny::markdown("Alkohol od wieków odgrywa istotną rolę w życiu społecznym i kulturowym wielu narodów. Towarzyszy ludziom podczas świąt, spotkań
                                    towarzyskich i tradycyjnych rytuałów. Ten typ napoju za święty obrali sobie przede wszytskim studenci i osoby, którym w życiu się 
                                    nie powiodło, potocznie zwani menelami. Choć spożywany z umiarem może być postrzegany jako element integrujący, jego nadmierne spożycie
                                    niesie za sobą poważne konsekwencje zdrowotne i społeczne. W różnych krajach i kulturach społecznych preferencje dotyczące rodzaju 
                                    spożywanego alkoholu są zróżnicowane. Niektórzy wolą piwo, inni wino,a jeszcze inni napoje wysokoprocentowe. O tych właśnie preferencjach 
                                    jest poniższa mapa. Tych ludzi łączy jednak niewątpliwie jedno - wszyscy przepadają za alkoholem.")),
                 
                 fluidRow(leafletOutput("alcohol_map", height = "600px")
                ))

################################################################################

ui2 <- fluidPage(style = "padding: 15px 60px 60px 40px;",
                div(style = "text-align: center;", titlePanel("Szczegółowe porównanie konsumpcji alkoholi na świecie na przestrzeni lat")),
                div(style = "padding: 20px 10px 40px 10px;",
                     shiny::markdown("W niniejszej zakładce, a właściwie zakładkach przedstawione zostało światowe zestawienie spożycia czystego alkoholu w litrach na osobę według roku. 
                      Dane dotyczą lat 1960-2019, a według własnego upodobania można kontrolować nie tylko wyświetlany na wykresie zakras lat w pierwszej podzakładce oraz rok w drugiej, ale także 
                      kraje, dla których dane mają być wyświetlane w zależności od roku. Odbiorca ma także możliwość manipulowania rodzajami alkoholu,
                      które go interesują. Do wyboru jest odpowiednio piwo, wino i alkohole wysokoprocentowe, takie jak np. wódka, rum czy whisky.")),
                tabsetPanel(id = "tabs",
                    tabPanel("Pierwsze porównanie",
                        fluidRow(
                            sidebarLayout(
                                sidebarPanel(style = "padding-bottom: 20px; padding-left: 20px; padding-right: 20px;",
                                      div(style = "text-align: center; padding-right: 50px; padding-left: 50px;",
                                          sliderInput("Years", "Zakres lat:",
                                                min = min(piwo_1$Year, na.rm=TRUE), max = max(piwo_1$Year, na.rm=TRUE),
                                                value= c(1960, 2019), step = 1, sep  = "", width = "100%")),
                                      div(style = "text-align: center;",
                                          pickerInput("Countries", "Zestawione kraje:",
                                                choices = sort(unique(piwo_1$Country)),
                                                selected = c("Germany", "United States", "Belize", "Iraq"), multiple = TRUE)),
                                      div(style = "text-align: center;",
                                          radioGroupButtons("source_data", "Rodzaj alkoholu:",
                                                choices = c("Piwo" = "beer", "Wino" = "wine", "Alkohole wysokoprocentowe" = "spirits"),
                                                selected = "beer")), width = 4.5),   
                            mainPanel(style = "padding-top: 20px;", width = 10.5, offset = 0.75, plotlyOutput("beer_plot", height = "475px") %>% withSpinner())))
                            ),
                    tabPanel("Drugie porównanie",
                        fluidRow(
                            sidebarLayout(
                                sidebarPanel(style = "padding-top: 30px;",
                                          pickerInput("compare_countries", "Zestawione kraje:", 
                                                choices  = sort(unique(piwo_1$Country)), 
                                                selected = c("Germany", "United States", "Belize", "Iraq"), multiple = TRUE),
                                          sliderInput("compare_year", "Rok zestawienia:",
                                                min = 1960, max = 2019, value = 2019, step = 1, sep = ""), width = 4),
                            mainPanel(style = "padding-top: 30px;", plotlyOutput("comparison_plot", height = "425px") %>% withSpinner(), width = 8)))
                            )
                  ))

################################################################################

ui3 <- fluidPage(style = "padding: 15px 60px 60px 40px;",
          div(style = "text-align: center;", titlePanel("Zgony na świecie spowodowane nadużyciem alkoholu")),
          div(style = "padding: 20px 10px 40px 10px;",
              shiny::markdown("W tej zakłace znajdziemy odniesienie do liczby zgonów spowodowanych nadmiernym spożyciem alkoholu w różnych krajach na świecie oraz współczynnika tych zgonów na 100 000 osób.
              Dane są zebrane z lat 1980-2021, a odbiorca ma najpierw możliwość wyboru liczby lub współczynnika, a następnie konkretnego roku oraz kontynentu, którego państwa chcemy zobaczyć. Wszystko to jest porównywane,
              a na wykresie w zależności od upodobania, wyświetlaja się zestawienie 12 krajów z największą lub najmniejszą liczbą zgonów w wybranym zestawieniu kontynentów, 
              w zależności od wybranego roku.")),
          sidebarLayout(
              sidebarPanel(style = "padding: 20px;",
                  radioButtons("fart", "Rodzaj metryki:",
                          choices = c("Dokładna liczba" = "top", "Współczynnik na 100,000 osób" = "bottom"), selected = "top"),
                  sliderInput("rok", "Rok zestawienia:", min = min(deaths$Year), max = max(deaths$Year),
                          value = max(deaths$Year), step = 1, sep = ""),
                  pickerInput("kontynent", "Kontynenty:",
                          choices  = sort(unique(deaths$Continent)), selected = c("Europe"), multiple = TRUE),
                  radioButtons("sort_mode", "Co ma być wyświetlane:",
                          choices = c("12 krajów z największą/ym" = "top", "12 krajów z najmniejszą/ym" = "bottom"), selected = "top"), width = 3
                          ),
              mainPanel(plotlyOutput("bar_plot", height = "450px") %>% withSpinner(), width = 9)
                ))

################################################################################

ui4 <- fluidPage(style = "padding: 15px 60px 60px 40px;",
          div(style = "text-align: center;", titlePanel("Konsumpcja alkoholu a religie dominujące w państwach świata")),
          div(style = "padding: 20px 10px 40px 10px;",
              shiny::markdown("Religia odgrywa bardzo ważną rolę w kulturze każdego kraju na świecie. Kształtuje ona powszechne społeczne przekonania, a także co dla na katolików 
              wydaje się nie do pomyślenia ustala w wielu częsciach naszej planety normy społeczne. Nie inaczej jest w powszechnej kwestii konsumpcji alkoholu.
              Poniżej za pomocą wykresu rozważana jest zależność pomiędzy średnim spożyciem czystego alkoholu w litrach na osobe na świecie w zależności od religii 
              dominującej. Po wybraniu odpowiednich religii wyświetla się boxplot wizualizujący zebrane w różnych krajach średnie spożycia.")),
          wellPanel(style = "padding: 20px;",
              fluidRow(
                  column(width = 6,
                      radioGroupButtons("rodzaj", "Rodzaj alkoholu:",
                          choices = c("Piwo" = "Liters.x", "Wino" = "Liters.y", "Alkohole wysokoprocentowe" = "Liters"),
                          selected = "Liters.x")),
                  column(width = 6,
                      pickerInput("rok", "Wybierz rok:", 
                          choices = sort(unique(rel$Year)), selected = 2019)
                        ))),
          
          mainPanel(plotlyOutput("boxplot", height = "400px") %>% withSpinner(), style = "padding-top: 20px;", width = 10.5, offset = 0.75)
                  )

################################################################################

ui5 <- fluidPage(style = "padding: 15px 60px 60px 40px;",
          div(style = "text-align: center;", titlePanel("Korelacja spożycia wybranych rodzajów alkoholu")),
          div(style = "padding: 20px 10px 40px 10px;",
              shiny::markdown("Ta zakładka pozwala na analizę zależności między spożyciem konkretnych rodzajów alkoholu, a wybranym krajem. 
              Odbiorca ma możliwość wyboru dwóch, koniecznie różnych rodzajó alkoholu do porównania, oraz kraj, które go interesuje, czyli dla którego 
              korelacja ma zostać przedstawiona.")),
          sidebarLayout(
              mainPanel(plotlyOutput("scatterPlot", height = "500px") %>% withSpinner(), width = 9),
              sidebarPanel(style = "padding: 20px;",
                  div(style = "text-align: center;",
                      pickerInput("alk1", "Pierwszy rodzaj alkoholu:",
                          choices = c("Piwo" = "beer", "Wino" = "wine", "Alkohole wysokoprocentowe" = "spirits"), selected = "beer")),
                  div(style = "text-align: center;",
                      pickerInput("alk2", "Drugi rodzaj alkoholu:",
                          choices = c("Piwo" = "beer", "Wino" = "wine", "Alkohole wysokoprocentowe" = "spirits"), selected = "wine")),
                  div(style = "text-align: center;",
                      pickerInput("country", "Wybrany kraj:", 
                          choices = sort(unique(c(piwo_1$Country, wino_1$Country, wodka_1$Country))), selected =  "Poland")), width = 3
                          ),
                ))

################################################################################

ui6 <- fluidPage(style = "padding: 15px 60px 60px 40px;",
          div(style = "text-align: center;", titlePanel("Produnkcja wina na świecie w latach 1961-2022")),
          div(style = "padding: 20px 10px 40px 10px;",
              shiny::markdown("Poniżej przedstawiona została interaktywna mapa prezentująca produkcję wina na świecie w latach 1961-2022.
              Użytkownik ma możliwość wyboru konkretnego roku z tego zestawienia, dla którego to mapka ma zostać wygenerowana, na podstawie 
              odpowiednich danych z tego właśnie okresu. Wyprodukowane litry wina są mierzone w tonach.")),
          fluidRow(
              column(width = 8, offset = 2,
                  div(style = "text-align: center;",
                      sliderInput("rok", "Rok zestawienia:", min = min(prod$Year), max = max(prod$Year), 
                              value = max(prod$Year), step = 1, sep = "", width = "100%")))),

          fluidRow(
              column(width = 10, offset = 1, leafletOutput("production_map", height = "600px"))
                ))

################################################################################

server <- function(input, output, session) {
  dane_wybrane <- reactive({
    switch(input$source_data, beer = piwo_1, wine = wino_1, spirits = wodka_1)
    })
  
  filtered <- reactive({
    dane_wybrane() %>%
      filter(Year >= input$Years[1],
             Year <= input$Years[2],
             Country %in% input$Countries)
    })
  
  output$beer_plot <- renderPlotly({
    dataf <- filtered()
    plott <- ggplot(dataf, aes(x = Year, y = Liters, group = Country, color = Country,
                          text = paste0(Country, "  |<i>  ", Year, "  </i>|<b>  ",round(Liters, 2), " </b>l/os"))) +
      geom_point(size = 0.75) +
      geom_path(size = 0.35) +
      scale_y_continuous(breaks = seq(0, max(piwo_1$Liters, na.rm=TRUE), by = 2), labels = label_number(suffix = " liters"), name = "litry na osobę czystego alkoholu") +
      scale_color_discrete(name = "Wybrane kraje") +
      labs(title = if (input$source_data=="beer") {paste0("Spożycie PIWA w wybranych krajach<br><sup>zestawienie z lat ", input$Years[1], "-", input$Years[2], "</sup>")} 
           else if (input$source_data=="wine") {paste0("Spożycie WINA w wybranych krajach<br><sup>zestawienie z lat ", input$Years[1], "-", input$Years[2], "</sup>")}
           else {paste0("Spożycie ALKOHOLI WYSOKOPROCENTOWYCH w wybranych krajach<br><sup>zestawienie z lat ", input$Years[1], "-", input$Years[2], "</sup>")}) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
        axis.line.x = element_line(color = "black", linetype = "dashed"), axis.line.y = element_line(color = "black", linetype = "dashed"),
        plot.title = element_text(size = 13, hjust = 0.5), plot.subtitle = element_text(size = 9, hjust = 0.5), panel.background = element_rect(fill = "#fffaf0"))
    
    ggplotly(plott, tooltip = "text") %>%
      style(marker = list(size = 4, sizemode = "diameter", line = list(width = 0.75, color = "black"))) %>%
      layout(hovermode = "x unified", xaxis = list(tickmode = "linear", dtick = 5), paper_bgcolor = "#fffaf0",
             hoverlabel = list(bgcolor = "white"))
    })
  
  dane_filtr <- reactive({
    df <- deaths %>%
      filter(Year == input$rok, Continent %in% input$kontynent, Metric == ifelse(input$fart == "top", "Liczba", "Współczynnik"))
    
    if (input$sort_mode == "top") {
      df <- df %>%
        arrange(desc(round(Amount))) %>%
        head(12)
    } else {
      df <- df %>%
        arrange(round(Amount)) %>%
        head(12)
    }
    df
    })
  
  output$bar_plot <- renderPlotly({
    sorted <- case_when(input$fart == "top" & input$sort_mode == "top" ~"12 państw z największą liczbą",
                        input$fart == "top" & input$sort_mode == "bottom" ~"12 państw z najmniejszą liczbą",
                        input$fart == "bottom" & input$sort_mode == "top" ~"12 państw z największym współczynnikiem",
                        input$fart == "bottom" & input$sort_mode == "bottom" ~"12 państw z najmniejszym współczynnikiem")
    continents <- paste(input$kontynent, collapse = ", ")
    dataf <- dane_filtr()
    plottt <- ggplot(dataf, aes(x = Amount, y = reorder(Country, Amount), fill = Country,
                                text = paste0("  <b>", comma(Rounded), "</b>  "))) +
      geom_col()+
      labs(x = NULL, y = NULL, title = paste0(ifelse(input$fart == "top", "Liczba zgonów", "Współczynnik zgonów na 100,000 osób"), " w ", input$rok, " roku", "<br><sup>", sorted, " - ", continents, "</sup>")) +
      theme_minimal()+
      theme(legend.position = "none", plot.title = element_text(size = 13, hjust = 0.5), axis.text.x = element_blank(), panel.background = element_rect(fill = "#fffaf0"))
    
    ggplotly(plottt, tooltip="text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")),
             xaxis = list(showgrid = FALSE), yaxis = list(showgrid = FALSE), paper_bgcolor = "#fffaf0")
    })

  output$alcohol_map <- renderLeaflet({
    pal <- colorFactor(palette = c("blue", "red", "yellow"), domain  = c("PIWO", "WINO", "ALKOHOL WYSOKOPROCENTOWY"), na.color = "lightgray")
    leaflet(map_data) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 1.5) %>%
      addPolygons(
        data = map_data,
        fillColor = ~pal(first),
        weight = 1.25,
        color = "black",
        fillOpacity = 0.5,
        popup = ~paste0("<b>", name, "</b><br/>Najczęściej spożywane rodzaje alkoholu: </br> 1. ", first, "</br> 2. ", second, "</br> 3. ", third),
        highlight = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)) %>% 
      addLegend(pal = pal, values = ~first, title = "Alkohol najczęściej konsumowany", position = "topright") %>% 
      addControl("<h4>Preferencje alkoholowe na świecie<h4>", position = "topleft")
    })

  output$comparison_plot <- renderPlotly({
    year_selected <- input$compare_year
      countries_selected <- input$compare_countries
    beer <- piwo_1 %>%
      filter(Year == year_selected, Country %in% countries_selected) %>%
      mutate(Type = "Piwo")
    wine <- wino_1 %>%
      filter(Year == year_selected, Country %in% countries_selected) %>%
      mutate(Type = "Wino")
    spirits <- wodka_1 %>%
      filter(Year == year_selected, Country %in% countries_selected) %>%
      mutate(Type = "Alkohole wysokoprocentowe")
    combined <- bind_rows(beer, wine, spirits)
  
    p <- ggplot(combined, aes(x = Country, y = Liters, fill = Type, text = paste0("  ", Type, ":  <b>", round(Liters, 2), "</b> l/os  "))) +
          geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
          labs(title = paste("Spożycie poszczególnych alkoholi w", year_selected, "roku w wybranych krajach"), y = "litry na osobę czystego alkoholu", x = NULL, fill = "Rodzaj alkoholu") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom",
                plot.title = element_text(size = 12, hjust = 0.5), panel.background = element_rect(fill = "#fffaf0"))
  
    ggplotly(p, tooltip = "text") %>% 
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")), xaxis = list(showgrid = TRUE), yaxis = list(showgrid = TRUE), paper_bgcolor = "#fffaf0", legend = list(bgcolor = "#fffaf0"))
      })
  
  output$scatterPlot <- renderPlotly({
    alk_translate_1 <- c(beer = "piwo", wine = "wino", spirits = "alkohole wysokoprocentowe")
    alk_translate_2 <- c(beer = "PIWEM", wine = "WINEM", spirits = "ALKOHOLAMI WYSOKOPROCENTOWYMI")
    
    dane <- piwo_1 %>%
      rename(beer = Liters) %>%
      full_join(wino_1 %>% rename(wine = Liters), by = c("Country", "Year")) %>%
      full_join(wodka_1 %>% rename(spirits = Liters), by = c("Country", "Year")) %>% 
      filter(Country == input$country)
    
    validate(
      need(input$alk1 != input$alk2, "Proszę wybrać dwa różne rodzaje alkoholu."))
    validate(
      need(all(c(input$alk1, input$alk2) %in% c("beer", "wine", "spirits")), "Wybrane kolumny nie istnieją w danych"))
    
    df <- dane %>%
      filter(!is.na(.data[[input$alk1]]), !is.na(.data[[input$alk2]])) %>%
      select(Country, Year, x = .data[[input$alk1]], y = .data[[input$alk2]])
    
    validate(
      need(nrow(df) > 0, "Brak danych dla wybranych kryteriów"))
    
    polt <- ggplot(df, aes(x = x, y = y, text = paste(" rok: <b>", Year, "</b><br>", alk_translate_1[[input$alk1]], ": <b>", round(x, 2), "</b>l/os", "<br>", alk_translate_1[[input$alk2]], ": <b>", round(y, 2), "</b>l/os"))) +
            geom_point(color = "blue", alpha = 0.75, size = 1.5) +
            geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = "red") +
            labs(title = paste("Zależność między", alk_translate_2[[input$alk1]], "a", alk_translate_2[[input$alk2]], "<br><sup>", input$country, ", zestawienie z lat 1960-2019<sup>"),
                x = paste("litry na osobę czystego alkoholu -", alk_translate_1[[input$alk1]]), y = paste("litry na osobę czystego alkoholu -", alk_translate_1[[input$alk2]])) +
            theme_bw()+
            theme(plot.title = element_text(size = 13, hjust = 0.5), plot.background = element_rect(fill = "#fffaf0"))
    
    ggplotly(polt, tooltip = "text") %>% 
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
    })
  
  dane_wybrane <- reactive({
    switch(input$source_data, beer = piwo_1, wine = wino_1, spirits = wodka_1)})
  
  filtered_brewery_data <- reactive({
    if (is.null(input$producenci) || length(input$producenci) == 0) {
      browary_dane
    } else {
      browary_dane %>% filter(brewery_name %in% input$producenci)
    }})
  
  output$boxplot <- renderPlotly({
    alk_tr <- c(Liters.x = "PIWA", Liters.y = "WINA", Liters = "ALKOHOLI WYSOKOPROCENTOWYCH")
    req(input$rodzaj, input$rok)
    rel_filtered <- rel %>%
      filter(Year == input$rok) %>%
      select(Country, Dominant, Alkohol = .data[[input$rodzaj]])
    
    pt <- ggplot(rel_filtered, aes(x = Alkohol, y = Dominant, fill = Dominant, text = paste(Country, "<br><b>  ", round(Alkohol, 2), "</b> l/os"))) +
          geom_point(alpha = 0.8, size = 3) +
          labs(title = paste("Rozkład średniej konsumpcji", alk_tr[[input$rodzaj]], "na świecie według religii <br><sup>w roku", input$rok, "</sup>"), x = "litry na osobę czystego alkoholu", y = NULL) +
          theme_linedraw()+
          theme(legend.position = "none", plot.title = element_text(size = 13, hjust = 0.5), plot.background = element_rect(fill = "#fffaf0", color = NA),
                panel.background = element_rect(fill = "#fffaf0", color = NA))
    
    ggplotly(pt, tooltip = "text")  
    })
  
  map_data_year <- reactive({
    req(input$rok)
    prod_year <- prod %>% 
      filter(Year == input$rok) %>% 
      select(iso_a3, Production)
    merged <- left_join(world_simple, prod_year, by = "iso_a3")
    return(merged)})
  
  output$production_map <- renderLeaflet({
    data <- map_data_year()
    pal <- colorBin(palette = "YlOrRd", domain = data$Production, bins = c(0, 10000, 30000, 100000, 300000, 1e6, 3e6, Inf), na.color = "lightgray")
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 1.5) %>%
      addPolygons(
        data = data,
        fillColor = ~pal(Production),
        weight = 1.25,
        color = "black",
        fillOpacity = 0.6,
        popup = ~paste0("<b>", name, "</b><br/>Wielkość produkcji wina: </br>   ", comma(round(Production)), " ton"),
        highlight = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)) %>% 
      addLegend(pal = pal, values = ~Production, title = "Produkcja wina (w tonach)", position = "topright") 
    })

}

################################################################################

app_ui <- navbarPage(title = "   ALKOHOL   ",
  tabPanel("Wstęp", ui1, icon = icon("house")),
  tabPanel("Konsumpcja", ui2, icon = icon("crown")),
  tabPanel("Religia", ui4, icon = icon("cross")),
  tabPanel("Zgony", ui3, icon = icon("skull-crossbones")),
  tabPanel("Korelacja", ui5, icon = icon("link")), 
  tabPanel("Ciekawostka", ui6, icon = icon("copyright")), 
  theme = shinytheme("united"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr><p class='text-center' style='font-size:16px;'>
                  Krzysztof Michalak, Stanisław Pawlak, Przemysław Kania; © 2025 MiNI: <a class='text-dark' href='https://www.youtube.com/watch?v=dIKYWxRkp78' target='_blank'>jabol</a></p></footer>"),
  tags$head(tags$style(HTML("body {background-color: #fffaf0 !important;}.well{background-color: #f57542 !important;border-color: #bf0d0d;border-radius: 12px;}
                            .well label, .well .control-label {color: white !important;}.bootstrap-select .dropdown-toggle {background-color: #fffaf0 !important;
                            border-color: #e85c29 !important;color: #e85c29 !important;}.btn-group-toggle .btn {background-color: #fffaf0 !important;
                            border-color: #e85c29 !important;color: #e85c29 !important;}.btn-group-toggle .btn.active {background-color: #fffaf0 !important;
                            color: #e85c29 !important;font-weight: bold;}select.form-control {background-color: #fffaf0 !important;border-color: #ffffff !important;
                            font-weight: bold;border-radius: 8px;}select.form-control:focus {border-color: #e85c29 !important;box-shadow: 0 0 5px rgba(232, 92, 41, 0.5);}")))
                      )

shinyApp(app_ui, server)




