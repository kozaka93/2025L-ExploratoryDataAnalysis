library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(plotly)
library(readr)
library(rnaturalearth)
library(sf)
library(RColorBrewer)
library(DT)
library(scales)
library(shinycssloaders)


# --- Wczytywanie danych ---
df <- read_csv("200k_blitz_rapid_classical_bullet.csv")
dane <- read.csv('GM_players_statistics.csv')
# dane2 <- read.csv('standard_rating_list1.csv', header = FALSE, sep = ";")
dane2 <- read.csv('standard_rating_list1.csv', header = FALSE, sep = ";", fileEncoding = "Latin1")

# --- WSTĘPNE PRZETWARZANIE DANYCH (GLOBALNE) ---
df <- df %>%
  filter(!is.na(TimeControl), Result %in% c("1-0", "0-1", "1/2-1/2")) %>%
  mutate(
    TimeSeconds = as.numeric(gsub("\\+.*", "", TimeControl)),
    TimeCategory = case_when(
      TimeSeconds <= 180 ~ "Bullet",
      TimeSeconds <= 600 ~ "Blitz",
      TimeSeconds <= 1800 ~ "Rapid",
      TimeSeconds > 1800 ~ "Classical",
      TRUE ~ "Unknown"
    ),
    Outcome = case_when(
      Result == "1-0" ~ "White wins",
      Result == "0-1" ~ "Black wins",
      TRUE ~ "Draw"
    ),
    MoveCount = rowSums(!is.na(select(., starts_with("Move_ply_"))))
  )


dane2 <- dane2[-1,-c(5,6,8,9)]
players1 <- data.frame(
  Id = dane2$V1,
  Name = dane2$V2,
  Rating = dane2$V7,
  Country = dane2$V3,
  Sex = dane2$V4,
  Birthday = dane2$V10,
  Flag = dane2$V11)

iso_codes <- read.csv("wikipedia-iso-country-codes.csv")
iso_codes <- iso_codes %>% rename(Country = English.short.name.lower.case, Code = Alpha.3.code) %>% select(Country, Code)

players1 <- inner_join(players1,iso_codes, by = c("Country" = "Code")) %>% select(-Country) %>% rename(Country = Country.y)
players1$Rating <- as.numeric(as.character(players1$Rating))
players1$Birthday <- as.numeric(as.character(players1$Birthday))

# --- Define UI for application ---
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Analiza Partii Szachowych"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Analiza graczy", tabName = "Agnieszka", icon = icon("chess-rook")),
                        menuItem("Analiza Wyników Arcymistrzów", tabName = "gm", icon = icon("chess-knight")),
                        menuItem("Analiza Statystyczna Partii", tabName = "moje_statystyki", icon = icon("chess"))
                        # Usunięto: menuItem("Wpływ Rankingu na Wynik", tabName = "ranking_wynik", icon = icon("trophy"))
                      )
                    ),
                    
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      
                      tabItems(
                        # --- Zakładka Agnieszki ---
                        tabItem(tabName = "Agnieszka",
                                
                                fluidRow(
                                  column(width = 3, box(selectInput("countryInput2", "Wybierz kraj:",
                                                                    choices = c("Świat" = "all", sort(unique(players1$Country))),
                                                                    selected = c("Poland", "Russia", "France"), multiple = TRUE),width = 12),
                                         
                                         box(title = "Najlepsi szachiści",
                                             withSpinner(DTOutput("filteredTable"),type = 7, color = "purple"), width = 12)),
                                  column(width = 9, 
                                         fluidRow(column(width = 8,box(title = "Liczba aktywnych graczy i średni ranking (maj 2025)",withSpinner(uiOutput("countryStatsBoxes", ),type = 7, color = "purple"),width = 12)),
                                                  column(width = 4,withSpinner(plotlyOutput("kobmen",height = "200px"),type = 7 ,color = "purple"))),
                                         fluidRow(
                                           column(width = 5, box(withSpinner(plotlyOutput("countrysgensity",height = "250px"),type = 7, color = "purple"),width = 12),),
                                           column(width = 7, box(withSpinner(plotlyOutput("birthYearLinePlot",height = "250px"),type = 7,color = "purple"),width = 12))
                                           
                                           
                                         ),
                                         fluidRow(
                                           box(title = "Gęstość kobiet i mężczyzn wg rankingu",
                                               withSpinner(uiOutput("densityChartsUI"),type = 7, color = "purple"),width = 12)
                                           
                                         )
                                  )
                                ),
                                fluidRow(
                                  box(
                                    width = 12,
                                    htmlOutput("opiszakladki")
                                  )
                                )
                                
                                
                        ),
                        # --- Zakładka Roksany ---
                        tabItem(tabName = "gm",
                                h2("Analiza Wyników Arcymistrzów (GM)"),
                                tabBox(
                                  width = 12,
                                  id = "roksanaTabs",
                                  
                                  # zakładka 1 wyniki w zależności od trybu gry
                                  tabPanel("Wyniki", icon = icon("chart-line"),
                                           fluidRow(
                                             box(
                                               title = "Filtry dla wykresów wyników", status = "primary", solidHeader = TRUE, width = 12,
                                               sliderInput(
                                                 inputId = "fideRangeFilter",
                                                 label = "Zakres rankingu FIDE:",
                                                 min = floor(2200),
                                                 max = ceiling(max(dane$fide, na.rm = TRUE)),
                                                 value = c(2200, 2800)
                                               ),
                                               checkboxGroupInput(
                                                 inputId = "gameTypes",
                                                 label = "Wybierz typy gier:",
                                                 choices = c("Rapid", "Blitz", "Bullet"),
                                                 selected = c("Rapid", "Blitz", "Bullet"),
                                                 inline = TRUE
                                               )
                                             )
                                           ),
                                           fluidRow(
                                             column(
                                               width = 4,
                                               box(
                                                 title = "Ranking FIDE vs Liczba wygranych", status = "info", solidHeader = TRUE, width = 12,
                                                 plotlyOutput("multiModeWinPlot", height = "300px")
                                               )
                                             ),
                                             column(
                                               width = 4,
                                               box(
                                                 title = "Ranking FIDE vs Liczba remisów", status = "info", solidHeader = TRUE, width = 12,
                                                 plotlyOutput("multiModeDrawPlot", height = "300px")
                                               )
                                             ),
                                             column(
                                               width = 4,
                                               box(
                                                 title = "Ranking FIDE vs Liczba przegranych", status = "info", solidHeader = TRUE, width = 12,
                                                 plotlyOutput("multiModeLossPlot", height = "300px")
                                               )
                                             )
                                           ),
                                           htmlOutput("opisWykresow")
                                  ),
                                  
                                  # zakładka 2 mapa 
                                  tabPanel("Mapa i Liczba Partii", icon = icon("globe"),
                                           fluidRow(
                                             box(
                                               title = "Filtry dla mapy", status = "primary", solidHeader = TRUE, width = 12,
                                               radioButtons(
                                                 inputId = "streamerFilter",
                                                 label = "Pokaż dane dla:",
                                                 choices = c("Wszyscy" = "all", "Streamerzy" = "TRUE", "Nie-streamerzy" = "FALSE"),
                                                 selected = "all",
                                                 inline = TRUE
                                               )
                                             )
                                           ),
                                           fluidRow(
                                             box(
                                               title = "Liczba arcymistrzów szachowych (GM) na Chess.com wg kraju", status = "info", solidHeader = TRUE, width = 12,
                                               plotlyOutput("mapPlot", height = "500px")
                                             )
                                           ),
                                           fluidRow(
                                             box(
                                               title = "Łączna liczba rozegranych partii przez GM-ów", status = "info", solidHeader = TRUE, width = 12,
                                               plotlyOutput("totalGamesPlot", height = "300px")
                                             )
                                           ),
                                           fluidRow(
                                             box(
                                               width = 12,
                                               htmlOutput("opisMapyGM")
                                             )
                                           )
                                  )
                                  
                                )
                        ),
                        # Analiza Statystyczna Partii ---
                        tabItem(tabName = "moje_statystyki",
                                h2("Analiza Statystyczna Partii Szachowych"),
                                
                                tabBox(
                                  width = 12,
                                  id = "tabset1",
                                  
                                  # --- ZAKŁADKA 1: Długość Partii ---
                                  tabPanel("Długość Partii", icon = icon("ruler-horizontal"),
                                           fluidRow(
                                             box(title = "Rozkład długości partii (liczba ruchów)", status = "info", solidHeader = TRUE, width = 12,
                                                 sliderInput(
                                                   inputId = "dlugoscPartiiEloRangeFilter",
                                                   label = "Zakres rankingu ELO:",
                                                   min = floor(min(df$WhiteElo, df$BlackElo, na.rm = TRUE)), # Zakładam, że df jest wczytane i dostępne
                                                   max = ceiling(max(df$WhiteElo, df$BlackElo, na.rm = TRUE)),
                                                   value = c(floor(min(df$WhiteElo, df$BlackElo, na.rm = TRUE)), ceiling(max(df$WhiteElo, df$BlackElo, na.rm = TRUE)))
                                                 ),
                                                 sliderInput("binwidthSlider", "Szerokość przedziałów (binwidth):", min = 1, max = 20, value = 5, step = 1),
                                                 plotlyOutput("dlugoscPartiiPlot")
                                             ),
                                             # --- Opis dla Długości Partii w osobnym boxie ---
                                             box(title = "O Długości Partii i Rankingu ELO", status = "primary", solidHeader = TRUE, width = 12,
                                                 div(
                                                   h4("Czym jest 'Długość Partii'?"),
                                                   p("Ten wykres wizualizuje, ile ruchów średnio trwały partie szachowe. Na osi poziomej (", strong("Liczba ruchów (ply)"), ") zobaczysz, jak długie były poszczególne partie. Pamiętaj, że 1 ", strong("ply"), " to jeden półruch (ruch białych lub czarnych), więc 100 ", strong("ply"), " oznacza 50 pełnych ruchów."),
                                                   h4("Ranking ELO – wskaźnik siły graczy:"),
                                                   p("Ranking ELO to powszechnie stosowany ", strong("system oceny umiejętności szachistów na całym świecie"), ". Działa na zasadzie porównywania wyników graczy w poszczególnych partiach. Kiedy wygrywasz z silniejszym przeciwnikiem (o wyższym ELO), zyskujesz więcej punktów ELO, a kiedy przegrywasz ze słabszym – tracisz więcej. I odwrotnie – wygrana ze słabszym daje mniej punktów, a przegrana z silniejszym skutkuje mniejszą stratą. Dzięki temu systemowi, wyższy ranking ELO automatycznie oznacza, że gracz jest statystycznie silniejszy."),
                                                   p("Dzięki filtrowi ", strong("Zakres rankingu ELO"), " możesz zobaczyć, czy doświadczeni gracze (o wyższym ELO) rozgrywają dłuższe, czy krótsze partie, i jak zmienia się ich dynamika w zależności od poziomu umiejętności. To pozwala na ciekawe porównania między graczami o różnym zaawansowaniu.")
                                                 )
                                             )
                                           )
                                  ),
                                  
                                  # --- ZAKŁADKA 2 : Popularność i Skuteczność Debiutów ---
                                  tabPanel("Popularność i Skuteczność Debiutów", icon = icon("chess-king"),
                                           fluidRow(
                                             # Cały opis w jednym, początkowym boxie
                                             box(title = "Wprowadzenie do debiutów szachowych", status = "primary", solidHeader = TRUE, width = 12,
                                                 h3("Popularność i Skuteczność Debiutów Szachowych"),
                                                 h4("Czym są Debiuty?"),
                                                 p("W szachach, ", strong("debiut"), " to początkowa faza partii, charakteryzująca się serią ustalonych, często wielowiekowych ruchów. Można to porównać do strategii otwarcia w innych grach czy sportach. Celem debiutów jest przede wszystkim:"),
                                                 tags$ul(
                                                   tags$li(strong("Kontrola centrum szachownicy"), ": Centralne pola są kluczowe dla mobilności figur."),
                                                   tags$li(strong("Rozwinięcie figur"), ": Ustawienie figur (skoczków, gońców, wież) na optymalnych pozycjach, aby były gotowe do ataku i obrony."),
                                                   tags$li(strong("Bezpieczeństwo króla"), ": Zazwyczaj poprzez roszadę i stworzenie solidnej struktury pionków.")
                                                 ),
                                                 p("Dla szachistów wybór odpowiedniego debiutu to kluczowa decyzja, która często decyduje o przebiegu całej partii. Istnieje wiele różnych debiutów, każdy z własną nazwą i charakterystycznymi ideami."),
                                                 
                                                 h4("Co przedstawiają wykresy?"),
                                                 p(strong("„Top N najczęściej granych debiutów”"), ": Ten wykres jest Twoim przewodnikiem po najpopularniejszych otwarciach w szachach. Pokazuje, które debiuty są najczęściej wybierane przez graczy w naszym zbiorze danych. Im dłuższy słupek, tym częściej dany debiut pojawiał się w partiach. To daje obraz dominujących trendów wśród szachistów."),
                                                 p(strong("„Skuteczność Top N debiutów”"), ": Ten wykres idzie o krok dalej i analizuje, jak skuteczny jest dany debiut w praktyce, czyli jakie wyniki osiągają gracze, którzy go stosują. Możesz wybrać, co chcesz zobaczyć:"),
                                                 tags$ul(
                                                   tags$li(strong("Procentowy Wynik (Stackowany)"), ": To kompleksowy widok. Pokazuje procentowy rozkład wyników dla każdego debiutu: ile partii zakończyło się wygraną Białych, wygraną Czarnych lub remisem. Dzięki temu od razu widzisz, czy dany debiut preferuje jedną ze stron, czy prowadzi do wielu remisów."),
                                                   tags$li(strong("Procent Wygranych Białych/Czarne/Remisów"), ": Jeśli chcesz skupić się na konkretnym rezultacie, te opcje pokażą Ci, jak często dany debiut prowadzi do zwycięstwa białych, czarnych lub do remisu, niezależnie od innych wyników.")
                                                 ),
                                                 h4("Filtry dla lepszej analizy:"),
                                                 p(strong("Zakres rankingu ELO dla debiutów"), ": (Przypominamy, że Ranking ELO to międzynarodowy system oceny siły graczy – im wyższe ELO, tym silniejszy gracz. Więcej szczegółów na temat ELO znajdziesz w zakładce ", em("Długość Partii"), "). Ten filtr pozwala Ci sprawdzić, czy popularność i skuteczność debiutów zmienia się w zależności od poziomu umiejętności graczy."),
                                                 p(strong("Liczba debiutów (TOP N)"), ": Dzięki temu suwakowi możesz dostosować, ile najpopularniejszych debiutów chcesz analizować na wykresach – od 5 do 30.")
                                             ),
                                             
                                             # Następnie idą filtry i wykresy, każdy w swoim boxie
                                             box(title = "Filtry dla Debiutów", status = "info", solidHeader = TRUE, width = 12,
                                                 # Filtrowanie po zakresie ELO graczy
                                                 sliderInput(
                                                   inputId = "debiutyEloRangeFilter",
                                                   label = "Zakres rankingu ELO dla debiutów:",
                                                   min = floor(min(df$WhiteElo, df$BlackElo, na.rm = TRUE)),
                                                   max = ceiling(max(df$WhiteElo, df$BlackElo, na.rm = TRUE)),
                                                   value = c(floor(min(df$WhiteElo, df$BlackElo, na.rm = TRUE)), ceiling(max(df$WhiteElo, df$BlackElo, na.rm = TRUE)))
                                                 ),
                                                 # Dynamiczny wybór metryki skuteczności
                                                 radioButtons(
                                                   inputId = "skutecznoscDebiutowMetric",
                                                   label = "Pokaż skuteczność debiutów:",
                                                   choices = c(
                                                     "Procentowy Wynik (Stackowany)" = "Stacked",
                                                     "Procent Wygranych Białych" = "WhiteWinRate",
                                                     "Procent Wygranych Czarnych" = "BlackWinRate",
                                                     "Procent Remisów" = "DrawRate"
                                                   ),
                                                   selected = "Stacked",
                                                   inline = TRUE
                                                 ),
                                                 sliderInput(
                                                   inputId = "topNFilter",
                                                   label = "Liczba debiutów (TOP N):",
                                                   min = 5, max = 30, value = 10, step = 1
                                                 )
                                             ),
                                             box(title = "Top N najczęściej granych debiutów", status = "info", solidHeader = TRUE, width = 12,
                                                 plotlyOutput("najczestrzeDebiutyPlot")
                                             ),
                                             box(title = "Skuteczność Top N debiutów", status = "info", solidHeader = TRUE, width = 12,
                                                 plotlyOutput("skutecznoscDebiutowPlot")
                                             )
                                           )
                                  ),
                                  
                                  # --- ZAKŁADKA 3 : Ruchy w Formacie ---
                                  tabPanel("Ruchy w Formacie", icon = icon("chess-board"),
                                           fluidRow(
                                             box(title = "Dynamika Ruchów w Formacie", status = "info", solidHeader = TRUE, width = 12,
                                                 radioButtons("metricChoice", "Wybierz Metrykę:",
                                                              choices = c("Średnia" = "AvgMoves", "Mediana" = "MedianMoves"),
                                                              selected = "AvgMoves", inline = TRUE),
                                                 checkboxInput("showGlobalAverage", "Pokaż globalną średnią/medianę", FALSE),
                                                 plotlyOutput("sredniaLiczbaRuchowPlot")
                                             ),
                                             # --- Opis dla Ruchów w Formacie w osobnym boxie ---
                                             box(title = "Czym są 'Ruchy w Formacie'?", status = "primary", solidHeader = TRUE, width = 12,
                                                 div(
                                                   h4("Czym jest 'Ruchy w Formacie'?"),
                                                   p("Ta zakładka analizuje, jak ", strong("format czasowy"), " partii szachowych wpływa na ich długość, mierzoną liczbą ruchów. Formaty czasowe to różne warianty partii, od bardzo szybkich (np. Blitz) do klasycznych, gdzie gracze mają dużo czasu na przemyślenie każdego posunięcia."),
                                                   h4("Jak interpretować wykres?"),
                                                   p("Wykres pokazuje średnią lub medianę liczby ruchów (w zależności od wybranej metryki) dla różnych formatów czasowych. Oś pozioma przedstawia formaty (np. Blitz, Bullet, Classical, Rapid), a oś pionowa – liczbę ruchów. Wyższe słupki oznaczają, że partie w danym formacie trwają średnio dłużej."),
                                                   h4("Metryki:"),
                                                   tags$ul(
                                                     tags$li(strong("Średnia"), ": Pokazuje średnią liczbę ruchów dla danego formatu."),
                                                     tags$li(strong("Mediana"), ": Pokazuje medianę liczby ruchów (wartość środkowa). Mediana jest mniej wrażliwa na skrajne wartości niż średnia.")
                                                   ),
                                                   h4("Globalna średnia/mediana:"),
                                                   p("Jeśli zaznaczysz opcję ", strong("Pokaż globalną średnią/medianę"), ", na wykresie pojawi się dodatkowa linia, pokazująca średnią/medianę liczby ruchów dla wszystkich partii, niezależnie od formatu. To pozwala na porównanie poszczególnych formatów z ogólnym trendem.")
                                                 )
                                             )
                                           )
                                  )
                                ) # Koniec tabBox
                        ) # Koniec tabItem "moje_statystyki"
                      ) # Koniec tabItems
                    ) # Koniec dashboardBody
) # Koniec ui (dashboardPage)

# --- WŁAŚCIWY BLOK KODU SERVER ---
server <- function(input, output, session) {
  
  # --- MIEJSCE NA Wczytywanie i przetwarzanie danych Agnieszki ---

  #Zakładka 1 – Tabela z Grand Masterami
  filteredData <- reactive({
    sel <- input$countryInput2
    if (is.null(sel) || length(sel) == 0 || "all" %in% sel) {
      data <- players1 %>% filter(!Flag %in% c("i","wi")) %>% select("Name", "Rating", "Country")
    } else {
      data <- subset(players1, Country %in% sel & !Flag %in% c("i","wi"), select = c("Name", "Rating", "Country"))
    }
    
    data <- data[order(-data$Rating), ]
    
    return(data)
  })
  
  output$filteredTable <- DT::renderDT({
    DT::datatable(filteredData(), 
                  rownames = FALSE,
                  options = list(
                    order = list(list(1, 'desc')),
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    pageLength = 10,
                    processing = FALSE 
                  ))
  })
  
  
  filteredData2 <- reactive({
    sel <- input$countryInput2
    
    if (is.null(sel) || length(sel) == 0 || "all" %in% sel) {
      data <- players1
    } else {
      data <- subset(players1, Country %in% sel)
    }
    
    
    return(data)
  })
  output$countryStatsBoxes <- renderUI({
    countries <- input$countryInput2
    
    if (is.null(countries) || "all" %in% countries) {
      data <- filteredData2() %>% filter(!Flag %in% c("i,wi"))
      sr <- round(mean(filteredData2()$Rating, na.rm = TRUE), 1)
      n <- nrow(data)
      return(
        fluidRow(
          valueBox(
            value = paste(sr),
            subtitle = paste(n, "graczy - świat"),
            icon = icon("users"),
            color = "purple",
            width = 12
          )
        )
      )
    }
    
    boxes <- lapply(countries, function(ctry) {
      country_data <- filteredData2() %>% filter(Country == ctry)
      data <- filteredData2() %>% filter(!Flag %in% c("i,wi")) %>% filter(Country == ctry)
      avg_rating <- round(mean(country_data$Rating, na.rm = TRUE), 1)
      valueBox(
        value = paste(avg_rating),
        subtitle = paste(nrow(data),"graczy","-",ctry),
        icon = icon("users"),
        color = "purple",
        width = 4
      )
    })
    
    do.call(fluidRow, boxes)
  })
  #Wykres countrysgensity
  output$countrysgensity <- renderPlotly({
    data <- filteredData2()
    if("all" %in% input$countryInput2 & any(data$Country %in% input$countryInput2)){
      violet_palette <- c("#8A2BE2","#FF00FF","#9932CC","#8B008B","#4B0082","#EE82EE", "#8A2BE2")
      world_color <- c("World" = "#6A0DAD")
      selected_data <- data %>% filter(Country %in% input$countryInput2)
      countries <- unique(selected_data$Country)
      country_colors <- setNames(
        rep(violet_palette, length.out = length(countries)),
        countries
      )
      
      
      all_colors <- c(world_color, country_colors)
      
      plocik <- ggplot() + 
        geom_density(data = data, aes(x = Rating, fill = "World"), alpha = 0.3) +
        geom_density(data = selected_data, aes(x = Rating, fill = Country), alpha = 0.5) +labs(title = "Rozkład rankingu graczy", x = "Ranking", y = "Gęstość")+
        theme_minimal() +
        scale_fill_manual(values = all_colors, name = "Państwa")
      
    }
    else if("all" %in% input$countryInput2 || is.null(input$countryInput2)){
      
      plocik <- data %>% select(Rating) %>% ggplot(aes(x = Rating)) + geom_density(alpha = 0.5, fill = "purple") + labs(title = "Rozkład rankingu graczy", x = "Ranking", y = "Gęstość")+
        theme_minimal()
    }
    
    else{
      violet_palette <- c("#8A2BE2","#FF00FF","#9932CC","#8B008B","#4B0082","#EE82EE", "#8A2BE2")
      plocik <- data %>% select(Country, Rating) %>% ggplot(aes(x = Rating, fill = Country)) + geom_density(alpha = 0.5) + scale_fill_manual(values = violet_palette, name = "Państwa")+
        labs(title = "Rozkład rankingu graczy", x = "Ranking", y = "Gęstość")+
        theme_minimal()
    }
    ggplotly(plocik)
    
  })
  
  
  output$densityChartsUI <- renderUI({
    countries <- input$countryInput2
    
    if (is.null(countries) || "all" %in% countries) {
      return(plotlyOutput("density_all"))
    }
    
    n <- length(countries)
    cols_per_chart <- floor(12 / n)
    
    plot_outputs <- lapply(seq_along(countries), function(i) {
      country <- countries[i]
      plotname <- paste0("densityChart_", i)
      column(cols_per_chart,
             plotlyOutput(plotname, height = "150px"))
    })
    
    do.call(fluidRow, plot_outputs)
  })
  
  observe({
    countries <- input$countryInput2
    
    if ("all" %in% countries || is.null(countries)) {
      output$density_all <- renderPlotly({
        data <- filteredData2()
        jol <- data %>%
          filter(!is.na(Rating), !is.na(Sex)) %>%
          ggplot(aes(x = Rating, fill = Sex)) +
          geom_density(alpha = 0.5) +
          scale_fill_manual(values = c("M" = "purple", "F" = "#ff99ff"),name = "Płeć") +
          labs(title = "Wszyscy gracze", x = "Ranking", y = "Gęstość") +
          theme_minimal()
        ggplotly(jol)
      })
    } else {
      for (i in seq_along(countries)) {
        local({
          my_i <- i
          country <- countries[my_i]
          plotname <- paste0("densityChart_", my_i)
          
          output[[plotname]] <- renderPlotly({
            data <- filteredData2() %>% filter(Country == country)
            ddd <- data %>%
              filter(!is.na(Rating), !is.na(Sex)) %>%
              ggplot(aes(x = Rating, fill = Sex)) +
              geom_density(alpha = 0.5) +
              scale_fill_manual(values = c("M" = "purple", "F" = "#ff99ff"), name = "Płeć") +labs(title = country, x = "Ranking", y = "Gęstość") + 
              theme_minimal()
            ggplotly(ddd)
          })
        })
      }
    }
  })
  
  #wykres liniowy
  output$birthYearLinePlot <- renderPlotly({
    data <- filteredData2()
    if("all" %in% input$countryInput2 & any(data$Country %in% input$countryInput2)){
      
      world_data <- data %>%
        filter(Birthday != 0, !is.na(Birthday)) %>%
        group_by(Birthday) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(Country = "World")
      
      selected_data <- data %>%
        filter(Country %in% input$countryInput2, Birthday != 0, !is.na(Birthday)) %>%
        group_by(Birthday, Country) %>%
        summarise(count = n(), .groups = "drop")
      
      combined_data <- bind_rows(world_data, selected_data)
      
      unique_countries <- unique(selected_data$Country)
      
      violet_palette <- c("#8A2BE2","#FF00FF","#9932CC","#8B008B","#4B0082","#EE82EE", "#8A2BE2")
      country_colors <- setNames(
        rep(violet_palette, length.out = length(unique_countries)),
        unique_countries
      )
      
      all_colors <- c("World" = "purple", country_colors)
      
      plot <- ggplot(combined_data, aes(x = Birthday, y = count, color = Country)) +
        geom_line() +
        scale_color_manual(values = all_colors, name = "Państwa") +
        labs(x = "Rok urodzenia", y = "Liczba graczy", title = "Liczba graczy wg roku urodzenia") +
        theme_minimal()
      
      ggplotly(plot)
      
      
    }
    else if("all" %in% input$countryInput2 || is.null(input$countryInput2)){
      plot <- data %>% filter(Birthday != 0) %>%
        group_by(Birthday) %>%
        summarise(count = n(),.groups = "drop") %>%
        filter(!is.na(Birthday)) %>%
        ggplot(aes(x = Birthday, y = count)) +
        geom_line(color = "purple") + 
        labs(x = "Rok urodzenia", y = "Liczba graczy", title = "Liczba graczy wg roku urodzenia") +
        theme_minimal() 
    }
    else{
      violet_palette <- c("#8A2BE2","#FF00FF","#9932CC","#8B008B","#4B0082","#EE82EE", "#8A2BE2")
      plot <- data %>% filter(Birthday != 0) %>%
        group_by(Birthday,Country) %>%
        summarise(count = n(),.groups = "drop") %>%
        filter(!is.na(Birthday)) %>%
        ggplot(aes(x = Birthday, y = count,color = Country)) +
        geom_line() +
        labs(x = "Rok urodzenia", y = "Liczba graczy", title = "Liczba graczy wg roku urodzenia") + scale_color_manual(values = violet_palette, name = "Państwa")+
        theme_minimal()
    }
    
    ggplotly(plot)
  })
  
  #wykres liczba kobiet vs mezczyzn
  output$kobmen <- renderPlotly({
    countries <- input$countryInput2
    
    data <- filteredData2()
    
    
    if (is.null(countries) || "all" %in% countries) {
      df <- data %>% filter(!is.na(Sex)) %>%
        group_by(Sex) %>%
        summarise(count = n(), .groups = "drop")
      
      plot <- ggplot(df, aes(x = Sex, y = count, fill = Sex)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Liczba graczy wg płci (globalnie)",x = NULL, y = NULL) + scale_fill_manual(values = c("M" = "purple", "F" = "#ff99ff")) +
        theme_minimal()
      
    } else {
      df <- data %>% filter(Country %in% countries, !is.na(Sex)) %>%
        group_by(Country, Sex) %>%
        summarise(count = n(), .groups = "drop")
      
      plot <- ggplot(df, aes(x = Country, y = count, fill = Sex)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Liczba graczy wg płci i kraju", x = NULL, y = NULL) +
        scale_fill_manual(values = c("M" = "purple", "F" = "#ff99ff"), name = "Płeć") +
        theme_minimal()
    }
    
    ggplotly(plot)
  })
  
  output$opiszakladki <- renderUI({
    HTML("
    <div style='padding: 20px; font-size: 15px; line-height: 1.6; background-color: #f9f9f9; border-radius: 10px; margin-top: 30px;'>
      <h4>Analiza graczy szachowych (maj 2025)</h4>
      <p>
        W zakładce użytkownik może wybrać jedno, kilka państw albo cały świat, aby przeanalizować dane dotyczące graczy zarejestrowanych w bazie gier standardowych FIDE od 1920r do maja 2025.
        Na podstawie tych danych przygotowano zestaw wizualizacji, które pozwalają lepiej zrozumieć strukturę i charakterystykę szachowej społeczności.
      </p>

      <h4>1. Tabela najlepszych graczy</h4>
      <p>
        Interaktywna tabela prezentuje aktywnych szachistów z najwyższym rankingiem standardowym w wybranych krajach. 
        Dane można sortować. Widoczne są: imię i nazwisko gracza, jego ranking oraz kraj pochodzenia.
      </p>

      <h4>2. Statystyki krajowe</h4>
      <p>
        Value boxy pokazują:
      </p>
      <ul>
        <li>Średni ranking graczy w wybranym kraju lub globalnie,</li>
        <li>Łączną liczbę aktywnych szachistów.</li>
      </ul>
      

      <h4>3. Liczba graczy według płci</h4>
      <p>
        Wykres słupkowy przedstawia liczbę zarejestrowanych kobiet i mężczyzn w szachach w poszczególnych krajach.
        Widoczna jest wyraźna przewaga liczby mężczyzn w większości państw.
      </p>

      <h4>4. Rozkład rankingu wg kraju</h4>
      <p>
        Wykres gęstości pokazuje, jak wygląda rozkład rankingu w wybranych państwach na podstawie wszystkich zarejestrowanych graczy od 1920r. 
        Można dzięki temu porównać, które kraje mają graczy o wyższych (lub niższych) średnich rankingach – 
        co pozwala identyfikować „silniejsze” szachowo kraje.
      </p>

      <h4>5. Ranking a płeć</h4>
      <p>
        Dodatkowe wykresy gęstości ilustrują rozkład rankingu z podziałem na płeć na podstawie wszystkich zarejestrowanych graczy od 1920r.
        Widać, że gęstość dla kobiet jest przesunięta nieco w lewo względem mężczyzn – co sugeruje, że kobiety mają statystycznie niższe rankingi. 
        Różnica ta widoczna jest zarówno globalnie, jak i w poszczególnych krajach.
      </p>

      <h4>6. Liczba graczy według roku urodzenia</h4>
      <p>
        Wykres liniowy przedstawia liczbę graczy urodzonych w danym roku. 
        Pozwala on zauważyć wzrost popularności szachów: od lat 90. XX w. liczba rejestrowanych zawodników bardzo rośnie. 
        Po 2010 roku widoczny jest pozorny spadek – wynika on z faktu, że młodsze dzieci nie zdążyły jeszcze zostać aktywnymi graczami w bazie FIDE.
      </p>
      <h4> Źródło danych</h4>
      <p>
        Dane pochodzą z oficjalnej bazy Międzynarodowej Federacji Szachowej (FIDE), zaktualizowanej w maju 2025 roku.
        <a href='https://ratings.fide.com/download_lists.phtml' target='_blank'>International
Chess Federation</a>.<br>
      </p>
    </div>
  ")
  })
  
 # --------------------------------------------------------------
  
  # --- MIEJSCE NA Wczytywanie i przetwarzanie danych Roksany ---
  
  output$opisWykresow <- renderUI({
    HTML("
    <div style='padding: 20px; font-size: 15px; line-height: 1.6; background-color: #f9f9f9; border-radius: 10px; margin-top: 30px;'>
      <h4>Czym jest ranking FIDE?</h4>
      <p>
        Ranking FIDE to międzynarodowy wskaźnik siły gry szachisty, opracowany przez Fédération Internationale des Échecs (FIDE). Jest on aktualizowany na podstawie wyników graczy w oficjalnych turniejach i reprezentuje ich globalny poziom umiejętności.
      </p>
      
      <h4>Typy gry w szachy online</h4>
      <ul>
        <li><strong>Rapid</strong>: 10–60 minut na zawodnika. Najbliższy klasycznym szachom.</li>
        <li><strong>Blitz</strong>: 3–10 minut na zawodnika. Szybsze tempo, więcej błędów, dynamiczna gra.</li>
        <li><strong>Bullet</strong>: mniej niż 60 sekund na zawodnika. Ekstremalnie szybka gra wymagająca refleksu.</li>
      </ul>
      
      <h4>Co przedstawiają wykresy?</h4>
      <p>Powyżej znajdują się trzy wykresy typu punktowego, które pokazują zależności pomiędzy rankingiem FIDE a liczbą wygranych, remisów i porażek w trzech różnych typach rozgrywek dostępnych na platformie:</p>

    <p>Każdy wykres zawiera punkty reprezentujące zawodników oraz linie trendu pokazujące zależność między rankingiem FIDE a liczbą partii:</p>
    <ul>
      <li><strong>Liczba wygranych</strong> – ile partii danego typu wygrał zawodnik.</li>
      <li><strong>Liczba remisów</strong> – liczba zremisowanych gier.</li>
      <li><strong>Liczba przegranych</strong> – ile gier dany zawodnik przegrał.</li>
    </ul>
      
      <h4>Źródło danych</h4>
      <p>
        Dane pochodzą z publicznego zbioru: 
        <a href='https://www.kaggle.com/datasets/crxxom/chess-gm-players' target='_blank'>Chess GM Players (Kaggle)</a>.<br>
        Dataset zawiera statystyki arcymistrzów z chess.com do dnia 17.07.2023 r. Dane obejmują rankingi FIDE, liczby wygranych/remisów/przegranych w różnych trybach gry, daty rejestracji, status konta i inne.
      </p>
      <p style='font-size: 13px; color: gray;'>
        Uwaga: Ranking FIDE w danych może być niedokładny, gdyż użytkownicy chess.com mogą wpisywać go ręcznie.
      </p>
    </div>
  ")
  })
  
  output$opisMapyGM <- renderUI({
    HTML("
    <div style='padding: 20px; font-size: 15px; line-height: 1.6; background-color: #f9f9f9; border-radius: 10px; margin-top: 20px;'>
      <h4>Kim są arcymistrzowie szachowi (GM)?</h4>
      <p>
        Arcymistrz (ang. Grandmaster, GM) to najwyższy tytuł szachowy nadawany przez FIDE (Międzynarodową Federację Szachową). Tytuł ten przyznawany jest dożywotnio i wymaga spełnienia określonych norm rankingowych oraz wyników w turniejach międzynarodowych.
      </p>

      <h4>O czym informuje mapa?</h4>
      <p>
        Mapa przedstawia liczbę aktywnych arcymistrzów szachowych zarejestrowanych na Chess.com według krajów. Kolorystyka wskazuje względną liczbę GM w poszczególnych państwach – im ciemniejszy fiolet, tym więcej arcymistrzów.
      </p>

      <h4>Łączna liczba rozegranych partii</h4>
      <p>
        Poniżej mapy znajdują się zestawienia całkowitej liczby partii rozegranych przez arcymistrzów w trzech głównych trybach gry:
      </p>
      <ul>
        <li><strong>Rapid</strong>: tempo 10–60 minut, zbliżone do klasycznych szachów.</li>
        <li><strong>Blitz</strong>: 3–10 minut, szybka gra wymagająca refleksu.</li>
        <li><strong>Bullet</strong>: poniżej 1 minuty, ekstremalnie dynamiczna rozgrywka.</li>
      </ul>
      <p>
        Dzięki tym statystykom możemy porównać popularność różnych trybów wśród zawodników najwyższej klasy.
      </p>
      
      <h4>Źródło danych</h4>
      <p>
        Dane pochodzą z publicznego zbioru: 
        <a href='https://www.kaggle.com/datasets/crxxom/chess-gm-players' target='_blank'>Chess GM Players (Kaggle)</a>.<br>
        Dataset zawiera statystyki arcymistrzów z chess.com do dnia 17.07.2023 r. Dane obejmują rankingi FIDE, liczby wygranych/remisów/przegranych w różnych trybach gry, daty rejestracji, status konta i inne.
      </p>
      <p style='font-size: 13px; color: gray;'>
        Uwaga: Ranking FIDE w danych może być niedokładny, gdyż użytkownicy chess.com mogą wpisywać go ręcznie.
      </p>
    </div>
  ")
  })
  
  
  #Zamiana napisów na wartosci logiczne
  dane <- dane %>%
    mutate(is_streamer = as.logical(toupper(is_streamer)))
  
  # Przygotowanie danych dla mapy (zależne od filtra streamera)
  gm_counts <- reactive({
    req(input$streamerFilter)
    data <- dane
    if (input$streamerFilter != "all") {
      data <- data %>% filter(is_streamer == as.logical(input$streamerFilter))
    }
    
    data <- data %>%
      filter(country != "Antarctica") %>% 
      # Zmiana nazw na pasujące do pakietu
      mutate(country = case_when(
        country == "United States" ~ "United States of America",
        country == "Czech Republic" ~ "Czechia",
        country == "Norway" ~ "Norway",
        country == "Ukraine" ~ "Ukraine",
        country == "Russia" ~ "Russia",
        country == "India" ~ "India",
        country == "France" ~ "France",
        country == "Netherlands" ~ "Netherlands",
        country == "Azerbaijan" ~ "Azerbaijan",
        country == "Poland" ~ "Poland",
        country == "China" ~ "China",
        country == "Vietnam" ~ "Vietnam",
        country == "Iran" ~ "Iran",
        country == "Georgia" ~ "Georgia",
        country == "Spain" ~ "Spain",
        country == "Germany" ~ "Germany",
        country == "Samoa" ~ "Samoa",
        country == "Romania" ~ "Romania",
        country == "Egypt" ~ "Egypt",
        country == "England" ~ "United Kingdom",
        country == "Tonga" ~ "Tonga",
        country == "Armenia" ~ "Armenia",
        country == "Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
        country == "Australia" ~ "Australia",
        country == "Hungary" ~ "Hungary",
        country == "Bulgaria" ~ "Bulgaria",
        country == "Serbia" ~ "Serbia",
        country == "Iceland" ~ "Iceland",
        country == "United Kingdom" ~ "United Kingdom",
        country == "Israel" ~ "Israel",
        country == "Slovenia" ~ "Slovenia",
        country == "Croatia" ~ "Croatia",
        country == "Philippines" ~ "Philippines",
        country == "Moldova" ~ "Moldova",
        country == "Uruguay" ~ "Uruguay",
        country == "Argentina" ~ "Argentina",
        country == "Peru" ~ "Peru",
        country == "Turkey" ~ "Turkey",
        country == "Uzbekistan" ~ "Uzbekistan",
        country == "Mexico" ~ "Mexico",
        country == "Chile" ~ "Chile",
        country == "Cayman Islands" ~ "Cayman Islands",
        country == "Latvia" ~ "Latvia",
        country == "Canada" ~ "Canada",
        country == "Papua New Guinea" ~ "Papua New Guinea",
        country == "Micronesia" ~ "Micronesia",
        country == "Cuba" ~ "Cuba",
        country == "Brazil" ~ "Brazil",
        country == "Sweden" ~ "Sweden",
        country == "Thailand" ~ "Thailand",
        country == "Switzerland" ~ "Switzerland",
        country == "Zimbabwe" ~ "Zimbabwe",
        country == "Italy" ~ "Italy",
        country == "Kazakhstan" ~ "Kazakhstan",
        country == "Belarus" ~ "Belarus",
        country == "Belgium" ~ "Belgium",
        country == "Paraguay" ~ "Paraguay",
        country == "Dominican Republic" ~ "Dominican Republic",
        country == "Denmark" ~ "Denmark",
        country == "Lithuania" ~ "Lithuania",
        country == "Tanzania" ~ "Tanzania",
        country == "Montenegro" ~ "Montenegro",
        country == "Mongolia" ~ "Mongolia",
        country == "Monaco" ~ "Monaco",
        country == "Greece" ~ "Greece",
        country == "Aruba" ~ "Aruba",
        country == "Colombia" ~ "Colombia",
        country == "Indonesia" ~ "Indonesia",
        country == "United Arab Emirates" ~ "United Arab Emirates",
        country == "Singapore" ~ "Singapore",
        country == "Andorra" ~ "Andorra",
        country == "Ecuador" ~ "Ecuador",
        country == "Algeria" ~ "Algeria",
        country == "Hong Kong" ~ "Hong Kong",
        country == "Austria" ~ "Austria",
        country == "Turkmenistan" ~ "Turkmenistan",
        country == "Bangladesh" ~ "Bangladesh",
        country == "Scotland" ~ "United Kingdom",
        country == "Jordan" ~ "Jordan",
        country == "Ireland" ~ "Ireland",
        country == "Qatar" ~ "Qatar",
        country == "Slovakia" ~ "Slovakia",
        country == "Portugal" ~ "Portugal",
        country == "British Virgin Islands" ~ "British Virgin Islands",
        country == "American Samoa" ~ "American Samoa",
        country == "Bolivia" ~ "Bolivia",
        country == "Estonia" ~ "Estonia",
        country == "North Macedonia" ~ "North Macedonia",
        country == "Fiji" ~ "Fiji",
        country == "Tunisia" ~ "Tunisia",
        country == "Japan" ~ "Japan",
        country == "The Gambia" ~ "Gambia",
        country == "Saint Lucia" ~ "Saint Lucia",
        country == "Togo" ~ "Togo",
        country == "South Africa" ~ "South Africa",
        country == "Saudi Arabia" ~ "Saudi Arabia",
        country == "Western Sahara" ~ "Western Sahara",
        country == "Sao Tome/Principe" ~ "São Tomé and Príncipe",
        country == "Tajikistan" ~ "Tajikistan",
        country == "Jamaica" ~ "Jamaica",
        country == "Isle of Man" ~ "Isle of Man",
        country == "Ivory Coast" ~ "Côte d'Ivoire",
        country == "Senegal" ~ "Senegal",
        country == "Taiwan" ~ "Taiwan",
        country == "Haiti" ~ "Haiti",
        country == "Guatemala" ~ "Guatemala",
        country == "North Korea" ~ "North Korea",
        country == "Canary Islands" ~ NA_character_,
        country == "International" ~ NA_character_,
        country == "Central Africa" ~ NA_character_,
        country == "Tuvalu" ~ "Tuvalu",
        country == "Panama" ~ "Panama",
        country == "Madagascar" ~ "Madagascar",
        country == "Liechtenstein" ~ "Liechtenstein",
        country == "Turks and Caicos Islands" ~ "Turks and Caicos Islands",
        country == "South Korea" ~ "South Korea",
        country == "Finland" ~ "Finland",
        country == "Trinidad/Tobago" ~ "Trinidad and Tobago",
        country == "Ghana" ~ "Ghana",
        country == "Maldives" ~ "Maldives",
        country == "Brunei" ~ "Brunei",
        country == "Morocco" ~ "Morocco",
        country == "Greenland" ~ "Greenland",
        TRUE ~ country
      )) %>%
      filter(!is.na(country)) %>%
      group_by(country) %>%
      summarise(gm_count = n())
    
    data
  })
  
  # załadowanie mapy
  world_map <- ne_countries(scale = "medium", returnclass = "sf")
  map_joined <- reactive({
    world_map %>%
      left_join(gm_counts(), by = c("name" = "country"))
  })
  
  # dane dla wykresu słupkowego
  games_summary <- dane %>%
    summarise(
      Rapid = sum(rapid_win + rapid_draw + rapid_loss, na.rm = TRUE),
      Blitz = sum(blitz_win + blitz_draw + blitz_loss, na.rm = TRUE),
      Bullet = sum(bullet_win + bullet_draw + bullet_loss, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Game_Type", values_to = "Total_Games")
  
  # dane dla wykresu
  data_filtered <- reactive({
    req(input$fideRangeFilter)
    dane %>%
      filter(fide >= input$fideRangeFilter[1], fide <= input$fideRangeFilter[2])
  })
  
  # wykres 1 ranking FIDE vs wyniki w różnych trybach (wygrane)
  output$multiModeWinPlot <- renderPlotly({
    req(length(input$gameTypes) > 0, nrow(data_filtered()) > 0)
    data <- data_filtered() %>%
      filter(rapid_win < 250, blitz_win < 250, bullet_win < 250)
    
    p <- ggplot(data) +
      labs(
        title = "Ranking vs Liczba wygranych",
        x = "Ranking FIDE",
        y = "Liczba wygranych") +
      theme_minimal() +
      coord_fixed()
    
    if ("Rapid" %in% input$gameTypes) {
      p <- p +
        geom_point(aes(x = fide, y = rapid_win, color = "Rapid"), alpha = 0.6, size = 1) +
        geom_smooth(aes(x = fide, y = rapid_win, color = "Rapid"), method = "lm", se = FALSE)
    }
    if ("Blitz" %in% input$gameTypes) {
      p <- p +
        geom_point(aes(x = fide, y = blitz_win, color = "Blitz"), alpha = 0.6, size = 1) +
        geom_smooth(aes(x = fide, y = blitz_win, color = "Blitz"), method = "lm", se = FALSE)
    }
    if ("Bullet" %in% input$gameTypes) {
      p <- p +
        geom_point(aes(x = fide, y = bullet_win, color = "Bullet"), alpha = 0.6, size = 1) +
        geom_smooth(aes(x = fide, y = bullet_win, color = "Bullet"), method = "lm", se = FALSE)
    }
    
    p <- p + scale_color_manual(
      name = "Typ gry",
      values = c("Rapid" = "darkorchid", "Blitz" = "deeppink", "Bullet" = "deepskyblue"),
      breaks = input$gameTypes
    )
    
    
    ggplotly(p)
  })
  
  # wykres 2 ranking FIDE vs remisy w różnych trybach
  output$multiModeDrawPlot <- renderPlotly({
    req(length(input$gameTypes) > 0, nrow(data_filtered()) > 0)
    data <- data_filtered() %>%
      filter(rapid_draw < 5000, blitz_draw < 500, bullet_draw < 500)
    
    p <- ggplot(data) +
      labs(
        title = "Ranking vs Liczba remisów",
        x = "Ranking FIDE",
        y = "Liczba remisów") +
      theme_minimal() +
      coord_fixed()
    
    if ("Rapid" %in% input$gameTypes) {
      p <- p +
        geom_point(aes(x = fide, y = rapid_draw, color = "Rapid"), alpha = 0.6, size = 1) +
        geom_smooth(aes(x = fide, y = rapid_draw, color = "Rapid"), method = "lm", se = FALSE)
    }
    if ("Blitz" %in% input$gameTypes) {
      p <- p +
        geom_point(aes(x = fide, y = blitz_draw, color = "Blitz"), alpha = 0.6, size = 1) +
        geom_smooth(aes(x = fide, y = blitz_draw, color = "Blitz"), method = "lm", se = FALSE)
    }
    if ("Bullet" %in% input$gameTypes) {
      p <- p +
        geom_point(aes(x = fide, y = bullet_draw, color = "Bullet"), alpha = 0.6, size = 1) +
        geom_smooth(aes(x = fide, y = bullet_draw, color = "Bullet"), method = "lm", se = FALSE)
    }
    
    p <- p + scale_color_manual(
      name = "Typ gry",
      values = c("Rapid" = "darkorchid", "Blitz" = "deeppink", "Bullet" = "deepskyblue"),
      breaks = input$gameTypes
    )
    
    ggplotly(p)
  })
  
  # wykres 3 ranking FIDE vs przegrane w różnych trybach
  output$multiModeLossPlot <- renderPlotly({
    req(length(input$gameTypes) > 0, nrow(data_filtered()) > 0)
    data <- data_filtered() %>%
      filter(rapid_loss < 500, blitz_loss < 500, bullet_loss < 500)
    
    p <- ggplot(data) +
      labs(
        title = "Ranking vs Liczba przegranych",
        x = "Ranking FIDE",
        y = "Liczba przegranych") +
      theme_minimal() +
      coord_fixed()
    
    if ("Rapid" %in% input$gameTypes) {
      p <- p +
        geom_point(aes(x = fide, y = rapid_loss, color = "Rapid"), alpha = 0.6, size = 1) +
        geom_smooth(aes(x = fide, y = rapid_loss, color = "Rapid"), method = "lm", se = FALSE)
    }
    if ("Blitz" %in% input$gameTypes) {
      p <- p +
        geom_point(aes(x = fide, y = blitz_loss, color = "Blitz"), alpha = 0.6, size = 1) +
        geom_smooth(aes(x = fide, y = blitz_loss, color = "Blitz"), method = "lm", se = FALSE)
    }
    if ("Bullet" %in% input$gameTypes) {
      p <- p +
        geom_point(aes(x = fide, y = bullet_loss, color = "Bullet"), alpha = 0.6, size = 1) +
        geom_smooth(aes(x = fide, y = bullet_loss, color = "Bullet"), method = "lm", se = FALSE)
    }
    
    p <- p + scale_color_manual(
      name = "Typ gry",
      values = c("Rapid" = "darkorchid", "Blitz" = "deeppink", "Bullet" = "deepskyblue"),
      breaks = input$gameTypes
    )
    
    ggplotly(p)
  })
  
  # wykres 4 mapa liczby arcymistrzów
  
  output$mapPlot <- renderPlotly({
    req(nrow(map_joined()) > 0)
    p <- ggplot(map_joined()) +
      geom_sf(aes(fill = gm_count), color = "black") +
      coord_sf(xlim = c(-180, 180), ylim = c(-62, 90), expand = FALSE) +
      scale_fill_gradient(
        name = "Liczba GM",
        low = "lavender",
        high = "darkviolet",
        na.value = "grey90"
      ) +
      theme_minimal() +
      labs(
        title = "Liczba arcymistrzów szachowych wg kraju",
        caption = "Źródło: chess.com."
      ) +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold") 
      )
    ggplotly(p)
  })
  
  # wykres 5 łączna liczba rozegranych partii
  output$totalGamesPlot <- renderPlotly({
    p <- ggplot(games_summary, aes(x = Game_Type, y = Total_Games, fill = Game_Type)) +
      geom_col() +
      labs(
        title = "Łączna liczba rozegranych partii przez GM-ów na chess.com",
        x = "Typ gry",
        y = "Liczba partii"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Blues") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "none"
      ) +
      scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = " tys."))
    ggplotly(p)
  })
  # ----------------------------------------------------------------
  

  
  # --- Dane dla długości partii ---
  data_dlugosc_partii <- reactive({
    req(input$dlugoscPartiiEloRangeFilter)
    
    data <- df %>%
      filter((WhiteElo >= input$dlugoscPartiiEloRangeFilter[1] & WhiteElo <= input$dlugoscPartiiEloRangeFilter[2]) |
               (BlackElo >= input$dlugoscPartiiEloRangeFilter[1] & BlackElo <= input$dlugoscPartiiEloRangeFilter[2]))
    data
  })
  
  # --- Dane dla ruchów w formacie ---
  data_ruchy_format <- reactive({
    req(input$metricChoice)
    df %>% filter(!is.na(TimeCategory))
  })
  
  # Obliczenie globalnej średniej/mediany MoveCount
  global_average_median_moves <- reactive({
    df %>%
      summarise(
        AvgMoves = mean(MoveCount, na.rm = TRUE),
        MedianMoves = median(MoveCount, na.rm = TRUE)
      )
  })
  
  # --- Dane dla debiutów (filtrowane po ELO) ---
  data_debiuty <- reactive({
    req(input$topNFilter, input$debiutyEloRangeFilter)
    
    df %>%
      filter(!is.na(Opening)) %>%
      # Filtrowanie po zakresie ELO dla debiutów
      filter((WhiteElo >= input$debiutyEloRangeFilter[1] & WhiteElo <= input$debiutyEloRangeFilter[2]) |
               (BlackElo >= input$debiutyEloRangeFilter[1] & BlackElo <= input$debiutyEloRangeFilter[2])) %>%
      group_by(Opening) %>%
      summarise(
        Total = n(),
        WhiteWins = sum(Result == "1-0"),
        BlackWins = sum(Result == "0-1"),
        Draws = sum(Result == "1/2-1/2"),
        .groups = "drop"
      ) %>%
      mutate(
        WhiteWinRate = round(WhiteWins / Total * 100, 1),
        BlackWinRate = round(BlackWins / Total * 100, 1),
        DrawRate = round(Draws / Total * 100, 1)
      ) %>%
      arrange(desc(Total)) %>%
      slice(1:input$topNFilter)
  })
  
  # Reaktywne dane dla skuteczności (rozszerzenie dla różnych metryk)
  skutecznosc_long_data <- reactive({
    req(input$skutecznoscDebiutowMetric)
    data_debiuty_df <- data_debiuty()
    
    if (input$skutecznoscDebiutowMetric == "Stacked") {
      openings_long <- data_debiuty_df %>%
        select(Opening, WhiteWinRate, BlackWinRate, DrawRate) %>%
        pivot_longer(cols = -Opening, names_to = "Outcome", values_to = "Percent")
      openings_long$Outcome <- factor(openings_long$Outcome,
                                      levels = c("WhiteWinRate", "BlackWinRate", "DrawRate"),
                                      labels = c("Białe wygrywają", "Czarne wygrywają", "Remis"))
    } else {
      openings_long <- data_debiuty_df %>%
        select(Opening, Percent = !!sym(input$skutecznoscDebiutowMetric)) %>%
        mutate(Outcome = input$skutecznoscDebiutowMetric)
    }
    openings_long
  })
  
  # --- WYKRES 1: Rozkład długości partii ---
  output$dlugoscPartiiPlot <- renderPlotly({
    req(nrow(data_dlugosc_partii()) > 0)
    
    p <- ggplot(data_dlugosc_partii(), aes(x = MoveCount)) +
      geom_histogram(binwidth = input$binwidthSlider, fill = "#4C72B0", color = "white") +
      labs(
        title = "Rozkład długości partii (liczba ruchów)",
        x = "Liczba ruchów (ply)",
        y = "Liczba partii"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  
  # --- WYKRES 2 : Top N najczęściej granych debiutów ---
  output$najczestrzeDebiutyPlot <- renderPlotly({
    req(nrow(data_debiuty()) > 0)
    p <- ggplot(data_debiuty(), aes(x = fct_reorder(Opening, Total), y = Total)) +
      geom_col(fill = "#4C72B0") +
      coord_flip() +
      labs(
        title = paste("Top", input$topNFilter, "najczęściej granych debiutów"),
        x = "Debiut",
        y = "Liczba partii"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # --- WYKRES 2 (część 2): Skuteczność Top N debiutów ---
  output$skutecznoscDebiutowPlot <- renderPlotly({
    req(nrow(skutecznosc_long_data()) > 0)
    
    plot_data <- skutecznosc_long_data()
    
    if (input$skutecznoscDebiutowMetric == "Stacked") {
      p <- ggplot(plot_data, aes(x = fct_reorder(Opening, Percent, .fun = max), y = Percent, fill = Outcome)) +
        geom_col(position = "stack") +
        scale_fill_manual(values = c("Białe wygrywają" = "#D55E00", "Czarne wygrywają" = "#0072B2", "Remis" = "#999999")) +
        labs(fill = "Wynik")
    } else {
      color_val <- switch(input$skutecznoscDebiutowMetric,
                          "WhiteWinRate" = "#D55E00",
                          "BlackWinRate" = "#0072B2",
                          "DrawRate" = "#999999")
      p <- ggplot(plot_data, aes(x = fct_reorder(Opening, Percent), y = Percent)) +
        geom_col(fill = color_val) +
        theme(legend.position = "none")
    }
    
    p <- p + coord_flip() +
      labs(
        title = paste("Skuteczność Top", input$topNFilter, "debiutów"),
        x = "Debiut",
        y = "Procentowy wynik"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # --- WYKRES 3 : Średnia liczba ruchów w zależności od formatu ---
  output$sredniaLiczbaRuchowPlot <- renderPlotly({
    req(nrow(data_ruchy_format()) > 0)
    
    summary_data <- data_ruchy_format() %>%
      group_by(TimeCategory) %>%
      summarise(
        AvgMoves = mean(MoveCount, na.rm = TRUE),
        MedianMoves = median(MoveCount, na.rm = TRUE),
        .groups = "drop"
      )
    
    metric_col <- sym(input$metricChoice)
    
    p <- ggplot(summary_data, aes(x = TimeCategory, y = !!metric_col)) + # Usunięto 'fill = TimeCategory' z aes
      geom_col(fill = "#4C72B0", color = "white") + # Dodano stały kolor słupków i obramowanie
      labs(
        title = paste(ifelse(input$metricChoice == "AvgMoves", "Średnia", "Mediana"), "liczba ruchów w zależności od formatu"),
        x = "Format czasowy partii",
        y = paste(ifelse(input$metricChoice == "AvgMoves", "Średnia", "Mediana"), "liczba ruchów")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Obróć etykiety osi X dla czytelności
    
   
    if (input$showGlobalAverage) {
      # Sprawdź, czy global_average_median_moves() jest funkcją reaktywną i czy zwraca listę
      global_val <- global_average_median_moves()[[input$metricChoice]]
      
      p <- p + geom_hline(yintercept = global_val, linetype = "dashed", color = "red", size = 1) +
        annotate("text", x = Inf, y = global_val, label = paste("Globalna", ifelse(input$metricChoice == "AvgMoves", "średnia", "mediana"), round(global_val, 2)), 
                 vjust = -0.5, hjust = 1.1, color = "red", size = 3) # Etykieta globalnej linii
    }
    
    ggplotly(p)
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
                
                