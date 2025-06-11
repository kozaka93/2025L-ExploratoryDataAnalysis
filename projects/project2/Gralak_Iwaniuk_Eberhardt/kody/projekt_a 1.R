install.packages('rnaturalearthdata')

library(shiny)
library(dplyr)
library(leaflet)
library(stringr)
library(countrycode)
library(rnaturalearth)
library(sf)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(bslib)
library(shinyWidgets)


# Wczytanie danych
titles <- read.csv("titles.csv")
data <- read.csv("data.csv")
netflix <- read.csv("netflix_titles.csv")
dane <- titles

# Przygotowanie danych
titles <- titles %>%
  mutate(description = as.character(description))

wrap_text <- function(text, width = 80) {
  sapply(strwrap(text, width = width, simplify = FALSE), function(x) paste(x, collapse = "<br>"))
}

df_clean <- netflix %>%
  filter(str_detect(type, "Movie"), !is.na(release_year), !is.na(listed_in)) %>%
  separate_rows(listed_in, sep = ",\\s*")

# UI
ui <- fluidPage(
  theme = bs_theme(
    bg = "#393636",  # Ciemne tło
    fg = "#f5f5f5",  # Jasny tekst
    primary = "#e50914",  # Netflix red
    base_font = font_google("Open Sans"),
    bootswatch = "darkly"
  ),
  tags$head(
    tags$style(HTML("
      .navbar, .tabbable > .nav-tabs {
        background-color: #000 !important;
      }
      .tab-pane {
        background-color: #121212;
        padding: 20px;
        border-radius: 10px;
      }
      .leaflet-container {
        background: #000 !important;
      }
    "))
  ),
  div(
    style = "background-color: #000; padding: 20px 0; text-align: center;",
    tags$h1("🎬 Movies and Series in Numbers", 
            style = "color: #f5f5f5; font-size: 42px; font-weight: bold; margin: 0; font-family: 'Open Sans', sans-serif;")
  ),
  tabsetPanel(
    
    # ---- SERIALS ----
    tabPanel("Series",
             
             # --- Sekcja 1: Mapa seriali wg kraju ---
             h3("How many series were filmed in each country?"),
             p("This interactive map shows the number of series produced in each country, 
               based on the selected release years. Hover over a country to see the number of series."),
             tags$hr(),
             sidebarLayout(
               sidebarPanel(
                 pickerInput("Selected", "Select year/s:",
                             choices = sort(unique(dane$release_year[!is.na(dane$release_year) & dane$type == "SHOW"]), decreasing = TRUE),
                             selected = max(dane$release_year, na.rm = TRUE),
                             multiple = TRUE
                 )
               ),
               mainPanel(
                 leafletOutput("mapa", height = 450)
               )
             ),
             
             tags$hr(style = "margin-top: 30px; margin-bottom: 30px;"),
             
             # --- Odstęp między wykresami ---
             div(style = "margin-top: 60px; margin-bottom: 100px;"),
             
             # --- Sekcja 2: Sezony wybranego serialu ---
             h3("How many seasons were in each series?"),
             p("This bar chart shows the number of episodes in each season of 
               the selected series. The tooltip also shows the average rating per season."),
             tags$hr(),
             sidebarLayout(
               sidebarPanel(
                 pickerInput("wybrany_serial", "Select series:",
                             choices = unique(PogromcyDanych::serialeIMDB$serial),
                             selected = unique(PogromcyDanych::serialeIMDB$serial)[1]
                 )
               ),
               mainPanel(
                 withSpinner(plotlyOutput("wykres_sezony", height = 550))
               )
             )
    ),
    
    # ---- MOVIES ----
    tabPanel("Movies",
             
             fluidRow(
               column(12,
                      h4("IMDb Votes vs Score"),
                      p("This plot shows the relationship between the number of IMDb 
                        votes and the IMDb score for movies. Each point represents a movie. 
                        The color indicates the score."),
                      div(style = "padding-right: 50px;",
                          withSpinner(plotlyOutput("votes_vs_score_graph", height = 400))
                      )
               )
             ),
             
             # --- Odstęp między wykresami ---
             div(style = "margin-top: 60px; margin-bottom: 60px;"),
             
             fluidRow(
               column(12,
                      h4("Movies Released per Year by Category"),
                      p("This chart displays the number of movies released each year, 
                        grouped by selected categories. You can filter the data using 
                        the year range slider and category selector below."),
                      br()
               )
             ),
             
             fluidRow(
               column(12,
                      sliderInput("yearInput", "Range of years:",
                                  min = min(df_clean$release_year),
                                  max = max(df_clean$release_year),
                                  value = c(min(df_clean$release_year), max(df_clean$release_year)),
                                  sep = "")
               )
             ),
             
             fluidRow(
               column(4, offset = 0,
                      pickerInput("categoryInput", "Select a category:",
                                  choices = sort(unique(df_clean$listed_in)),
                                  selected = "Drama",
                                  multiple = TRUE)
               )
             ),
             
             fluidRow(
               column(12,
                      br(),
                      withSpinner(plotlyOutput("plot", height = 400))
               )
             )
    )
    
  )
)

# Server
server <- function(input, output, session) {
  
  # ---- SERIALS: MAP ----
  seriale_dane <- reactive({
    dane %>%
      filter(
        type == "SHOW",
        !is.na(production_countries),
        !is.na(release_year),
        release_year %in% input$Selected
      ) %>%
      mutate(kraj_lista = str_extract_all(production_countries, "[A-Z]{2}")) %>%
  unnest(kraj_lista) %>%
  mutate(iso2 = as.character(kraj_lista)) %>%
  select(-kraj_lista)
  })
  
  mapa_dane <- reactive({
    liczba_seriali <- seriale_dane() %>%
      group_by(iso2) %>%
      summarise(liczba = n(), .groups = "drop") %>%
      mutate(iso3 = countrycode(iso2, "iso2c", "iso3c"))
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    left_join(world, liczba_seriali, by = c("iso_a3" = "iso3"))
  })
  
  output$mapa <- renderLeaflet({
    df <- mapa_dane()
    bins <- c(0, 1, 2, 5, 10, 20, 50, 100, 200, 300, Inf)
    custom_colors <- colorBin(
      palette = colorRampPalette(c("#cccccc", "#cc0000", "#003366", "#000000"))(length(bins) - 1),
      domain = df$liczba,
      bins = bins,
      na.color = "#f0f0f0"
    )
    
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~custom_colors(liczba),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0(name, ": ", ifelse(is.na(liczba), 0, liczba), " series"),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = custom_colors,
        values = df$liczba,
        title = htmltools::HTML("<b>Number of series</b>"),
        opacity = 0.7,
        position = "bottomright"
      )
  })
  
  # ---- SERIALS: SEASONS ----
  dane_serialu <- reactive({
    PogromcyDanych::serialeIMDB %>%
      filter(serial == input$wybrany_serial) %>%
      group_by(sezon) %>%
      summarise(
        liczba_odcinkow = n(),
        srednia_ocena = mean(ocena, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(sezon)
  })
  
  output$wykres_sezony <- renderPlotly({
    dane <- dane_serialu()
    
    gg <- ggplot(dane, aes(x = sezon, y = liczba_odcinkow,
                           text = paste0("Season: ", sezon,
                                         "<br>Number of episodes: ", liczba_odcinkow,
                                         "<br>Mean score: ", round(srednia_ocena, 2)))) +
      geom_col(fill = "#E50914") +
      labs(
        title = paste("Number of episodes in each season -", input$wybrany_serial),
        x = "Season", y = "Number of episodes"
      ) +
      theme(
        panel.background = element_rect(fill = "#000000", color = NA),
        plot.background = element_rect(fill = "#000000", color = NA),
        panel.grid.major = element_line(color = "#333333"),
        panel.grid.minor = element_line(color = "#222222"),
        axis.text = element_text(color = "#f5f5f5"),
        axis.title = element_text(color = "#f5f5f5"),
        plot.title = element_text(color = "#f5f5f5", face = "bold")
      )
    
    ggplotly(gg, tooltip = "text") %>%
      layout(
        paper_bgcolor = "#000000",
        plot_bgcolor = "#000000"
      )
  })
  
  # ---- MOVIES: IMDb VOTES vs SCORE ----
  output$votes_vs_score_graph <- renderPlotly({
    df_movies <- titles %>%
      filter(type == "MOVIE") %>%
      filter(!is.na(imdb_votes), !is.na(imdb_score), !is.na(title)) %>%
      mutate(wrapped_description = wrap_text(description, width = 80))
    
    plot_ly(
      data = df_movies,
      x = ~imdb_votes,
      y = ~imdb_score,
      type = 'scatter',
      mode = 'markers',
      text = ~paste0(
        "<b>", title, "</b><br>",
        "Score: ", imdb_score, "<br>",
        "Votes: ", imdb_votes, "<br><br>",
        wrapped_description
      ),
      hoverinfo = "text",
      marker = list(
        color = ~imdb_score,
        colorscale = list(c(0, "#ffa600"), c(0.5, "#cc0000"), c(1, "#bc5090")),
        showscale = FALSE,
        size = 10,
        line = list(width = 0.5, color = '#555555')
      )
    ) %>%
      layout(
        title  = "IMDb Votes vs. Score",
        margin = list(t = 80),
        xaxis = list(title = 'IMDb Votes', type = "log", color = '#f5f5f5'),
        yaxis = list(title = 'IMDb Score', color = '#f5f5f5'),
        hovermode = 'closest',
        plot_bgcolor = '#121212',
        paper_bgcolor = '#121212',
        font = list(color = '#f5f5f5')
      )
  })
  
  # ---- MOVIES: Kategorie wg lat ----
  filtered_data <- reactive({
    df_clean %>%
      filter(release_year >= input$yearInput[1],
             release_year <= input$yearInput[2],
             listed_in %in% input$categoryInput)
  })
  
  output$plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      count(release_year, listed_in)
    
    plot_ly(
      plot_data,
      x = ~release_year,
      y = ~n,
      color = ~listed_in,
      colors = c("#ff1c4a", "#955196", "#dd5182", "#ff6e54", "#ffa600", "#00bcd4"),
      type = 'bar'
    ) %>%
      layout(
        barmode = "stack",
        title = list(text = "Distribution of Movie Categories Over the Years", font = list(color = "#f5f5f5")),
        margin = list(t = 80),
        xaxis = list(title = "Year", color = '#f5f5f5', tickfont = list(color = "#f5f5f5"), titlefont = list(color = "#f5f5f5")),
        yaxis = list(title = "Number of movies", color = '#f5f5f5', tickfont = list(color = "#f5f5f5"), titlefont = list(color = "#f5f5f5")),
        legend = list(title = list(text = "Category"), font = list(color = "#f5f5f5")),
        plot_bgcolor = '#000000',
        paper_bgcolor = '#000000'
      )
  })
}

shinyApp(ui, server)
