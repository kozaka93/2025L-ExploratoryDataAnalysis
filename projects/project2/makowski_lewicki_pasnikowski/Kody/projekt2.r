library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(readr)

# Wczytanie danych
spotify <- read_csv("dataset.csv")  # do scatterplotu
sciezka_do_csv <- "spotify_full_list_20102023.csv"  # do wykresu udziałów

# UI
ui <- fluidPage(
  titlePanel("🎧 Spotify Analyzer – Eksploracja danych i analiza koncentracji"),
  
  tabsetPanel(
    tabPanel("Wykres rozrzutu utworów",
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_var", "Wybierz zmienną X:", 
                             choices = c("danceability", "energy", "valence", "tempo", "popularity")),
                 selectInput("y_var", "Wybierz zmienną Y:", 
                             choices = c("popularity", "danceability", "energy", "valence", "tempo")),
                 selectInput("extra_line", "Dodaj linię:", 
                             choices = c("Brak", "Regresja", "Mediana", "Średnia"),
                             selected = "Brak"),
                 sliderInput("bin_count", "Poziom wygładzenia (ilość przedziałów):",
                             min = 5, max = 100, value = 30, step = 1),
                 selectInput("genre_filter", "Filtruj według gatunku:", 
                             choices = c("Wszystkie", unique(spotify$track_genre)), selected = "Wszystkie"),
                 sliderInput("popularity_range", "Zakres popularności:",
                             min = 0, max = 100, value = c(80, 100))
               ),
               mainPanel(
                 plotlyOutput("scatterPlot")
               )
             )),
    
    tabPanel("Udział N% najpopularniejszych artystów",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("top_percent", "Wybierz N% artystów:", min = 0.1, max = 100, value = 10)
               ),
               mainPanel(
                 plotOutput("percentPlot")
               )
             ))
  )
)

# SERVER
server <- function(input, output) {
  
  # === WYKRES ROZRZUTU ===
  filtered_data <- reactive({
    data <- spotify %>%
      filter(popularity >= input$popularity_range[1],
             popularity <= input$popularity_range[2])
    
    if (input$genre_filter != "Wszystkie") {
      data <- data %>% filter(track_genre == input$genre_filter)
    }
    
    if (nrow(data) > 5000) {
      data <- data %>% sample_n(5000)
    }
    
    return(data)
  })
  
  output$scatterPlot <- renderPlotly({
    df <- filtered_data()
    xvar <- input$x_var
    yvar <- input$y_var
    
    p <- ggplot(df, aes_string(x = xvar, y = yvar)) +
      geom_point(alpha = 0.6, color = "#1DB954") +
      theme_minimal(base_family = "Arial") +
      theme(
        panel.background = element_rect(fill = "#191414", color = NA),
        plot.background = element_rect(fill = "#191414", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white")
      )
    
    if (input$extra_line == "Regresja") {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "white")
      
    } else if (input$extra_line %in% c("Średnia", "Mediana")) {
      bin_count <- input$bin_count
      
      df_bins <- df %>%
        mutate(bin = cut(.data[[xvar]], breaks = bin_count)) %>%
        group_by(bin) %>%
        summarise(
          x_mid = mean(range(.data[[xvar]], na.rm = TRUE)),
          y_stat = if (input$extra_line == "Średnia") mean(.data[[yvar]], na.rm = TRUE)
          else median(.data[[yvar]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!is.na(x_mid), !is.na(y_stat))
      
      line_color <- if (input$extra_line == "Średnia") "#1ED760" else "#15883E"
      
      p <- p + geom_line(data = df_bins, aes(x = x_mid, y = y_stat),
                         inherit.aes = FALSE,
                         color = line_color,
                         size = 1.2)
    }
    
    ggplotly(p) %>%
      layout(
        plot_bgcolor = "#191414",
        paper_bgcolor = "#191414",
        font = list(color = "white")
      )
  })
  
  # === WYKRES KONCENTRACJI ARTYSTÓW ===
  dane <- reactive({
    read.csv(sciezka_do_csv, stringsAsFactors = FALSE)
  })
  
  output$percentPlot <- renderPlot({
    df <- dane()
    
    df <- df %>%
      group_by(year, Artist) %>%
      summarise(total_streams = sum(Streams), .groups = "drop")
    
    wyniki <- df %>%
      group_by(year) %>%
      arrange(desc(total_streams)) %>%
      mutate(rank = row_number(),
             total_artists = n()) %>%
      filter(rank <= ceiling(input$top_percent / 100 * total_artists)) %>%
      summarise(
        top_streams = sum(total_streams),
        total_streams_all = sum(df$total_streams[df$year == unique(year)]),
        share = top_streams / total_streams_all * 100
      )
    
    wykres_dane <- wyniki %>%
      mutate(other_share = 100 - share) %>%
      select(year, share, other_share) %>%
      pivot_longer(cols = c("share", "other_share"), names_to = "type", values_to = "value")
    
    ggplot(wykres_dane, aes(x = factor(year), y = value, fill = type)) +
      geom_col(position = "stack") +
      scale_fill_manual(
        values = c("share" = "blue", "other_share" = "#d98880"),
        labels = c("Pozostali", "Udział top N%")
      ) +
      labs(
        x = "Rok",
        y = "Odsetek odtworzeń (%)",
        fill = "",
        title = paste0("Udział ", input$top_percent, "% najpopularniejszych artystów wśród wszystkich odtworzeń")
      ) +
      theme_minimal() +
      ylim(0, 100)
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)

