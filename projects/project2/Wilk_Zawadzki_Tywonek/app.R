library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(thematic)
library(paletteer)
library(plotly)

options(scipen = 12)

value_fide <- list(
  value_box(
    "Liczba utytułowanych szachistów: ",
    fill = FALSE,
    height = "200px",
    length(unique(rating_name$fide_id))
  ),
  value_box(
    "Najwyższy rating",
    fill = FALSE,
    height = "200px",
    max(rating_name$rating_standard[!is.na(rating_name$rating_standard)])
  ),
  value_box(
    "Liczba szachistów z Polski:",
    fill = FALSE,
    height = "200px",
    length(unique(rating_name[rating_name$federation == "POL",]$fide_id))
  )
)


ui <- page_navbar(
  title = "Projekt nr.2",
  theme = bs_theme(
    version = 5,
    bootswatch = "morph",
    bg = "#fcfaf8",
    fg = "#5C4033",
    primary = "#1B9E77",
    secondary = "#1B9E77",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),
  nav_panel(
    title = "Gracze FIDE",
    page_sidebar(
      card(
        fill = FALSE,
        card_header(tags$b("Wprowadzenie")),
        card_body(
          tags$p("W tej części znajdziesz różnorodne analizy ratingów profesjonalnych zawodników FIDE. Strona prezentuje zależności ratingu w odniesieniu do różnych zmiennych, takich jak wiek, płeć czy typ rozgrywek.")
        )
      ),
      layout_columns(
        fill = FALSE,
        value_fide[[1]], value_fide[[2]], value_fide[[3]]
      ),
      tags$p(
        "Wykres poniżej przedstawia gęstość rozkładu ratingu zawodników FIDE.",
        "Wybierz interesujące cię lata po prawej stronie, aby porównać jak wtedy rozkładał się rating."
      ),
      card(
        fill = FALSE,
        card_header("Rozkład ratingu profesjonalnych graczy"),
        fluidRow(
          column(10, 
                 plotOutput("density_plot")
          ),
          column(2,
                 checkboxGroupInput("year_roz",
                                    "Rok rozkładu ratingu",
                                    unique(years), selected = years[1])
          )
        )
      ),
      tags$p(
        "Wykres prezentuje rozkład ratingu profesjonalnych zawodników w zależności od ich wieku.
        Wybierz kraje które cię interesują i sprawdż jak prezentują się ratingi zawodników 
        pochodzących z tych krajów"
      ),
      card(
        fill = FALSE,
        card_header("Wiek graczy, a ich ratingu"),
        fluidRow(
          column(2,
                 selectInput("country",
                             "Dla jakiego kraju narysować wykres",
                             unique(countries$federation),
                             multiple = T, selected = "POL")
          ),
          column(10, 
                 plotOutput("point_plot")
          )
        )
      ),
      tags$p(
        "Wykres poniżej prezentuje jakie tytuły szachowe mają profesjonalni szachiści.
        Wybierz wiek oraz płeć aby porównać rozkłady między interesującymi cię grupami"
      ),
      card(
        fill = FALSE,
        card_header("Histogram"),
        fluidRow(
          column(10, 
                 plotOutput("hist_plot")
          ),
          column(2,
                 sliderInput(
                   inputId = "wiek",
                   label = "Wiek:",
                   min = min(rating_name$wiek),
                   max = max(rating_name$wiek),
                   value = c(min(rating_name$wiek), max(rating_name$wiek))
                 ),
                 checkboxGroupInput("sex",
                                    "Płeć:",
                                    unique(rating_name$gender), selected = "M")
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Nasze ratingi", 
    page_sidebar(
      card(
        fill = FALSE,
        card_header(tags$b("Wprowadzenie")),
        card_body(
          tags$p("Na tej stronie analizujemy nasze ratingi na stronie chess.com w zależności 
                 od typu rozgrywek (Rapid, Blitz, Bullet) oraz wybranego zakresu dat. 
                 Możesz filtrować dane, aby zobaczyć, jak zmieniały się nasze wyniki w różnych 
                 formatach gry na przestrzeni czasu.")
        )
      ),
      uiOutput("ranking_boxes"),
      card(plotOutput("Plot")),
      sidebar = sidebar(
        padding = "30px",
        selectInput( 
          "select", 
          "Wybierz typ rozgrywek:",
          c("Rapid","Blitz","Bullet"),
          selected = "Rapid"
        ),
        checkboxInput("dominik", "Dominik", TRUE),
        checkboxInput("jakub", "Jakub", TRUE),
        checkboxInput("daniel", "Daniel", TRUE),
        sliderInput(
          inputId = "date_range",
          label = "Wybierz zakres dat:",
          min = min(df_rapid$Date),
          max = max(df_rapid$Date),
          value = c(min(df_rapid$Date), max(df_rapid$Date)),
          timeFormat = "%Y-%m-%d"
        )
      )
    )
  ),
  nav_panel(
    title = "Statystyki", 
    page_sidebar(
      card(
        fill = FALSE,
        card_header(tags$b("Wprowadzenie")),
        card_body(
          tags$p("Na tej stronie badamy, jak kolor figur (biały lub czarny) wpływa 
                 na procent wygranych graczy na różnym poziomie.",
                 "Analiza uwzględnia podział na typy rozgrywek oraz rating zawodników,
                 co pozwala lepiej zrozumieć, czy i jak przewaga koloru różni się w różnych 
                 warunkach.")
        )
      ),
      card(
        title = "Procent i liczba zwycięstw białych i czarnych",
        fill = TRUE,
        plotOutput("barPlot")
      ),
      sidebar = sidebar(
        padding = "30px",
        checkboxGroupInput("event_choice", "Typ partii:",
                           choices = c("Bullet", "Blitz", "Classical"),
                           selected = c("Bullet", "Blitz", "Classical")),
        sliderInput("rating_range", "Zakres ratingu obu graczy:",
                    min = 1000, max = 3200, value = c(1000, 3200))
      )
    )
  )
)

server <- function(input, output, session) {
  thematic_shiny()
  
  output$density_plot <- renderPlot({
    rating_name %>% 
      filter(year %in% input$year_roz, !is.na(rating_standard)) %>% 
      ggplot(aes(x = rating_standard, color = factor(year), fill = factor(year))) + 
      geom_density(linewidth = 1.2, alpha = 0.1) +
      labs(
        color = "Rok",
        x = "Rating",
        y = "Gęstość"
      ) +
      theme_minimal() + 
      theme(
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)
      ) + 
      scale_color_paletteer_d("RColorBrewer::Dark2") +
      scale_fill_paletteer_d("RColorBrewer::Dark2") +
      guides(fill = "none")
  })
  
  output$point_plot <- renderPlot({
    rating_name %>% 
      filter(federation %in% input$country) %>% 
      ggplot(aes(x = wiek, y = rating_standard, color = federation)) +
      geom_point() +
      labs(
        color = "Kraj",
        x = "Wiek",
        y = "Rating"
      ) +
      theme_minimal() + 
      theme(
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)
      ) + 
      scale_color_paletteer_d("RColorBrewer::Dark2")
  })
  
  output$hist_plot <- renderPlot({
    rating_name %>% 
      filter(year == 2020, wiek >= input$wiek[1], wiek <= input$wiek[2], gender %in% input$sex) %>%
      group_by(year, title) %>% 
      summarize(n = n(), .groups = "drop") %>% 
      mutate(title = forcats::fct_reorder(title, n, .desc = T)) %>% 
      ggplot(aes(x = title, y = n)) + 
      geom_col() +
      labs(
        x = "Typ klasyfikacji", 
        y = "Liczba graczy",
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)
      )
  })
  
  output$barPlot <- renderPlot({
    base_data <- szachy %>%
      mutate(Event = trimws(Event)) %>%
      filter(Event %in% input$event_choice,
             WhiteElo >= input$rating_range[1],
             WhiteElo <= input$rating_range[2],
             BlackElo >= input$rating_range[1],
             BlackElo <= input$rating_range[2])
    
    
    base_counts <- base_data %>%
      group_by(Event) %>%
      summarise(TotalGames = n())
    
    filtered_data <- base_data %>%
      filter(Result %in% c("1-0", "0-1")) %>%
      mutate(
        Winner = case_when(
          Result == "1-0" ~ "Biały",
          Result == "0-1" ~ "Czarny"
        )) %>%
      mutate(Event = factor(Event, levels = c("Bullet", "Blitz", "Classical"))) %>% 
      group_by(Event, Winner) %>%
      summarise(Count = n()) %>% 
      left_join(base_counts, by = "Event") %>%
      mutate(Percent = Count / TotalGames)
    
    ggplot(filtered_data, aes(x = Event, y = Count, fill = Winner)) +
      geom_col(position = position_dodge(width = 0.9), color = "black") +
      geom_text(aes(label = scales::percent(Percent, accuracy = 0.1)),
                position = position_dodge(width = 0.9),
                vjust = -0.3, size = 6) +
      scale_fill_manual(values = c("Biały" = "white", "Czarny" = "black")) +
      labs(
        x = "Typ partii",
        y = "Liczba zwycięstw",
        fill = "Zwycięzca"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)
      )
  })
  
  wybrani <- reactive({
    osoby <- c()
    if (input$dominik) osoby <- c(osoby, "Dominik")
    if (input$jakub)   osoby <- c(osoby, "Jakub")
    if (input$daniel)  osoby <- c(osoby, "Daniel")
    return(osoby)
  })
  
  selected_data <- reactive({
    switch(input$select,
           "Rapid" = df_rapid,
           "Blitz" = df_blitz,
           "Bullet" = df_bullet)
  })
  
  output$ranking_boxes <- renderUI({
    data <- selected_data()
    
    max_dominik <- max(data$Elo[data$Osoba == "Dominik" & data$Date >= input$date_range[1] & data$Date <= input$date_range[2]])
    max_daniel  <- max(data$Elo[data$Osoba == "Daniel" & data$Date >= input$date_range[1] & data$Date <= input$date_range[2]])
    max_jakub   <- max(data$Elo[data$Osoba == "Jakub" & data$Date >= input$date_range[1] & data$Date <= input$date_range[2]])
    
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Najwyższy rating Dominika:",
        value = max(0,max_dominik),
        fill = FALSE,
        height = "100px"
      ),
      value_box(
        title = "Najwyższy rating Daniela:",
        value = max(0,max_daniel),
        fill = FALSE,
        height = "100px"
      ),
      value_box(
        title = "Najwyższy rating Jakuba:",
        value = max(max_jakub,0),
        fill = FALSE,
        height = "100px"
      )
    )
  })
  
  observeEvent(input$select, {
    dates <- selected_data()$Date
    updateSliderInput(session, "date_range",
                      min = min(dates),
                      max = max(dates),
                      value = c(min(dates), max(dates)))
  })
  
  output$Plot <- renderPlot(
    if(input$select == "Rapid"){
      rapid_dominik <- wolfdomix_rapid
      rapid_kuba <- kuba_rapid
      rapid_daniel <- daniel_rapid
      df <- bind_rows(rapid_dominik,rapid_kuba)
      df <- bind_rows(df,rapid_daniel)
      df %>% 
        filter(Osoba %in% wybrani(), Date >= input$date_range[1], Date <= input$date_range[2]) %>% 
        group_by(Osoba) %>% 
        ggplot(aes(x=Date,y=Elo,group=Osoba,color=Osoba)) +
        geom_line(linewidth=0.9, alpha = 1) +
        scale_color_manual(values = c(
          "Dominik" = "#0D5B3A",
          "Jakub" = "#88131C",
          "Daniel" = "#E86E28"
        )) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.text.y = element_text(size = 16),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )
      
    }
    else if(input$select == "Bullet"){
      bullet_dominik <- wolfdomix_bullet
      bullet_daniel <- daniel_bullet
      df <- bind_rows(bullet_dominik,bullet_daniel)
      df %>% 
        filter(Osoba %in% wybrani(),Date >= input$date_range[1], Date <= input$date_range[2]) %>% 
        group_by(Osoba) %>% 
        ggplot(aes(x=Date,y=Elo,group=Osoba,color=Osoba)) +
        geom_line(linewidth=0.9, alpha = 1) +
        scale_color_manual(values = c(
          "Dominik" = "#0D5B3A",
          "Jakub" = "#88131C",
          "Daniel" = "#E86E28"
        )) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.text.y = element_text(size = 16),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )
    }
    else if(input$select == "Blitz"){
      blitz_dominik <- wolfdomix_blitz
      blitz_daniel <- daniel_blitz
      blitz_kuba <- kuba_blitz
      df <- bind_rows(blitz_dominik,blitz_daniel)
      df <- bind_rows(df,blitz_kuba)
      df %>% 
        filter(Osoba %in% wybrani(),Date >= input$date_range[1], Date <= input$date_range[2]) %>% 
        group_by(Osoba) %>% 
        ggplot(aes(x=Date,y=Elo,group=Osoba,color=Osoba)) +
        geom_line(linewidth=0.9, alpha = 1) +
        scale_color_manual(values = c(
          "Dominik" = "#0D5B3A",
          "Jakub" = "#88131C",
          "Daniel" = "#E86E28"
        )) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.text.y = element_text(size = 16),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )
    }
    
  )
  output$Tabela <- renderDataTable({
    if(input$select == "Rapid"){
      rapid_dominik <- wolfdomix_rapid
      rapid_kuba <- kuba_rapid
      rapid_daniel <- daniel_rapid
      df <- bind_rows(rapid_dominik,rapid_kuba)
      df <- bind_rows(df,rapid_daniel)
      df %>% 
        filter(Osoba %in% wybrani(), Date >= input$date_range[1], Date <= input$date_range[2]) %>% 
        group_by(Osoba) %>% 
        mutate(Date = as.Date(Date)) %>%
        summarise("Maksymalny ranking" = max(Elo))
      
    }
    else if(input$select == "Bullet"){
      bullet_dominik <- wolfdomix_bullet
      bullet_daniel <- daniel_bullet
      df <- bind_rows(bullet_dominik,bullet_daniel)
      df %>% 
        filter(Osoba %in% wybrani(), Date >= input$date_range[1], Date <= input$date_range[2]) %>% 
        group_by(Osoba) %>% 
        mutate(Date = as.Date(Date)) %>%
        summarise("Maksymalny ranking" = max(Elo))
    }
    else if(input$select == "Blitz"){
      blitz_dominik <- wolfdomix_blitz
      blitz_daniel <- daniel_blitz
      blitz_kuba <- kuba_blitz
      df <- bind_rows(blitz_dominik,blitz_daniel)
      df <- bind_rows(df,blitz_kuba)
      df %>% 
        filter(Osoba %in% wybrani(), Date >= input$date_range[1], Date <= input$date_range[2]) %>% 
        group_by(Osoba) %>% 
        summarise("Maksymalny ranking" = max(Elo))
    }
  })
}

shinyApp(ui = ui, server = server)
