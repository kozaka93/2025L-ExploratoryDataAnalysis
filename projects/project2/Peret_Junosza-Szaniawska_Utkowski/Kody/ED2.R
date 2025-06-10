ui <- navbarPage("Muzyka",
                 tags$head(
                   tags$style(HTML("
                      body {
                        background-image: url('bg.jpg');
                        background-size: cover;
                        background-repeat: no-repeat;
                        background-attachment: fixed;
                      }
                      
                      /* Tło całego navbar */
                      .navbar-default, .navbar {
                        background-color: #5b6f6d !important;
                        border-color: #5b6f6d !important;
                      }
                      /* Kolor linków w navbar (nieaktywne zakładki) */
                      .navbar-default .navbar-nav > li > a {
                        color: #ffffff !important;
                      }
                      /* Kolor linków w navbar po najechaniu */
                      .navbar-default .navbar-nav > li > a:hover,
                      .navbar-default .navbar-nav > li > a:focus {
                        color: #a7b7aa !important;
                        background-color: transparent !important;
                      }
                      /* Kolor aktywnej zakładki */
                      .navbar-default .navbar-nav > .active > a,
                      .navbar-default .navbar-nav > .active > a:hover,
                      .navbar-default .navbar-nav > .active > a:focus {
                        color: #bc7254 !important;
                        background-color: #a7b7aa !important;
                      }
                      /* Kolor tytułu navbar */
                      .navbar-default .navbar-brand {
                        color: #ffffff !important;
                      }
                      .navbar-default .navbar-brand:hover {
                        color: #ffffff !important;
                      }
                    ")),
                 ),
                 tabPanel("O aplikacji",
                          fluidPage(
                            style = "background-color: rgba(255, 255, 255, 0.8); padding: 15px; border-radius: 5px;",
                            fluidRow(
                              column(6,
                                     h3("Muzyka"),
                                     h4("Agnieszka Peret, Ewa Junosza-Szaniawska, Piotr Utkowski"),
                                     p("Aplikacja służy do analizy danych na temat piosenek (przede wszystkim ze Spotify, ale też z innych głównych platform streamingowych)."),
                                     p("Ramki danych:",
                                       tags$ul(
                                         tags$li(strong("df"), tags$span(paste0("[", nrow(df), ":", ncol(df), "]"), style = "color: #bc7254"), "- podstawowa ramka z danymi o utworach, zawierająca między innymi informacje o roku wydania, długości i innych cechach akustycznych",
                                                 tags$br(), tags$span("używana we wszystkich wykresach poza niżej wymienionymi", style = "color: #5b6f6d")),
                                         tags$li(strong("df23"), tags$span(paste0("[", nrow(df23), ":", ncol(df23), "]"), style = "color: #bc7254"), "- ramka z danymi dotyczącymi analizy najbardziej popularnych piosenek w 2023 roku, wraz z rankingami na różnych platformach streamingowych",
                                                 tags$br(), tags$span("używana w wykresie ", style = "color: #5b6f6d"), tags$span("Tonacja a rozkład miejsc w rankingu", style = "color: #5b6f6d; font-style: italic;")),
                                         tags$li(strong("dgatunki"), tags$span(paste0("[", nrow(dgatunki), ":", ncol(dgatunki), "]"), style = "color: #bc7254"), "- zbiór danych z informacjami o gatunkach muzycznych oraz ich popularności na podstawie różnych źródeł",
                                                 tags$br(), tags$span("używana w wykresie ", style = "color: #5b6f6d"), tags$span("Naj___ popularne gatunki", style = "color: #5b6f6d; font-style: italic;")),
                                         tags$li(strong("all_data"), tags$span(paste0("[", nrow(all_data), ":", ncol(all_data), "]"), style = "color: #bc7254"), "- dane pochodzące z naszych prywatnych playlist 'Twoje ulubione utwory' eksportowane z Spotify. Zawierają szczegółowe informacje o utworach, takie jak tytuł, wykonawca, gatunek i data wydania",
                                                 tags$br(), tags$span("używana w wykresie ", style = "color: #5b6f6d"), tags$span("Słuchana przez nas muzyka", style = "color: #5b6f6d; font-style: italic;"))
                                       )),
                                     p("Źródła poszczególnych ramek: ",
                                       tags$a(href = "https://github.com/gabminamedez/spotify-data/tree/master?tab=readme-ov-file", "df", target = "_blank"),
                                       tags$a(href = "https://github.com/jivanjotk/Most-Streamed-Spotify-Songs-2023-Analysis-?fbclid=IwZXh0bgNhZW0CMTEAAR7ZKX5WIIl9fHyio5chL1j73cXX1GHkRqac84W_PA52NzpyXPBZznnmAMJzjw_aem_lnlk79sDbbQpcrqDXvkOTQ", "df23", target = "_blank"),
                                       tags$a(href = "https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset", "dgatunki", target = "_blank"),
                                       tags$a(href = "https://exportify.net/", "all_data", target = "_blank"))
                              ),
                              column(6,plotlyOutput("yearsPlot")
                              )
                            )
                          )
                 ),
                 tabPanel("Czas wydania a charakterystyka piosenek",
                        fluidPage(
                          style = "background-color: rgba(255, 255, 255, 0.8); padding: 15px; border-radius: 5px;",
                            fluidRow(
                              column(6, h3("Czas wydania a charakterystyka piosenek")),
                              column(6, selectInput('zmienna','Dla jakiej zmiennej narysować rozkład?',
                                                    choices = choices1))
                            ),
                            fluidRow(
                              column(6,
                                     p("W tej części można się przyjrzeć zależnościom między datą wydania utworu, a jego cechami, a także temu, w które dni roku wydawanych jest najwięcej piosenek."),
                                     p("Wykresy:",
                                       tags$ul(
                                         tags$li(strong("Rozkład zmiennej ____"), "- wykres pokazujący medianę i rozstęp między kwartylami (Q1–Q3) wybranej cechy (np. energia, taneczność) w zależności od miesiąca wydania.
                                                 Można zaobserwować, że na przełomie roku piosenki mają tendecję do bycia bardziej pozytywnymi i akustycznymi, a jednocześnie mniej energicznymi niż w innych miesiącach. 
                                                 Ta zależność może być powodowana wydawaniem piosenek bożonarodzeniowych czy noworocznych. Taneczność piosenek przez cały rok utrzymuje sie na podobnym poziomie."),
                                         tags$li(strong("Długość w podziale na dekady"), "- violin plot przedstawiający rozkład długości utworów w różnych dekadach. Najdłuższe piosenki były wydawane pod koniec XX wieku. 
                                                 Aktualnie z dekady na dekadę utwory stają się coraz krótsze."),
                                         tags$li(strong("Liczba wydanych piosenek"), "- mapa ciepła prezentująca, w które dni miesiąca ukazuje się najwięcej nowych piosenek. Wykres zawiera opcję usunięcia danych z pierwszych dni miesiąca.
                                                 W pierwszej połowie minionego wieku część piosenek, których dzień wydania był nieznany, automatycznie ma przypisany jako dzień wydania pierwszy dzień danego miesiąca, co może zaburzać dane.
                                                 Ciekawą obserwacją jest fakt, że w ostatnich latach data wydawania piosenek zależy od dnia tygodnia. Dawniej większy wpływ miały inne czynniki jak na przykład pora roku. 
                                                 Na przełomie wieków najwięcej utworów wydawano jesienią.")))),
                              column(6, plotlyOutput("linePlot"))
                            ),
                            fluidRow(
                              column(6),
                              column(6, 
                                     sliderInput("zakres_lat", "Wybierz zakres lat:", 
                                                    min = 1921, max = 2020, value = c(2000, 2020),sep = ""),
                                        checkboxInput("pierwsze_dni", 
                                                      "Czy uwzględnić pierwsze dni miesiąca", 
                                                        value = TRUE)),
                            ),
                            fluidRow(
                              column(6, plotlyOutput("violinPlot")),
                              column(6, plotlyOutput("heatmapPlot"))
                            )
                          )
                 ),
                 tabPanel("Jakie utwory są najbardziej popularne?",
                          fluidPage(
                            style = "background-color: rgba(255, 255, 255, 0.8); padding: 15px; border-radius: 5px;",
                            fluidRow(
                              column(6,h3("Jakie utwory są najbardziej popularne?")),
                              column(6,
                                     checkboxGroupInput("wybrane_osoby", "Wybierz osoby do analizy:",
                                                        choices = c("Wybrani (razem)", "Aga", "Ewa", "Piotr"), 
                                                        selected = c("Wybrani (razem)", "Aga", "Ewa", "Piotr")))
                            ),
                            fluidRow(
                              column(6,
                                     p("W tej części można sprawdzić, jakich roczników muzyki zazwyczaj słuchamy, jakie tonacje pojawiają się najczęściej w rankingach różnych platform streamingowych oraz które gatunki muzyczne cieszą się największą (lub najmniejszą) popularnością."),
                                     p("Wykresy:",
                                       tags$ul(
                                         tags$li(strong("Słuchana przez nas muzyka"), "- wykres gęstości pokazujący rozkład lat wydania najczęściej słuchanych utworów, z podziałem na osoby. Każde z nas w większości słucha muzyki wydanej w ostatnich latach."),
                                         tags$li(strong("Tonacja a rozkład miejsc w rankingu"), "- quasi-random plot ilustrujący zależność między tonacją utworu a jego pozycją w rankingach popularności na wybranej platformie. Można zauważyć, że wiele z najpopularniejszych
                                                 piosenek jest w tonacji F lub C#. "),
                                         tags$li(strong("Naj___ popularne gatunki"), "- boxplot prezentujący popularność wybranej liczby najbardziej/najmniej popularnych gatunków muzycznych. Z dość oczywistych powodów najpopularniejsza jest muzyka popularna. Najmniej popularna natomiast 
                                                 muzyka irańska.")
                                       )
                                     )
                              ),
                              column(6, plotlyOutput("densityPlot"))
                           ),
                           fluidRow(
                             column(6, selectInput("platforma", "Wybierz platformę streamingową:",
                                                   choices = choices2)),
                             column(6, sliderInput("ile_gatunkow", "Ile gatunków pokazać?",
                               min = 1,max = 30, value = 10),
                             
                             radioButtons("typ_gatunkow", "Które gatunki pokazać?",
                               choices = c("Najbardziej popularne" = "top", "Najmniej popularne" = "bottom"),
                               selected = "top"))
                           ),
                           fluidRow(
                             column(6, plotlyOutput("quasiPlot")),
                             column(6, plotlyOutput("gatunkiPlot"))
                           )
                 )
        )
)



server <- function(input, output) {
  

  output$yearsPlot<- renderPlotly({
    
    df_yearsPlot <- df %>%
      filter(!is.na(year)) %>%
      mutate(
        decade_start = floor((year - 1) / 10) * 10 + 1,
        decade_end = decade_start + 9,
        decade_label = paste0(decade_start, "-", decade_end)
      )
    
    p<-ggplot(df_yearsPlot, aes(x = decade_label))+
      geom_bar(color = "#5b6f6d", fill = "#5b6f6d")+
      labs(
        title = paste("Liczba analizowanych piosenek"),
        x = "Dekada",
        y = "Liczba")+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
    
    
  })
  
  output$linePlot <- renderPlotly({
    
    miesiace <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec",
                  "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")
    
    idx <- match(input$zmienna, zmienne1)
    etykieta <- etykiety1[idx]
    
    zmienna_sym <- rlang::sym(input$zmienna)
    
    dane <- df %>%
      group_by(month) %>%
      summarise(
        mediana = median(!!zmienna_sym, na.rm = TRUE),
        q25 = quantile(!!zmienna_sym, 0.25, na.rm = TRUE),
        q75 = quantile(!!zmienna_sym, 0.75, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(q25) & !is.na(q75)) %>%
      arrange(month)
    
    p <- ggplot(dane, aes(x = month)) +
      geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.2, color = "#5b6f6d")+
      geom_line(aes(y = mediana), color = "#d3aa6a") +
      geom_point(aes(y = mediana), color = "#bc7254") +
      scale_x_continuous(breaks = 1:12, labels = miesiace) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_minimal() +
      labs(
        title = paste("Rozkład zmiennej",etykieta),
        x = "Miesiąc",
        y = "Wartość"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$violinPlot <- renderPlotly({
    
    df <- df %>%
      filter(!is.na(year)) %>% 
      mutate(
        decade_start = floor((year - 1) / 10) * 10 + 1,
        decade_end = decade_start + 9,
        decade_label = paste0(decade_start, "-", decade_end),
        duration_min = duration_ms / 60000
      )
    
    ggplot(df, aes(x = decade_label, y = duration_min)) +
      geom_violin(fill = "#5b6f6d", alpha = 0.7) +
      coord_cartesian(ylim = c(0, 10)) +
      labs(
        title = "Długość w podziale na dekady",
        x = "Dekada",
        y = "Długość (min)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$heatmapPlot <- renderPlotly({
    req(input$zakres_lat)
    
    df_heatmap <- df %>%
      filter(
        year >= input$zakres_lat[1],
        year <= input$zakres_lat[2]
      )
    
    if (!input$pierwsze_dni) {
      df_heatmap <- df_heatmap %>% filter(day != 1)
    }
    
    df_heatmap<- df_heatmap%>%
      group_by(month, day) %>%
      summarise(
        liczba_piosenek = n(),
        .groups = "drop"
      )
    
    p <- ggplot(df_heatmap, aes(x = day, y = month, fill = liczba_piosenek)) +
      geom_tile() +
      scale_fill_gradient(low = "#ebdac0", high = "#8e4f36", name = "Liczba piosenek",
                          limits = c(0, quantile(df_heatmap$liczba_piosenek, 0.99)),na.value = "black" ) +
      labs(
        title = paste("Liczba wydanych piosenek"),
        x = "Dzień miesiąca",
        y = "Miesiąc"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()
      )
    
    ggplotly(p)
  })
  
  output$densityPlot <- renderPlotly({
    
    req(input$wybrane_osoby)
    
    df_filtered <- all_data %>%
      filter(person %in% input$wybrane_osoby)
    
    df_wszyscy <- df_filtered %>%
      mutate(person = "Razem")
    
    df_combined <- bind_rows(df_wszyscy, df_filtered)
    
    levels_combined <- unique(c("Razem", input$wybrane_osoby))
    df_combined$person <- factor(df_combined$person, levels = levels_combined)
    
    kolory_combined <- c("Razem" = "gray50", osoby_kolory)
    
    p <- ggplot(df_combined, aes(x = release_year, fill = person, color = person)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = kolory_combined) +
      scale_color_manual(values = kolory_combined, guide = FALSE) +
      coord_cartesian(ylim = c(0, 0.2)) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Słuchana przez nas muzyka",
        x = "Rok wydania",
        y = "Procent utworów",
        fill = "Osoby",
        color = NULL
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$quasiPlot <- renderPlotly({
    
    kolumna <- input$platforma
    
    df <- df23 %>%
      mutate(ranking = as.numeric(.data[[kolumna]])) %>%  
      filter(!is.na(ranking)) %>% 
      filter(ranking!=0) %>% 
      filter(key!='') %>%                       
      select(key, ranking)
    
    p <- ggplot(df, aes(x = factor(key), y = ranking)) +
      geom_quasirandom(color = "#bc7254", alpha = 0.7, size = 0.5) +
      coord_cartesian(ylim = c(0, 150))+
      theme_minimal() +
      labs(
        title = "Tonacja a rozkład miejsc w rankingu",
        x = "Tonacja",
        y = "Pozycja w rankingu"
      )
    
    ggplotly(p) %>% layout(margin = list(l = 1, r = 20, b = 100, t = 50))
  })
  
  gatunki_ranked <- reactive({
    dgatunki %>%
      left_join(pom, by = "track_genre") %>%
      group_by(track_genre) %>%
      summarise(
        median_popularity = median(popularity, na.rm = TRUE),
        mean_popularity = mean(popularity, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      {
        if (input$typ_gatunkow == "top") {
          arrange(., desc(median_popularity), desc(mean_popularity))
        } else {
          arrange(., median_popularity, mean_popularity)
        }
      } %>%
      slice_head(n = input$ile_gatunkow) %>%
      mutate(track_genre = factor(track_genre, levels = track_genre))
  })
  
  output$gatunkiPlot <- renderPlotly({
    gatunki <- gatunki_ranked()  
    
    dane_wykres <- dgatunki %>%
      filter(track_genre %in% gatunki$track_genre) %>%
      mutate(track_genre = factor(track_genre, levels = levels(gatunki$track_genre)))
    
    outliers_df <- dane_wykres %>%
      group_by(track_genre) %>%
      mutate(
        Q1 = quantile(popularity, 0.25),
        Q3 = quantile(popularity, 0.75),
        IQR = Q3 - Q1,
        is_outlier = popularity < (Q1 - 1.5 * IQR) | popularity > (Q3 + 1.5 * IQR)
      ) %>%
      filter(is_outlier)
    
   p<- ggplot(dane_wykres, aes(x = track_genre, y = popularity)) +
     geom_boxplot(fill = "#e7c99c") +
     geom_jitter(
        data = outliers_df,
        aes(text = paste("title:", track_name, "<br>artist(s):", artists)),
        size = 0.5,
        color = "#d3aa6a",
      ) +
      labs(
        title = paste(
          ifelse(input$typ_gatunkow == "top", "Najbardziej", "Najmniej"),
          "popularne gatunki"
        ),
        x = "Gatunek",
        y = "Popularność"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = -90, hjust = 0))
   
   ggplotly(p)
  })
  
}

shinyApp(ui = ui, server = server)
  


