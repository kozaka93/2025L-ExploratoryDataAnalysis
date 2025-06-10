library(tidyr)
library(lubridate)
library(jsonlite)
library(stringr)
library(Polychrome)
library(shiny)
library(dplyr)
library(plotly)
library(shinycssloaders)
library(bslib)

## Wcszytujemy dane

print(getwd())

data_pom <- read.csv("data_pom.csv")
zdarzenia_df <- read.csv("zdarzenia_df.csv")

transfers <- read.csv("archive/transfers.csv")
clubs <- read.csv("archive/clubs.csv")
players1 <- read.csv("archive/player_valuations.csv")
players2 <- read.csv("archive/players.csv")
colnames(players2)
### Czyszczenie i przygotowanie danych do wykresu piłkarzy
players <- players1 %>%
  left_join(
    clubs %>% select(club_id, domestic_competition_id),
    by = c("current_club_id" = "club_id")
  ) %>%
  mutate(league = case_when(
    domestic_competition_id == "GB1" ~ "Premier League",
    domestic_competition_id == "ES1" ~ "La Liga",
    domestic_competition_id == "FR1" ~ "Ligue 1",
    domestic_competition_id == "IT1" ~ "Serie A",
    domestic_competition_id == "L1"  ~ "Bundesliga",
    TRUE                             ~ "Inne"
  )) %>%
  filter(league != "Inne")

players <- players %>%
  left_join(players2 %>% select(player_id, name, position, height_in_cm, country_of_citizenship), by = "player_id")

players <- players %>%
  left_join(
    clubs %>% select(club_id, club_name = name),
    by = c("current_club_id" = "club_id")
  )
colnames(players)
head(players)
league_colors <- c(
  "Premier League" = "#2ca02c",
  "La Liga"        = "#1f77b4",
  "Ligue 1"        = "#d62728",
  "Serie A"        = "#bcbd22",
  "Bundesliga"     = "#ff7f0e"
)


## Czyszczenie i przygotowanie danych do wykresu sankey

transfers <- transfers %>%
  select(c(player_id, transfer_season, from_club_id, to_club_id, transfer_fee, market_value_in_eur, player_name))

clubs <- clubs %>%
  select(c(club_id, name, domestic_competition_id, net_transfer_record))

transfers1 <- transfers %>%
  left_join(clubs, by = join_by(from_club_id == club_id)) %>%
  left_join(clubs, by = join_by(to_club_id == club_id), suffix = c("_from", "_to"))

transfers2 <- transfers1 %>%
  mutate(domestic_competition_id_from = case_when((domestic_competition_id_from == "GB1") ~ "Premier League",
                                                  (domestic_competition_id_from == "ES1") ~ "La Liga",
                                                  (domestic_competition_id_from == "FR1") ~ "Ligue 1",
                                                  (domestic_competition_id_from == "IT1") ~ "Serie A",
                                                  (domestic_competition_id_from == "L1") ~ "Bundesliga",
                                                  TRUE ~ "Inne"),
         domestic_competition_id_to = case_when((domestic_competition_id_to == "GB1") ~ "Premier League",
                                                (domestic_competition_id_to == "ES1") ~ "La Liga",
                                                (domestic_competition_id_to == "FR1") ~ "Ligue 1",
                                                (domestic_competition_id_to == "IT1") ~ "Serie A",
                                                (domestic_competition_id_to == "L1") ~ "Bundesliga",
                                                TRUE ~ "Inne"),
         transfer_fee = case_when(is.na(transfer_fee) ~ 0,
                                  TRUE ~ transfer_fee)) %>%
  filter(!(domestic_competition_id_from == "Inne" & domestic_competition_id_to == "Inne"), 
         !(transfer_season %in%  c("99/00", "00/01", "01/02", "02/03", "03/04", "04/05", "05/06", "06/07", "07/08", "08/09", "09/10", "26/27")))

from_labels <- c("Bundesliga", "Inne", "La Liga", "Ligue 1", "Premier League", "Serie A")
to_labels <- c("Bundesliga", "Inne", "La Liga", "Ligue 1", "Premier League", "Serie A")
nodes_labels <- c(from_labels, to_labels)
league_colors <- c("La Liga" = "#1f77b4",
                   "Bundesliga" = "#ff7f0e",
                   "Premier League" = "#2ca02c",
                   "Ligue 1" = "#d62728",
                   "Inne" = "#7f7f7f",
                   "Serie A" = "#bcbd22")
node_colors <- league_colors[nodes_labels]
nodes_map_from <- setNames(seq_along(from_labels) - 1, from_labels)
nodes_map_to <- setNames(seq_along(to_labels) - 1 + length(from_labels), to_labels)


## Dobieramy kolorystykę do danych

ligi = unique(data_pom$League)

kolory_ligi = lapply(ligi, function(liga) {
  druzyny = sort(unique(data_pom$Team[data_pom$League == liga]))
  paleta = createPalette(length(druzyny), c("#000000", "#FFFFFF"))
  setNames(paleta, druzyny)
})

names(kolory_ligi) = ligi

# APLIKACJA SHINY

ui1 <- fluidPage(
  
  theme = bs_theme(
    version = 5,                   
    bootswatch = "flatly",         
    primary = "purple",          
    base_font = font_google("Roboto"),  
    code_font = font_google("Fira Code")
  ),
  
  tags$h1("Statystyki z 5 najlepszych lig w latach 2003–2021", style = "text-align: center;"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("liga", "Wybierz ligę:",
                  choices = unique(data_pom$League)),
      
      uiOutput("sezon_ui"),
      
      checkboxGroupInput("statystyki", "Statystyki:",
                         choices = c("Punktacja w sezonie" = "CumulativePoints",
                                     "Gole zdobyte i stracone" = "Gole",
                                     "Gole według minuty meczu" = "licznosci"),
                         selected = "CumulativePoints"),
      
      checkboxGroupInput("teams", "Wybierz drużyny:",
                         choices = NULL)
    ),
    
    mainPanel(
      
      tags$p("W tej sekcji można porównać drużyny pod względem wybranej statystyki: punktacji w trakcie sezonu, liczby bramek zdobytych i straconych oraz rozkładu goli według minut meczu. 
   Wybierz sezon, drużyny (posortowane według końcowej pozycji w tabeli) i interesujące Cię statystyki, aby zobaczyć szczegółowe dane."),
      
      conditionalPanel(
        condition = "input.statystyki.includes('CumulativePoints')",
        withSpinner(plotlyOutput("wykres_punkty"), type = 1, color = "purple")
      ),
      br(),  
      conditionalPanel(
        condition = "input.statystyki.includes('Gole')",
        withSpinner(plotlyOutput("wykres_bramki"), type = 1, color = "purple")
      ),
      br(),  
      conditionalPanel(
        condition = "input.statystyki.includes('licznosci')",
        withSpinner(plotlyOutput("wykres_heatmapa"), type = 1, color = "purple")
      ))))


ui2 <- fluidPage(
  titlePanel("Przepływy transferowe w piłce nożnej w XXI wieku"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "dane_sankey", 
                   label = "Wybierz dane", 
                   choices = list("Liczba transferów" = "count", 
                                  "Przepływy pieniężne" = "transfer_fees_sum")),
      checkboxGroupInput( 
        "sezony_sankey", 
        "Wybierz sezony", 
        unique(transfers2$transfer_season)
      )
    ),
    mainPanel(
      tags$p("W tej sekcji można przeanalizować liczbę transferów oraz przepływy pieniędzy pomiędzy ligami. Wybierz interesujące Cię sezony oraz rodzaj statystyki."),
      plotlyOutput("sankey"))
  )
)

ui3 <- fluidPage(
  titlePanel("Najbardziej wartościowi piłkarze"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "liga_pilkarze",
        label = "Wybierz ligę(i):",
        choices = sort(unique(players$league)),
        selected = NULL
      ),
      
      uiOutput("klub_ui")  
    ),
    
    mainPanel(
      tags$p("W tej sekcji możesz porównać najbardziej wartościowych piłkarzy według ligi i drużyny. Wybierz interesujące Cię ligii oraz drużyny. Wyświetlone zostanie top 5 najwyżej wycenianych zawodników."),
      withSpinner(plotlyOutput("top_players"), type = 1, color = "purple")
    )
  )
)



## Serwer
server <- function(input, output, session) {
  output$sezon_ui <- renderUI({
    sezony <- data_pom %>%
      filter(League == input$liga) %>%
      pull(SeasonID) %>%
      unique() %>%
      sort()
    
    selectInput("sezon", "Wybierz sezon:",
                choices = sort(unique(sezony)),
                selected = min(sezony))
  })
  
  observe({
    req(input$sezon)
    
    zespoly <- data_pom %>% filter(League == input$liga, SeasonID == input$sezon) %>%
      group_by(Team) %>% summarise(Punkty = max(CumulativePoints, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Punkty)) %>% pull(Team)
    
    updateCheckboxGroupInput(session, "teams",
                             choices = zespoly,
                             selected = intersect(input$teams, zespoly))
  })
  
  # 1. WIZUALIZACJA
  output$wykres_punkty = renderPlotly({
    req("CumulativePoints" %in% input$statystyki, input$teams)
    
    sezon_numeric = as.numeric(input$sezon)
    
    dane = data_pom %>%
      filter(League == input$liga, SeasonID == sezon_numeric, Team %in% input$teams) %>%
      mutate(Round = as.integer(Round)) %>%
      arrange(Team, Round)
    
    kolory_druzyn = kolory_ligi[[input$liga]]
    
    plot_ly(data = dane,
            x = ~Round,
            y = ~CumulativePoints,
            color = ~Team,
            colors = kolory_druzyn,
            type = 'scatter',
            mode = 'lines+markers',
            text = ~tooltip,
            hoverinfo = 'text') %>%
      layout(
        title = paste("Punkty –", input$liga, "sezon", sezon_numeric, "/", sezon_numeric + 1),
        xaxis = list(title = "Runda"),
        yaxis = list(title = "Punkty"),
        legend = list(x = 1.02, y = 1, xanchor = "left", yanchor = "top", orientation = "v"),
        margin = list(r = 130)
      )
  })
  
  # 2. WIZUALIZACJA
  output$wykres_bramki = renderPlotly({
    req("Gole" %in% input$statystyki, input$teams)
    
    sezon_numeric = as.numeric(input$sezon)  
    
    dane = data_pom %>%
      filter(League == input$liga, SeasonID == sezon_numeric, Team %in% input$teams) %>%
      group_by(Team) %>%
      summarise(
        `Bramki zdobyte` = max(CumulativeGoals, na.rm = TRUE),
        `Bramki stracone` = max(CumulativeLostGoals, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = c("Bramki zdobyte", "Bramki stracone"),
        names_to = "Rodzaj",
        values_to = "Wartosc"
      ) %>%
      filter(!is.na(Wartosc))
    
    dane_bramki_zdobyte = dane %>% filter(Rodzaj == "Bramki zdobyte")
    dane_bramki_stracone = dane %>% filter(Rodzaj == "Bramki stracone")
    
    plot_ly(data = dane_bramki_zdobyte,
            x = ~Team,
            y = ~Wartosc,
            type = 'bar',
            name = "Bramki zdobyte",
            marker = list(color = "#1B9E77"),
            text = ~paste("Gole zdobyte:", Wartosc),
            textposition = 'none',
            hoverinfo = 'text') %>%
      add_trace(data = dane_bramki_stracone,
                x = ~Team,
                y = ~Wartosc,
                type = 'bar',
                name = "Bramki stracone",
                marker = list(color = "#FF6347"),
                text = ~paste("Gole stracone:", Wartosc),
                textposition = 'none',
                hoverinfo = 'text') %>% 
      layout(
        title = paste("Bramki zdobyte i stracone –", input$liga, "sezon", sezon_numeric, "/", sezon_numeric + 1),
        barmode = "group",
        xaxis = list(title = "Drużyna"),
        yaxis = list(title = "Liczba bramek"),
        legend = list(x = 1.02, y = 1, xanchor = "left", yanchor = "top"),
        margin = list(r = 130)
      )
  })
  
  
  # 3. WIZUALIZACJA
  output$wykres_heatmapa = renderPlotly({
    
    req("licznosci" %in% input$statystyki, input$teams)
    
    sezon_numeric = as.numeric(input$sezon) 
    
    minuty = c('0-10', '11-20', '21-30', '31-40', '41-50', 
               '51-60', '61-70', '71-80', '81-90')
    
    gole_minutowe = zdarzenia_df %>%
      filter(team %in% input$teams, League == input$liga, SeasonID == sezon_numeric) %>%
      mutate(Minuta = as.character(Minuta)) %>%
      group_by(team, Minuta) %>%
      summarise(Gole = sum(Gole), .groups = "drop")
    
    pelna_siatka = expand.grid(
      team = unique(input$teams), Minuta = minuty, stringsAsFactors = FALSE
    )
    
    gole_minutowe = pelna_siatka %>%
      left_join(gole_minutowe, by = c("team", "Minuta")) %>%
      mutate(
        Gole = replace_na(Gole, 0),
        Minuta = factor(Minuta, levels = minuty)
      )
    
    plot_ly(
      data = gole_minutowe,  
      x = ~Minuta,
      y = ~team,
      z = ~Gole,
      type = "heatmap",
      colorscale = list(
        list(0.0, "#FFFFD9"), list(0.2, "#A1DAB4"), list(0.4, "#41B6C4"), 
        list(0.6, "#2C7FB8"), list(0.8, "#225EA8"), list(1.0, "#0C2C84")
      ),
      colorbar = list(len = 1),
      hoverinfo = "text",
      text = ~paste(Gole, "goli<br>Drużyna:", team, "<br>Minuta:", Minuta)
    ) %>%
      layout(
        title = paste("Rozkład goli wg minut –", input$liga, "sezon", sezon_numeric, "/", sezon_numeric + 1),
        xaxis = list(title = "Minuta"),
        yaxis = list(title = "Drużyna")
      )
  })
  
  
  output$sankey <- renderPlotly({
    
    transfers2 <- transfers2 %>%
      filter(transfer_season %in% input$sezony_sankey) %>%
      group_by(domestic_competition_id_from, domestic_competition_id_to) %>%
      summarise(count = n(), transfer_fees_sum = sum(transfer_fee))
    
    source <- nodes_map_from[transfers2$domestic_competition_id_from]
    target <- nodes_map_to[transfers2$domestic_competition_id_to]
    value <- transfers2[[input$dane_sankey]]
    
    fig <- plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(label = nodes_labels,
                  pad = 15,
                  thickness = 20,
                  line = list(color = "black", width = 0.5),
                  color = node_colors),
      link = list(source = source,
                  target = target, 
                  value = value)
    )
    
    fig
    
  })
  output$klub_ui <- renderUI({
    req(input$liga_pilkarze)
    
    klubydf <- players %>%
      filter(league %in% input$liga_pilkarze) %>%
      pull(club_name) %>%
      unique() %>%
      sort()
    
    checkboxGroupInput(
      inputId = "klub_pilkarze",
      label = "Wybierz klub(y):",
      choices = klubydf,
      selected = NULL
    )
  })
  
  # Wykres – top 5 piłkarzy
  output$top_players <- renderPlotly({
    req(input$liga_pilkarze, input$klub_pilkarze)
    
    dane <- players %>%
      filter(
        league %in% input$liga_pilkarze,
        club_name %in% input$klub_pilkarze,
        !is.na(market_value_in_eur)
      ) %>%
      group_by(name, club_name, league, position, height_in_cm, country_of_citizenship) %>%
      summarise(
        market_value_in_eur = max(market_value_in_eur, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(market_value_in_eur)) %>%
      head(5)
 
    plot_ly(
      data = dane,
      x = ~reorder(name, market_value_in_eur),
      y = ~market_value_in_eur,
      type = "bar",
      color = ~league,                # koloruj wg ligi
      colors = league_colors,         # użyj wcześniej zdefiniowanego wektora
      hovertext = ~paste0(
        "Klub: ", club_name,
        "<br>Wartość: €", format(market_value_in_eur, big.mark = ","),
        "<br>Pozycja: ", position,
        "<br>Wzrost: ", height_in_cm, " cm",
        "<br>Narodowość: ", country_of_citizenship
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Top 5 najcenniejszych piłkarzy",
        xaxis = list(
          title = "Piłkarz",
          categoryorder = "array",
          categoryarray = dane$name[order(-dane$market_value_in_eur)]
        ),
        yaxis = list(title = "Wartość (€)"),
        margin = list(r = 100),
        showlegend = TRUE              
      )
  })
  
}




app_ui <- navbarPage(
  
  title = "Analiza danych: piłka nożna",
  tabPanel("Statystyki", ui1),
  tabPanel("Transfery", ui2),
  tabPanel("Piłkarze", ui3),
  theme = bslib::bs_theme(bootswatch = "cosmo"),

  footer = shiny::HTML("
  <footer class='text-center text-sm-start' style='width:100%;'>
    <hr>
    <p class='text-center' style='font-size:12px;'>
      © 2025 Autorzy: Jan Pieczywok, Jan Wiśniewski, Kacper Wróbel
    </p>
  </footer>
  "),
  header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css")))

shinyApp(app_ui, server)
