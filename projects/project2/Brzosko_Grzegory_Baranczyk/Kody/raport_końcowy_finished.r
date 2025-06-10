library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(leaflet)
library(rnaturalearth)
library(sf)
library(htmltools)
library(shinyWidgets)
library(plotly)
library(tidyr)
#https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020?select=drivers.csv
#Wszystkie dane pochodzą z linku podanego wyżej
circuits <- read.csv("circuits.csv")
constructor_results <- read.csv("constructor_results.csv")
constructor_standings <- read.csv("constructor_standings.csv")
constructors <- read.csv("constructors.csv")
driver_standings <- read.csv("driver_standings.csv")
drivers <- read.csv("drivers.csv")
lap_times <- read.csv("lap_times.csv")
pit_stops <- read.csv("pit_stops.csv")
qualifying <- read.csv("qualifying.csv")
races <- read.csv("races.csv")
results <- read.csv("results.csv")
seasons <- read.csv("seasons.csv")
sprint_results <- read.csv("sprint_results.csv")
status <- read.csv("status.csv")

#ramki danych
constructorsv2 <- constructors %>%
  select(constructorId, constructorRef, name) %>%
  distinct()

#join constuctors_standings z races i potem z constructors
constructors_named <- constructor_standings %>%
  inner_join(races, by = "raceId") %>%
  select(raceId, constructorId, points, year, round) %>%
  left_join(constructorsv2, by = "constructorId")

results_extended <- results %>%
  left_join(races %>% select(raceId, year, circuitId), by = "raceId") %>% 
  left_join(drivers %>%
              mutate(driver_name = paste(forename, surname)) %>% 
              select(driverId, driver_name), by = "driverId") %>% 
  left_join(constructors %>% select(constructorId, constructor_name = name), by = "constructorId") %>% 
  left_join(circuits %>% select(circuitId, circuit_name = name), by = "circuitId") %>%
  left_join(status %>% select(statusId, status_name = status), by = "statusId")

results_clean_01 <- results_extended %>%
  mutate(
    position = na_if(position, "\\N"),
    position = as.numeric(position),
    grid = as.numeric(grid),
    diff = position - grid) %>%
  filter(!is.na(diff))

driver_stats_01 <- results_clean_01 %>%
  group_by(year, driver_name) %>%
  summarise(avg_diff = mean(diff, na.rm = TRUE), .groups = "drop") %>%
  mutate(type = "Driver")

constructor_stats_01 <- results_clean_01 %>%
  group_by(year, constructor_name) %>%
  summarise(avg_diff = mean(diff, na.rm = TRUE), .groups = "drop") %>%
  mutate(type = "Constructor")

all_stats_01 <- bind_rows(
  driver_stats_01 %>% rename(name = driver_name),
  constructor_stats_01 %>% rename(name = constructor_name))

dnf_by_race <- results_extended %>% 
  mutate(position = na_if(position, "\\N"),
         position = as.numeric(position),
         dnf = is.na(position)) %>% 
  group_by(raceId, year) %>%
  summarise(dnf_rate = mean(dnf), .groups = "drop")

dnf_by_year <- dnf_by_race %>%
  group_by(year) %>%
  summarise(
    avg_dnf_rate = mean(dnf_rate), .groups = "drop")

races_per_country_year <- races %>%
  left_join(circuits, by = "circuitId") %>%
  filter(!is.na(country)) %>%
  mutate(country = recode(country,
                          "UK" = "United Kingdom",
                          "USA" = "United States of America",
                          "United States" = "United States of America",
                          "UAE" = "United Arab Emirates",
                          "Korea" = "South Korea")) %>%
  group_by(country, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(year), country)

races_locations <- races %>%
  left_join(circuits, by = "circuitId") %>%
  filter(!is.na(country)) %>%
  mutate(country = recode(country,
                          "UK" = "United Kingdom",
                          "USA" = "United States of America",
                          "United States" = "United States of America",
                          "UAE" = "United Arab Emirates",
                          "Korea" = "South Korea")) %>%
  distinct(year, country, circuitRef, lat, lng, circuit_name = name.y)

world_sf <- ne_countries(scale = "small", returnclass = "sf")

df_1<-pit_stops %>% 
  left_join(drivers,by='driverId') %>% 
  left_join(races,by='raceId') %>% 
  left_join(results,by=c('raceId','driverId')) %>%
  left_join(constructors,by='constructorId') %>% 
  select(c(name.y,milliseconds.x,year)) %>% 
  mutate(seconds=milliseconds.x/1000) %>% 
  filter(seconds<=50) %>% 
  mutate(name.y=as.factor(name.y))

df_const<-constructor_standings %>% 
  left_join(races,by='raceId') %>% 
  left_join(constructors,by='constructorId')

kubica_id <- 9

kubica_results <- results %>%
  filter(driverId == kubica_id) %>%
  left_join(races, by = "raceId") %>%
  select(year, round, raceId, positionOrder, constructorId)

teammates <- results %>%
  filter(raceId %in% kubica_results$raceId) %>%
  left_join(races, by = "raceId") %>%
  filter(driverId != kubica_id) %>%
  left_join(kubica_results, by = c("raceId", "year", "round", "constructorId")) %>%
  filter(!is.na(positionOrder.y)) %>%
  select(year, round, raceId, positionOrder_teammate = positionOrder.x, 
         positionOrder_kubica = positionOrder.y, driverId)

teammates_mean <- teammates %>%
  group_by(year, driverId) %>%
  summarise(
    positionOrder_teammate = mean(positionOrder_teammate),
    positionOrder_kubica = mean(positionOrder_kubica),
    .groups = "drop")

#UI
ui1 <- fluidPage(
  titlePanel("Ogólne informacje"),
  
  
  fluidRow(
    column(4, 
           sidebarPanel(
             radioButtons("type", "Wybierz rodzaj mapy:",
                          choices = c("Kierowcy" = "drivers", "Konstruktorzy" = "constructors"),
                          inline = TRUE),
             
           ),div(
             style = "background-color:#f8f9fa; padding:10px; margin-top:20px; border-left:4px solid #e10600;",
             HTML("Mapa prezentuje ilu kierowców/konstruktorów z Formuły 1 pochodzi z danego państwa.
                    Wykres mówi nam jaka część kierowców średnio w danym roku nie ukańczało wyścigów.
                    W obu wizualizacjach po najechaniu kursorem poznamy dokładne wartości.
                   Klikając na państwa na mapie uzyskamy informację na temat zwycięzców wyścigów z danego państwa.")
           )
    ),
    column(8,
           
           uiOutput("map_title"),
           leafletOutput("map")
    )
  ),
  fluidRow(
    column(6,
           h3("Średni udział nieukończonych wyścigów przez kierowców")
    ),
    column(6)
    
  ),
  fluidRow(
    
    column(12,
           plot_ly(
             dnf_by_year,
             x = ~year,
             y = ~avg_dnf_rate,
             type = 'scatter',
             mode = 'lines+markers',
             marker = list(color = '#7a0930'),
             line = list(color = '#7a0930')) %>%
             layout(
               title = "",
               xaxis = list(title = "Rok"),
               yaxis = list(title = "Średni udział DNF", tickformat = ".0%", range = c(0, max(dnf_by_year$avg_dnf_rate) * 1.1))))
  )
)


ui2<-fluidPage(titlePanel("Omówienie sezonów"),
               fluidRow(
                 column(4,
                        sidebarPanel(
                          selectInput("rok", "Wybierz rok:",
                                      choices = unique(races_per_country_year$year),
                                      selected = 2024),
                          selectInput("statystyka", "Wybierz statystykę do wykresu:",
                                      choices = c("Total_Points", "Pole_Positions", "Best_Qualifying_Position",
                                                  "Laps_Led", "Fastest_Laps", "Podiums", "DNFs"),
                                      selected = "Total_Points"),
                        ),
                        div(
                          style = "background-color:#f8f9fa; padding:10px; margin-top:20px; border-left:4px solid #e10600;",
                          HTML("Mapa prezentuje liczbę torów w danych państwach w wybranym sezonie i ich lokalizacje.
                          Tabela zawiera podstawowe informacje na temat najlepszych kierowców z tego sezonu.
                          Po wybraniu statystyki, na dole strony pojawi się odpowiadający jej wykres.
                          Rok wybrany na tej stronie będzie dotyczył wykresów na kolejnej stronie.")
                        )),
                 
                 column(8,
                        uiOutput("map_title2"),
                        leafletOutput("map_tor"))
               ),
               fluidRow(
                 column(12,
                        dataTableOutput("table"),
                        plotlyOutput("stat_plot"))
               )
               
)

ui3<-fluidPage(titlePanel("Szczegóły sezonów"),
               #ROW1
               fluidRow(
                 column(8,
                        plotOutput("boxplot")
                 ),
                 column(4,
                        sidebarPanel(selectInput("year", "Wybierz rok pit stopów:", choices = unique(df_1$year),
                                                 selected = 2024)),
                        div(
                          style = "background-color:#f8f9fa; padding:10px; margin-top:20px; border-left:4px solid #e10600;",
                          HTML("Na tej stronie możemy dowiedzieć się więcej o wybranym sezonie.
                               Wykresy pokazują rozkład czasu pit-stopów, rozkład punktów runda po rundzie najlepszych zespołów 
                               oraz średnią różnice w zajętym miejscu a startowym.")
                        )),
                 
                 
               ),
               #ROW2
               fluidRow(
                 column(4,
                        sidebarPanel(
                          selectInput("top_n", "Wybierz liczbę najlepszych konstruktorów:", 
                                      choices = setNames(c(3, 5, 7), c("Top 3", "Top 5", "Top 7")),
                                      selected = 5),
                          sliderInput("round", "Wybierz rundę:", 
                                      min = 1, max = 22, value = 1, step = 1,
                                      #poniżej tempo animacji można dostosować
                                      animate = animationOptions(interval = 1000, loop = FALSE))
                        )),
                 column(8,
                        plotlyOutput("animacja_wykresu"))
               )
               ,
               fluidRow(
                 column(4,
                        sidebarPanel(
                          radioButtons("entity", "Typ:", choices = c("Driver", "Constructor"), inline = TRUE)
                        )),
                 column(8,
                        plotlyOutput("barPlot"))
               )
               
)


ui4<-fluidPage(titlePanel("Kariera Roberta Kubicy"),
               tags$img(
                 src = "https://s4.tvp.pl/images2/d/1/d/uid_d1d35b4007c244c5aef3edf2f71c624d_width_1280_play_0_pos_0_gs_0_height_720_robert-kubica-fot-getty.jpg"
               ),
               fluidRow(
                 column(4,
                        div(
                          style = "background-color:#f8f9fa; padding:10px; margin-top:20px; border-left:4px solid #e10600;",
                          HTML("Możemy zobaczyć statystyki jedynego polskiego kierowcy w Formule 1.
                               W wybranym sezonie pokazujemy zajęte przez niego miejsca 
                               oraz w latach kiedy jeździł porównanie jego wyników z jego kolegą z zespołu. ")
                        ), sidebarPanel(
                          selectInput("year_kubica", "Wybierz rok:", choices = c(2006,2007,2008,2009,2010,2019), selected = 2008)
                        )),
                 column(8,
                        plotOutput("kubicaPlot"))
               ),
               fluidRow(
                 column(6,
                        h3("Porównanie średnich miejsc Kubicy i kolegi z zespołu")
                 ),
                 column(6)
                 
               )
               ,
               fluidRow(
                 
                 plot_ly() %>%
                   add_trace(
                     data = teammates_mean,
                     x = ~year,
                     y = ~positionOrder_teammate,
                     type = 'scatter',
                     mode = 'lines+markers',
                     name = 'Średnie miejsce kolega z zespołu',
                     marker = list(color = 'royalblue'),
                     line = list(color = 'royalblue')
                   ) %>%
                   add_trace(
                     data = teammates_mean,
                     x = ~year,
                     y = ~positionOrder_kubica,
                     type = 'scatter',
                     mode = 'lines+markers',
                     name = 'Średnie miejsce Roberta Kubicy',
                     marker = list(color = '#e10600'),
                     line = list(color = '#e10600')
                   ) %>%
                   layout(
                     title = '',
                     xaxis = list(title = 'Rok'),
                     yaxis = list(title = 'Średnie miejsce (odwrocona oś)', autorange = "reversed"),
                     legend = list(x = 1, y = 1, xanchor = "right", yanchor = "top")
                   )
                 
               )
               
)

server <- function(input, output, session) {
  
  output$table <- renderDataTable({
    season_race_ids <- races %>% filter(year == input$rok) %>% pull(raceId)
    season_qualifying <- qualifying %>% filter(raceId %in% season_race_ids)
    
    # Liczba pole position
    pole_counts <- season_qualifying %>%
      filter(position == 1) %>%
      count(driverId, name = "Pole_Positions")
    
    # Najlepsza pozycja kwalifikacyjna
    best_qualifying <- season_qualifying %>%
      group_by(driverId) %>%
      summarise(Best_Qualifying_Position = min(position, na.rm = TRUE))
    
    season_standings <- driver_standings %>%
      filter(raceId %in% season_race_ids)
    
    last_race_id <- max(season_race_ids)
    
    total_points <- season_standings %>%
      filter(raceId == last_race_id) %>%
      select(driverId, Total_Points = points)
    season_laps <- lap_times %>%
      filter(raceId %in% season_race_ids)
    
    # Liderzy poszczególnych okrążeń (najmniejszy czas)
    lap_leaders <- season_laps %>%
      group_by(raceId, lap) %>%
      filter(milliseconds == min(milliseconds, na.rm = TRUE)) %>%
      ungroup()
    
    laps_led <- lap_leaders %>%
      count(driverId, name = "Laps_Led")
    season_results <- results %>%
      filter(raceId %in% season_race_ids & !is.na(fastestLapTime))
    
    # Najszybsze okrążenie w każdym wyścigu
    fastest_laps <- season_results %>%
      group_by(raceId) %>%
      filter(fastestLapTime == min(fastestLapTime, na.rm = TRUE)) %>%
      ungroup()
    
    fastest_lap_counts <- fastest_laps %>%
      count(driverId, name = "Fastest_Laps")
    
    active_driver_ids <- results %>%
      filter(raceId %in% season_race_ids) %>%
      distinct(driverId) %>%
      pull(driverId)
    
    results_joined <- results %>%
      left_join(status, by = "statusId")
    
    
    dnfs <- results_joined %>%
      filter(raceId %in% season_race_ids, status != "Finished") %>%
      count(driverId, name = "DNFs")
    
    podiums <- results %>%
      filter(raceId %in% season_race_ids, positionOrder %in% 1:3) %>%
      count(driverId, name = "Podiums")
    
    summary <- drivers %>%
      filter(driverId %in% active_driver_ids) %>%
      select(driverId, forename, surname) %>%
      mutate(Driver = paste(forename, surname)) %>%
      left_join(pole_counts, by = "driverId") %>%
      left_join(best_qualifying, by = "driverId") %>%
      left_join(total_points, by = "driverId") %>%
      left_join(laps_led, by = "driverId") %>%
      left_join(fastest_lap_counts, by = "driverId") %>%
      left_join(podiums, by = "driverId") %>%
      left_join(dnfs, by = "driverId") %>%
      select(Driver, Pole_Positions, Best_Qualifying_Position, Total_Points,
             Laps_Led, Fastest_Laps, Podiums, DNFs) %>%
      replace_na(list(
        Pole_Positions = 0,
        Best_Qualifying_Position = NA,
        Total_Points = 0,
        Laps_Led = 0,
        Fastest_Laps = 0,
        Podiums = 0,
        DNFs = 0
      )) %>%
      arrange(desc(Total_Points))
    
    
  })
  
  
  
  
  output$stat_plot <- renderPlotly({
    season_race_ids <- races %>% filter(year == input$rok) %>% pull(raceId)
    
    season_qualifying <- qualifying %>% filter(raceId %in% season_race_ids)
    pole_counts <- season_qualifying %>%
      filter(position == 1) %>%
      count(driverId, name = "Pole_Positions")
    best_qualifying <- season_qualifying %>%
      group_by(driverId) %>%
      summarise(Best_Qualifying_Position = min(position, na.rm = TRUE))
    season_standings <- driver_standings %>% filter(raceId %in% season_race_ids)
    last_race_id <- max(season_race_ids)
    total_points <- season_standings %>% filter(raceId == last_race_id) %>%
      select(driverId, Total_Points = points)
    season_laps <- lap_times %>% filter(raceId %in% season_race_ids)
    lap_leaders <- season_laps %>%
      group_by(raceId, lap) %>%
      filter(milliseconds == min(milliseconds, na.rm = TRUE)) %>%
      ungroup()
    laps_led <- lap_leaders %>% count(driverId, name = "Laps_Led")
    season_results <- results %>%
      filter(raceId %in% season_race_ids & !is.na(fastestLapTime))
    fastest_laps <- season_results %>%
      group_by(raceId) %>%
      filter(fastestLapTime == min(fastestLapTime, na.rm = TRUE)) %>%
      ungroup()
    fastest_lap_counts <- fastest_laps %>% count(driverId, name = "Fastest_Laps")
    active_driver_ids <- results %>%
      filter(raceId %in% season_race_ids) %>%
      distinct(driverId) %>% pull(driverId)
    results_joined <- results %>% left_join(status, by = "statusId")
    dnfs <- results_joined %>%
      filter(raceId %in% season_race_ids, status != "Finished") %>%
      count(driverId, name = "DNFs")
    podiums <- results %>%
      filter(raceId %in% season_race_ids, positionOrder %in% 1:3) %>%
      count(driverId, name = "Podiums")
    
    summary <- drivers %>%
      filter(driverId %in% active_driver_ids) %>%
      select(driverId, forename, surname) %>%
      mutate(Driver = paste(forename, surname)) %>%
      left_join(pole_counts, by = "driverId") %>%
      left_join(best_qualifying, by = "driverId") %>%
      left_join(total_points, by = "driverId") %>%
      left_join(laps_led, by = "driverId") %>%
      left_join(fastest_lap_counts, by = "driverId") %>%
      left_join(podiums, by = "driverId") %>%
      left_join(dnfs, by = "driverId") %>%
      select(Driver, Pole_Positions, Best_Qualifying_Position, Total_Points,
             Laps_Led, Fastest_Laps, Podiums, DNFs) %>%
      replace_na(list(
        Pole_Positions = 0,
        Best_Qualifying_Position = NA,
        Total_Points = 0,
        Laps_Led = 0,
        Fastest_Laps = 0,
        Podiums = 0,
        DNFs = 0
      ))
    
  #   stat_name <- input$statystyka
  #   
  #   plot_data <- summary %>%
  #     select(Driver, !!sym(stat_name)) %>%
  #     arrange(desc(!!sym(stat_name)))
  #   
  #   
  #   p <- ggplot(plot_data, aes(x = reorder(Driver, !!sym(stat_name)), y = !!sym(stat_name))) +
  #     geom_bar(stat = "identity", fill = "#d43f3a", width = 0.95) +
  #     coord_flip() +
  #     scale_y_continuous(expand = c(0, 0)) +
  #     labs(x = "", y = stat_name, title = paste(stat_name, "w sezonie", input$rok)) +
  #     theme_minimal()
  #   
  #   
  #   ggplotly(p)
  # })
  # 
  # 
  # 
  # 
  
  stat_name <- input$statystyka
  
  plot_data <- summary %>%
    select(Driver, !!sym(stat_name)) %>%
    arrange((!!sym(stat_name)))
  
  plot_data <- plot_data %>%
    mutate(Driver = factor(Driver, levels = rev(Driver)))
  
  n_drivers <- nrow(plot_data)
  
  plot_ly(
    data = plot_data,
    x = ~get(stat_name),
    y = ~Driver,
    type = 'bar',
    orientation = 'h',
    marker = list(color = '#d43f3a'),
    height = 25 * n_drivers
  ) %>%
    layout(
      title = paste(stat_name, "w sezonie", input$rok),
      xaxis = list(title = stat_name),
      yaxis = list(
        title = "",
        automargin = TRUE,
        tickmode = "array",
        tickvals = plot_data$Driver,
        ticktext = plot_data$Driver
      ),
      margin = list(l = 200),
      bargap = 0.2
    )
})
  
  
  
  
  
    
  output$barPlot <- renderPlotly({
    filtered <- all_stats_01 %>%
      filter(year == input$rok, type == input$entity) %>%
      arrange(avg_diff)
    filtered <- filtered %>%
      mutate(name = factor(name, levels = name))
    plot_ly(
      data = filtered,
      x = ~avg_diff,
      y = ~name,
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#ff365a'),
      height = ifelse(filtered$type == "Constructor", 30 * nrow(filtered), 25 * nrow(filtered))) %>%
      layout(
        title = paste("Średnia różnica pozycji -", input$entity, "w roku", input$rok),
        xaxis = list(title = "Średnia różnica pozycji (finish - start)"),
        yaxis = list(
          title = "",
          automargin = TRUE,
          tickmode = "array",
          tickvals = filtered$name,
          ticktext = filtered$name),
        margin = list(l = 200),
        bargap = 0.2)})
  
  output$map_tor <- renderLeaflet({
    data_year_country <- races_per_country_year %>%
      filter(year == input$rok)
    data_year_locations <- races_locations %>%
      filter(year == input$rok)
    world_joined <- world_sf %>%
      left_join(data_year_country, by = c("name" = "country")) %>%
      mutate(n = replace_na(n, 0),
             category = case_when(
               n == 0 ~ "Brak",
               n == 1 ~ "1 wyścig",
               n == 2 ~ "2 wyścigi",
               n == 3 ~ "3 wyścigi"),
             fillColor = case_when(
               category == "Brak" ~ "#dadada",
               category == "1 wyścig" ~ "#ff8fa3",
               category == "2 wyścigi" ~ "#ff4d6d",
               category == "3 wyścigi" ~ "#a4133c",
               TRUE ~ "#dadada"))
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = world_joined,
        fillColor = ~fillColor,
        weight = 0.5,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0(name, ": ", n, " wyścig(i)")
      ) %>%
      addCircleMarkers(
        data = data_year_locations,
        lng = ~lng, lat = ~lat,
        radius = 5,
        color = "#24051f",
        fillColor = "#590d22",
        fillOpacity = 0.9,
        weight = 1,
        popup = ~paste0("<strong>", circuit_name, "</strong><br>", country)
      ) %>%
      addLegend("bottomright",
                colors = c("#dadada", "#ff8fa1", "#ff4d6d", "#a4133c"),
                labels = c("Brak", "1 wyścig", "2 wyścigi", "3 wyścigi"),
                title = "Liczba wyścigów",
                opacity = 0.7)%>% 
      setView(lng=18.6435,lat=30.1282,zoom=1.5)})
  
  output$map_title <- renderUI({
    if (input$type == "drivers") {
      h3("Mapa narodowości kierowców")
    } else {
      h3("Mapa narodowości konstruktorów")
    }
  })
  output$map_title2 <- renderUI({
    h3(paste("Mapa lokalizacji torów w sezonie ",input$rok))
  })
  
  output$boxplot <- renderPlot({

    
    req(input$year)
    
    order<-as.vector(df_const %>% 
                       filter(year==input$year) %>% 
                       filter(round==max(round)) %>% 
                       arrange(position) %>% 
                       select(name.y))[[1]]
    df_filtered <- df_1 %>%
      filter(year == input$year) %>%
      group_by(name.y) %>%
      mutate(avg_duration = mean(seconds, na.rm = TRUE)) %>% 
      mutate(name = factor(name.y, levels = order))
    
    ggplot(df_filtered, aes(x = name, y = seconds)) +
      geom_boxplot(fill = "#ff8fa3", color = "#a4133c") +  
      coord_flip() +  
      theme_minimal(base_size = 14) +
      labs(
        title = paste0("Rozkład czasu pit stopu w sekundach w podziale na zespoły w sezonie ", input$year),
        subtitle = "Kolejność zespołów według klasyfikacji konstruktorów",
        x = "Zespół",
        y = "Długość pit stopu w sekundach"
      ) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(face = "bold")
      )
  })
  
  output$map <- renderLeaflet({
    
    
    nationality_to_country <- c(
      "British" = "United Kingdom",
      "German" = "Germany",
      "Spanish" = "Spain",
      "Finnish" = "Finland",
      "Japanese" = "Japan",
      "French" = "France",
      "Polish" = "Poland",
      "Brazilian" = "Brazil",
      "Italian" = "Italy",
      "Australian" = "Australia",
      "Austrian" = "Austria",
      "American" = "USA",
      "Dutch" = "Netherlands",
      "Colombian" = "Colombia",
      "Portuguese" = "Portugal",
      "Canadian" = "Canada",
      "Indian" = "India",
      "Hungarian" = "Hungary",
      "Irish" = "Ireland",
      "Danish" = "Denmark",
      "Argentine" = "Argentina",
      "Czech" = "Czech Republic",
      "Malaysian" = "Malaysia",
      "Swiss" = "Switzerland",
      "Belgian" = "Belgium",
      "Monegasque" = "Monaco",
      "Swedish" = "Sweden",
      "Venezuelan" = "Venezuela",
      "New Zealander" = "New Zealand",
      "Chilean" = "Chile",
      "Mexican" = "Mexico",
      "South African" = "South Africa",
      "Liechtensteiner" = "Liechtenstein",
      "Rhodesian" = "Zimbabwe",
      "American-Italian" = "United States of America",
      "Uruguayan" = "Uruguay",
      "Argentine-Italian" = "Argentina",
      "Thai" = "Thailand",
      "East German" = "Germany",
      "Russian" = "Russia",
      "Indonesian" = "Indonesia",
      "Chinese" = "China",
      "Argentinian " = "Argentina"
    )
    
    world_map_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      select(region = name, geometry)
    
    selected_df <- if (input$type == "drivers") drivers else constructors
    
    
    selected_df$country <- nationality_to_country[selected_df$nationality]
    
    count_df <- selected_df %>%
      count(country, name = "count")
    
    
    wins <- drivers %>%
      left_join(results, by = "driverId") %>%
      filter(positionOrder == 1) %>%
      group_by(forename, surname, nationality) %>%
      summarise(wins = n(), .groups = "drop")
    
    wins$country <- nationality_to_country[wins$nationality]
    
    
    top_winners <- wins %>%
      group_by(country) %>%
      arrange(desc(wins)) %>%
      slice_head(n = 3) %>%
      summarise(
        top5 = paste0(seq_along(wins), ". ", forename, " ", surname, " (", wins, " zwycięstw)", collapse = "<br/>"),
        .groups = "drop"
      )
    
    
    map_data_joined <- world_map_sf %>%
      left_join(count_df, by = c("region" = "country")) %>%
      left_join(top_winners, by = c("region" = "country"))
    
    max_drivers<-max(map_data_joined$count)
    
    palette_colors <- colorBin(
      palette = c("#fff5f0", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#a50f15", "#67000d"),
      domain = map_data_joined$count,
      bins = c(0, 3, 5, 15, 30, 50, 75, 100, 170),
      na.color = "grey"
    )
    
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Liczba: %s",
      map_data_joined$region,
      ifelse(is.na(map_data_joined$count), "0", map_data_joined$count)
    ) %>% lapply(htmltools::HTML)
    
    
    popups <- sprintf(
      "<strong>%s</strong><br/>Top 3 zwycięzców:<br/>%s",
      map_data_joined$region,
      ifelse(is.na(map_data_joined$top5), "Brak danych", map_data_joined$top5)
    )
    
    # Render leaflet map
    leaflet(map_data_joined) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~palette_colors(count),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        popup = popups,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = palette_colors,
        values = map_data_joined$count,
        title = if (input$type == "drivers") "Liczba kierowców" else "Liczba konstruktorów",
        position = "bottomright",
        na.label = "0"
      ) %>% 
      setView(lng=18.6435,lat=30.1282,zoom=1.5)
  })
  observeEvent(input$rok, {
    rundy <- sort(unique(races$round[races$year == input$rok]))
    updateSliderInput(session, "round", min = 1, max = max(rundy), value = 1)
  })
  
  output$animacja_wykresu <- renderPlotly({
    req(input$rok, input$round, input$top_n)
    
    #top iles konstr
    final_rank <- constructors_named %>%
      filter(year == input$rok) %>%
      group_by(constructorId, name) %>%
      summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_points)) %>%
      slice_head(n = as.integer(input$top_n))
    
    final_levels <- final_rank$name
    
    #tu się suma punktow zlicza
    df <- constructors_named %>%
      filter(year == input$rok, name %in% final_levels, round <= input$round) %>%
      group_by(name, round) %>%
      summarise(points = sum(points, na.rm = TRUE), .groups = "drop") %>%
      complete(name, round = 1:input$round, fill = list(points = 0)) %>%
      ungroup() %>%
      mutate(cumulative_points = points,
             name = factor(name, levels = final_levels))
    
    #zerowy punkt zeby nie od 1 oś x
    
    zero_points <- tibble(name = final_levels, round = 0, points = 0, cumulative_points = 0)
    
    df <- bind_rows(zero_points, df) %>%
      arrange(name, round) %>%
      mutate(name = factor(name, levels = final_levels))
    
    #tu można zmienić max y na klatkę
    
    max_y <- if (nrow(df) == 0) 1 else max(df$cumulative_points, na.rm = TRUE)
    max_round <- max(races$round[races$year == input$rok])
    
    #etykiety po najechaniu
    
    plot_ly(
      df,
      x = ~round,
      y = ~cumulative_points,
      color = ~name,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~paste("Runda:", round,
                    "<br>Konstruktor:", name,
                    "<br>Punkty:", cumulative_points),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = paste("Top", input$top_n, "konstruktorów - runda", input$round, "rok", input$rok),
        xaxis = list(title = "Runda", range = c(0, max_round)),
        yaxis = list(title = "Punkty w rundzie", range = c(0, max_y * 1.2)),
        legend = list(title = list(text = "Konstruktorzy"),
                      x = 1, y = 1,
                      traceorder = "normal")
      )
  })
  
  output$kubicaPlot <- renderPlot({
    race_ids <- races %>%
      filter(year == input$year_kubica) %>%
      pull(raceId)
    
    # Filter Kubica's results
    kubica_results <- results %>%
      filter(driverId == kubica_id, raceId %in% race_ids) %>%
      mutate(
        position_label = case_when(
          positionOrder %in% 1:10 ~ as.character(positionOrder),
          TRUE ~ ">10"
        )
      )
    
    # Count results per position label
    finish_counts <- kubica_results %>%
      count(position_label, name = "count")
    
    # Ensure all labels are present
    all_labels <- data.frame(position_label = c(as.character(1:10), ">10"))
    finish_counts <- all_labels %>%
      left_join(finish_counts, by = "position_label") %>%
      mutate(
        count = ifelse(is.na(count), 0, count),
        position_label = factor(position_label, levels = c(as.character(1:10), ">10"))
      )
    
    # Plot
    ggplot(finish_counts, aes(x = position_label, y = count)) +
      geom_bar(stat = "identity", fill = "#e10600") +
      labs(
        title = paste("Miejsca ukończenia wyścigów przez Roberta Kubicę w sezonie", input$year_kubica),
        x = "Miejsce ukończenia",
        y = "Liczba Wyścigów"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, hjust = 0.5,face= "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = "bold"), 
        axis.text.y = element_text(size = 12, face = "bold") 
      )
  })
  
}


app_ui <- navbarPage(
  title = "Analiza danych: F1",
  tabPanel("Ogólne informacje", ui1),
  tabPanel("Omówienie sezonów", ui2,
           icon = icon("database")
  ),
  tabPanel("Szczegóły sezonów", ui3,
           icon = icon("search",lib="glyphicon")
  ),
  tabPanel("Robert Kubica", ui4,
           icon = icon("star",lib="glyphicon")
  ),
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "lux",        
    primary = "#e10600",
    base_font = bslib::font_google("Roboto Condensed")  
  ),
  tags$style(HTML("
  .navbar {
    background-color: #ffffff !important; 
    border-bottom: 2px solid #e10600;
  }
  .navbar-brand {
    font-family: 'Roboto Condensed', sans-serif;
    font-size: 1.8rem;
    font-weight: 700;
    color: #e10600 !important; 
    text-transform: uppercase;
  }


  .nav-link {
    color: #e10600 !important;       
    font-weight: 600;
    font-family: 'Roboto Condensed', sans-serif;
    text-transform: uppercase;
  }

  .nav-link.active {
    color: #000000 !important;       /* Black for contrast */
    border-bottom: 2px solid #e10600;
    background-color: transparent !important;
  }

  /* On hover */
  .nav-link:hover {
    color: #b00000 !important;
  }
  
  h1, h2, h3 {
    color: #e10600 !important;
    font-weight: 700;
    font-family: 'Roboto Condensed', sans-serif;
    text-transform: uppercase;
    display: inline-block;
    border-bottom: 3px solid #e10600;
    padding-bottom: 5px;
    margin-bottom: 15px;
  }
  
  .irs-bar, .irs-bar-edge, .irs-single {
      background-color: #e10600 !important;
      border-color: #e10600 !important;
    }
    .irs-from, .irs-to, .irs-single {
      background-color: #e10600 !important;
    }
    .irs-slider {
      background: #e10600 !important;
    }

"))
  
  )

shinyApp(app_ui, server)

