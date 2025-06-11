library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(tibble)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(bsicons)
library(bslib)


#dog_adoption <- read.csv("/Users/tlenekksenonu/Downloads/animal-data-1.csv")
#dog_breeds <- read.csv("/Users/tlenekksenonu/Downloads/Dog-Breads-Around-The-World.csv")
dog_adoption <- read.csv("eksploracja danych projekt 2/animal-data-1.csv")
dog_breeds <- read.csv("eksploracja danych projekt 2/Dog Breads Around The World.csv")


dog_adoption <- dog_adoption %>% separate(breedname, into = c("breedname1", "breedname2"), sep = "/", fill = "right")

dog_adoption$breedname1 <- recode(dog_adoption$breedname1,
                                  "German Shepherd Dog" = "German Shepherd",
                                  "Heeler" = "Australian Cattle Dog",
                                  "Poodle, Miniature" = "Poodle (Miniature)",
                                  "Pitbull" = "Staffordshire Bull Terrier",
                                  "American Staffordshire Terrier" = "Staffordshire Bull Terrier",
                                  "Pit Bull Terrier" = "Staffordshire Bull Terrier",
                                  "Coonhound" = "Redbone Coonhound",
                                  "Bluetick Coonhound" = "Redbone Coonhound",
                                  "Schnauzer, Miniature" = "Miniature Schnauzer",
                                  "Schnauzer, Standard" = "Standard Schnauzer",
                                  "Husky" = "Siberian Husky",
                                  "Poodle" = "Poodle (Standard)",
                                  "Poodle, Toy" = "Poodle (Toy)",
                                  "Cattle Dog" = "Australian Cattle Dog",
                                  "Walker Hound" = "Treeing Walker Coonhound"
)


dog_adoption_filter <- dog_adoption %>%
  left_join(dog_breeds, by = join_by("breedname1" == "Name")) %>%
  filter(speciesname == "Dog", movementtype == "Adoption", !is.na(Origin)) %>%
  slice(-1) %>% # This slice(-1) seems arbitrary, consider if truly needed.
  mutate(
    intake_date = ymd_hms(intakedate),
    movement_date = ymd_hms(movementdate)
  ) %>%
  mutate(
    days_in_shelter = as.numeric(difftime(movement_date, intake_date, units = "days")),
    months = str_extract(animalage, "\\d+(?=\\s*months?)"),
    years = str_extract(animalage, "\\d+(?=\\s*years?)"),
    years = as.numeric(years),
    months = as.numeric(months),
    total_months = coalesce(years, 0) * 12 + coalesce(months, 0)
  ) %>%
  filter(days_in_shelter > 0, total_months > 0) %>% # Filter out non-positive days/months
  mutate(total_years = total_months / 12) %>%
  
  rename(
    `Friendly Rating (1-10)` = Friendly.Rating..1.10.,
    `Life Span` = Life.Span,
    `Exercise Requirements (hrs/day)` = Exercise.Requirements..hrs.day.,
    `Intelligence Rating (1-10)` = Intelligence.Rating..1.10.,
    `Shedding Level` = Shedding.Level,
    `Health Issues Risk` = Health.Issues.Risk,
    `Training Difficulty (1-10)` = Training.Difficulty..1.10.,
    `Average Weight (kg)` = Average.Weight..kg.,
    `Grooming Needs` = Grooming.Needs
  )


dog_adoption_filter <- dog_adoption_filter %>%
  mutate(
    days_range = cut(
      days_in_shelter,
      breaks = seq(0, max(days_in_shelter, na.rm = TRUE) + 150, by = 150), # Ensure upper bound covers all data
      include.lowest = TRUE,
      right = FALSE,
      labels = paste0(seq(0, max(seq(0, max(days_in_shelter, na.rm = TRUE) + 150, by = 150)) - 150, 150), "-", seq(150, max(seq(0, max(days_in_shelter, na.rm = TRUE) + 150, by = 150)), 150))
    ),
    age_range = cut(
      total_years,
      breaks = seq(0, max(total_years, na.rm = TRUE) + 1, by = 1), # Breaks up to max year + 1
      include.lowest = TRUE,
      right = FALSE,
      labels = paste0("[", seq(0, max(seq(0, max(total_years, na.rm = TRUE) + 1, by = 1)) - 1, 1), "-", seq(1, max(seq(0, max(total_years, na.rm = TRUE) + 1, by = 1)), 1), ")")
    )
  )
dog_adoption_filter$`Average Weight (kg)` <- as.numeric(as.character(dog_adoption_filter$`Average Weight (kg)`))


top_breeds <- dog_adoption_filter %>%
  count(breedname1, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(breedname1)
dogs_top_breeds <- dog_adoption_filter %>% filter(breedname1 %in% top_breeds)


country_breeds <- dog_breeds %>%
  group_by(Origin) %>%
  summarise(breeds = paste(Name, collapse = ", "))

country_breeds$iso_a3 <- countrycode(country_breeds$Origin, "country.name", "iso3c")
world <- ne_countries(scale = "medium", returnclass = "sf")
world_dogs <- left_join(world, country_breeds, by = "iso_a3")


categories <- c(
  "Friendly.Rating..1.10.", "Life.Span", "Size", "Grooming.Needs",
  "Exercise.Requirements..hrs.day.", "Good.with.Children",
  "Intelligence.Rating..1.10.", "Shedding.Level",
  "Health.Issues.Risk", "Training.Difficulty..1.10."
)

dogs_top_breeds_2 <- dog_breeds %>%
  filter(Name != "Xoloitzcuintli") %>%
  mutate(
    Size = case_when(
      Size == "Small" ~ 2,
      Size == "Small-Medium" ~ 4,
      Size == "Medium" ~ 6,
      Size == "Large" ~ 8,
      Size == "Gigant" ~ 10,
      TRUE ~ NA_real_
    ),
    `Friendly.Rating..1.10.` = as.numeric(`Friendly.Rating..1.10.`),
    `Life.Span` = as.numeric(`Life.Span`),
    `Grooming.Needs` = case_when(
      `Grooming.Needs` == "Low" ~ 2,
      `Grooming.Needs` == "Moderate" ~ 6,
      `Grooming.Needs` == "High" ~ 10,
      TRUE ~ NA_real_
    ),
    `Exercise.Requirements..hrs.day.` = as.numeric(`Exercise.Requirements..hrs.day.`),
    `Good.with.Children` = case_when(
      `Good.with.Children` == "Yes" ~ 10,
      `Good.with.Children` == "No" ~ 1,
      TRUE ~ 5
    ),
    `Intelligence.Rating..1.10.` = as.numeric(`Intelligence.Rating..1.10.`),
    `Shedding.Level` = case_when(
      `Shedding.Level` == "Low" ~ 2,
      `Shedding.Level` == "Moderate" ~ 4,
      `Shedding.Level` == "High" ~ 8,
      TRUE ~ 10 # Assuming very high or unknown
    ),
    `Health.Issues.Risk` = case_when(
      `Health.Issues.Risk` == "Low" ~ 1,
      `Health.Issues.Risk` == "Moderate" ~ 5,
      `Health.Issues.Risk` == "High" ~ 10,
      TRUE ~ NA_real_
    ),
    `Average.Weight..kg.` = as.numeric(`Average.Weight..kg.`),
    `Training.Difficulty..1.10.` = as.numeric(`Training.Difficulty..1.10.`)
  )


feature_choices_plot1 <- c(
  "Płeć" = "sexname",
  "Dzienna aktywność fizyczna [h]" = "Exercise Requirements (hrs/day)",
  "Wielkość" = "Size",
  "Ryzyko zachorowań" = "Health Issues Risk",
  "Przyczyna pobytu w schronisku" = "intakereason",
  "Wiek [lata]" = "total_years",
  "Schronisko" = "location",
  "Typ" = "Type",
  "Długość życia [lata]" = "Life Span",
  "Potrzeby pielęgnacyjne" = "Grooming Needs",
  "Waga [kg]" = "Average Weight (kg)",
  "Poziom wypadania sierści" = "Shedding Level",
  "Przyjazny dla dzieci" = "Good.with.Children" # This column comes from dog_breeds join
)

feature_choices_plot2 <- c(
  "Trenowalność" = "Training Difficulty (1-10)",
  "Stosunek do ludzi" = "Friendly Rating (1-10)",
  "Poziom inteligencji" = "Intelligence Rating (1-10)"
)


means <- c(nrow(dog_breeds), dog_adoption %>% filter(speciesname == "Dog", movementtype == "Adoption") %>% summarise(n = n_distinct(id)) %>% pull(n), length(categories))

# --- UI ---
ui1 <- fluidPage( # <-- Tutaj otaczamy cały układ funkcją fluidPage()
  #theme = bs_theme(bootswatch = "flatly",
   ##                base_font = font_google("Inter"),
     #              navbar_bg = "#25443B"), # <-- I tutaj przekazujemy motyw
  
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Inter"), # Kontynuujemy z "Inter" - czytelna i nowoczesna czcionka
    "navbar-bg" = "#607d8b", # Głęboki, stonowany szaro-niebieski, przypominający kolor sierści
    "primary" = "#8BC34A",   # Akcent zieleni, nawiązujący do trawy, natury
    "secondary" = "#FFC107", # Ciepły, słoneczny żółty, dodający pozytywnej energii
    "success" = "#4CAF50",   # Standardowy zielony dla sukcesów
    "info" = "#2196F3",      # Standardowy niebieski dla informacji
    "warning" = "#FF9800",   # Standardowy pomarańczowy dla ostrzeżeń
    "danger" = "#F44336"     # Standardowy czerwony dla zagrożeń
  ),
  
  layout_column_wrap(
  width = "100%",
  
  layout_columns(
    col_widths = c(4, 8),
    h2("📊 Psy w schronisku w Bloomington", style = "margin-top: 10px; margin-bottom: 10px;"),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Rasy", value = scales::unit_format(unit = "")(means[[1]]), showcase = bsicons::bs_icon("heart-fill")),
      value_box(title = "Adopcje", value = scales::unit_format(unit = "")(means[[2]]), showcase = bsicons::bs_icon("house-fill")),
      value_box(title = "Cechy", value = scales::unit_format(unit = "")(means[[3]]), showcase = bsicons::bs_icon("check"))
    ),
    tags$style(HTML("
      .bslib-value-box {
        padding: 10px;
        font-size: 14px;
      }
    "))
  ),
  layout_columns(
    col_widths = c(6, 6),
    
    card(
      card_header("📈 Liczba adopcji w czasie"),
      plotOutput("sumadoptionPlot", height = "300px"),
      p("Z wykresu wynika, że liczba adopcji wzrosła z 2017 do 2018 roku, po czym w 2019 roku znacząco spadła.")
    ),
    
    card(
      card_header("📋 Top 6 ras"),
      tableOutput("tabela"),
      p('Najdłużej w schronisku przebywały Staffordshire Bull Terriery, najkrócej (niemalże dwa razy krócej) German Shepherdy.')
    )
  )
))

ui2 <- page_sidebar(
  theme = bs_theme(
    bootswatch = "flatly",
    navbar_bg = "#607d8b",
    base_font = font_google("Inter"), # Kontynuujemy z "Inter" - czytelna i nowoczesna czcionka
    "navbar-bg" = "#607d8b", # Głęboki, stonowany szaro-niebieski, przypominający kolor sierści
    "primary" = "#8BC34A",   # Akcent zieleni, nawiązujący do trawy, natury
    "secondary" = "#FFC107", # Ciepły, słoneczny żółty, dodający pozytywnej energii
    "success" = "#4CAF50",   # Standardowy zielony dla sukcesów
    "info" = "#2196F3",      # Standardowy niebieski dla informacji
    "warning" = "#FF9800",   # Standardowy pomarańczowy dla ostrzeżeń
    "danger" = "#F44336"     # Standardowy czerwony dla zagrożeń
  ),
  title = "Wpływ wybranego czynnika na długość pobytu psa w schronisku.",
  sidebar = sidebar(
    title = "Wybierz czynniki",
    width = "200px",
    class = "custom-sidebar-style",
    selectInput("factor", "Wybierz czynnik:", choices = feature_choices_plot1, selected = "sexname", width = "100%"),
    selectInput("factor2", "Wybierz drugi czynnik:", choices = feature_choices_plot2, selected = "`Intelligence Rating (1-10)`", width = "100%")
  ),
  tags$head(
    tags$style(HTML("
      /* Styl dla custom-sidebar-style */
      .bslib-sidebar-layout > .custom-sidebar-style {
        background-color: #f0f8ff; /* Delikatny, bardzo jasny niebieski */
        border-right: 2px solid #8BC34A; /* Zielona ramka po prawej */
        box-shadow: 2px 0 5px rgba(0,0,0,0.1); /* Delikatny cień */
        padding: 20px; /* Zwiększony padding wewnętrzny */
        border-radius: 0 15px 15px 0; /* Zaokrąglony prawy górny i dolny róg */
      }

      /* Styl dla nagłówka sidebara, jeśli chcesz go zmienić */
      .bslib-sidebar-layout > .custom-sidebar-style .card-header {
        background-color: #8BC34A; /* Nagłówek w kolorze primary */
        color: white; /* Biały tekst nagłówka */
        font-weight: bold;
        border-radius: 0; /* Usuń zaokrąglenie, jeśli jest domyślne */
      }

      /* Styl dla ogólnych inputów w sidebarze (opcjonalnie) */
      .custom-sidebar-style .form-control {
        border-color: #8BC34A; /* Ramki inputów w kolorze primary */
      }
    "))),
  layout_columns(
    col_widths = c(6, 6),
    plotlyOutput("interactivePlot"), # Changed from interactivePlot1
    plotlyOutput("interactivePlot2")
  ),
  br(), br(),
  p('Przy wyborze czynników "Płeć" oraz "Trenowalność" z wykresów wynika, że średni czas pobytu psów nieznacznie różni się w zależności od płci, ale różnica nie jest duża.
      Ponadto można zaobserwować, że długość pobytu zmienia się w zależności od wieku psa i poziomu trudności szkolenia. 
      Psy trudniejsze w szkoleniu (kolor czerwony) mają tendencję do dłuższego pobytu w schronisku, zwłaszcza w młodym wieku (0–1 lat). Podobną analizę, bazując na zawartości
      wykresów, można przeprowadzić dla dowolnie wybranych cech.'),
      
 br(),     
    p('Zaskakującym wnioskiem może okazać się fakt, że ryzyko zachorowań danej rasy nie stanowi istotnego kryterium w kontekście jego pobytu w schronisku. Co ciekawsze - zdarza się,
    iż mimo niskiego ryzyka zachorowań czas pobytu w schronisku jest względnie długi. 
    Kolejną ciekawą obserwacją jest zależność między wiekiem a długością pobytu - psy w okolicy wieku 11 - 13 lat średnio przebywają w schronisku krócej, niż te w wieku około 9 - 10 lat.')
)


ui3 <- page_sidebar(
  theme = bs_theme(
    bootswatch = "flatly",
    navbar_bg = "#607d8b",
    base_font = font_google("Inter"), # Kontynuujemy z "Inter" - czytelna i nowoczesna czcionka
    "navbar-bg" = "#607d8b", # Głęboki, stonowany szaro-niebieski, przypominający kolor sierści
    "primary" = "#8BC34A",   # Akcent zieleni, nawiązujący do trawy, natury
    "secondary" = "#FFC107", # Ciepły, słoneczny żółty, dodający pozytywnej energii
    "success" = "#4CAF50",   # Standardowy zielony dla sukcesów
    "info" = "#2196F3",      # Standardowy niebieski dla informacji
    "warning" = "#FF9800",   # Standardowy pomarańczowy dla ostrzeżeń
    "danger" = "#F44336"     # Standardowy czerwony dla zagrożeń
  ),
  titlePanel("Pojedynek ras psów"),
  sidebar = sidebar(
    width = "200px",
    class = "custom-sidebar-style",
    selectInput("breed1", "Wybierz pierwszą rasę:",
                choices = dogs_top_breeds_2$Name,
                selected = dogs_top_breeds_2$Name[1],
                width = "100%"),
    selectInput("breed2", "Wybierz drugą rasę:",
                choices = dogs_top_breeds_2$Name,
                selected = dogs_top_breeds_2$Name[2],
                width = "100%")
  ),
  tags$head(
    tags$style(HTML("
    /* Zwiększ odstępy między elementami formy w sidebarze dla lepszej czytelności */
    .form-group {
      margin-bottom: 1rem; /* Standardowy odstęp Bootstrapa, można zwiększyć */
    }

    /* Dopasowanie styli dla radioButtons i checkboxes */
    .form-check-input:checked {
      background-color: var(--bs-primary); /* Używa primary_color z motywu */
      border-color: var(--bs-primary);
    }
    .form-check-input {
        border-radius: 0.25rem; /* Delikatne zaokrąglenie dla checkboxów i radio */
    }

    /* Zwiększenie czcionki dla etykiet inputów */
    .form-label, .control-label {
      font-size: 0.95rem; /* Nieco większa czcionka dla etykiet */
      font-weight: 600; /* Nieco pogrubione */
      color: var(--bs-body-color); /* Kolor tekstu z motywu */
      margin-bottom: 0.3rem; /* Zmniejsz odstęp pod etykietą */
    }

    /* Styl dla rozwijanych list (selectInput) */
    .form-select {
        border-color: var(--bs-border-color); /* Domyślny kolor ramki Bootstrapa */
        box-shadow: none; /* Usuń domyślny box-shadow przy focusie, jeśli nie pasuje */
    }
    .form-select:focus {
        border-color: var(--bs-primary); /* Kolor primary przy focusie */
        box-shadow: 0 0 0 0.25rem rgba(var(--bs-primary-rgb), .25); /* Delikatny cień focusu */
    }

    /* Styl dla sliderInput (może wymagać więcej zabawy) */
    .irs-bar, .irs-bar--single {
        background-color: var(--bs-primary); /* Kolor paska slidera */
        border-top: 1px solid var(--bs-primary);
        border-bottom: 1px solid var(--bs-primary);
    }
    .irs-from, .irs-to, .irs-single {
        background-color: var(--bs-primary); /* Kolor wartości na sliderze */
    }
    .irs-grid-text {
      color: var(--bs-secondary); /* Kolor tekstu siatki slidera */
    }
  "))
  ),
  layout_columns(
    col_widths = c(6, 6),
    
    
    div(
      plotOutput("mirrorBarPlot"),
      br(),  
      p("Przy wyborze ras Afghan Hound oraz Affenpinscher możemy zaobserwować, że Afghan Hound dłużej żyje, jest większego rozmiaru i ma większe potrzeby pielęgnacyjne,
        natomiast Affenpinscher jest łatwiejszy w szkoleniu i wymaga mniej ruchu. Podobną analizę na podstawie wykresu można przeprowadzić dla dowolnie wybranych ras.",
        style = "margin-top: 20px;")
    ),
    
    
    div(
      leafletOutput("dog_map"),
      br(),  
      p("Mapa pozwala na zdobycie informacji na temat pochodzenia ras – np. Labrador Retriever pochodzi z Kanady (Newfoundland). Może to służyć jako kontekst kulturowy lub historyczny,
        przydatny przy analizie cech danej rasy.",
        style = "margin-top: 20px;")
    )
  )
)



# --- Server ---

server <- function(input, output) {
  
  # Definicja kolorów zgodnych z motywem bslib
  primary_color <- "#8BC34A"   # Zieleń (dla słupków, linii, punktów)
  secondary_color <- "#FFC107" # Ciepły żółty (dla akcentów, tła, lub w heatmapie)
  # Standardowe kolory dla mirrorBarPlot, które pasują do reszty, ale są kontrastowe
  breed1_color <- "#E74C3C" # Czerwony (mocny kontrast, ale pasuje do palety)
  breed2_color <- "#3498DB" # Niebieski (mocny kontrast)
  
  output$sumadoptionPlot <- renderPlot({
    dog_adoption_filter %>%
      mutate(adoption.year = year(movement_date)) %>%
      filter(adoption.year > 2016) %>%
      group_by(adoption.year) %>% summarise(n = n()) %>%
      ggplot(aes(adoption.year, n)) +
      geom_col(fill = primary_color) + # Zmieniono fill na primary_color
      labs(title = "Liczba adoptowanych psów ze schroniska w Bloomington",
           subtitle = "2017 - 2019",
           x = "Rok",
           y = "Liczba") +
      theme_minimal() +
      scale_y_continuous(expand = c(0,0))
  })
  
  output$dog_map <- renderLeaflet({
    world_dogs$has_data <- !is.na(world_dogs$breeds)
    
    leaflet(world_dogs) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ifelse(has_data, primary_color, "#dddddd"), # Zmieniono kolor na primary_color
        weight = 1,
        color = "white",
        fillOpacity = ~ifelse(has_data, 0.8, 0.2),
        label = ~paste0(Origin, ": ", ifelse(is.na(breeds), "Brak danych", breeds)),
        highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)
      )
  })
  
  output$tabela <- renderTable({
    dog_adoption_filter %>%
      filter(breedname1 %in% top_breeds) %>%
      group_by(breedname1) %>%
      summarise(Liczba = n(), mean = mean(days_in_shelter, na.rm = TRUE)) %>%
      arrange(-mean) %>%
      rename(
        "Średnia długość pobytu w schroniku w dniach" = "mean",
        "Rasa" = "breedname1",
        "Liczba psów" = "Liczba"
      )
  })
  
  output$interactivePlot <- renderPlotly({
    factor_raw <- input$factor
    
    if (factor_raw %in% c("total_years",  "`Life Span`", "`Exercise Requirements (hrs/day)`")) {
      
      variable <- sym(factor_raw)
      
      plot_data <- dog_adoption_filter %>%
        mutate(grupa = cut(as.numeric(!!variable), breaks = 20)) %>%  
        group_by(grupa) %>%
        summarise(
          srednia_days = mean(days_in_shelter, na.rm = TRUE),
          srodek = mean(as.numeric(!!variable), na.rm = TRUE),
          .groups = "drop"
        )
      
      p <- ggplot(plot_data, aes(x = srodek, y = srednia_days)) +
        geom_point(color = primary_color, size = 2) +
        geom_line(color = primary_color, linewidth = 1) +
        scale_y_continuous(
          expand = expansion(),
          limits = c(min(plot_data$srednia_days, 0), max(plot_data$srednia_days))
        ) +
        labs(
          x = names(feature_choices_plot1)[feature_choices_plot1 == factor_raw],
          y = "Średnia długość pobytu w dniach",
          title = paste("Średnia długość pobytu psa vs", names(feature_choices_plot1)[feature_choices_plot1 == factor_raw])
        ) +
        theme_minimal()
      
    }else if (factor_raw == "Average Weight (kg)") {
      
      pom <- dog_adoption_filter %>%
        filter(!is.na(.data[[factor_raw]])) %>%
        mutate(waga_bin = cut(.data[[factor_raw]], breaks = 20)) %>%
        group_by(waga_bin) %>%
        summarise(
          srednia = mean(days_in_shelter, na.rm = TRUE),
          srodek = mean(.data[[factor_raw]], na.rm = TRUE),  
          .groups = "drop"
        )
      
      p <- ggplot(pom, aes(x = srodek, y = srednia)) +
        geom_line(color = primary_color, linewidth = 1) +
        geom_point(color = primary_color, size = 2) +
        labs(
          x = names(feature_choices_plot1)[feature_choices_plot1 == factor_raw],#factor_labels[[factor_raw]],
          y = "Średnia długość pobytu psów tej grupy",
          title = paste("Średnia długość pobytu psa vs", names(feature_choices_plot1)[feature_choices_plot1 == factor_raw])
        ) +
        theme_minimal()
    }
    else if (factor_raw %in% c("intakereason", "location", "Good.with.Children", "sexname", "Type")) {
      
      plot_data <- dog_adoption_filter %>%
        group_by(!!sym(factor_raw)) %>%
        summarise(srednia_days = mean(days_in_shelter, na.rm = TRUE), .groups = "drop") %>%
        arrange(srednia_days) %>%
        mutate(!!sym(factor_raw) := factor(!!sym(factor_raw), levels = !!sym(factor_raw)))  # KLUCZOWE!
      
      p <- ggplot(plot_data, aes(y = !!sym(factor_raw), x = srednia_days)) +
        geom_col(fill = primary_color) +
        labs(
          y = names(feature_choices_plot1)[feature_choices_plot1 == factor_raw],
          x = "Średnia długość pobytu w dniach",
          title = paste("Średnia długość pobytu psa vs", names(feature_choices_plot1)[feature_choices_plot1 == factor_raw])
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(angle = 0, hjust = 1),
          axis.ticks.y = element_blank()
        ) +
        scale_x_continuous(expand = c(0, 0))
    }else {
      
      p <- ggplot(dog_adoption_filter, aes(x = factor(!!sym(factor_raw)), y = days_in_shelter)) +
        geom_violin(fill = primary_color, trim = FALSE) +
        labs(
          x = names(feature_choices_plot1)[feature_choices_plot1 == factor_raw],
          y = "Długość pobytu w dniach",
          title = paste("Długość pobytu psa vs", names(feature_choices_plot1)[feature_choices_plot1 == factor_raw])
        ) +
        theme_minimal()
      
      
    }
    ggplotly(p)
  })
  
  output$interactivePlot2 <- renderPlotly({
    factor_raw2 <- input$factor2
    
    p2 <- ggplot(dog_adoption_filter, aes(x = age_range, y = days_range, fill = !!sym(factor_raw2))) +
      geom_tile() +
      scale_fill_gradient(
        low = "#A7D945",  
        high = "#4A8B28"  
      )  +
      labs(
        x = "Przedział wiekowy [lata]",
        y = "Długość pobytu",
        title = paste("Długość pobytu psa w zależności od wieku i ", names(feature_choices_plot2)[feature_choices_plot2 == factor_raw2])
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p2)
  })
  
  output$mirrorBarPlot <- renderPlot({
    b1 <- dogs_top_breeds_2 %>% filter(Name == input$breed1)
    b2 <- dogs_top_breeds_2 %>% filter(Name == input$breed2)
    
    selected_categories <- c(
      "Friendly.Rating..1.10.", "Life.Span", "Size", "Grooming.Needs",
      "Exercise.Requirements..hrs.day.", "Good.with.Children",
      "Intelligence.Rating..1.10.", "Shedding.Level",
      "Health.Issues.Risk", "Training.Difficulty..1.10."
    )
    
    df <- tibble(
      Category = selected_categories,
      `Rasa 1` = -as.numeric(b1[1, selected_categories]),
      `Rasa 2` = as.numeric(b2[1, selected_categories])
    ) %>%
      pivot_longer(cols = c(`Rasa 1`, `Rasa 2`), names_to = "Rasa", values_to = "Wartość") %>%
      mutate(
        Category = recode(Category,
                          "Friendly.Rating..1.10." = "Stosunek do ludzi",
                          "Life.Span" = "Długość życia",
                          "Size" = "Wielkość",
                          "Grooming.Needs" = "Potrzeby pielęgnacyjne",
                          "Exercise.Requirements..hrs.day." = "Dzienna aktywność fizyczna [h]",
                          "Good.with.Children" = "Przyjazny dla dzieci",
                          "Intelligence.Rating..1.10." = "Inteligencja",
                          "Shedding.Level" = "Poziom wypadania sierści",
                          "Health.Issues.Risk" = "Ryzyko zachorowań",
                          "Training.Difficulty..1.10." = "Trenowalność"
        ),
        Category = factor(Category, levels = rev(c(
          "Stosunek do ludzi", "Długość życia", "Wielkość", "Potrzeby pielęgnacyjne",
          "Dzienna aktywność fizyczna [h]", "Przyjazny dla dzieci",
          "Inteligencja", "Poziom wypadania sierści", "Ryzyko zachorowań", "Trenowalność"
        )))
      )
    
    ggplot(df, aes(x = Wartość, y = Category, fill = Rasa)) +
      geom_col(width = 0.6, position = "identity") +
      scale_x_continuous(labels = abs, limits = c(-15, 15)) +
      labs(title = paste(input$breed1, "vs", input$breed2),
           x = "Wartość (1-10)", y = "Kategoria") +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("Rasa 2" = "#4A8B28", "Rasa 1" = "#A7D945")) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5)
      )
  })
}

app_ui <- navbarPage(
  title = "Analiza danych: psy w schronisku",
  tabPanel("Wstęp", ui1),
  tabPanel("Czynniki wpływające na adopcje", ui2),
  tabPanel("Dokładna analiza ras psów", ui3)
)

shinyApp(app_ui, server)

