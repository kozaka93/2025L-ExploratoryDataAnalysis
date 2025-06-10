library(shiny)
library(tidyverse)
library(leaflet)
library(shinyWidgets)
library(dplyr)
library(bslib)
library(ggplot2)
library(maps)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(tidyr)
library(collapsibleTree)
library(DT)
library(countrycode)


#przygotowanie danych

#app1
# --- Wczytanie i przygotowanie danych ---
language_coordinates <- read.csv('language_coordinates.csv')
phoible_ulepszone <- read.csv('chyba_ostateczna4.csv')
def <- read.csv('definicje_slownik.csv')


# Przygotowanie głównego zbioru danych
sounds <- phoible_ulepszone %>%
  filter(!(LanguageName == 'Polish' & Phoneme == 'c')) %>% 
  filter(!is.na(NAME)) %>%    # Usuwamy wiersze bez nazwy fonemu
  select(Glottocode, Phoneme, LanguageName, SpecificDialect, SegmentClass, NAME) %>%
  left_join(select(language_coordinates, Glottocode, Longitude, Latitude), by = "Glottocode") %>% # Dołączamy współrzędne
  group_by(Phoneme) %>%
  mutate(frac = n() / 1990) %>%   # Obliczamy frakcję wystąpień dźwięku na podstawie liczby wierszy
  ungroup() %>%
  mutate(frequency_category = case_when(  # Przypisujemy kategorię częstotliwości na podstawie frac
    frac > 0.85 ~ "Near universal",
    frac > 0.60 ~ "Very common",
    frac > 0.35 ~ "Common",
    frac > 0.15 ~ "Uncommon",
    frac > 0.05 ~ "Rare",
    TRUE ~ "Very rare"
  )) %>% 
  mutate(NAME = str_replace(NAME, 
                            "(diphthong|consonant|vowel|tone)", 
                            "(\\1)")) %>% 
  mutate(LanguageName = str_to_title(LanguageName))

def2 <- def %>% 
  mutate(definition = tolower(paste0(term, ': ', definition)))


# Tworzymy ostateczny zbiór danych do aplikacji
ipa_data <- sounds %>%
  transmute(
    lang = LanguageName,
    ipa_sound = Phoneme,
    latitude = as.numeric(Latitude),
    longitude = as.numeric(Longitude),
    type = SegmentClass,
    description = NAME,
    freq = frequency_category
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))  # Usuwamy punkty bez współrzędnych

#app2
data_genders <- read.csv("dane_no_of_genders.csv", sep = "\t")
data_negative <- read.csv("dane_negative_morphemes.csv", sep = "\t")
data_plurality <- read.csv("dane_nominal_plurality.csv", sep = "\t")
data_order <- read.csv("dane_order_of_numeral_and_noun.csv", sep = "\t")

world_map <- map_data("world")

continents <- ne_countries(scale = "medium", returnclass = "sf")
continents$continent <- as.character(continents$continent)

add_continent <- function(df) {
  points_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  joined <- st_join(points_sf, continents, join = st_intersects, left = TRUE)
  df$continent <- joined$continent
  df$continent[is.na(df$continent) | df$continent == "Seven seas (open ocean)"] <- "Other"
  return(df)
}

data_genders <- add_continent(data_genders)
data_negative <- add_continent(data_negative)
data_plurality <- add_continent(data_plurality)
data_order <- add_continent(data_order)


#app3
languages11 <- read.csv("language.csv", stringsAsFactors = FALSE)
languages11 <- languages11 %>%
  filter(!is.na(family), !is.na(genus), !is.na(Name))


languages11$country_list <- strsplit(as.character(languages11$countrycodes), ",")
languages11 <- languages11 %>% unnest(country_list)
languages11$iso3 <- countrycode(languages11$country_list, origin = "iso2c", destination = "iso3c")


# === Main UI with tabs ===
ui <- page_fillable(
  theme = bs_theme(bootswatch = "flatly"),
  tags$style(HTML("
      body, .shiny-ui-output {
        font-family: 'Lucida Sans Unicode', sans-serif;
      }
    ")),
  titlePanel("Linguistic data analysis"),
  tabsetPanel(
    tabPanel("Phonetics", 
             theme = bs_theme(bootswatch = "flatly"),
             
             #czcionka obsługująca IPA
             tags$style(HTML("body, .shiny-ui-output 
                             {font-family: 'Lucida Sans Unicode', sans-serif;}")),
             
             layout_sidebar(
               sidebar = sidebar(
                 title = "Filters",
                 checkboxGroupInput("category", "Choose phoneme category:",
                                    choices = c("Consonant" = "consonant",
                                                "Vowel" = "vowel",
                                                "Tone" = "tone"),
                                    selected = NULL),
                 
                 
                 checkboxGroupInput("freq", "Choose phoneme frequency:",
                                    choices = c("Near universal" = "Near universal",
                                                "Very common" = "Very common",
                                                "Common" = "Common",
                                                "Uncommon" = "Uncommon",
                                                "Rare" = "Rare",
                                                "Very rare" = "Very rare"),
                                    selected = NULL),
                 
                 
                 selectizeInput("lang", "Choose language(s):",
                                choices = NULL,
                                selected = NULL,
                                multiple = TRUE,
                                options = list(
                                  placeholder = 'Select language'
                                )
                 )
               ),
               
               layout_columns(
                 card(full_screen = TRUE,
                      style = "overflow: hidden;",
                      card_header(
                        div(style = "display: flex; align-items: center; gap: 2em;",
                            div(style = "flex: 2;", uiOutput("sound_picker_ui")),    
                            div(style = "flex: 3;", uiOutput("dynamic_card3_header"))  
                        )
                      ),
                      card_body(
                        leafletOutput("map")
                      )
                 ),
                 card(full_screen = TRUE,
                      card_body(
                        layout_columns(
                          selectizeInput("term", NULL,
                                         choices = NULL,
                                         selected = NULL,
                                         multiple = TRUE,
                                         options = NULL
                          ),
                          uiOutput("definition_ui"),
                          col_widths = c(6,6)
                        )
                      )
                 ),
                 col_widths = 12,
                 row_heights = c(3,1)
               )
               )),

     tabPanel("Grammar", 
              theme = bs_theme(bootswatch = "flatly"),
              
              tags$style(HTML("body, .shiny-ui-output 
                             {font-family: 'Lucida Sans Unicode', sans-serif;}")),
              sidebarLayout(
                sidebarPanel(
                  selectInput("dataset", "Choose data to display:",
                              choices = c("Number of Genders" = "genders",
                                          "Negative Morphemes" = "negative",
                                          "Nominal Plurality" = "plurality",
                                          "Order of Numeral and Noun" = "order")),
                  
                  selectInput("kontynent", "Choose a continent or a whole world:",
                              choices = c("World"))
                ),
                mainPanel(
                  card(
                    card(plotOutput("histogram")),
                    card(uiOutput("text"))
                  )
                  
                )
              )),
    tabPanel("Language Families", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("family", "Choose a language family:",
                             choices = sort(unique(languages11$family))),
                 hr(),
                 uiOutput("summary_info")
               ),
               
               mainPanel(
                 collapsibleTreeOutput("language_tree", height = "450px", width = "100%"),
                 br(),
                 div(
                   leafletOutput("language_map", height = "450px", width = "100%"),
                   style = "margin-bottom: 20px;"
                 ),
                 h4("📄 List of languages in selected family"),
                 DTOutput("language_table")
               )
             )
             )
  )
)

# === Main server calls all 3 servers ===
server <- function(input, output, session) {
  #app1 
  # Reaktywne filtrowanie dostępnych głosek na podstawie wybranej kategorii, występowania i języków
  filtered_sounds <- reactive({
    data <- ipa_data
    if (!is.null(input$category) && length(input$category) > 0) {
      data <- data %>% filter(type %in% input$category)
    }
    if (!is.null(input$freq) && length(input$freq) > 0) {
      data <- data %>% filter(freq %in% input$freq)
    }
    if (!is.null(input$lang) && length(input$lang) > 0) {
      data <- data %>% filter(lang %in% input$lang)
    }
    data %>% distinct(ipa_sound, description)      # Unikalne dźwięki z opisami
  })
  
  # Dynamiczne renderowanie wyboru głoski - dostosowane do wybranej kategorii
  output$sound_picker_ui <- renderUI({
    choices <- filtered_sounds()
    
    # Jeśli brak dźwięków dla wybranych filtrów, pokazujemy komunikat
    if (nrow(choices) == 0) {
      return(tags$div("No sounds available for selected filters"))
    }
    
    # Tworzymy pickerInput z listą dostępnych dźwięków
    pickerInput('sound', 
                choices = setNames(choices$ipa_sound, choices$ipa_sound),
                selected = NULL,
                options = list(`live-search` = TRUE, size = 10),
                choicesOpt = list(subtext = choices$description))  # Podpisy z opisami dźwięków
  })
  
  
  # Filtrowanie danych do wyświetlenia na mapie - na podstawie wybranego dźwięku i filtrów
  filtered_data <- reactive({
    sound_id <- input$sound
    req(sound_id)
    data <- ipa_data %>% filter(ipa_sound == sound_id)
    
    if (!is.null(input$category) && length(input$category) > 0) {
      data <- data %>% filter(type %in% input$category)
    }
    if (!is.null(input$freq) && length(input$freq) > 0) {
      data <- data %>% filter(freq %in% input$freq)
    }
    if (!is.null(input$lang) && length(input$lang) > 0) {
      data <- data %>% filter(lang %in% input$lang)
    }
    data
  })
  
  #definicje do słownika pod mapą
  selected_description <- reactive({
    sound <- input$sound
    if (is.null(sound) || sound == "") return("No sound selected")
    
    desc <- ipa_data %>%
      filter(ipa_sound == sound) %>%
      pull(description) %>%
      unique()
    
    if (length(desc) == 0) return("Description not found")
    paste0('/', sound, '/', ' — ', desc[1])
  })
  
  output$dynamic_card3_header <- renderUI({
    
    if (nrow(filtered_sounds()) != 0){
      selected_description()
    }
  })
  
  #lista języków
  languages <- ipa_data %>%
    distinct(lang) %>%
    arrange(lang) %>%
    pull(lang)
  
  #wybor jezyka
  updateSelectizeInput(session, "lang", "Choose language(s):",
                       choices = languages,
                       selected = NULL,
                       options = list(
                         placeholder = 'Select language'
                       ),
                       server = TRUE
  )
  
  #wyszukiwarka terminów fonologicznych
  updateSelectizeInput(session, "term", "Phonetic terminology dictionary:",
                       choices = NULL,
                       selected = NULL,
                       options = list(
                         placeholder = "Type a term and press Enter.",
                         create = T,  
                         persist = FALSE,    
                         openOnFocus = FALSE, 
                         dropdownParent = "body"     
                       ),
                       server = TRUE
  )
  
  #wyswietlanie definicji
  output$definition_ui <- renderUI({
    if (is.null(input$term) || length(input$term) == 0) {
      return(tags$p("", style = "font-style: italic;"))
    }
    
    definitions <- def2 %>%
      filter(term %in% input$term) %>%
      pull(definition)
    
    if (length(definitions) == 0) {
      return(tags$p("Definition not found.", style = "color: red;"))
    }
    
    HTML(paste(definitions, collapse = "<br><br>"))
  })
  
  
  # Renderowanie bazowej mapy Leaflet
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 10, lat = 20, zoom = 1.5)      # Początkowy widok na świat
  })
  
  # Aktualizacja znaczników na mapie po zmianie wybranego dźwięku lub filtrów
  observe({
    data <- filtered_data()
    
    # Czyścimy poprzednie znaczniki (obstawiam, że gdzieś tu jest problem)
    leafletProxy("map") %>% clearMarkers()    
    
    # Jeśli brak danych (brak wybranego dźwięku lub brak wyników), nic nie dodajemy
    if (is.null(data) || nrow(data) == 0) {
      return()
    }
    
    # Dodajemy kółka na mapie z lokalizacją języków mających dany dźwięk
    leafletProxy("map", data = data) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 6,
        fillColor = "blue",
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~lang,   # etykieta z nazwą języka
        labelOptions = labelOptions(noHide = FALSE, direction = "auto")
      )
  })
  
  
  #app2
  observeEvent(input$dataset, {
    data_selected <- switch(input$dataset,
                            genders = data_genders,
                            negative = data_negative,
                            plurality = data_plurality,
                            order = data_order)
    
    continents_available <- unique(data_selected$continent)
    continents_available <- continents_available[!is.na(continents_available)]
    
    updateSelectInput(session, "kontynent",
                      choices = c("World", continents_available),
                      selected = "World")
  })
  
  output$histogram <- renderPlot({
    data_selected <- switch(input$dataset,
                            genders = data_genders,
                            negative = data_negative,
                            plurality = data_plurality,
                            order = data_order)
    filtered <- if (input$kontynent == "World") {
      data_selected
    } else {
      subset(data_selected, continent == input$kontynent)
    }
    
    if (input$dataset == "genders") {
      filtered$description <- factor(filtered$description,
                                     levels = c("None", "Two", "Three", "Four", "Five or more"))
    }
    
    ggplot(filtered, aes(x = description)) +
      geom_bar(fill = "blue") +
      labs(title = paste(input$dataset, " — ", input$kontynent),
           x = "Value", y = "Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(family = "Lucida Sans Unicode"),
        axis.title = element_text(family = "Lucida Sans Unicode"),
        axis.text = element_text(family = "Lucida Sans Unicode")
      ) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
    
  })
  
  output$text <- renderUI({
    HTML(switch(input$dataset,
                
                genders = paste(
                  "<p>The graph presents the number of grammatical genders in various languages. Globally, most languages have no gender, with two genders being the next most common. When analyzing by continent, we observe that Asia, the Americas, and Oceania exhibit patterns similar to the global distribution. Europe stands out with a noticeably higher proportion of languages featuring three genders.</p>",
                  "<p>Africa differs the most: the majority of African languages have five or more genders, followed by those with two genders. Languages with no gender form only the third-largest group in Africa.</p>",
                  sep = ""
                ),
                
                negative = paste(
                  "<p>This graph shows the nature of morphemes used to signal clausal negation in declarative sentences. Clausal negation refers to the straightforward negation of an entire clause (e.g., John is eating pizza → John is not eating pizza). It does not include noun phrase negation (No students were present), negative pronouns (Nobody came), or negative adverbs (She never eats pizza).</p>",
                  
                  "<p>Languages use different strategies to mark clausal negation. These include:</p>",
                  "<ol>",
                  "<li>Negative affix: A bound morpheme attached directly to the verb to indicate negation. It becomes part of the verb itself (e.g., Japanese <i>-nai</i>, Finnish <i>e-</i>).</li>",
                  "<li>Negative particle: A separate, independent word placed before or after the verb to express negation (e.g., English <i>not</i>, Polish <i>nie</i>).</li>",
                  "<li>Negative auxiliary verb: A verb that marks negation and functions similarly to an auxiliary. It typically carries tense or agreement features (e.g., Finnish <i>en</i>, Estonian <i>ei</i>).</li>",
                  "<li>Negative word (unclear if verb or particle): A standalone negative element whose grammatical status is ambiguous - it could be either an auxiliary verb or a particle.</li>",
                  "<li>Variation between negative word and affix: Found in languages that allow both strategies, selecting between them based on context.</li>",
                  "<li>Double negation: Involves two distinct negative forms within a clause - such as a particle plus an affix, or two particles (e.g., French <i>ne...pas</i>).</li>",
                  "</ol>",
                  
                  "<p>As shown in the graph, the most common strategy worldwide is the use of a negative particle, followed by the negative affix. Interestingly, the only continents where more languages use negative affixes than negative particles are South America and Asia.</p>",
                  sep = ""
                ),
                
                plurality = paste(
                  "<p>This graph shows the methods by which languages indicate plurality in nouns. There are essentially two major strategies. The first and most common uses morphological changes to the noun itself, as in English (<i>dog</i> → <i>dogs</i>). The second strategy marks plurality elsewhere in the noun phrase using a separate plural word, such as <i>mau</i> in Hawaiian, which modifies the noun and functions similarly to a plural suffix in English.</p>",
                  
                  "<p>Six values on the graph represent different morphological methods of forming plurals on the noun:</p>",
                  "<ol>",
                  "<li>Plural prefixes</li>",
                  "<li>Plural suffixes</li>",
                  "<li>Stem-internal changes (e.g., <i>humar</i> → <i>humaar</i> in Maricopa)</li>",
                  "<li>Tone changes (e.g., <i>kamà</i> → <i>kámá</i> in Ngiti)</li>",
                  "<li>Reduplication of the noun stem (e.g., <i>rumah</i> → <i>rumah-rumah</i> in Indonesian)</li>",
                  "<li>Multiple morphological strategies, where two or more of the above occur without one clearly being primary</li>",
                  "</ol>",
                  
                  "<p>In addition to these morphological types, the map also includes two non-morphological strategies: plural words, like Hawaiian <i>mau</i>, and plural clitics. Clitics behave like plural words in meaning but occupy a fixed position in the noun phrase and attach phonologically to nearby words.</p>",
                  
                  "<p>The final type includes languages without a known morphological plural, where available data shows no use of plural words or clitics, though such elements may exist but remain undocumented.</p>",
                  
                  "<p>As shown on the graph, the most common method of indicating nominal plurality is the plural suffix. Europe and South America show relatively low diversity, each with only three or four plurality strategies. Oceania stands out, with a notable concentration of languages using plural words to indicate plurality, setting it apart from other continents.</p>",
                  sep = ""
                ),
                
                order = paste(
                  "<p>This graph shows the order of cardinal numerals with respect to the noun they modify. The Numeral–Noun type includes languages where the numeral precedes the noun (e.g., <i>three women</i>). The Noun–Numeral type includes languages where the numeral follows the noun (e.g., <i>qüa xüé</i> in Pumi, meaning “eight pigs”). No dominant order refers to languages that allow both orders without one being clearly preferred. Finally, some languages show no syntactic combination between numerals and nouns - here, the numeral modifies the verb rather than the noun.</p>",
                  "<p>According to the graph, most languages fall into either the Numeral–Noun or Noun–Numeral categories. The type where the numeral only modifies the verb is rare and appears only in a few South American languages. Numeral–Noun is the dominant pattern in the Americas and Europe, with no languages in Europe showing the Noun–Numeral order. In Asia, Numeral–Noun is slightly more common than Noun–Numeral. In contrast, Noun–Numeral is the most frequent pattern in both Africa and Oceania. The No dominant order category occurs only occasionally across different continents.</p>",
                  sep = ""
                )
    ))
  })
  
  
  
  #app3
  filtered <- reactive({
    languages11 %>% filter(family == input$family)
  })
  
  output$language_tree <- renderCollapsibleTree({
    data <- filtered()
    collapsibleTree(
      data,
      hierarchy = c("family", "genus", "Name"),
      root = input$family,
      fill = "#3182bd",
      fontSize = 14,
      width = "100%"
    )
  })
  
  output$summary_info <- renderUI({
    data <- filtered()
    n <- length(unique(data$Name))
    regions <- sort(unique(data$macroarea[!is.na(data$macroarea)]))
    
    tagList(
      p(strong("Number of languages:"), n),
      if (length(regions) > 0) {
        p(strong("Macroareas:"), paste(regions, collapse = ", "))
      }
    )
  })
  
  output$language_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 10, lat = 20, zoom = 2)
  })
  
  observe({
    countries <- filtered() %>%
      distinct(iso3) %>%
      pull(iso3) %>%
      na.omit()
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world$highlight <- ifelse(world$iso_a3 %in% countries, "Yes", "No")
    
    leafletProxy("language_map", data = world) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ifelse(highlight == "Yes", "#2b8cbe", "#f0f0f0"),
        weight = 0.5,
        color = "white",
        fillOpacity = 0.7,
        label = ~name,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      )
  })
  
  output$language_table <- renderDT({
    filtered() %>%
      distinct(Name, genus, macroarea, countrycodes, iso_code, glottocode) %>%
      arrange(Name)
  })
}

shinyApp(ui, server)