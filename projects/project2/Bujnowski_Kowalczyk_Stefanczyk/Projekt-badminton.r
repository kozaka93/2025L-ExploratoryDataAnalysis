library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(geodata)
library(sf)
library(ggiraph)
library(later)
library(leaflet)
library(rnaturalearth)
library(plotly)
library(countrycode)
library(shinycssloaders)
library(bslib)
library(rnaturalearthdata)
library(shinythemes)
library(stringr)
library(png)
library(grid)


wczytanie_sedziowie_polska_aktualni <- function(){
  # 1. Wczytanie całej tabeli jako jedna kolumna X1
  dane <- read_excel("Polska sędziowie aktualni.xlsx", col_names = FALSE) %>%rename(X1 = 1)
  tytuly <- c(
    "Państwowi sędziowie Główni",
    "Państwowi sędziowie Prowadzący",
    "Krajowi sędziowie Prowadzący",
    "Państwowi sędziowie Zwykli - główni",
    "Państwowi sędziowie Liniowi",
    "Państwowi sędziowie Zwykli")
  woj_list <- c(
    "Dolnośląskie", "Kujawsko-Pomorskie", "Lubelskie", "Lubuskie",
    "Łódzkie", "Małopolskie", "Mazowieckie", "Opolskie",
    "Podkarpackie", "Podlaskie", "Pomorskie", "Śląskie",
    "Świętokrzyskie", "Warmińsko-Mazurskie", "Wielkopolskie", "Zachodniopomorskie")
  # Indeksy, gdzie zaczyna się każdy blok
  df_names <- list()
  idx_tytul <- which(dane$X1 %in% tytuly)
  idx_tytul <- c(idx_tytul, nrow(dane) + 1)
  # Pętla: dla każdego tytułu wycinamy podblok
  globalenv <- .GlobalEnv
  for(i in seq_along(tytuly)) {
    start <- idx_tytul[i] + 1
    stop  <- idx_tytul[i+1] - 1
    blok <- dane[start:stop, , drop = FALSE]
    # ustawienia do iteracji
    current_woj <- NA_character_
    records <- list()
    j <- 1
    while(j <= nrow(blok)) {
      line <- blok$X1[j]
      # jeśli linia to województwo
      if(line %in% woj_list) {
        current_woj <- blok$X1[j]
        j <- j + 1
      } else {
        # oczekujemy 5 kolejnych wierszy: id, imię, nazwisko, data, email
        if(j + 4 <= nrow(blok)) {
          rec <- tibble::tibble(
            Województwo = current_woj,
            numer_id = blok$X1[j],
            imie = blok$X1[j+1],
            nazwisko = str_to_sentence(tolower(blok$X1[j+2])),
            data_urodzenia = blok$X1[j+3],
          )
          records <- append(records, list(rec))}
        j <- j + 5}}
    # po pętli łączymy w ramkę
    if(length(records) > 0) {
      out_df <- bind_rows(records)
      # nazwa obiektu bez polskich znaków/spacji
      nazwa <- str_replace_all(tytuly[i], "[^A-Za-z0-9]+", "_")
      df_name <- paste0("Polscy_sedziowie_aktualni_", nazwa)
      assign(df_name, out_df, envir = globalenv)
    }
  }}

wczytanie_sedziowie_polska_aktualni()
remove(wczytanie_sedziowie_polska_aktualni)

zawodnicy_polska <- read_excel("ewidencja zawodników polska.xlsx") %>% 
  rename(
    Numer_ID = `Member ID`,
    Imię = `First name`,
    Płeć = Gender,
    Klub = Club,
    Data_urodzenia = `Date of birth`,
    Narodowość = Narowdowość) %>%
  mutate(
    Województwo = case_when(
      State == "DŚL" ~ "Dolnośląskie",
      State == "KPM" ~ "Kujawsko-Pomorskie",
      State == "LBU" ~ "Lubuskie",
      State == "LUB" ~ "Lubelskie",
      State == "ŁDZ" ~ "Łódzkie",
      State == "MAZ" ~ "Mazowieckie",
      State == "MŁP" ~ "Małopolskie",
      State == "OPO" ~ "Opolskie",
      State == "PDL" ~ "Podlaskie",
      State == "PKR" ~ "Podkarpackie",
      State == "POM" ~ "Pomorskie",
      State == "ŚLĄ" ~ "Śląskie",
      State == "ŚWI" ~ "Świętokrzyskie",
      State == "WLP" ~ "Wielkopolskie",
      State == "WMA" ~ "Warmińsko-Mazurskie",
      State == "ZPM" ~ "Zachodniopomorskie")) %>% 
  mutate(Nazwisko = str_to_sentence(tolower(name))) %>% 
  select(Numer_ID, Imię, Nazwisko, Płeć, Data_urodzenia, Województwo, Klub, Licencja, Klubowość, Narodowość, Drużynowość)

sedziowie_polska_wszyscy <- read_excel("Polska sędziowie wszyscy.xlsx")%>% 
  rename(
    Numer_ID = `NUMER`,
    Imię = IMIĘ,
    Rok_urodzenia = `ROK_UR.`,
    Województwo = WOJEWÓDZTWO) %>%
  mutate(Nazwisko = str_to_sentence(tolower(NAZWISKO))) %>% 
  select(Numer_ID, Imię, Nazwisko, Rok_urodzenia, Województwo)
sedziowie_polska_wszyscy$Województwo <- tools::toTitleCase(sedziowie_polska_wszyscy$Województwo)

aktualni_sedziowie <- bind_rows(Polscy_sedziowie_aktualni_Krajowi_s_dziowie_Prowadz_cy, 
                                Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_G_wni, 
                                Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Liniowi, 
                                Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Prowadz_cy, 
                                Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli, 
                                Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli_g_wni) %>% 
  { bind_rows(
    filter(., is.na(numer_id)),                # zostaw wszystkie z NA
    distinct(filter(., !is.na(numer_id)), numer_id, .keep_all = TRUE)  # unikalne wg numer_id spośród nie-NA
  )
  }

aktualni_sedziowie_kategoria <- bind_rows(
  Polscy_sedziowie_aktualni_Krajowi_s_dziowie_Prowadz_cy %>% 
    mutate(Kategoria = "Krajowi sędziowie Prowadzący"),
  Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_G_wni %>% 
    mutate(Kategoria = "Państwowi sędziowie Główni"),
  Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Liniowi %>% 
    mutate(Kategoria = "Państwowi sędziowie Liniowi"),
  Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Prowadz_cy %>% 
    mutate(Kategoria = "Państwowi sędziowie Prowadzący"),
  Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli %>% 
    mutate(Kategoria = "Państwowi sędziowie Zwykli"),
  Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli_g_wni %>% 
    mutate(Kategoria = "Państwowi sędziowie Zwykli-główni"))

wojewodztwa_df  <-data.frame(Województwo = c(
  "Dolnośląskie", "Kujawsko-Pomorskie", "Lubelskie", "Lubuskie",
  "Łódzkie", "Małopolskie", "Mazowieckie", "Opolskie",
  "Podkarpackie", "Podlaskie", "Pomorskie", "Śląskie",
  "Świętokrzyskie", "Warmińsko-Mazurskie", "Wielkopolskie", "Zachodniopomorskie"))

################################################################################
################################################################################
olimpiada <- read.csv("athlete_events.csv")

olimpiada_badminton <- olimpiada %>% filter(Sport == "Badminton") %>% 
  mutate(Sex = recode(Sex, "M" = "Mężczyzna", "F" = "Kobieta"))

olimpiada_badminton$Team <- sub("-.*", "", olimpiada_badminton$Team)

olimpiada_badminton$TeamPL <- countrycode(olimpiada_badminton$Team, "country.name", "cldr.name.pl")

druzyny <- olimpiada_badminton %>% distinct(Team, TeamPL) %>% filter(!is.na(TeamPL))

kontynenty_polskie <- c("Świat" = "World",
                        "Europa" = "Europe",
                        "Azja" = "Asia",
                        "Ameryka Północna" = "North America",
                        "Ameryka Południowa" = "South America",
                        "Oceania" = "Oceania",
                        "Afryka" = "Africa")

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(country_name = countrycode(sovereignt, origin = "country.name", destination = "country.name"))

olimpiada_badminton_grouped <- olimpiada_badminton %>% group_by(Year, Team, TeamPL) %>% summarise(ilosc_zawodnikow = n(), .groups = "drop") %>%
  mutate(country_name = countrycode(Team, origin = "country.name", destination = "country.name"))

merged_olimp_world <- left_join(world, olimpiada_badminton_grouped, by = "country_name") %>%
  filter(!is.na(ilosc_zawodnikow))

bounding_boxes <- list(
  "Europe" = list(lng1 = -30, lat1 = 30, lng2 = 60, lat2 = 75),
  "Asia" = list(lng1 = 25, lat1 = -10, lng2 = 150, lat2 = 80),
  "North America" = list(lng1 = -170, lat1 = 5, lng2 = -30, lat2 = 85),
  "South America" = list(lng1 = -90, lat1 = -60, lng2 = -30, lat2 = 15),
  "Oceania" = list(lng1 = 110, lat1 = -50, lng2 = 180, lat2 = 10),
  "Africa" = list(lng1 = -20, lat1 = -40, lng2 = 55, lat2 = 38)
)

################################################################################
################################################################################
# Wczytaj dane
df <- read.csv("athlete_events.csv")
badminton_df <- df %>% filter(Sport == "Badminton")

# Mapowanie NOC → ISO2 i Polska nazwa
noc_to_iso2 <- c(
  USA = "us", CHN = "cn", GBR = "gb", RUS = "ru", GER = "de", FRA = "fr",
  JPN = "jp", KOR = "kr", DEN = "dk", INA = "id", IND = "in", MAS = "my",
  NED = "nl", ESP = "es", BRA = "br", CAN = "ca", THA = "th", TPE = "tw",
  SGP = "sg", HKG = "hk", POL = "pl", AUS = "au"
)

noc_to_polish <- c(
  USA = "Stany Zjednoczone", CHN = "Chiny", GBR = "Wielka Brytania", RUS = "Rosja",
  GER = "Niemcy", FRA = "Francja", JPN = "Japonia", KOR = "Korea Południowa",
  DEN = "Dania", INA = "Indonezja", IND = "Indie", MAS = "Malezja",
  NED = "Holandia", ESP = "Hiszpania", BRA = "Brazylia", CAN = "Kanada",
  THA = "Tajlandia", TPE = "Tajwan", SGP = "Singapur", HKG = "Hongkong"
)

event_to_polish <- c(
  "Badminton Men's Singles" = "Singiel mężczyzn",
  "Badminton Women's Singles" = "Singiel kobiet",
  "Badminton Men's Doubles" = "Debel mężczyzn",
  "Badminton Women's Doubles" = "Debel kobiet",
  "Badminton Mixed Doubles" = "Debel mieszany"
)

get_flag_grob <- function(noc) {
  code <- noc_to_iso2[[noc]]
  if (is.null(code)) return(NULL)  # brak kodu → brak flagi
  
  url <- paste0("https://flagcdn.com/w320/", code, ".png")
  tmp <- tempfile(fileext = ".png")
  
  tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    img <- png::readPNG(tmp)
    grid::rasterGrob(img, interpolate = TRUE)
  }, error = function(e) {
    message(paste("Nie udało się pobrać flagi dla NOC:", noc))
    NULL
  })
}


################################################################################
################################################################################
################################################################################

# Uniwersalne obliczanie do mapy, jednorazowo.
pol_sf  <- geodata::gadm("POL", level = 1, path = tempdir()) %>% sf::st_as_sf() 
centroidy <- pol_sf %>% # Obliczamy środki województw, żeby dodać etykiety z liczbą
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2],
    Województwo = NAME_1)


################################################################################
################################################################################
# ==== UI ==== #################################################################
################################################################################
################################################################################
ui <- fluidPage(
  
  tags$head(
    tags$script(HTML("
    $(document).on('shiny:connected', function() {
      var width = $(window).width();
      Shiny.setInputValue('window_width', width);});
    $(window).resize(function() {
      var width = $(window).width();
      Shiny.setInputValue('window_width', width);});")),
    tags$style(HTML("
    .custom-title {
      color: #002147;
      font-weight: 900;
      font-size: 230%;
      margin-bottom: 10px;
      margin-top: 10px;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;}
    /* Styl zakładek (nav-tabs) */
    .nav-tabs > li > a {
      background-color: #004c99 !important;  /* ciemnoniebieskie tło */
      color: white !important;               /* biały tekst */
      font-weight: bold !important;          /* pogrubienie */}
    /* Styl zakładki aktywnej */
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background-color: #003366 !important;  /* ciemniejszy granat */
      color: white !important;}
    /* Styl zakładek po najechaniu */
    .nav-tabs > li > a:hover {
      background-color: #004080 !important;  /* hover: jaśniejszy granat */
      color: white !important;}
    /* Styl opisu badmintonu */
    #opis_badminton {
      font-size: 110%;        /* powiększony tekst */
      margin-bottom: 1em;     /* odstęp pod spodem */
      margin-top: 1em;        /* odstęp nad tekstem (oddalenie od zakładek) */
      padding-left: 10px;     /* odsunięcie od lewej */
      font-weight: normal;    /* zwykła grubość */
      color: #003366;         /* granatowy kolor tekstu */}"))),
  uiOutput("dynamic_css"),
    
    ############################################################################
    # Tytuł
    div(class = "custom-title","Badminton – szybkość, precyzja i pasja"),
    
    div(id = "opis_badminton", uiOutput("opis_badminton")),

    tabsetPanel(
      ###########################################################################
      # ==== Pierwsza strona ==== ###############################################
      ###########################################################################
      tabPanel("Badminton w Polsce", # Tytuł I strony

              tags$p("W tej części przyjrzymy się danym dotyczącym polskich zawodników 
                 i sędziów – od poziomu krajowego po lokalne struktury – aby pokazać, 
                 jak rozwija się badminton w Polsce i jakie wyzwania czekają na przyszłość.",
                     style = "font-size: 110%; margin-top: 20px; margin-bottom: 15px; padding-left: 10px;"),
               
               fluidRow(
                 ###############################################################
                 # ==== Lewa kolumna ==== ######################################
                 ###############################################################
                 
                 column(
                   width = 6, # lewa kolumna
                   h3("Jak to wygląda w podziale na województwa?"),
                   
                   fluidRow(
                     column(width = 6,
                   ###############################################################
                   # Pierwsze okno wyboru I kolumny
                   selectInput(
                     inputId = "kategoria",
                     label = "Wybierz kategorię danych:",
                     choices = c("Zawodnicy", "Sędziowie", "Liczba zawodników na jednego sędziego")),
                   
                     ), column(width = 6,
                   ###############################################################
                   # Drugie okno wyboru I kolumny
                   selectInput(
                     inputId = "zakres",
                     label = "Zakres danych:",
                     choices = c("Posiadający aktualną licencję" = "aktualne", "Wszyscy" = "wszyscy")),
                   )),
                   
                   ###############################################################
                   # Drugi okno wyboru I kolumny, tylko jeśli wybrano "Zawodnicy"
                   conditionalPanel(
                     condition = "input.kategoria == 'Zawodnicy'",
                     selectInput(
                       inputId = "kategoria_wiekowa",
                       label = "Kategoria wiekowa:",
                       choices = c(
                         "Wszyscy" = "wszyscy",
                         "Elita (E)" = "elita",
                         "Młodzieżowcy (U23)" = "u23",
                         "Juniorzy (J – U19)" = "u19",
                         "Juniorzy Młodsi (JM – U17)" = "u17",
                         "Młodzicy (M – U15)" = "u15",
                         "Młodzicy Młodsi (MM – U13)" = "u13",
                         "Żacy (Ż – U11)" = "u11",
                         "Żacy Młodsi (ŻM – U9)" = "u9"))),
                   
                   ###############################################################
                   # Drugi okno wyboru I kolumny, tylko jeśli wybrano "Sędziowie" i "aktualne"
                   conditionalPanel(
                     condition = "input.kategoria == 'Sędziowie' && input.zakres == 'aktualne'",
                     selectInput(
                       inputId = "kategoria_sedziow",
                       label = "Kategoria sędziów:",
                       choices = c(
                         "Wszyscy" = "wszyscy",
                         "Państwowi sędziowie Główni" = "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_G_wni",
                         "Państwowi sędziowie Prowadzący" = "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Prowadz_cy",
                         "Krajowi sędziowie Prowadzący" = "Polscy_sedziowie_aktualni_Krajowi_s_dziowie_Prowadz_cy",
                         "Państwowi sędziowie Zwykli-główni" = "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli_g_wni",
                         "Państwowi sędziowie Liniowi" = "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Liniowi",
                         "Państwowi sędziowie Zwykli" = "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli"
                       ))),
                   
                   uiOutput("tekst_opisowy"),
                   
                   girafeOutput("mapa_plot", width = "100%", height = "auto")
                 ), # Koniec kolumny I
                 

                 ###############################################################
                 # ==== Prawa kolumna ==== #####################################
                 ###############################################################
                 column(
                   width = 6, # prawa kolumna
                   h3("Jak to wygląda w podziale na kategorie?"),
                   
                   fluidRow(
                     column(width = 6,
                   ###############################################################
                   # Pierwsze okno wyboru II kolumny
                   selectInput(inputId = "kol2_kogo", 
                               label = "Kogo analizujesz?", 
                               choices = c("Zawodnicy", "Sędziowie")),
                   
                     ), column(width = 6,
                   ###############################################################
                   # Drugie okno wyboru II kolumny
                   selectInput(inputId = "kol2_licencja", 
                               label = "Zakres danych:", 
                               choices = c("Wszyscy" = "wszyscy", "Posiadający aktualną licencję" = "aktualne")
                   ))),
                   fluidRow(column(width = 6,
                   ###############################################################
                   # Trzecie okno wyboru II kolumny, tylko jeśli wybrano "Sędziowie" i "aktualne"
                   conditionalPanel(
                     condition = "input.kol2_kogo == 'Sędziowie' && input.kol2_licencja == 'aktualne'",
                     selectInput(inputId = "kol2_tryb", 
                                 label = "Tryb analizy:", 
                                 choices = c("Stopnie sędziowskie", "Wiek sędziów"))),
                   
                     ), column(width = 6,
                   ###############################################################
                   # Czwarte okno wyboru II kolumny, tylko jeśli wybrano "Sędziowie" i "aktualne" i "Wiek sędziów"
                   conditionalPanel(
                     condition = "input.kol2_kogo == 'Sędziowie' && input.kol2_licencja == 'aktualne' && input.kol2_tryb == 'Wiek sędziów'",
                     selectInput(inputId = "kol2_stopien", 
                                 label = "Wybierz kategorię sędziów:", 
                       choices = c(
                         "Wszyscy" = "wszyscy",
                         "Państwowi sędziowie Główni" = "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_G_wni",
                         "Państwowi sędziowie Prowadzący" = "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Prowadz_cy",
                         "Krajowi sędziowie Prowadzący" = "Polscy_sedziowie_aktualni_Krajowi_s_dziowie_Prowadz_cy",
                         "Państwowi sędziowie Zwykli-główni" = "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli_g_wni",
                         "Państwowi sędziowie Liniowi" = "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Liniowi",
                         "Państwowi sędziowie Zwykli" = "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli"))),
                   
                   ###############################################################
                   # Czwarte okno wyboru II kolumny, tylko jeśli wybrano "Sędziowie" i "aktualne" i "Stopnie sędziowskie"
                   conditionalPanel(
                     condition = "input.kol2_kogo == 'Sędziowie' && input.kol2_licencja == 'aktualne' && input.kol2_tryb == 'Stopnie sędziowskie'",
                     selectInput(inputId = "kol2_woj_sedziowie", 
                                 label = "Wybierz region sędziów:", 
                                 choices = c("Polska", wojewodztwa_df$Województwo))))),
                   
                   ###############################################################
                   # Trzecie okno wyboru II kolumny, tylko jeśli wybrano "Zawodnicy"
                   conditionalPanel(
                     condition = "input.kol2_kogo == 'Zawodnicy'",
                     selectInput(inputId = "kol2_woj", 
                                 label = "Wybierz region:", 
                                 choices = c("Polska", wojewodztwa_df$Województwo))),
                   
                   uiOutput("tekst_opisowy_2"),
                   
                   girafeOutput("kol2_wykres", width = "100%", height = "auto")
                   ) # Koniec kolumny II
          )), # Koniec strony I
      
      
      ##########################################################################
      # ==== Druga strona ==== #################################################
      ##########################################################################
      tabPanel("Olimpiady na mapach", # Tytuł II strony
               
           tags$p("Mapa przedstawia liczbę zawodników biorących udział
                  w olimpiadzie w danym zakresie lat.",
                  style = "font-size: 110%; margin-top: 20px; margin-bottom: 15px; padding-left: 10px;"),
           
           fluidRow(
             sidebarLayout(
               sidebarPanel(
                 textOutput("tekstmapa"),
                 selectInput("Kontynent", "Wybierz kontynent", 
                             choices = names(kontynenty_polskie), 
                             selected = "Europa", multiple = FALSE),
                 
                 sliderInput("Rok", "Wybierz rok", 
                             min = as.integer(min(olimpiada_badminton$Year)), 
                             max = as.integer(max(olimpiada_badminton$Year)), 
                             value = c(2000L, 2016L), step = 4, sep = ""),
                 br(), br(), br(), br(), br(),
                 textOutput("tekstbox"),
                 
                 selectizeInput("Druzyna", "Wybierz kraj(e):", 
                                choices = setNames(druzyny$Team, druzyny$TeamPL), 
                                selected = "Poland", multiple = TRUE,
                                options = list(maxItems = 5)),
                 
                 selectInput("Plec", "Wybierz płeć:", 
                             choices = unique(olimpiada_badminton$Sex), 
                             selected = "Mężczyzna", multiple = TRUE)
               ),
               
               mainPanel(
                 withSpinner(leafletOutput("continentPlot"), type = 6, color = "darkorange", caption = "ładowanie"),
                 br(),
                 withSpinner(plotlyOutput("heightboxPlot"), type = 6, color = "darkorange", caption = "ładowanie")
               )
             )
           )),
      
      ##########################################################################
      # ==== Trzecia strona ==== ###############################################
      ##########################################################################
      tabPanel("Wyniki Olimpiad", # Tytuł III strony
               
               fluidRow(  
                 column(
                   width = 6, # lewa kolumna
                   h3("Podium medalistów"),
                   fluidRow(
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("year_podium", "Wybierz rok:",
                                     choices = sort(unique(badminton_df$Year))),
                         selectInput("event_podium", "Wybierz konkurencję:",
                                     choices = setNames(unique(badminton_df$Event),
                                                        event_to_polish[unique(badminton_df$Event)])),
                         br(),
                         p("Wykres pokazuje medalistów wybranej konkurencji w danym roku. Zawodnicy są ułożeni na podium, a obok ich nazwisk wyświetlają się flagi krajów.")
                       ),
                       mainPanel(
                         girafeOutput("podium_plot", height = "600px")
                       )
                     )
                   )),
                 column(
                   width = 6, # prawa kolumna
                   h3("Liczba zawodników w czasie"),
                   fluidRow(
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("country_trend", "Wybierz drużynę (opcjonalnie):",
                                     choices = c("Wszystkie", sort(unique(badminton_df$NOC))),
                                     selected = "Wszystkie"),
                         selectInput("event_trend", "Wybierz konkurencję (opcjonalnie):",
                                     choices = c("Wszystkie", setNames(sort(unique(badminton_df$Event)),
                                                                       event_to_polish[sort(unique(badminton_df$Event))])),
                                     selected = "Wszystkie"),
                         sliderInput("year_range", "Zakres lat:",
                                     min = min(badminton_df$Year),
                                     max = max(badminton_df$Year),
                                     value = c(min(badminton_df$Year), max(badminton_df$Year)),
                                     step = 4, sep = ""),
                         br(),
                         p("Ten wykres pokazuje liczbę badmintonistów biorących udział w igrzyskach w wybranym okresie czasu. Możesz filtrować dane według drużyny oraz konkurencji.")
                       ),
                       mainPanel(
                         plotlyOutput("trend_plot")
                       )
                     )
                   ))
               ))
)) # Koniec ui


selected_region <- reactiveVal(NULL)
zmiana_z_mapy <- reactiveVal(FALSE)
mapa_trigger <- reactiveVal(0)
col_trigger <- reactiveVal(0)
################################################################################
################################################################################
# ==== SERVER ==== #############################################################
################################################################################
################################################################################
server <- function(input, output, session) {
  
  ##############################################################################
  # ==== Tekst ==== ############################################################
  ##############################################################################
  output$opis_badminton <- renderUI({
    HTML("Badminton to jedna z najszybszych i najbardziej dynamicznych dyscyplin sportowych na świecie. Choć na pierwszy rzut oka może wydawać się 
         prostą grą z lekką rakietą i lotką, wymaga od zawodników wyjątkowej precyzji, zwinności i błyskawicznej reakcji. Od amatorskich rozgrywek 
         na podwórku po zawodowe turnieje międzynarodowe – badminton łączy pokolenia i kultury, zachwycając zarówno grą pojedynczą, jak i deblową.")})
  
  output$dynamic_css <- renderUI({
    req(input$window_width)
    base_width <- 800
    max_width <- 1200
    size_pct <- 100 + (input$window_width - base_width) / (max_width - base_width) * 70
    size_pct <- min(max(size_pct, 100), 170)
    tags$style(HTML(sprintf("
    body {
      font-size: %.1f%% !important;}", size_pct)))})
  
  ##############################################################################
  # ==== Zaznaczenie regionu na MAPA ==== ######################################
  ##############################################################################
    # Obserwujemy kliknięcie na mapie 
    observeEvent(input$mapa_plot_selected, {
      clicked_region <- input$mapa_plot_selected
      if (!is.null(clicked_region) && clicked_region != "") {
        if (
          (input$kategoria == "Zawodnicy" && input$zakres == "aktualne" && input$kategoria_wiekowa == "wszyscy") ||
          (input$kategoria == "Zawodnicy" && input$zakres == "wszyscy" && input$kategoria_wiekowa == "wszyscy") ||
          (input$kategoria == "Sędziowie" && input$zakres == "aktualne" && input$kategoria_sedziow == "wszyscy")) 
          {zmiana_z_mapy(TRUE)
          selected_region(clicked_region)
          updateSelectInput(session, "kol2_woj", selected = clicked_region)
          updateSelectInput(session, "kol2_woj_sedziowie", selected = clicked_region)
          if (input$kategoria == "Zawodnicy") {
            updateSelectInput(session, "kol2_kogo", selected = "Zawodnicy")
            updateSelectInput(session, "kol2_licencja", selected = input$zakres)} 
          else if (input$kategoria == "Sędziowie") {
            updateSelectInput(session, "kol2_kogo", selected = "Sędziowie")
            updateSelectInput(session, "kol2_licencja", selected = "aktualne")}
        later::later(function() zmiana_z_mapy(FALSE), 0.5)
        col_trigger(isolate(col_trigger()) + 1)}
        else {
          showNotification("Zaznaczenie województwa działa tylko przy określonych ustawieniach.", type = "warning")
          selected_region(NULL)
          updateSelectInput(session, "kol2_woj", selected = "Polska")
          updateSelectInput(session, "kol2_woj_sedziowie", selected = "Polska")}}})
    
    # Obserwujemy zmianę pola wyboru przy zaznaczonej mapie
    observeEvent(list(
      input$kol2_kogo,
      input$kol2_licencja,
      input$kol2_tryb,
      input$kol2_stopien,
      input$kol2_woj_sedziowie,
      input$kol2_woj),
      if (!zmiana_z_mapy() && !is.null(selected_region())) {
        selected_region(NULL)
        mapa_trigger(isolate(mapa_trigger()) + 1)})

    # Obserwujemy odkliknięcie na mapie
    observeEvent(list(
      input$mapa_plot_selected,
      input$kategoria_sedziow,
      input$kategoria_wiekowa,
      input$zakres,
      input$kategoria),
      if (!zmiana_z_mapy()) {
        col_trigger(isolate(col_trigger()) + 1)
        selected_region(NULL)})
  
  ##############################################################################
  # ==== Dane reaktywne do MAPY ==== ###########################################
  ##############################################################################
  dane_dla_mapy <- reactive({
    
    ############################################################################
    # ==== Zawodnicy === #######################################################
    ############################################################################
    if (input$kategoria == "Zawodnicy") {
      zawodnicy <- zawodnicy_polska
      
      ##########################################################################
      # ==== Zawodnicy - aktualni ==== #########################################
      if (input$zakres == "aktualne") { # Filtrowanie po aktualnej licencji
        zawodnicy <- zawodnicy %>%
          filter(Licencja > as.POSIXct(Sys.Date()))}
      
      ##########################################################################
      # ==== Zawodnicy - kategoria wiekowa ==== ################################
      if (!is.null(input$kategoria_wiekowa) && input$kategoria_wiekowa != "wszyscy") {
        zawodnicy <- zawodnicy %>% # Filtrowanie po kategorii wiekowej
          mutate(Rok_urodzenia = lubridate::year(Data_urodzenia)) %>%
          filter(case_when(
            input$kategoria_wiekowa == "elita" ~ Rok_urodzenia <= 2006,
            input$kategoria_wiekowa == "u23" ~ Rok_urodzenia >= 2003 & Rok_urodzenia <= 2006,
            input$kategoria_wiekowa == "u19" ~ Rok_urodzenia >= 2007 & Rok_urodzenia <= 2008,
            input$kategoria_wiekowa == "u17" ~ Rok_urodzenia >= 2009 & Rok_urodzenia <= 2010,
            input$kategoria_wiekowa == "u15" ~ Rok_urodzenia >= 2011 & Rok_urodzenia <= 2012,
            input$kategoria_wiekowa == "u13" ~ Rok_urodzenia >= 2013,
            input$kategoria_wiekowa == "u11" ~ Rok_urodzenia >= 2015,
            input$kategoria_wiekowa == "u9"  ~ Rok_urodzenia >= 2017,
            TRUE ~ TRUE))}
      
      ##########################################################################
      # Zliczanie zawodników w województwach - już dla zaznaczonych pól wyboru #
      zawodnicy %>%
        count(Województwo, name = "liczba") %>%
        full_join(wojewodztwa_df, by = "Województwo") %>%
        replace_na(list(liczba = 0))} 
    
    ############################################################################
    # ==== Sędziowie === #######################################################
    ############################################################################
    else if (input$kategoria == "Sędziowie") {
      
      ##########################################################################
      # ==== Sędziowie - aktualni ==== #########################################
      if (input$zakres == "aktualne") {
        
        ########################################################################
        # ==== Sędziowie - kategoria sędziów ==== ##############################
        if (!is.null(input$kategoria_sedziow) && input$kategoria_sedziow != "wszyscy") {# Wybrana kategoria sędziego
          sedziowie_kat <- get(input$kategoria_sedziow) # Pobierz ramkę danych dynamicznie po nazwie  
          sedziowie_kat %>%
            count(Województwo, name = "liczba") %>%
            full_join(wojewodztwa_df, by = "Województwo") %>%
            replace_na(list(liczba = 0))} 
        else { # Wszyscy aktualni sędziowie
          aktualni_sedziowie %>%
            count(Województwo, name = "liczba") %>%
            full_join(wojewodztwa_df, by = "Województwo") %>%
            replace_na(list(liczba = 0))}} 
      
      ##########################################################################
      # ==== Sędziowie - wszyscy ==== ##########################################
      else {
        sedziowie_polska_wszyscy %>%
          count(Województwo, name = "liczba") %>%
          full_join(wojewodztwa_df, by = "Województwo") %>%
          replace_na(list(liczba = 0))}} 
    
    ############################################################################
    # ==== Liczba zawodników na jednego sędziego === ###########################
    ############################################################################
    else if (input$kategoria == "Liczba zawodników na jednego sędziego") {
      
      ##########################################################################
      # ==== Liczba zawodników na jednego sędziego - aktualni ==== #############
      if (input$zakres == "aktualne") {
        zawodnicy_polska %>%
          filter(Licencja > as.POSIXct(Sys.Date())) %>%
          count(Województwo, name = "zawodnicy") %>%
          full_join(
            aktualni_sedziowie %>%
              count(Województwo, name = "sedziowie"),
            by = "Województwo") %>%
          mutate(liczba = round(zawodnicy / sedziowie, 0)) %>%
          select(Województwo, liczba) %>%
          mutate_all(~ ifelse(is.na(.), 0, .)) %>%
          full_join(wojewodztwa_df, by = "Województwo") %>%
          replace_na(list(liczba = 0))} 
      
      ##########################################################################
      # ==== Liczba zawodników na jednego sędziego - wszyscy ==== ##############
      else {
        zawodnicy_polska %>%
          count(Województwo, name = "zawodnicy") %>%
          full_join(
            sedziowie_polska_wszyscy %>%
              filter(!is.na(Numer_ID)) %>%
              count(Województwo, name = "sedziowie"),
            by = "Województwo") %>%
          mutate(liczba = round(zawodnicy / sedziowie, 0)) %>%
          select(Województwo, liczba) %>%
          mutate_all(~ ifelse(is.na(.), 0, .)) %>%
          full_join(wojewodztwa_df, by = "Województwo") %>%
          replace_na(list(liczba = 0))}} 
    else {NULL}
  }) # Koniec danych reaktywnych do mapy
  
  
  ##############################################################################
  # ==== Tekst opisowy MAPY ==== ###############################################
  ##############################################################################
  output$tekst_opisowy <- renderUI({
    
    ############################################################################
    # ==== Opis główny ==== ####################################################
    opis_glowny <- case_when(
      input$kategoria == "Zawodnicy" & input$zakres == "aktualne" ~
        "Poniższa wizualizacja przedstawia zawodników z ważną licencją Polskiego Związku Badmintona. Są to osoby aktywne, uprawnione do udziału w oficjalnych rozgrywkach i reprezentujące bieżący potencjał sportowy poszczególnych województw.",
      input$kategoria == "Zawodnicy" & input$zakres == "wszyscy" ~
        "Poniżej przedstawiono dane obejmujące wszystkich zawodników – zarówno obecnie aktywnych, jak i tych, którzy posiadali licencję w przeszłości. To szersze spojrzenie pozwala uchwycić historyczne trendy i zmiany w zainteresowaniu badmintonem w różnych regionach.",
      input$kategoria == "Sędziowie" & input$zakres == "aktualne" ~
        "Wizualizacja pokazuje sędziów z aktualną licencją, którzy mogą obecnie prowadzić zawody zgodnie z regulaminem Polskiego Związku Badmintona. To aktualny stan kadry sędziowskiej w Polsce.",
      input$kategoria == "Sędziowie" & input$zakres == "wszyscy" ~
        "Zestawienie dotyczy wszystkich sędziów zarejestrowanych w systemie – zarówno tych czynnych, jak i tych, którzy posiadali licencję w przeszłości. Dzięki temu można obserwować zmiany w liczbie i strukturze kadry sędziowskiej na przestrzeni lat.",
      input$kategoria == "Liczba zawodników na jednego sędziego" & input$zakres == "aktualne" ~
        "Poniżej przedstawiono wskaźnik liczby aktywnych zawodników przypadających na jednego aktualnie licencjonowanego sędziego. To użyteczna miara pokazująca, jak wygląda relacja między uczestnikami, a wsparciem organizacyjnym w poszczególnych województwach.",
      input$kategoria == "Liczba zawodników na jednego sędziego" & input$zakres == "wszyscy" ~
        "W tej wersji wizualizacji pokazano ogólną relację między wszystkimi zawodnikami, a wszystkimi zarejestrowanymi sędziami – niezależnie od ich aktualnej aktywności. Pozwala to ocenić strukturę wsparcia organizacyjnego z szerszej, również historycznej perspektywy.",
      TRUE ~ "")
    
    ############################################################################
    # ==== Opis dodatkowy dla zawodników ==== ##################################
    opis_dodatkowy <- if (input$kategoria == "Zawodnicy" && input$kategoria_wiekowa != "wszyscy") {
      case_when(
        input$kategoria_wiekowa == "elita" ~ "Elita (E) to zawodnicy powyżej 18 roku życia, rywalizujący na najwyższym poziomie krajowym i często międzynarodowym. To kategoria najbardziej doświadczonych badmintonistów.",
        input$kategoria_wiekowa == "u23" ~ "Młodzieżowcy (U23) to zawodnicy w wieku od 19 do 23 lat, będący często pomostem między kategorią juniorską, a elitarną.",
        input$kategoria_wiekowa == "u19" ~ "Juniorzy (U19) obejmują zawodników w wieku 17–18 lat, będących często uczestnikami mistrzostw Polski i turniejów międzynarodowych w tej grupie wiekowej.",
        input$kategoria_wiekowa == "u17" ~ "Juniorzy Młodsi (U17) to grupa rozwijająca swoje umiejętności techniczne i taktyczne – ważny etap w ścieżce sportowego rozwoju.",
        input$kategoria_wiekowa == "u15" ~ "Młodzicy (U15) to jedna z kluczowych kategorii młodzieżowych, gdzie kształtowane są podstawy rywalizacji i profesjonalnego podejścia.",
        input$kategoria_wiekowa == "u13" ~ "Młodzicy Młodsi (U13) to zawodnicy rozpoczynający bardziej regularne treningi i pierwsze starty w oficjalnych zawodach krajowych.",
        input$kategoria_wiekowa == "u11" ~ "Żacy (U11) to najmłodsza kategoria licencjonowanych zawodników z regularnymi startami. Często jest to początek ich przygody z badmintonem.",
        input$kategoria_wiekowa == "u9" ~ "Żacy Młodsi (U9) to dzieci, które dopiero zaczynają uczestniczyć w szkoleniu i zabawach badmintonowych.",
        TRUE ~ "")} else {""}
    
    ############################################################################
    # ==== Opis dodatkowy dla sędziów ==== #####################################
    opis_dodatkowy_sedziowie <- if (
      input$kategoria == "Sędziowie" && input$zakres == "aktualne" && input$kategoria_sedziow != "wszyscy") {
      case_when(
        input$kategoria_sedziow == "Polscy_sedziowie_aktualni_Krajowi_s_dziowie_Prowadz_cy" ~
          "Sędziowie prowadzący klasy krajowej odpowiadają za prawidłowy przebieg meczu na boisku podczas turniejów krajowych. Ich zadaniem jest stosowanie przepisów gry i egzekwowanie zasad w czasie trwania meczu. Podlegają sędziemu głównemu turnieju.",
        input$kategoria_sedziow == "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_G_wni" ~
          "Sędziowie główni mają najwyższą odpowiedzialność za przebieg całego turnieju. Czuwają nad zgodnością rozgrywek z regulaminem, zarządzają zespołem sędziowskim i podejmują ostateczne decyzje w przypadku spornych sytuacji. Reprezentują PZBad wobec organizatora i uczestników.",
        input$kategoria_sedziow == "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Liniowi" ~
          "Sędziowie liniowi odpowiadają za obserwację linii boiska i wspierają sędziego prowadzącego w ocenie, czy lotka była w polu czy poza nim. Współtworzą zespół boiskowy razem z sędzią serwisowym i prowadzącym. Podlegają bezpośrednio sędziemu prowadzącemu i głównemu.",
        input$kategoria_sedziow == "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Prowadz_cy" ~
          "Sędziowie prowadzący klasy międzynarodowej lub państwowej są odpowiedzialni za prowadzenie meczu zgodnie z przepisami BWF i PZBad. Oprócz obowiązków boiskowych, często uczestniczą w turniejach wyższej rangi, w tym międzynarodowych.",
        input$kategoria_sedziow == "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli" ~
          "Sędziowie zwykli mogą pełnić funkcje liniowego, serwisowego lub prowadzącego – głównie na turniejach niższej rangi (rangi 3–5). To pierwszy poziom uprawnień sędziowskich umożliwiający aktywne uczestnictwo w turniejach krajowych.",
        input$kategoria_sedziow == "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli_g_wni" ~
          "Sędziowie zwykli – główni posiadają rozszerzone kompetencje. Mogą pełnić funkcję sędziego głównego na turniejach rangi 5, być jego zastępcą w turniejach rangi 3–5 oraz wykonywać zadania prowadzącego, serwisowego i liniowego.",
        TRUE ~ "")} else { "" }
    
    ############################################################################
    # Zwracanie HTML z łamaniem linii ##########################################
    HTML(paste(
      opis_glowny,
      if (opis_dodatkowy != "") paste0("<br>", opis_dodatkowy) else "",
      if (opis_dodatkowy_sedziowie != "") paste0("<br>", opis_dodatkowy_sedziowie) else "",
      "<br>&nbsp;",
      sep = ""))
  }) # Koniec tesktu do MAPY

    
  ##############################################################################
  # ==== MAPA ==== #############################################################
  ##############################################################################
  output$mapa_plot <- renderGirafe({
    mapa_trigger()
    
    if (is.null(input$window_width)) return(NULL)
    width_px <- input$window_width /2
    width_svg <- width_px / 96
    height_svg <- width_svg * 0.75
    
    counts <- dane_dla_mapy()
    if (is.null(counts)) return(NULL)
    
    plot_title <- reactive({
      if (input$kategoria == "Zawodnicy" && input$zakres == "aktualne") {
        "Zawodnicy z aktualną licencją"
      } else if (input$kategoria == "Zawodnicy" && input$zakres == "wszyscy") {
        "Wszyscy zawodnicy (niezależnie od ważności licencji)"
      } else if (input$kategoria == "Sędziowie" && input$zakres == "aktualne") {
        "Sędziowie z aktualną licencją"
      } else if (input$kategoria == "Sędziowie" && input$zakres == "wszyscy") {
        "Wszyscy sędziowie (niezależnie od ważności licencji)"
      } else if (input$kategoria == "Liczba zawodników na jednego sędziego" && input$zakres == "aktualne") {
        "Liczba zawodników na jednego sędziego z aktualnymi licencjami"
      } else if (input$kategoria == "Liczba zawodników na jednego sędziego" && input$zakres == "wszyscy") {
        "Liczba zawodników na jednego sędziego (niezależnie od ważności licencji)"
      } else {"Mapa"}})
    
    mapa_danych <- pol_sf %>%
      left_join(counts, by = c("NAME_1" = "Województwo"))
    
    centroidy_dopasowane <- centroidy %>% # dopasowujemy liczbę do punktu
      left_join(counts, by = "Województwo")
    
    mapa_danych <- mapa_danych %>%
      mutate(fill_color = "red")
    
    p_mapa <- ggplot(mapa_danych) +
      geom_sf_interactive(
        aes(
          fill = liczba,
          tooltip = paste0(NAME_1, ": ", liczba), 
          data_id = NAME_1),
        color = "grey90",
        size = width_px/1500
        ) +
      scale_fill_viridis_c( # dodana kolorystyka
        option = "viridis",
        na.value = "white",
        name = "Liczba wystąpień",
        guide = guide_colorbar(
          ticks.linewidth = width_px/900,  # grubość kresek
          ticks.length = width_px  # długość kresek
        )) +
      shadowtext::geom_shadowtext( # liczby zawodników na mapie, w środku województwa, z czarnym obramowaniem
        data = centroidy_dopasowane,
        aes(x = x, y = y, label = liczba),
        size = width_px/130,
        color = "white",
        bg.color = "black",
        bg.r = 0.1) +
      labs(title = str_wrap(plot_title(), width = width_px*2/30)) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),     # usuwa siatkę
        axis.text = element_blank(),      # usuwa etykiety osi (np. wartości stopni)
        axis.ticks = element_blank(),     # usuwa kreski osi
        axis.title = element_blank(),     # usuwa tytuły osi
        plot.title = element_text(size = (width_px/50), face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        legend.box = "vertical",
        legend.title = element_text(size = width_px/60, 
                                    margin = margin(b = width_px/60,r = width_px/75), 
                                    hjust = 0  # 0 = lewo, 1 = prawo, 0.5 = środek
                                    ),
        legend.text = element_text(size = width_px*4/300),
        legend.key.height = unit(width_px/1500, "cm"), 
        legend.key.width = unit(width_px/700, "cm"))
    
    pozwol_na_zaznaczanie <- FALSE
    if (input$kategoria == "Zawodnicy") {
      if (is.null(input$kategoria_wiekowa) || input$kategoria_wiekowa == "wszyscy") {
        pozwol_na_zaznaczanie <- TRUE}
    } else if (input$kategoria == "Sędziowie") {
      if (input$zakres == "aktualne" && 
          (!is.null(input$kategoria_sedziow) && input$kategoria_sedziow == "wszyscy")) {
        pozwol_na_zaznaczanie <- TRUE}}
    
    girafe(
      ggobj = p_mapa,
      width_svg = width_svg,
      height_svg = height_svg,
      options = list(
        opts_toolbar(saveaspng = FALSE),
        opts_zoom(min = 1, max = 1),
        opts_tooltip(css = "
          background-color: #000;
          color: #fff;
          padding: 8px;
          border-radius: 6px;
          font-size: 15px;
          font-weight: bold;
          box-shadow: 2px 2px 6px rgba(0, 0, 0, 0.3);"),
        opts_hover(css = "fill-opacity:width_px/2500;stroke-width:3px;cursor:pointer;"),
        if (pozwol_na_zaznaczanie) {
          opts_selection(type = "single", css = "stroke:red;stroke-width:2px;")
        } else {
          opts_selection(type = "none")}))
    }) # Koniec mapy
  

  ##############################################################################
  # ==== Dane reaktywne do SŁUPKOWEGO ==== #####################################
  ##############################################################################
  dane_dla_wykresu <- reactive({
    dane <- NULL
    tytul <- ""
    ety_pion <- "Liczba wystąpień"
    ety_poziom <- ""

    ############################################################################
    # ==== Sędziowie === #######################################################
    if (input$kol2_kogo == "Sędziowie") {
      
      ##########################################################################
      # ==== Sędziowie - wszyscy ==== ##########################################
      if (input$kol2_licencja == "wszyscy") {
        sedziowie_df <- sedziowie_polska_wszyscy
        sedziowie_df <- sedziowie_df %>%
          filter(!is.na(Rok_urodzenia)) %>%
          mutate(Wiek = lubridate::year(Sys.Date()) - Rok_urodzenia,
                 Przedzial = cut(Wiek, breaks = c(16, 26, 36, 46, 56, 65, 85), right = FALSE,
                                 labels = c("16 – 25", "26 – 35", "36 – 45", "46 – 55", "56 – 64", "65+")))
        przedzialy_wiekowe <- c("16 – 25", "26 – 35", "36 – 45", "46 – 55", "56 – 64", "65+")
        wszystkie_przedzialy <- data.frame(Przedzial = przedzialy_wiekowe)
        
        dane <- sedziowie_df %>%
          count(Przedzial, name = "Liczba") %>%
          right_join(wszystkie_przedzialy, by = "Przedzial") %>%
          mutate(
            Liczba = tidyr::replace_na(Liczba, 0),
            Przedzial = factor(Przedzial, levels = przedzialy_wiekowe)) %>%
          arrange(Przedzial)
        tytul      <- "Wiek wszystkich sędziów"
        ety_poziom <- "Przedział wiekowy"}
      
      ##########################################################################
      # ==== Sędziowie - aktualni ==== #########################################
      else {
        
        ########################################################################
        # ==== Sędziowie - aktualni - kategoria sędziów ==== ###################
        if (input$kol2_tryb == "Stopnie sędziowskie") {
          kolejnosc_sedziowie <- c(
            "Państwowi sędziowie Zwykli",
            "Państwowi sędziowie Liniowi",
            "Państwowi sędziowie Zwykli-główni",
            "Krajowi sędziowie Prowadzący",
            "Państwowi sędziowie Prowadzący",
            "Państwowi sędziowie Główni")
          
          dane <- aktualni_sedziowie_kategoria
          if (!is.null(input$kol2_woj_sedziowie) && input$kol2_woj_sedziowie != "Polska") {
            dane <- dane %>% filter(Województwo == input$kol2_woj_sedziowie)}
          dane <- dane %>% count(Kategoria, name = "Liczba") %>%
            mutate(
              Kategoria = factor(Kategoria, levels = kolejnosc_sedziowie)) %>%
            tidyr::complete(Kategoria, fill = list(Liczba = 0))
          tytul <- paste0("Liczba sędziów (z aktualną licencją) według rodzaju licencji – ", input$kol2_woj_sedziowie)
          ety_poziom <- "Stopień sędziowski"} 
        
        ########################################################################
        # ==== Sędziowie - aktualni - wiek sędziów ==== ########################
        else if (input$kol2_tryb == "Wiek sędziów") {
          
          ######################################################################
          # ==== Sędziowie - aktualni - wiek sędziów - kategoria sędziów =======
          sedziowie_df <- if (input$kol2_stopien == "wszyscy") aktualni_sedziowie_kategoria else get(input$kol2_stopien)
          
          
        sedziowie_df <- sedziowie_df %>%
          filter(!is.na(data_urodzenia)) %>%
          mutate(Wiek = lubridate::year(Sys.Date()) - as.integer(data_urodzenia),
                 Przedzial = cut(Wiek, breaks = c(16, 26, 36, 46, 56, 65, 85), right = FALSE,
                                 labels = c("16 – 25", "26 – 35", "36 – 45", "46 – 55", "56 – 64", "65+")))

        przedzialy_wiekowe <- c("16 – 25", "26 – 35", "36 – 45", "46 – 55", "56 – 64", "65+")
        wszystkie_przedzialy <- data.frame(Przedzial = przedzialy_wiekowe)
        
        dane <- sedziowie_df %>%
          count(Przedzial, name = "Liczba") %>%
          right_join(wszystkie_przedzialy, by = "Przedzial") %>%
          mutate(
            Liczba = tidyr::replace_na(Liczba, 0),
            Przedzial = factor(Przedzial, levels = przedzialy_wiekowe)) %>%
          arrange(Przedzial)
        
        tytuly_stopni <- c(
          "wszyscy" = "sędziów (z aktualną licencją)",
          "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli" = "państwowych sędziów zwykłych (z aktualną licencją)",
          "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Liniowi" = "państwowych sędziów liniowych (z aktualną licencją)",
          "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli_g_wni" = "państwowych sędziów zwykłych-głównych (z aktualną licencją)",
          "Polscy_sedziowie_aktualni_Krajowi_s_dziowie_Prowadz_cy" = "krajowych sędziów prowadzących (z aktualną licencją)",
          "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Prowadz_cy" = "państwowych sędziów prowadzących (z aktualną licencją)",
          "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_G_wni" = "państwowych sędziów głównych (z aktualną licencją)")
        if (input$kol2_stopien %in% names(tytuly_stopni)) {
          tytul <- paste("Wiek", tytuly_stopni[[input$kol2_stopien]])} 
        else {
          tytul <- "Wiek sędziów (z aktualną licencją)"}
        ety_poziom <- "Przedział wiekowy"}}} 
    
    
    ############################################################################
    # ==== Zawodnicy === #######################################################
    else if (input$kol2_kogo == "Zawodnicy") {
      zawodnicy_df <- zawodnicy_polska
      
      ##########################################################################
      # ==== Zawodnicy - aktualni ==== #########################################
      if (input$kol2_licencja == "aktualne") {
        zawodnicy_df <- zawodnicy_df %>%
          filter(Licencja > as.POSIXct(Sys.Date()))
        tytul <- paste("Liczba zawodników w kategorii wiekowej (z aktualną licencją) –", input$kol2_woj)
      }
      
      ##########################################################################
      # ==== Zawodnicy - region ==== ###########################################
      if (input$kol2_woj != "Polska") {
        zawodnicy_df <- zawodnicy_df %>%
          filter(Województwo == input$kol2_woj)}
      
      # Podział zawodników na kategorie wiekowe
      zawodnicy_df <- zawodnicy_df %>%
        mutate(Rok_urodzenia = lubridate::year(Data_urodzenia),
               Kategoria = case_when(
                 Rok_urodzenia >= 2017                     ~ "Żacy Młodsi (ŻM – U9)",
                 Rok_urodzenia >= 2015 & Rok_urodzenia <= 2016 ~ "Żacy (Ż – U11)",
                 Rok_urodzenia >= 2013 & Rok_urodzenia <= 2014 ~ "Młodzicy Młodsi (MM – U13)",
                 Rok_urodzenia >= 2011 & Rok_urodzenia <= 2012 ~ "Młodzicy (M – U15)",
                 Rok_urodzenia >= 2009 & Rok_urodzenia <= 2010 ~ "Juniorzy Młodsi (JM – U17)",
                 Rok_urodzenia >= 2007 & Rok_urodzenia <= 2008 ~ "Juniorzy (J – U19)",
                 Rok_urodzenia >= 2003 & Rok_urodzenia <= 2006 ~ "Młodzieżowcy (U23)",
                 Rok_urodzenia <= 2002                        ~ "Elita (E)",
                 TRUE ~ "Inni"
               ))
      
      kolejnosc_kategorii <- c(
        "Żacy Młodsi (ŻM – U9)",
        "Żacy (Ż – U11)",
        "Młodzicy Młodsi (MM – U13)",
        "Młodzicy (M – U15)",
        "Juniorzy Młodsi (JM – U17)",
        "Juniorzy (J – U19)",
        "Młodzieżowcy (U23)",
        "Elita (E)")
      zawodnicy_df <- zawodnicy_df %>%
        mutate(Kategoria = factor(Kategoria, levels = kolejnosc_kategorii))
      wszystkie_kategorie <- data.frame(Kategoria = kolejnosc_kategorii)
      
      dane <- zawodnicy_df %>%
        count(Kategoria, name = "Liczba") %>%
        right_join(wszystkie_kategorie, by = "Kategoria") %>%
        mutate(Liczba = replace_na(Liczba, 0),
               Kategoria = factor(Kategoria, levels = kolejnosc_kategorii)) %>%
        arrange(Kategoria)
      if (tytul == "") (tytul <- paste("Liczba zawodników w kategorii wiekowej –", input$kol2_woj))
      ety_poziom <- "Kategoria wiekowa"}
    
    # Zwracamy listę: dane + etykiety
    list(dane = dane, tytul = tytul, ety_poziom = ety_poziom, ety_pion = ety_pion)
  }) # Koniec danych do słupkowego
  
  
  ##############################################################################
  # ==== Tekst opisowy SŁUPKOWEGO ==== #########################################
  ##############################################################################
    output$tekst_opisowy_2 <- renderUI({
      
      ##############################################################################
      # ==== Główny opis w zależności od typu danych ==== #########################
      opis_glowny_kol2 <- case_when(
        input$kol2_kogo == "Zawodnicy" & input$kol2_licencja == "aktualne" ~
          "Poniżej przedstawiono dane dotyczące zawodników z aktualną licencją zarejestrowanych w systemie Polskiego Związku Badmintona. Wykres obejmuje wyłącznie osoby aktywne.",
        input$kol2_kogo == "Zawodnicy" & input$kol2_licencja == "wszyscy" ~
          "Poniżej przedstawiono dane dotyczące wszystkich zawodników zarejestrowanych w systemie Polskiego Związku Badmintona, zarówno aktywnych, jak i nieaktywnych.",
        input$kol2_kogo == "Sędziowie" & input$kol2_licencja == "aktualne" ~
          "Poniższa wizualizacja ukazuje sędziów, którzy posiadają aktualną licencję uprawniającą ich do prowadzenia zawodów. Dane można filtrować według typu uprawnień i sposobu prezentacji.",
        input$kol2_kogo == "Sędziowie" & input$kol2_licencja == "wszyscy" ~
          "Przedstawione zestawienie obejmuje wszystkich sędziów wpisanych do ewidencji PZBad – zarówno obecnie aktywnych, jak i tych, którzy zakończyli działalność. To pełny przekrój środowiska sędziowskiego w kraju.",
        TRUE ~ "")
      
      ##############################################################################
      # ==== Dodatkowy opis – wiek sędziów ==== ####################################
      opis_wiek <- if (input$kol2_kogo == "Sędziowie" &&input$kol2_licencja == "aktualne" &&input$kol2_tryb == "Wiek sędziów") {
        opis_kategorii <- case_when(
          input$kol2_stopien == "Polscy_sedziowie_aktualni_Krajowi_s_dziowie_Prowadz_cy" ~
            "Sędziowie prowadzący klasy krajowej odpowiadają za prowadzenie meczów na poziomie ogólnopolskim, często podczas centralnych wydarzeń sportowych.",
          input$kol2_stopien == "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_G_wni" ~
            "Sędziowie główni to osoby nadzorujące przebieg całego turnieju. Odpowiadają za organizację rozgrywek, kontrolę dokumentacji i podejmowanie ostatecznych decyzji na zawodach.",
          input$kol2_stopien == "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Liniowi" ~
            "Sędziowie liniowi są odpowiedzialni za obserwację linii boiska i wspomaganie sędziego prowadzącego w ocenie poprawności rozegrania lotki. Pełnią kluczową rolę przy najbardziej prestiżowych meczach.",
          input$kol2_stopien == "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Prowadz_cy" ~
            "Sędziowie prowadzący klasy państwowej uczestniczą w meczach wyższej rangi – w tym mistrzostwach Polski i wydarzeniach międzynarodowych – odpowiadając za przebieg gry na korcie.",
          input$kol2_stopien == "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli" ~
            "Sędziowie zwykli mogą pełnić funkcje boiskowe (liniowy, serwisowy, prowadzący) podczas lokalnych i regionalnych zawodów. Stanowią podstawę kadry sędziowskiej.",
          input$kol2_stopien == "Polscy_sedziowie_aktualni_Pa_stwowi_s_dziowie_Zwykli_g_wni" ~
            "Sędziowie zwykli – główni mogą kierować turniejami niskiej rangi oraz wspierać sędziów głównych w większych wydarzeniach. Łączą obowiązki techniczne i organizacyjne.",
          input$kol2_stopien == "wszyscy" ~
            "Wykres pokazuje strukturę wiekową sędziów aktywnych zawodowo. Dzięki temu można dostrzec, czy dominują osoby młode, czy też większy udział mają doświadczeni arbitrzy. Wybór kategorii sędziowskiej pozwala zawęzić analizę do konkretnych funkcji – np. sędziów głównych czy liniowych. Dane pozwalają też przewidzieć przyszłe potrzeby szkoleniowe.",
          TRUE ~ "")
        # Jeśli nie wybrano 'wszyscy', dodajemy ogólną końcówkę
        if (input$kol2_stopien != "wszyscy") {
          paste(
            opis_kategorii,
            "Wykres pokazuje strukturę wiekową sędziów aktywnych zawodowo. Dzięki temu można dostrzec, czy dominują osoby młode, czy też większy udział mają doświadczeni arbitrzy.")
        } else {
          opis_kategorii}
        } else { "" }
      
      ##############################################################################
      # ==== Dodatkowy opis – województwo dla zawodników ==== #############################
      opis_zawodnicy <- if (input$kol2_kogo == "Zawodnicy") {
        case_when(
          input$kol2_woj == "Polska" ~
            "Wykres przedstawia rozkład zawodników w skali całego kraju, co pozwala zobaczyć ogólną strukturę i rozmieszczenie aktywności sportowej.",
          TRUE ~ paste0(
            "Dane dotyczą zawodników z województwa ", input$kol2_woj,
            ". Pozwala to na analizę lokalnych różnic w liczbie zawodników oraz poziomie ich aktywności."))
        } else {""}
      
      ##############################################################################
      # ==== Dodatkowy opis – stopnie sędziowskie ==== #############################
      opis_stopnie <- if (input$kol2_kogo == "Sędziowie" && input$kol2_licencja == "aktualne" && input$kol2_tryb == "Stopnie sędziowskie") {
        # Dla wybranego województwa lub Polski
        opis_regionu <- case_when(
          input$kol2_woj_sedziowie == "Polska" ~
            "Wykres pokazuje rozkład aktualnych sędziów w podziale na stopnie i typy funkcji w skali całego kraju. To ogólna charakterystyka struktury sędziowskiej w Polsce – zarówno pod kątem liczby, jak i zróżnicowania funkcji.",
          TRUE ~ paste0(
            "Dane przedstawiają strukturę sędziów aktualnie zarejestrowanych w województwie: ",
            input$kol2_woj_sedziowie,
            ". Pokazano, jak rozkładają się funkcje – od sędziów liniowych po głównych – w lokalnej strukturze kadry."))
        opis_koncowy <- "Pozwala to zauważyć, w których regionach dominują sędziowie z wyższymi uprawnieniami, a gdzie mogą występować luki kompetencyjne. Taka wiedza wspiera decyzje o kierunkach szkoleń i rozwoju środowiska sędziowskiego."
        paste(opis_regionu, opis_koncowy)
      } else { "" }

      ##############################################################################
      # ==== Łączenie tekstów w całość (HTML) ==== #################################
      HTML(paste(
        opis_glowny_kol2,
        if (opis_wiek != "") paste0("<br>", opis_wiek) else "",
        if (opis_stopnie != "") paste0("<br>", opis_stopnie) else "",
        if (opis_zawodnicy != "") paste0("<br>", opis_zawodnicy) else "",
        "<br>&nbsp;",
        sep = ""
      ))
    }) # Koniec tekstu do słupkowego
  
  
  ##############################################################################
  # ==== Wykres słupkowy ==== ##################################################
  ##############################################################################
  output$kol2_wykres <- renderGirafe({
    col_trigger()
    
    wykres <- dane_dla_wykresu()
    width_px <- input$window_width /2
    width_svg <- width_px / 96
    height_svg <- width_svg * 0.6

    if (is.null(input$window_width)) return(NULL)
    
    if (!is.null(wykres$dane)) {
      x_col <- names(wykres$dane)[1]
      p_slupkowy <- ggplot(wykres$dane, aes(x = .data[[x_col]], y = Liczba)) +
        geom_col_interactive(aes(
            tooltip = paste0(.data[[x_col]], ": ", Liczba),
            data_id = .data[[x_col]]),
            fill = if (!is.null(selected_region())) {"green"} else {"#0072E9"},
            width = width_px/1600) +
        labs( title = str_wrap(wykres$tytul, width = width_px*16/300),
              x = wykres$ety_poziom,
              y = wykres$ety_pion) +
        theme_bw() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
        theme(axis.text.x = element_text(angle = 40, hjust = 1, color = 'black', size = width_px*17/1500
                                         ),
              axis.text.y = element_text(color = 'black', size = width_px*17/1500
                                         ),
          panel.grid = element_blank(),     # usuwa siatkę
          panel.grid.major.x = element_line(color = "gray", size = width_px/6000
                                            ),
          panel.grid.major.y = element_line(color = "gray", size = width_px/6000
                                            ),
          plot.title = element_text(size = width_px/50, face = "bold", hjust = 0.5),
          axis.title = element_text(size = width_px/60),  # Osie
          axis.title.y = element_text(margin = margin(r = width_px/150)),
          panel.border = element_rect(color = "black", fill = NA, size = width_px/3000),
          axis.title.x = element_text(margin = margin(t = width_px/150)))
      width_px <- width_px * 2
      girafe(
        ggobj = p_slupkowy,
        width_svg = width_svg,
        height_svg = height_svg,
        options = list(
          opts_toolbar(saveaspng = FALSE),
          opts_zoom(min = 1, max = 1),
          opts_tooltip(css = "
            background-color: #000;
            color: #fff;
            padding: 8px;
            border-radius: 6px;
            font-size: 15px;
            font-weight: bold;
            box-shadow: 2px 2px 6px rgba(0, 0, 0, 0.3);"),
          opts_hover(css = "fill:orange;stroke-width:1.5px;cursor:pointer;"),
          opts_selection(type = "none")))
    }
  }) # Koniec wykresu słupkowego

################################################################################
################################################################################
# ==== Część druga ==== ########################################################
################################################################################
################################################################################
    
    output$tekstbox <- renderText({
      "Na wykresie obok jest przedstawiony rozkład wzrostu zawodników ze względu
       na kraje i płeć."
    })
    
    output$continentPlot <- renderLeaflet({
      validate(
        need(length(input$Kontynent) > 0, "Proszę podać co najmniej jeden kontynent")
      )
      
      kontynent_ang <- kontynenty_polskie[input$Kontynent]
      
      if (kontynent_ang == "World") {
        return(renderWorldMap())
      } else {
        return(renderContinentMap(kontynent_ang))
      }
    }) %>% bindCache(input$Kontynent, input$Rok)
    
    renderWorldMap <- function() {
      filtered <- merged_olimp_world %>% 
        filter(between(Year, input$Rok[1], input$Rok[2]))
      
      filtered_sum <- filtered %>% 
        group_by(country_name, geometry, TeamPL) %>%
        summarise(ilosc_zawodnikow = sum(ilosc_zawodnikow), .groups = "drop")
      
      pal <- colorNumeric("YlOrRd", domain = filtered_sum$ilosc_zawodnikow)
      
      leaflet(filtered_sum) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(ilosc_zawodnikow),
                    weight = 1, color = "white", fillOpacity = 0.7,
                    label = ~paste(TeamPL, ":", ilosc_zawodnikow)) %>%
        addLegend(position = "bottomright", pal = pal, values = filtered_sum$ilosc_zawodnikow, title = "Liczba zawodników") %>%
        setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85)
    }
    
    renderContinentMap <- function(kontynent_ang) {
      bbox <- bounding_boxes[[kontynent_ang]]
      
      filtered <- merged_olimp_world %>% 
        filter(continent == kontynent_ang,
               between(Year, input$Rok[1], input$Rok[2]))
      
      filtered_sum <- filtered %>% 
        group_by(country_name, geometry, TeamPL) %>%
        summarise(ilosc_zawodnikow = sum(ilosc_zawodnikow), .groups = "drop")
      
      pal <- colorNumeric("YlOrRd", domain = filtered_sum$ilosc_zawodnikow)
      
      leaflet(filtered_sum, options = leafletOptions(minZoom = 2, maxZoom = 7)) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(ilosc_zawodnikow),
                    weight = 1, color = "white", fillOpacity = 0.7,
                    label = ~paste(TeamPL, ":", ilosc_zawodnikow)) %>%
        addLegend(position = "bottomright", pal = pal, values = filtered_sum$ilosc_zawodnikow, title = "Liczba zawodników") %>%
        setMaxBounds(lng1 = bbox$lng1, lat1 = bbox$lat1, lng2 = bbox$lng2, lat2 = bbox$lat2) %>%
        fitBounds(lng1 = bbox$lng1, lat1 = bbox$lat1, lng2 = bbox$lng2, lat2 = bbox$lat2)
    }
    
    output$heightboxPlot <- renderPlotly({
      width_px <- input$window_width /2
      
      validate(
        need(length(input$Druzyna) > 0, "Proszę podać conajmniej jeden kraj."),
        need(length(input$Plec) > 0, "Proszę podać płeć.")
      )
      
      
      olimpiada_badminton_bez_NA <- olimpiada_badminton %>% 
        filter(!is.na(Height), Team %in% input$Druzyna, Sex %in% input$Plec) %>% 
        mutate(Team = as.character(Team), Height = as.numeric(Height)) %>%
        left_join(druzyny, by = "Team")
      
      
      plot2 <- ggplot(olimpiada_badminton_bez_NA, 
                      aes(x = TeamPL.x, y = Height, fill = Sex, group = Sex)) + 
        geom_boxplot() + facet_wrap(~Sex) +
        labs(title = "Rozkład wzrostu ze względu na kraje i płeć",
             x = "Kraj",
             y = "Wzrost",
             fill = "Płeć") +
        scale_fill_manual(values = c("Mężczyzna" = "lightblue", "Kobieta" = "green")) +
        coord_cartesian(ylim =
                          c(min(olimpiada_badminton_bez_NA$Height), max(olimpiada_badminton_bez_NA$Height))) + 
        theme(axis.text.x = element_text(angle = 20, size = width_px/70),
              axis.title = element_text(size = width_px/65),  # Osie
              plot.title = element_text(size = width_px/50, face = "bold"),
              panel.background = element_rect(fill = "gray90", color = "black"),
              plot.background = element_rect(fill = "gray90", color = "black"),
              panel.grid.major = element_line(colour = "white", linetype = 3),
              legend.title = element_text(size = width_px / 65, face = "bold"),
              legend.text = element_text(size = width_px / 70),
              strip.background = element_rect(fill = NA, color = NA),
              strip.text = element_text(colour = NA))
      ggplotly(plot2)
      
    }) %>% bindCache(input$Druzyna, input$Plec)
    
################################################################################
################################################################################
# ==== Część trzecia ==== ######################################################
################################################################################
################################################################################
    
    output$podium_plot <- renderGirafe({
      df <- badminton_df %>%
        filter(Year == input$year_podium,
               Event == input$event_podium,
               !is.na(Medal)) %>%
        distinct(Name, Medal, NOC)
      
      if (nrow(df) == 0) {
        no_data_plot <- ggplot() +
          annotate("text", x = 1, y = 1, label = "Brak danych dla wybranego roku i konkurencji", size = 6, fontface = "bold") +
          theme_void() +
          coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
        return(girafe(ggobj = no_data_plot))
      }
      
      dfg <- df %>%
        group_by(Medal) %>%
        summarise(names = paste(Name, collapse = ",\n"),
                  noc   = unique(NOC)[1],
                  .groups = "drop") %>%
        mutate(
          x    = case_when(Medal == "Gold"   ~ 2,
                           Medal == "Silver" ~ 1,
                           TRUE              ~ 3),
          height = case_when(Medal == "Gold"   ~ 3.5,
                             Medal == "Silver" ~ 2.5,
                             TRUE              ~ 1.5),
          xmin = x - 0.4, xmax = x + 0.4,
          ymin = 0,       ymax = height,
          kolor = c(Gold = "#FFD700", Silver = "#C0C0C0", Bronze = "#CD7F32")[Medal],
          wrapped = str_wrap(names, 15),
          lines = str_count(wrapped, "\n") + 1,
          text_y = ymax + 0.5 + (lines - 1) * 0.1
        )
      dfg <- dfg %>%
        mutate(kraj_pl = noc_to_polish[noc])
      dfg <- dfg %>%
        mutate(
          wrapped_names = str_wrap(names, 15),
          line_count = str_count(wrapped_names, "\n") + 1,
          text_y = ymax + 0.5 + (line_count - 1) * 0.22  # ten offset i wysokość linii
        )
      
      p <- ggplot(dfg) +
        geom_rect_interactive(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                  fill = kolor,
                                  tooltip = paste0("Kraj: ", kraj_pl)),
                              color = "black", size = 1, alpha = 0.9) +
        geom_text(aes(x = x, y = text_y, label = wrapped_names),
                  size = 5, fontface = "bold", lineheight = 0.9, vjust = 1) +
        scale_fill_identity() +
        coord_cartesian(xlim = c(0.5, 3.5), ylim = c(0, 5), expand = FALSE) +
        theme_void()
      
      # Dodajemy flagi przez annotation_custom (nieinteraktywne)
      for (i in seq_len(nrow(dfg))) {
        fg <- get_flag_grob(dfg$noc[i])
        if (is.null(fg)) next
        
        box_w <- dfg$xmax[i] - dfg$xmin[i]
        flag_w <- 0.90 * box_w  # 90%
        center_x <- (dfg$xmin[i] + dfg$xmax[i]) / 2
        
        xmin_flag <- center_x - flag_w / 2
        xmax_flag <- center_x + flag_w / 2
        
        ymin_flag <- dfg$ymin[i]
        ymax_flag <- dfg$ymax[i]
        
        p <- p + annotation_custom(fg,
                                   xmin = xmin_flag,
                                   xmax = xmax_flag,
                                   ymin = ymin_flag,
                                   ymax = ymax_flag)
      }
      
      girafe(ggobj = p,
             options = list(
               opts_tooltip(css = "background-color: lightgray; padding:4px;")
             ))
    })
    
    
    
    output$trend_plot <- renderPlotly({
      df <- badminton_df
      
      if (input$country_trend != "Wszystkie") {
        df <- df %>% filter(NOC == input$country_trend)
      }
      
      if (input$event_trend != "Wszystkie") {
        df <- df %>% filter(Event == input$event_trend)
      }
      
      df <- df %>%
        filter(Year >= input$year_range[1], Year <= input$year_range[2]) %>%
        distinct(ID, Year, NOC) %>%
        count(Year, NOC)
      
      if (nrow(df) == 0) {
        return(plotly_empty() %>% layout(title = "Brak danych dla wybranych filtrów"))
      }
      
      df$Kraj <- noc_to_polish[df$NOC]
      df$Kraj[is.na(df$Kraj)] <- df$NOC[is.na(df$Kraj)]
      
      df_total <- df %>%
        group_by(Year) %>%
        summarise(n = sum(n), .groups = "drop")
      
      p <- ggplot(df_total, aes(x = factor(Year), y = n,
                                text = paste("Rok:", Year, "<br>Liczba zawodników:", n))) +
        geom_bar(stat = "identity", fill = "#1f77b4") +
        labs(title = "Liczba zawodników w czasie",
             x = "Rok", y = "Liczba zawodników") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5))
      
      ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
    
} # Koniec server

# ==== Uruchom aplikację ====
shinyApp(ui = ui, server = server)
