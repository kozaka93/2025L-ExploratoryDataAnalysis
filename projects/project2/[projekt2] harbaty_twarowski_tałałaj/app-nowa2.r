# biblioteki

library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(DT)
library(purrr)
library(tidyr)
library(shinydashboard)



# dane Michal
df2 <- read.csv("sprzatniete.csv")

df2$main_genre <- sapply(df2$genres, function(x) {
  cleaned <- gsub("\\[|\\]|'", "", x)
  parts <- strsplit(cleaned, ",")[[1]]
  trimws(parts[1])
})

# dane Kamil
books1 <- read.csv("sprzatniete.csv") #Dane z goodreads
books1$gatunek <- str_match(books1$genres, "\\['([^']+)")[,2]


books_sales <- read.csv("Books_Data_Clean.csv") #dataset o sprzedażach
books_sales <- books_sales %>%
  mutate(period = case_when(
    Publishing.Year < 1900 ~ "przed 1900",
    Publishing.Year >= 1900 & Publishing.Year <= 1950 ~ "1901–1950",
    Publishing.Year > 1950 & Publishing.Year <= 1980 ~ "1951–1980",
    Publishing.Year > 1980 & Publishing.Year <= 2000 ~ "1981–2000",
    Publishing.Year > 2000 & Publishing.Year <= 2010 ~ "2001–2010",
    Publishing.Year > 2010 & Publishing.Year <= 2020 ~ "2011–2020",
    Publishing.Year > 2020 ~ "po 2021"
  ))
books_sales <- books_sales %>% mutate(genre = case_when(
  genre == "genre fiction" ~"fiction",
  TRUE ~ genre
))

# dane Marysia

df <- read.csv("books.csv")

df$year <- str_extract(df$publication_date, "\\b\\d{4}\\b")
df$year <- as.numeric(df$year)
df$title_short <- str_trunc(df$title, width = 35) 
df$average_rating <- as.numeric(df$average_rating)
df1 <- df %>% 
  count(year) %>% 
  filter(n >= 5)
books <- df %>% 
  filter(year %in% df1$year) %>% 
  filter(!is.na(year)) %>% 
  filter(ratings_count> 10) %>% 
  mutate(period = case_when(
    year < 1900 ~ "przed 1900",
    year >= 1900 & year <= 1950 ~ "1901–1950",
    year > 1950 & year <= 1980 ~ "1951–1980",
    year > 1980 & year <= 2000 ~ "1981–2000",
    year > 2000 & year <= 2010 ~ "2001–2010",
    year > 2010 & year <= 2020 ~ "2011–2020",
    year > 2020 ~ "po 2021"
  )) %>% 
  group_by(period) %>%
  arrange(desc(average_rating)) %>%
  slice_head(n = 5) %>%
  ungroup()

data <- read.csv('bestsellers with categories.csv')
sprzatniete <-  read.csv('sprzatniete.csv')

sprzatniete$main_genre <- sub("^\\['([^']+)'.*", "\\1", sprzatniete$genres)
sprzatniete$main_genre <- factor(sprzatniete$main_genre)


# Trzeba więc zrobić bins do heatmapy
sprzatniete <- sprzatniete %>% 
  mutate(rating_bin = cut(rating, breaks = seq(1, 5, by = 0.5), include.lowest = TRUE)) %>% 
  filter()


# dane ostatnie (mam nadzieje)
#plik od Michała sprzątnięte
books_last <- read.csv("sprzatniete.csv")

books_last <- books_last %>% 
  mutate(
    gatunek_wek = str_extract_all(genres, "'(.*?)'") %>% 
      map(~str_remove_all(., "'")) %>% 
      map(~.[1:4]))  # wybierz 4 pierwsze elementy ze stringa z gatunkami

#do wykresu z popularnością
#posumuję liczbę ocen na każde wystąpienie gatunku

df_gatunki_last <- books_last %>%
  unnest(gatunek_wek) %>%       # rozwija listę gatunków na osobne wiersze
  group_by(gatunek_wek) %>%     # grupuje po nazwie gatunku
  summarise(totNumRatings = sum(numRatings, na.rm = TRUE)/1e6) %>% 
  filter(!(gatunek_wek %in% c("Inne","Fiction","Nonfiction","Historical Fiction")))

gat <- df_gatunki_last %>%  arrange(desc(totNumRatings)) %>%
  slice_head(n = 15) %>% select(gatunek_wek)

#do wykresu o stronach

# Tutaj jest błąd i wychodza nan
df_pages_last <- books_last %>% mutate(pages=as.numeric(pages)) %>% filter(!is.na(pages)) %>% 
  unnest(gatunek_wek) %>% left_join(df_gatunki_last, join_by(gatunek_wek)) %>% 
  filter(pages <=2000) %>% 
  filter(totNumRatings>=30 & !(gatunek_wek %in% c("Inne","Fiction","Nonfiction","Historical Fiction")))



##################


dark_palette <- c(
  "#0B3D91",  # ciemny niebieski
  "#2F4F4F",  # ciemny grafitowy (dark slate gray)
  "#4B0082",  # indygo
  "#3B3B6D",  # ciemny lawendowy granat
  "#556B2F",  # ciemny oliwkowy zielony
  "#800000"
)

add_alpha <- function(col, alpha=0.3) {
  alpha_hex <- sprintf("%02X", round(alpha * 255))
  paste0(col, alpha_hex)
}


ui <- dashboardPage(
  dashboardHeader(title = "Analiza dotycząca książek",
                  titleWidth = 360),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gatunki książek", tabName = "violin", icon = icon("chart-area")),
      menuItem("Data publikacji", tabName = "price", icon = icon("dollar-sign")),
      menuItem("Popularność i języki", tabName = "popularity", icon = icon("globe")),
      width = 25
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto&display=swap", rel = "stylesheet"),
      tags$style(HTML("
    body, .content-wrapper, .main-header, .main-sidebar,
.box, .box-body, .box-header, h1, h2, h3, h4, h5, p, span, div {
  font-family: 'Roboto', sans-serif !important;
}
"))
    ),
    tabItems(
      # 1. Violin plot
      tabItem(tabName = "violin",
              fluidRow(
                box(
                  title = "Wybierz gatunki książek",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("selected_genres", label = NULL,
                              choices = gat,
                              multiple = TRUE,
                              selected = c("Fantasy", "Classics"))
                )
              ),
              fluidRow(
                box(
                  title = "Rozkład ocen książek według gatunków",
                  width = 6,
                  status = "primary",
                  plotOutput("violinPlot"),
                  height = 500
                ),
                box(
                  title = "Rozkład liczby stron według gatunków",
                  plotOutput("gat_pagesPlot", height = 440),
                  status = "primary", 
                  width = 6,
                  height = 500
                )),
              
              fluidRow(
                box(
                  title = "Top 5 najwyżej ocenianych książek w wybranych gatunkach",
                  DT::dataTableOutput("topBooksTable1"),
                  status = "primary",
                  width = 6
                ),
                box(
                  title = "Opis",
                  width = 6,
                  status = "info",
                  solidHeader = FALSE,
                  p("Jeśli chodzi o rozkład ocen po gatunkach, z pewnością oceny fantastyki rozkładają się najszerzej, jako jeden z gatunków najpopularniejszych. Średnio najwyższe oceny osiągają książki historyczne, literatura współczesna czy książki dla dzieci. Z liczniej reprezentowanych gatunków słabo wypada Chick-lit, czyli lekkie powieści skierowane do nastolatek czy młodych kobiet o wartościach raczej czysto rozrywkowych."),
                  p("Spodziewanym było, że gatunkiem o średnio najkrótszych wydaniach będzie kategoria Childrens, ale drugim zdecydowanie średnio najkrótszym gatunkiem okazuje się być Klasyka. Jest to spowodowane częściowo tym, że mnóstwo klasyki z datasetu powstawało w XIX i XX wieku, kiedy to często krótsze wydania były preferowane ze względu na koszty produkcji i łatwość dystrybucji. Jeśli chodzi o gatunki średnio najdłuższe, przeważają Thrillery.")
                )
              )
              ),
      
      
      # 2. Cena a ocena
      tabItem(
        tabName = "price",
        fluidRow(
          box(
            title = "Opcje filtrowania",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            selectInput("period", "Wybierz okres publikacji:",
                        choices = sort(unique(na.omit(books_sales$period))),
                        selected = "2001–2010"),
            checkboxGroupInput("kategoria", "Wybierz kategorie:",
                               choices = c("fiction","nonfiction"),
                               selected = "fiction"),
            checkboxInput("splitSmooth", "Osobna linia trendu dla każdej kategorii", value = FALSE)
          )
        ),
        fluidRow(
          box(
          plotOutput("pricePlot"),
          status = "primary",
          title = "Zależnośc pomiędzy ceną a oceną oraz gatunkiem" ),
          box(
            title = "Najlepiej oceniane książki z danego okresu czasowego",
            DT::dataTableOutput("topBooksTable"),
            status = "primary"
          ),
          box(
            title = "Opis",
            width = 6,
            status = "info",
            solidHeader = FALSE,
            p("W kategorii Fikcja niekoniecznie widać wyraźną tendencję jeśli chodzi o wpływ ceny na ocenę książki, jednak to w okolicach kwoty 5$ oceny w większości okresów lat są średnio najwyższe. Sprawa ma się nieco inaczej w przypadku mniej licznie reprezentowanej literatury Faktu, gdzie ocena wydaje się średnio rosnąć wraz ze wzrostem ceny wydawnictwa, szczególnie w trochę lepiej reprezentowanych latach w XXI wieku.")
          )
        )
        
      ),
      
      # 3. zmergowane 3 ostatnie
      tabItem(
        tabName = "popularity",
        
        # Sekcja 1: Rozkład ocen wg kraju i gatunku
        # Sekcja 2: Ilość wystąpień najpopularniejszych gatunków wg języka
        fluidRow(
          box(
            title = "Opcje filtrowania (język i gatunek)",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            selectInput("selected_language2", "Wybierz język:",
                        choices = c("English", "Polish", "Italian", "Russian", "Spanish", "Turkish", "German", "French", "Portuguese"),
                        selected = "English"),
            uiOutput("dynamic_genre_selector")
          )
        ),
        fluidRow(
          box(
            title = "Rozkład ocen według kraju i gatunku",
            width = 6,
            status = "primary",
            plotOutput("countryGenreViolinPlot")
          ),
          
          box(
            title = "Ilość wystąpień najpopularniejszych gatunków w wybranym języku",
            width = 6,
            status = "primary",
            plotOutput("countPlot")
          )
        ),
        
        
        # Sekcja 3: Popularność gatunków
        fluidRow(
          box(
            title = "Opcje filtrowania",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            numericInput("n",
                         "Liczba pozycji w rankingu:",
                         min = 3,
                         max = 40,
                         value = 10)
          )
          
          
        ),
        fluidRow(
          box(
            title = "Popularność gatunków ogólnie",
            width = 6,
            status = "primary",
            plotOutput("gat_popPlot")
          ),
          box(
            title = "Opis",
            width = 6,
            status = "info",
            solidHeader = FALSE,
            p("Skupmy się na najbardziej wpływowych językach, czyli przede wszystkim na angielskim. W jego wypadku, co nie dziwi, tendencje wyglądają bardzo podobnie jak te z zestawienia ogólnego. Polska bardzo nisko stoi w romansach, za to przodujemy w Sci-FI. Jeśli chodzi o romanse, dobrze radzi sobie język Niemiecki. Mało reprezentowany język Rosyjski z kolei ma średnio oceny najwyższe."),
            p("Fantastyka we wszystkich językach powtarza się jako bardzo częsty gatunek, do najpopularniejszych gatunków ogólnie należą jeszcze Young Adult, Romanse, Klasyka oraz literatura współczesna. Jeśli chodzi o ciekawe charakterystyki dla danych języków to tym hiszpański i portugalski mają dużo romansów, włoski, niemiecki oraz francuski mają wiele klasyki, natomiast rosyjski obfituje w Sci-Fi.")
          )
          
        )
        
  
      )
      
    )
  )
)


server <- function(input, output) {
  
  # Violin Plots side by side
  output$violinPlot <- renderPlot({
    req(input$selected_genres)
    data_filtered <- books1 %>%
      filter(gatunek %in% input$selected_genres)
    
    selected <- unique(data_filtered$gatunek)
    palette_length <- length(dark_palette)
    colors_for_selected <- rep(dark_palette, length.out = length(selected))
    kolorystyka <- setNames(colors_for_selected, selected)
    
    ggplot(data_filtered, aes(x = gatunek, y = rating, fill = gatunek)) +
      geom_violin(alpha = 0.3, color = NA) +
      geom_boxplot(aes(color = gatunek), width = 0.1, outlier.shape = 16,
                   alpha = 0.5, outlier.size = 2, outlier.alpha = 0.8) +
      scale_fill_manual(values = kolorystyka) +
      scale_color_manual(values = kolorystyka)+
      labs(
           x = "Gatunek", y = "Ocena", fill = "Gatunek", color = "Gatunek") +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15)
      )
      
  })
  
  #tabela nizej
  output$topBooksTable1 <- renderDT({
    req(input$selected_genres)
    
    top_books <- books1 %>%
      filter(gatunek %in% input$selected_genres) %>%
      arrange(desc(rating)) %>%
      slice_head(n = 5) %>%
      select(Tytuł = title, Gatunek = gatunek, Ocena = rating)
    
    datatable(
      top_books,
      options = list(
        dom = 't',
        paging = FALSE
      ),
      rownames = FALSE
    )
  })
  
  # Price vs Rating
  output$pricePlot <- renderPlot({
  req(input$period)
  req(input$kategoria)
  
  filtered_data <- books_sales %>%
    filter(period %in% input$period,
           genre %in% input$kategoria)
  
  if (nrow(filtered_data) == 0) return(NULL)
  
  selected <- unique(filtered_data$genre)
  
  # Paleta 3 kontrastujących odcieni niebieskiego
  palette <- c("#CC6600", "#2171B5")
  
  # Przydziel kolory cyklicznie do gatunków
  colors_for_selected <- rep(palette, length.out = length(selected))
  kolorystyka <- setNames(colors_for_selected, selected)
  
  p <- ggplot(filtered_data, aes(x = sale.price, y = Book_average_rating)) +
    geom_point(size = 3.5, aes(color = genre), alpha = 0.8) +
    labs(x = "Cena ($)", y = "Ocena 1–5", color = "Kategoria") +
    theme_minimal() +
    coord_cartesian(ylim = c(3, 5)) +
    scale_color_manual(values = kolorystyka)+
    theme(
      plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 13),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 15)
    )
  
  if (isTRUE(input$splitSmooth)) {
    p <- p + geom_smooth(linewidth=0.8,aes(group = genre, color = genre), se = TRUE)
  } else {
    p <- p + geom_smooth(linewidth=0.8,se = TRUE, color = "black")
  }
  
  p
})

  
  
  # Top Books by Decade (table)
  output$topBooksTable <- DT::renderDataTable({
    req(input$period)
    
    top_books <- books %>%
      filter(period == input$period) %>%
      arrange(desc(average_rating)) %>%
      head(10)
    
    top_books %>%
      select(Tytuł = title_short,
             Autorzy = authors,
             Ocena = average_rating)
  }, options = list(
    dom = 't',
    responsive = TRUE
  ))
  
  output$topBooksPlot <- renderPlot({
    req(input$selected_year)
    
    top_books <- books %>%
      filter(dekada == input$selected_year) %>%
      arrange(desc(average_rating)) %>%
      head(10)
    
    ggplot(top_books, aes(x = reorder(title_short, average_rating), y = average_rating)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
           x = "Tytuł",
           y = "Ocena") +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15)
      )
  })

  
  filtered_data <- reactive({
    req(input$selected_language2)
    
    df2 %>%
      filter(language == input$selected_language2) %>%
      count(main_genre, name = "count_n") %>%
      arrange(desc(count_n)) %>%
      slice_head(n = 5)
  })
  
  # Plot: Count of books per genre
  
  output$countPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = reorder(main_genre, -count_n), y = count_n)) +
      geom_col(fill = "steelblue") +
      scale_y_continuous(expand = c(0, 0)) +
      labs(
           x = "Gatunek", y = "Liczba książek") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 13),
            axis.title = element_text(size = 18),
            axis.text.y = element_text(),
            plot.margin = margin(10, 30, 50, 30)) +
      theme_minimal()
  })
  
  
  # Genre ratings boxplot by country
  output$dynamic_genre_selector <- renderUI({
    req(input$selected_language2)
    
    available_genres <- books1 %>%
      filter(language == input$selected_language2) %>%
      distinct(gatunek) %>%
      arrange(gatunek) %>%
      pull(gatunek)
    
    selectInput("selected_genres_country", "Wybierz gatunki:",
                choices = gat,
                multiple = TRUE,
                selected = c("Fantasy","Classics") )
  })
  
  output$countryGenreViolinPlot <- renderPlot({
    req(input$selected_language2, input$selected_genres_country)
    
    data_filtered <- books1 %>%
      filter(language == input$selected_language2,
             gatunek %in% input$selected_genres_country)
    
    selected <- unique(data_filtered$gatunek)
    palette_length <- length(dark_palette)
    colors_for_selected <- rep(dark_palette, length.out = length(selected))
    kolorystyka <- setNames(colors_for_selected, selected)
    
    ggplot(data_filtered, aes(x = gatunek, y = rating, fill = gatunek)) +
      geom_boxplot(aes(color = gatunek), size=0.8, width = 0.5, outlier.shape = 16,
                   alpha = 0.5, outlier.size = 2, outlier.alpha = 0.8, color = "black") +
      labs(
           x = "Gatunek", y = "Ocena", fill = "Gatunek", color = "Gatunek") +
      theme_minimal() +
      scale_fill_manual(values = kolorystyka)+
      theme(
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15)
      )
  })
  
  ###koncowka
  output$gat_popPlot <- renderPlot({
    df_gatunki_last  %>% slice_max(totNumRatings, n = input$n) %>% 
      ggplot(aes(x=reorder(gatunek_wek,totNumRatings),y=totNumRatings)) + 
      geom_col(fill="steelblue")+theme_minimal()+
      labs(x="Gatunek",y="liczba ocen (mln)") +
      theme(
        plot.title = element_text(size=30,hjust = 0.5, face = "bold"), axis.text.x = element_text(size=10,angle=45,vjust=1.2,hjust=0.8),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 13),
        legend.title = element_blank()
      )+
      scale_y_continuous(breaks=seq(0,700,by=50))
  })
  
  output$gat_pagesPlot <- renderPlot({
    req(input$selected_genres)
    
    data_filtered <- df_pages_last %>%
      filter(gatunek_wek %in% input$selected_genres) %>%
      select(title, pages, gatunek_wek)
    
    selected <- unique(data_filtered$gatunek_wek)
    palette_length <- length(dark_palette)
    colors_for_selected <- rep(dark_palette, length.out = length(selected))
    colors_with_alpha <- sapply(colors_for_selected, add_alpha, alpha = 0.3)
    kolorystyka <- setNames(colors_with_alpha, selected)
    
    ggplot(data_filtered, aes(y = pages, x = gatunek_wek, fill = gatunek_wek)) +
      geom_boxplot(size = 0.5, color = "black") +  # dodałem color "black" dla obrysu
      scale_y_continuous(limits = c(100, 500)) +
      scale_fill_manual(values = kolorystyka) +
      theme_minimal() +
      labs(x = "Gatunek", y = "Liczba stron", fill = "Gatunek")+
      theme(
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15)
      )
      
  })
  
}

shinyApp(ui = ui, server = server)

