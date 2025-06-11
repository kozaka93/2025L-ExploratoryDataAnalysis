library(shiny)
library(bslib)
library(tidyverse)
library(readr)
library(janitor)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)

# ----- DATA LOADING -----
## Steam data
steam_url <- "https://raw.githubusercontent.com/youngopos/WED_Projekt2/main/Dane/Steam_cleaned/cleaned_steam.csv"
steam <- read_csv(steam_url, show_col_types = FALSE) %>% 
  clean_names() %>%
  mutate(
    price = as.numeric(gsub(",", ".", gsub("[^0-9.,]", "", price))),
    score = as.numeric(score),
    score_number = as.numeric(score_number)
  )

steam_tags <- steam %>%
  filter(!is.na(tags), !is.na(score)) %>%
  mutate(tags = strsplit(tags, "/")) %>%
  unnest(tags) %>%
  mutate(tags = trimws(tags))

unique_tags <- sort(unique(steam_tags$tags))

steam_pub <- steam %>%
  filter(!is.na(publisher) & publisher != "") %>%
  separate_rows(publisher, sep = "/") %>%
  mutate(publisher = str_trim(publisher))

publisher_stats <- steam_pub %>%
  group_by(publisher) %>%
  summarise(
    number_of_games = n(),
    average_score = mean(score, na.rm = TRUE)
  ) %>%
  filter(number_of_games > 1 & number_of_games != max(number_of_games)) %>%
  arrange(desc(number_of_games))

default_selection <- which(tolower(publisher_stats$publisher) == "cd projekt red")[1]

## Metacritic data
meta_url <- "https://raw.githubusercontent.com/youngopos/WED_Projekt2/main/Dane/Metacritic/Metacritic.csv"
meta <- read_csv(meta_url, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    release_date = suppressWarnings(mdy(release_date)),
    year         = year(release_date),
    score        = as.numeric(score)
  ) %>%
  filter(!is.na(score), !is.na(year))

publisher_list <- meta %>%
  filter(!is.na(publisher)) %>%
  count(publisher) %>%
  filter(n > 50) %>%    # keep only publishers with >50 games
  pull(publisher)

# Define reception category function
define_reception <- function(df) {
  df %>% mutate(
    reception = case_when(
      score >= 95 & score_number >= 500 ~ "Overwhelmingly Positive",
      score >= 85 & score_number >= 50  ~ "Very Positive",
      score >= 80                       ~ "Positive",
      score >= 70                       ~ "Mostly Positive",
      score >= 40                       ~ "Mixed",
      score >= 20                       ~ "Mostly Negative",
      score >= 0 & score_number >= 500  ~ "Overwhelmingly Negative",
      score >= 0 & score_number >= 50   ~ "Very Negative",
      TRUE                              ~ "Negative"
    )
  )
}
steam <- define_reception(steam)

# Prepare data splits for Early Access charts
df_ea <- steam %>% 
  filter(!is.na(score) & score_number >= 500) %>% 
  mutate(
    Group = if_else(early_access != "N/A", "Yes", "No"),
    Date = suppressWarnings(dmy(release_date)),
    Year = year(Date),
    Month = month(Date, label = TRUE)
  )

# ----- UI PANELS -----
## Community vs Critics panel
ui1 <- fluidPage(
  titlePanel("Community vs Critics"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("selected_tags", "Select up to 5 Steam tags:",
                     choices = unique_tags,
                     selected = c("2D","3D"),
                     multiple = TRUE,
                     options = list(maxItems = 5)),
      radioButtons("plot_type_steam", "Steam Visualization Type:",
                   choices = c("Density" = "density",
                               "Violin" = "violin",
                               "Boxplot" = "box"),
                   selected = "density", inline = TRUE),
      tags$hr(),
      selectizeInput("selected_genres", "Select Metacritic genres (max 5):",
                     choices = sort(unique(meta$genres)),
                     selected = sort(unique(meta$genres))[1:3],
                     multiple = TRUE, options = list(maxItems = 5)),
      radioButtons("plot_type_meta", "Metacritic Visualization Type:",
                   choices = c("Density" = "density",
                               "Violin" = "violin",
                               "Boxplot" = "box"),
                   selected = "box", inline = TRUE),
      tags$hr(),
      h4("Critics' Rating Distribution"),
      plotlyOutput("plotCompare", width = "100%", height = "250px")
    ),
    mainPanel(
      plotlyOutput("plot1"),
      tags$hr(),
      plotlyOutput("plotMetaGenres"),
      tags$hr()
    )
  )
)

## Publishers panel
ui3 <- fluidPage(
  tags$style(HTML("
  /* Table cells & headers */
  .dataTable td,
  .dataTable th {
    color: #c6d4df !important;
  }
  /* “Showing 1 to 10 of …” text */
  .dataTables_info {
    color: #c6d4df !important;
  }
  /* “Show X entries” dropdown label */
  .dataTables_length label {
    color: #c6d4df !important;
  }
  /* Search label and input text */
  .dataTables_filter label,
  .dataTables_filter input {
    color: #c6d4df !important;
  }
  /* (Optional) give the search box a matching dark background */
  .dataTables_filter input {
    background: #171a21 !important;
    border-color: #2a475e !important;
  }
")),
  h3("Publishers"),
  p("Select 1 to 3 publishers to view the relationship between price and score and an overview of average Metacritic scores over time."),
  fluidRow(
    column(width = 5, DT::dataTableOutput("publisher_table")),
    column(width = 7, plotlyOutput("publisher_plot"))
  ),
  tags$hr(),
  h4("Average Metacritic Score Over Time"),
  fluidRow(
    column(width = 4,
           selectizeInput(
             inputId = "mc_publishers",
             label   = "Select Metacritic publisher(s):",
             choices = publisher_list,
             selected= publisher_list[1],
             multiple= TRUE,
             options = list(server = TRUE)
           )
    ),
    column(width = 4,
           sliderInput(
             inputId = "mc_year_range",
             label   = "Year range (Metacritic):",
             min     = min(meta$year, na.rm = TRUE),
             max     = max(meta$year, na.rm = TRUE),
             value   = c(min(meta$year, na.rm = TRUE), max(meta$year, na.rm = TRUE)),
             step    = 1
           )
    )
  ),
  fluidRow(
    column(width = 12,
           plotOutput("heatmap", height = "400px")
    )
  )
)

## Early Access panel
ui2 <- fluidPage(
  h3("Early Access Analysis"),
  fluidRow(
    column(6,
           h4("Rating Distribution (Early Access vs Non-Early Access)"),
           radioButtons("recept_plot_type", "Chart Type:",
                        choices = c("Density" = "density",
                                    "Violin" = "violin",
                                    "Boxplot" = "box"),
                        selected = "density", inline = TRUE),
           plotlyOutput("plotReceptionDist", height = "300px")
    ),
    column(6)
  ),
  tags$hr(),
  fluidRow(
    column(6,
           h4("Price Distribution Comparison"),
           plotlyOutput("priceDistributionPlot", height = "350px")
    ),
    column(6,
           h4("Opinion Category Distribution"),
           plotlyOutput("receptionCategoryPlot", height = "350px")
    )
  ),
  tags$hr(),
  fluidRow(
    column(12, h4("Average Game Scores [2020-2024]"),
           plotlyOutput("heatmapEA", height = "350px")
    )
  )
)


# ----- MAIN UI -----
app_ui <- navbarPage(
  title = div(style = "font-weight:bold; color:#66c0f4;", "Steam"),
  theme = bs_theme(
    bg = "#171a21", fg = "#c6d4df", primary = "#66c0f4", secondary = "#2a475e",
    base_font = font_google("Roboto"), code_font = "Consolas"
  ),
  tabPanel("Community vs Critics", ui1),
  tabPanel("Publishers", ui3),
  tabPanel("Early Access", ui2),
  footer = shiny::HTML(
    "<footer style='background:#1b2838; color:#c6d4df; padding:10px; font-size:12px;'>
      <hr style='border-color:#2a475e;'/>
      <strong>Placeholder App</strong> - conceptual layout based on Steam theme.
    </footer>"
  )
)

# ----- SERVER -----
server <- function(input, output, session) {
  # Server-side selectize for mc_publishers
  updateSelectizeInput(session, "mc_publishers", choices = publisher_list, server = TRUE)
  
  # Steam plot
  output$plot1 <- renderPlotly({
    req(input$selected_tags, input$plot_type_steam)
    df <- steam_tags %>% filter(tags %in% input$selected_tags)
    p1 <- switch(input$plot_type_steam,
                 density = ggplot(df, aes(score, fill = tags, color = tags)) + geom_density(alpha = 0.3) +
                   labs(title = "Steam - Rating Density by Tag", x = "Positive [%]", y = "Density"),
                 violin = ggplot(df, aes(tags, score, fill = tags, color = tags)) + geom_violin(alpha = 0.4) +
                   labs(title = "Steam - Violin Plot by Tag", x = "Tag", y = "Positive [%]"),
                 box    = ggplot(df, aes(tags, score, fill = tags, color = tags)) + geom_boxplot(alpha = 0.5, outlier.shape = NA) +
                   labs(title = "Steam - Boxplot by Tag", x = "Tag", y = "Positive [%]")
    )
    ggplotly(p1 + theme_minimal())
  })
  
  # Metacritic genres plot
  output$plotMetaGenres <- renderPlotly({
    req(input$selected_genres, input$plot_type_meta)
    dfm <- meta %>% filter(genres %in% input$selected_genres)
    p2 <- switch(input$plot_type_meta,
                 density = ggplot(dfm, aes(score, fill = genres, color = genres)) + geom_density(alpha = 0.3) +
                   labs(title = "Metacritic - Rating Density by Genre", x = "Critic Score", y = "Density"),
                 violin = ggplot(dfm, aes(genres, score, fill = genres)) + geom_violin(alpha = 0.4) +
                   labs(title = "Metacritic - Violin Plot by Genre", x = "Genre", y = "Critic Score"),
                 box    = ggplot(dfm, aes(genres, score, fill = genres)) + geom_boxplot(alpha = 0.5, outlier.shape = NA) +
                   labs(title = "Metacritic - Boxplot by Genre", x = "Genre", y = "Critic Score")
    )
    ggplotly(p2 + theme_minimal() + theme(legend.position = "right"))
  })
  
  # Comparison plot
  output$plotCompare <- renderPlotly({
    hist_data <- meta %>% filter(!is.na(score)) %>% mutate(score = round(score)) %>% count(score) %>% rename(value = n)
    steam_data <- steam %>% filter(!is.na(score) & score_number >= 500) %>% mutate(score = round(score)) %>% count(score) %>% rename(value = n)
    scale_factor <- round(max(hist_data$value) / max(steam_data$value), 1)
    steam_data <- steam_data %>% mutate(value = value * scale_factor)
    p_cmp <- ggplot() +
      geom_col(data = hist_data, aes(score, value), fill = "blue", alpha = 0.6, width = 1) +
      geom_line(data = steam_data, aes(score, value), color = "red", size = 1.2) +
      labs(title = "Comparison of Critic and Steam User Rating Distributions", x = "Score", y = "Number of Games") +
      theme_minimal()
    ggplotly(p_cmp)
  })
  
  # Publisher table and plot
  output$publisher_table <- DT::renderDataTable({
    DT::datatable(publisher_stats,
                  selection = list(mode = "multiple", selected = default_selection),
                  options = list(pageLength = 10))
  })
  output$publisher_plot <- renderPlotly({
    req(input$publisher_table_rows_selected)
    pubs <- publisher_stats$publisher[input$publisher_table_rows_selected]
    df_pub <- steam_pub %>% filter(publisher %in% pubs, !is.na(price), !is.na(score))
    plot_ly(df_pub, x = ~price, y = ~score, color = ~publisher, type = 'scatter', mode = 'markers', marker = list(size = 8),
            text = ~paste0(
              "<b>Game:</b> ", title, "<br>",
              "<b>Price:</b> ", price, " PLN<br>",
              "<b>Score:</b> ", score, "%"
            ),
            hoverinfo = "text") %>%
      layout(title = "Scores vs Price for Selected Publishers", xaxis = list(title = "Price [PLN]"), yaxis = list(title = "Positive [%]"))
  })
  
  # Metacritic heatmap
  heatmap_data <- reactive({
    req(input$mc_year_range, input$mc_publishers)
    meta %>%
      filter(
        year >= input$mc_year_range[1],
        year <= input$mc_year_range[2],
        publisher %in% input$mc_publishers
      ) %>%
      group_by(year, publisher) %>%
      summarise(AvgScore = mean(score, na.rm = TRUE), .groups = "drop")
  })
  output$heatmap <- renderPlot({
    df_heat <- heatmap_data()
    ggplot(df_heat, aes(x = year, y = publisher, fill = AvgScore)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#edf8b1", high = "#2c7fb8", na.value = "grey90") +
      labs(title = "Average Metacritic Score Over Time", x = "Year", y = "Publisher", fill = "Avg Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # Early Access reception distribution
  output$plotReceptionDist <- renderPlotly({
    req(input$recept_plot_type)
    df_rec <- df_ea
    pR <- switch(input$recept_plot_type,
                 density = ggplot(df_rec, aes(score, fill = Group, color = Group)) + geom_density(alpha = 0.3),
                 violin = ggplot(df_rec, aes(Group, score, fill = Group)) + geom_violin(alpha = 0.6),
                 box    = ggplot(df_rec, aes(Group, score, fill = Group)) + geom_boxplot(alpha = 0.6)
    ) + theme_minimal() + theme(legend.position = 'top')
    ggplotly(pR)
  })
  
  # Heatmap - average score over time
  output$heatmapEA <- renderPlotly({
    df_hm <- df_ea %>% filter(Year >= 2020 & Year <= 2024) %>%
      group_by(Group, Year, Month) %>% summarise(avg_score = mean(score, na.rm = TRUE), .groups = 'drop')
    p_hm <- ggplot(df_hm, aes(x = Month, y = factor(Year), fill = avg_score)) +
      facet_wrap(~Group) + geom_tile(color = 'white') +
      labs(x = 'Month', y = 'Year', title = 'Average Game Scores [2020-2024]', fill = 'Avg Score') +
      theme_minimal() + coord_fixed(ratio = 1.3)
    ggplotly(p_hm)
  })
  
  # Opinion category distribution
  output$receptionCategoryPlot <- renderPlotly({
    levels_order <- c('Overwhelmingly Negative','Very Negative','Negative',
                      'Mostly Negative','Mixed','Mostly Positive',
                      'Positive','Very Positive','Overwhelmingly Positive')
    df_rc <- df_ea %>% group_by(Group, reception) %>% summarise(count = n(), .groups = 'drop') %>%
      group_by(Group) %>% mutate(pct = count / sum(count) * 100) %>%
      mutate(reception = factor(reception, levels = levels_order))
    p_rc <- ggplot(df_rc, aes(x = reception, y = pct, fill = Group)) +
      geom_col(position = position_dodge()) + coord_flip() +
      labs(x = 'Category', y = 'Percentage', title = 'Opinion Category Distribution') + theme_minimal()
    ggplotly(p_rc)
  })
  
  # Price distribution comparison
  output$priceDistributionPlot <- renderPlotly({
    bins <- df_ea %>% filter(!is.na(price)) %>% pull(price) %>% quantile(probs = seq(0,1,0.2), na.rm = TRUE)
    df_price <- df_ea %>% filter(!is.na(price)) %>%
      mutate(bin = cut(price, bins, include.lowest = TRUE)) %>%
      group_by(Group, bin) %>% summarise(n = n(), .groups = 'drop') %>%
      group_by(Group) %>% mutate(pct = n / sum(n) * 100)
    p_pr <- ggplot(df_price, aes(x = bin, y = pct, fill = Group)) +
      geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
      labs(x = 'Price Range [PLN]', y = 'Percentage', title = 'Price Distribution Comparison') +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p_pr)
  })
}

# ----- RUN APP -----
shinyApp(ui = app_ui, server = server)
