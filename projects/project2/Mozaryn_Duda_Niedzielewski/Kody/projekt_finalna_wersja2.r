library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(readr)
library(tidytext)
library(wordcloud2)
library(stopwords)

df <- read.csv("/Users/pawelmozaryn/Desktop/Mozaryn_Duda_Niedzielewski/rizzma.csv")

# Tworzenie identyfikatora
df$id <- 1:15000
recipes <- df

# Czyszczenie i preprocessing
time_cols <- c("prep_time", "cook_time", "total_time")
recipes[time_cols] <- lapply(recipes[time_cols], function(x) {
  x_clean <- gsub("mins", "", x, ignore.case = TRUE)
  x_clean <- trimws(x_clean)
  suppressWarnings(as.numeric(x_clean))
})

recipes$dietary_fiber_amount <- as.numeric(gsub("g", "", recipes$dietary_fiber_amount))

mg_cols <- c("sodium_amount", "cholesterol_amount", "potassium_amount", "calcium_amount", "iron_amount", "vitamin_c_amount")
recipes[mg_cols] <- lapply(recipes[mg_cols], function(x) {
  x_clean <- gsub("mg", "", x, ignore.case = TRUE)
  x_clean <- trimws(x_clean)
  suppressWarnings(as.numeric(x_clean) / 1000)
})

recipes$total_fat_amount <- as.numeric(gsub("g", "", recipes$total_fat_amount))
recipes$total_carbohydrate_amount <- as.numeric(gsub("g", "", recipes$total_carbohydrate_amount))
recipes$protein_amount <- as.numeric(gsub("g", "", recipes$protein_amount))

recipes <- recipes %>% filter(!is.na(cuisine))

df_efficiency <- recipes %>%
  filter(!is.na(calories), !is.na(total_time), total_time > 0) %>%
  mutate(calories_per_minute = calories / total_time) %>%
  group_by(cuisine) %>%
  summarise(
    avg_calories = mean(calories, na.rm = TRUE),
    avg_time = mean(total_time, na.rm = TRUE),
    avg_cpm = mean(calories_per_minute, na.rm = TRUE),
    count = n()
  )

top_recipes <- recipes %>%
  filter(!is.na(title), !is.na(rating)) %>%
  filter(!(is.na(sodium_amount)&is.na(cholesterol_amount)&is.na(potassium_amount)&is.na(calcium_amount)&is.na(iron_amount)&is.na(vitamin_c_amount))) %>% 
  arrange(desc(rating)) %>%
  slice(1:10)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Zbiorcza analiza przepisów"),
  
  h3("1. Kalorie vs czas przygotowania wg kuchni"),
  plotlyOutput("calories_plot"),
  
  hr(),
  
  h3("2. Wartości odżywcze – top 10 dań"),
  selectInput("dish_input", "Wybierz danie:", choices = top_recipes$title),
  plotOutput("nutrient_plot"),
  textOutput("last_updated_text2"),
  
  hr(),
  
  h3("3. Makroskładniki wg kuchni"),
  plotlyOutput("macroPlot", height = "700px"),
  
  hr(),
  
  h3("4. Boxplot najlepszych przepisów według kuchni"),
  fluidRow(
    column(4,
           selectInput("percentile", "Procent najlepiej ocenianych przepisów", choices = c(10, 20, 30, 50), selected = 10)
    ),
    column(4,
           selectInput("cuisines", "Wybierz 3 kuchnie",
                       choices = sort(unique(recipes$cuisine)),
                       selected = c("Italian", "Mexican", "Chinese"),
                       multiple = TRUE)
    ),
    column(4,
           selectInput("metric", "Zmienna", choices = c(
             "Kalorie" = "calories",
             "Czas przygotowania" = "prep_time",
             "Czas gotowania" = "cook_time",
             "Całkowity czas" = "total_time"
           ))
    )
  ),
  plotOutput("boxplot"),
  
  hr(),
  
  h3("5. Jakie słowa pojawiają się w przepisach różnych kuchni?"),
  selectInput("wordcloud_cuisine", "Wybierz kuchnię:",
              choices = sort(unique(recipes$cuisine)),
              selected = "Italian"),
  actionButton("generate_wc", "Generuj!"),
  wordcloud2Output("wordcloud", height = "400px")
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  output$calories_plot <- renderPlotly({
    plot_ly(df_efficiency,
            x = ~avg_time,
            y = ~avg_calories,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              size = ~avg_cpm * 5,
              color = ~avg_cpm,
              colorscale = 'Viridis',
              showscale = TRUE,
              line = list(width = 1, color = 'black')
            ),
            hoverinfo = 'text',
            text = ~paste("Kuchnia:", cuisine,
                          "<br>Kalorie/min:", round(avg_cpm, 2),
                          "<br>Kalorie:", round(avg_calories),
                          "<br>Czas:", round(avg_time), "min")
    ) %>%
      layout(
        title = "Kalorie vs średni czas przygotowania",
        xaxis = list(title = "Czas (min)"),
        yaxis = list(title = "Kalorie (kcal)")
      )
  })
  
  output$nutrient_plot <- renderPlot({
    selected <- top_recipes %>% filter(title == input$dish_input)
    nutrients <- selected %>%
      select(cholesterol = cholesterol_amount,
             potassium = potassium_amount,
             calcium = calcium_amount,
             iron = iron_amount,
             vitaminC = vitamin_c_amount,
             sodium = sodium_amount) %>%
      pivot_longer(everything(), names_to = "Nutrient", values_to = "Value")
    
    ggplot(nutrients, aes(x = Nutrient, y = Value, fill = Nutrient)) +
      geom_col(width = 0.6) +
      scale_fill_brewer(palette = "Pastel1") +
      labs(y = "Zawartość (g)", x = NULL) +
      theme_minimal() + theme(legend.position = "none")
  })
  
  output$last_updated_text2 <- renderText({
    selected <- top_recipes %>% filter(title == input$dish_input)
    paste("Ostatnia aktualizacja:", selected$last_updated)
  })
  
  output$macroPlot <- renderPlotly({
    df_macros <- recipes %>%
      group_by(cuisine) %>%
      summarise(
        protein_summary = mean(protein_amount, na.rm = TRUE),
        fat_summary = mean(total_fat_amount, na.rm = TRUE),
        carbs_summary = mean(total_carbohydrate_amount, na.rm = TRUE)
      ) %>% drop_na()
    
    df_long <- df_macros %>%
      pivot_longer(cols = -cuisine, names_to = "nutrient", values_to = "amount") %>%
      mutate(nutrient = recode(nutrient,
                               protein_summary = "Białko",
                               fat_summary = "Tłuszcze",
                               carbs_summary = "Węglowodany"))
    
    plot_ly(df_long,
            y = ~cuisine,
            x = ~amount,
            color = ~nutrient,
            type = 'bar',
            orientation = 'h',
            hoverinfo = 'text',
            text = ~paste0("<b>", cuisine, "</b><br>", nutrient, ": ", round(amount, 1), "g"),
            colors = c("Białko" = "dodgerblue", "Tłuszcze" = "goldenrod", "Węglowodany" = "hotpink"),
            width = 800) %>%
      layout(
        title = "Makroskładniki wg kuchni",
        xaxis = list(title = "Ilość (g)"),
        yaxis = list(title = "Kuchnia"),
        barmode = 'group',
        showlegend = TRUE,
        margin = list(l = 200)
      )
  })
  
  filtered_data_multi <- reactive({
    req(input$cuisines)
    if (length(input$cuisines) != 3) return(NULL)
    
    quantile_val <- quantile(recipes$rating, probs = 1 - (as.numeric(input$percentile) / 100), na.rm = TRUE)
    recipes %>%
      filter(
        rating >= quantile_val,
        cuisine %in% input$cuisines,
        !is.na(.data[[input$metric]])
      )
  })
  
  output$boxplot <- renderPlot({
    data <- filtered_data_multi()
    req(data)
    
    ggplot(data, aes(x = cuisine, y = .data[[input$metric]], fill = cuisine)) +
      geom_boxplot() +
      labs(
        y = input$metric,
        x = "Kuchnia",
        title = "Boxplot najlepszych przepisów (wg wybranych kuchni)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  observeEvent(input$generate_wc, {
    req(input$wordcloud_cuisine)
    
    df_slowa <- recipes %>%
      filter(cuisine == input$wordcloud_cuisine) %>%
      select(title, ingredients) %>%
      mutate(slowa = paste(title, ingredients, sep = ";"))
    
    if (nrow(df_slowa) == 0) {
      output$wordcloud <- renderWordcloud2({
        wordcloud2(data.frame(word = "Brak danych", freq = 1), size = 1.2)
      })
      return()
    }
    
    tidy_tekst <- df_slowa %>%
      unnest_tokens(output = word, input = slowa) %>%
      anti_join(get_stopwords(language = "en"), by = "word")
    
    word_count <- tidy_tekst %>%
      count(word, sort = TRUE)
    
    output$wordcloud <- renderWordcloud2({
      wordcloud2(data = word_count, size = 1.6)
    })
  })
}

shinyApp(ui, server)
