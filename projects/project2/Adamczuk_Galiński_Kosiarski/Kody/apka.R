library(shiny)
library(tidyr)
library(plotly)
library(dplyr)
library(ggplot2)
library(readxl)
dane <- read_excel("nagrody.xlsx")
dane2 <- read_excel("ogladalnosc.xlsx")
colors <- c("red", "blue", "green", "orange", "purple", "brown", "pink", "cyan", "black")
zwyciezcy <- read_excel("zwyciezcy.xlsx")
tournaments <- c("World Darts Championship","UK Open","World Matchplay","World Grand Prix","Grand Slam of Darts","Players Championship Finals","European Championship","World Masters","Premier League Darts")

mod_apka_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  
  # Górny rząd – 5 ikon
  fluidRow(
    column(12, align = "center",
           lapply(1:5, function(i) {
             tags$img(
               src = paste0("icon", i, ".jpg"),
               width = "100px",
               style = "cursor:pointer; margin: 10px;",
               id = paste0("icon", i),
               onclick = paste0("Shiny.setInputValue('", ns("selected_icon"), "', ", i, ", {priority: 'event'})")
             )
           })
    )
  ),
  
  # Dolny rząd – 4 ikonki, wyśrodkowane
  fluidRow(
    column(12, align = "center",
           lapply(6:9, function(i) {
             tags$img(
               src = paste0("icon", i, ".jpg"),
               width = "100px",
               style = "cursor:pointer; margin: 10px;",
               id = paste0("icon", i),
               onclick = paste0("Shiny.setInputValue('", ns("selected_icon"), "', ", i, ", {priority: 'event'})")
             )
           })
    )
  ),
  
  # Wykres główny
  plotlyOutput(ns("mainPlot"), height = "400px"),
  
  # Dodatkowy wykres, tylko jeśli input$selected_icon == 1
  fluidRow(
    column(width=5,uiOutput(ns("extraTextUI"))),
    column(width=7,uiOutput(ns("extraPlotUI")))
  )
)
}
mod_apka_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns<-NS(id)
    output$mainPlot <- renderPlotly({
      req(input$selected_icon)
      i <- input$selected_icon
      
      color <- colors[i]
      
      p <- dane %>% 
        select(Year, Prizes = tournaments[i]) %>% 
        mutate(
          Prizes = case_when(
            Prizes == 0 ~ NA_real_,
            TRUE ~ as.numeric(Prizes)
          )
        ) %>% 
        ggplot(aes(
          x = Year, 
          y = Prizes / 1000,
          group = 1,
          text = paste0("Rok: ", Year, "<br>",
                        "Nagrody: ", format(Prizes, big.mark = ",", scientific = FALSE), " funtów")
        )) +
        geom_point(colour = "#37A8E8") +
        geom_line(colour = "#37A8E8") +
        scale_x_continuous(
          breaks = unique(dane$Year),
          labels = as.character(unique(dane$Year))
        ) +
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
        labs(title = paste0("Suma nagród w ", tournaments[i]),
             x = "Rok", y = "Suma nagród (tys. funtów)") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
    })
  output$extraPlotUI <- renderUI({
    req(input$selected_icon == 1)
    plotlyOutput(ns("extraPlot"), height = "400px")
  })
  output$extraTextUI <- renderUI({
    tekst <- paste(zwyciezcy[[input$selected_icon]], collapse = "<br>")
    div(
      style = "font-size: 20px; color: black; font-weight: bold;",
      HTML(tekst)
    )
  })
  
  output$extraPlot <- renderPlotly({
    req(input$selected_icon == 1)
    
    ggplotly(
      ggplot(dane2, aes(x = as.factor(Year), y = Peak_viewership/1000000,text = paste("Rok:", as.factor(Year), "<br>Szczytowa oglądalność (mln. ludzi):", Peak_viewership/1000000))) +
        geom_bar(stat = "identity",colour="#37A8E8",fill="#37A8E8") +
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
        labs(title="Szczytowa oglądalność w World Darts Championship",x = "Rok", y = "Oglądalność (mln. ludzi)")+
        theme_minimal()
    ,tooltip = "text")
  })
})
}