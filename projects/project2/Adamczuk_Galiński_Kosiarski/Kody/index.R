source("bismilion.r")

library(shiny)
library(shinythemes)

source("dart.r")
source("apka.r")
source("proba.r")
source("jackpot.r")

ui <- navbarPage(theme = shinytheme("cerulean"),"",
                 
                 tabPanel("Dart", mod_dart_ui("dart")),
                 tabPanel("Najważniejsze turnieje", mod_apka_ui("apka")),
                 tabPanel("Statystyki Premier League", mod_proba_ui("proba")),
                 tabPanel("Kraje rankingu PDC", mod_jackpot_ui("jackpot"))
)

server <- function(input, output, session) {
  mod_apka_server("apka")
  mod_proba_server("proba")
  mod_jackpot_server("jackpot")
}

shinyApp(ui, server)