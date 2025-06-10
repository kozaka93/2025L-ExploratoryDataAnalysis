mod_dart_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
        html, body {
          height: 100%;
          margin: 0;
          padding: 0;
        }
        #page-container {
          min-height: 93vh;          
          display: flex;
          flex-direction: column;
        }
        #content-wrap {
          flex: 1 0 auto;              
          padding: 10px;
        }
        footer {
          flex-shrink: 0;
          background-color: #f8f9fa;
          padding: 15px;
          text-align: center;
          border-top: 1px solid #ddd;
          font-weight: bold;
          height: 60px;
          line-height: 30px;
        }
      "))
    ),
    div(
      id = "page-container",
      div(
        id = "content-wrap",
        titlePanel("Czym jest dart?"),
        fluidRow(
          column(
            width = 8,
            p("Dart zapewne większości z nas kojarzy się z prostą grą barową i pijanymi Brytyjczykami próbującymi rzucić w tarczę po x-nastym piwie."),
            p("Jest to w pewnym sensie uzasadnione skojarzenie, ponieważ ten sport jest zdecydowanie najpopularniejszy na wyspach brytyjskich."),
            p("Jednak Dart od wielu lat jest popularnym sportem, który w ostatnich latach zyskuje na popularności również w Polsce."),
            p("Nasza strona prezentuje kilka podstawowych informacji na tego sportu."),
            tags$ul(
              tags$li("Zakładka 'Najważniejsze turnieje' - zawiera informacje o kluczowych wydarzeniach w kalendarzu Dartowym."),
              tags$li("Zakładka 'Statystyki Premier League' - szczegółowe dane statystyczne dotyczące gry najlepszych zawodników."),
              tags$li("Zakładka 'Kraje rankingu PDC' - mapa narodowości zawodników z rankingu światowego.")
            ),
            p("Zapraszamy do eksploracji.")
          ),
          column(
            width = 4,
            div(
              style = "text-align: center;",
              tags$img(
                src = "https://ocdn.eu/sport-images-transforms/1/9RTk9lBaHR0cHM6Ly9vY2RuLmV1L3B1bHNjbXMvTURBXy80MjFhZGNmMWZkY2IwYjRmY2QyM2EyZGIyNjNhZjllMy5qcGeTlQMAzK_NFfDNDFeVAs0EsADCw5MJpmQzMTQwYgbeAAKhMAGhMQE/dart.jpg",
                alt = "Zdjęcie dart",
                style = "max-width: 100%; height: auto; border-radius: 8px;"
              )
            )
          )
        )
      ),
      tags$footer(
        "Autorzy: Maciej Adamczuk, Piotr Kosiarski, Filip Galiński"
      )
    )
  )
}