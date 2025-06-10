library(plotly)
library(shiny)

mod_proba_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  titlePanel("Premier League 2025"),
  
  fluidRow(
    column(
      width = 3, # A - sidebar panel
      wellPanel(
        selectInput(ns("typ"), "Wybierz jedno:", choices = c("Procent", "Ilosc")),
        p("(Procent oznacza jaki procent wszystkich doubli zawodnika stanowi dany double, w przypadku wszystkich zawodników pokazuje tego, u którego ten procent jest największy)"),
        radioButtons(ns("zawodnik"), "Wybierz zawodnika:", 
                     choices = c("Wszyscy", "Nathan Aspinall", "Rob Cross", "Chris Dobey",
                                 "Luke Humphries", "Luke Littler", "Stephen Bunting", 
                                 "Gerwyn Price", "Michael Van Gerwen"),
                     selected = "Wszyscy")
      )
    ),
    column(width=2),
    column(
      width = 4, 
      plotlyOutput(ns("Plot"))
    ),
    column(width=3)
    )
  ,
  fluidRow(
    column(
      width = 5, # C - image + text
      uiOutput(ns("dynamicImage")),
      uiOutput(ns("dynamicInfo"))),
    
    column(
      width = 7, # D - full width for second plot
      plotlyOutput(ns("liniowy"))
    )
    
  )
  
)
}
mod_proba_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  output$dynamicImage <- renderUI({
    src <- switch(input$zawodnik,
                  "Wszyscy" = "https://igamingexpress.com/wp-content/uploads/2024/01/obraz_2024-01-05_113221736.png",
                  "Nathan Aspinall" = "https://www.gokarli.de/mediafiles/kategorien/nathan-aspinal_UK_Open_2019_828.jpg",
                  "Rob Cross" = "https://static.independent.co.uk/2022/12/23/23/494cb8326915c06e809203ebc12eff13Y29udGVudHNlYXJjaGFwaSwxNjcxOTIxNTQ3-2.70364161.jpg",
                  "Chris Dobey" = "https://e0.365dm.com/24/04/2048x1152/skysports-chris-dobey-austrian-darts-open_6534688.jpg?20240426224349",
                  "Luke Humphries" = "https://www.thesun.co.uk/wp-content/uploads/2025/05/luke-humphries-england-poses-trophy-957191866.jpg?strip=all&quality=100&w=1920&h=1080&crop=1",
                  "Luke Littler" = "https://static.independent.co.uk/2024/01/04/15/4072ab3e70aaf02afe44c131940ff2e7Y29udGVudHNlYXJjaGFwaSwxNzA0NDYyODY4-2.74990494.jpg",
                  "Stephen Bunting" = "https://r.testifier.nl/Acbs8526SDKI/resizing_type:fit/watermark:PDC/width:3840/height:2560/plain/https://s3-newsifier.ams3.digitaloceanspaces.com/dartsnews.com/images/2024-12/stephen-bunting-676ede4c53ce5.jpg@webp",
                  "Gerwyn Price" = "https://www.sportphotogallery.com/content/images/cmsfiles/product/34586/35338-list.jpg",
                  "Michael Van Gerwen" = "https://img.welt.de/img/sport/mobile242687015/3582506307-ci102l-w1024/Michael-van-Gerwen-File-Photo.jpg")
    tags$img(src = src, width = "100%")
  })
  
  output$dynamicInfo <- renderUI({
    tekst <- switch(input$zawodnik,
                    "Wszyscy" = "Premier League Darts to turniej organizowany przez PDC, w którym mierzy się ze sobą 8 najlepszych
                    darterów na świecie wybieranych przez federację. Turniej rozgrywany jest na zasadzie 16 nocy, podczas każdej
                    rozgrywana jest 8-osobowa drabinka play-off. Finał turnieju odbywa się po zakończeniu wszystkich 16 nocy i zawiera
                    4-osobową drabinkę najlepszych graczy w danym sezonie. Odbywa się on głównie w Wielkiej Brytanii, ale pojedyncze
                    noce są rozgrywane również w Niemczech, Irlandii, Holandii.",
                    "Nathan Aspinall" = "Nathan Aspinall to były zwycięzca UK Open (2019)
                      oraz World Matchplay (2023). Znany z waleczności i mocnej psychiki w końcówkach.
                      Dwukrotnie dotarł do półfinału Mistrzostw Świata (2019, 2020).
                      Zadebiutował w Premier League w 2020 roku i od tego czasu jest groźnym przeciwnikiem.
                      Styl gry oparty na wysokich seriach punktowych i solidnych finiszkach.
                      Jeden z najbardziej lubianych zawodników przez kibiców.",
                    "Rob Cross" = "Rob 'Voltage' Cross zasłynął jako sensacyjny mistrz świata
                      w 2018 roku, pokonując Phila Taylora. Ma na koncie triumfy w takich 
                      turniejach jak European Championship (2019, 2021, 2023) i World Matchplay (2019). 
                      Uznawany za jednego z najrówniejszych graczy w tourze. Charakteryzuje się spokojem,
                      precyzją i doświadczeniem. Regularnie dociera do późnych faz turniejów TV.
                      W Premier League często sprawia niespodzianki.",
                    "Chris Dobey" = "Chris 'Hollywood' Dobey wygrał Mastersa w 2023 roku – 
                      to jego największy sukces jak dotąd. Przez lata był uznawany za solidnego gracza
                      bez większych tytułów, aż do przełamania. Często prezentuje efektowny styl gry
                      i potrafi rzucać wysokie serie. Gra w Premier League to dla niego szansa 
                      na ugruntowanie pozycji w ścisłej czołówce. W 2023 roku zadebiutował 
                      w Premier League jako 'dzika karta'. Jego forma często rośnie w ważnych meczach.",
                    "Luke Humphries" = "Luke 'Cool Hand' Humphries to aktualny mistrz świata PDC 2024. 
                      Oprócz tego wygrał Grand Slam of Darts (2023), World Grand Prix (2023) i
                      Players Championship Finals (2023). Jego szybki awans do ścisłej czołówki
                      to jedno z największych objawień ostatnich lat. Charakteryzuje się regularnością 
                      i świetną formą na podwójnych. W 2023 był liderem rankingu PDC. Jego styl to zimna
                      krew i niemal chirurgiczna precyzja.",
                    "Luke Littler" = "Luke 'The Nuke' Littler to prawdziwa sensacja świata darta –
                      w wieku 16 lat dotarł do finału MŚ 2024. W 2024 roku wygrał Bahrain Masters,
                      Dutch Masters i Belgian Open. Jego styl gry opiera się na niesamowitej dynamice
                      i odwadze. Pomimo młodego wieku pokonał największe nazwiska w PDC. Już uznawany
                      za przyszłego mistrza świata. Media i kibice mówią o nim jako o 'następnym wielkim nazwisku'.",
                    "Stephen Bunting" = "Stephen 'The Bullet' Bunting to były mistrz świata BDO (2014)
                      i zwycięzca Mastersa w 2024 roku. Znany z płynnej techniki rzutu i opanowania 
                      na scenie. Przez lata balansował pomiędzy średnią a topową formą, ale wrócił
                      na szczyt w ostatnich sezonach. W Premier League występuje po dłuższej przerwie. 
                      Potrafi zaskoczyć faworytów i wygrać z każdym. Często niedoceniany, ale bardzo niebezpieczny.",
                    "Gerwyn Price" = "Gerwyn 'The Iceman' Price to mistrz świata z 2021 roku i zwycięzca wielu turniejów
                      rankingowych. Były zawodnik rugby, który przeniósł agresję z boiska na scenę darta.
                      Znany z wyrazistej osobowości, głośnych reakcji i mocnej psychiki. 
                      Wygrał Grand Slam cztery razy z rzędu (2018–2021). Czołowy gracz rankingu od kilku lat. 
                      Mimo kontrowersji, jest jednym z najbardziej utytułowanych zawodników ostatniej dekady.",
                    "Michael Van Gerwen" = "Michael 'Mighty Mike' van Gerwen to legenda darta 
                      – trzykrotny mistrz świata (2014, 2017, 2019). Ma na koncie ponad 150 tytułów rankingowych
                      i rekordy w wielu kategoriach. Wygrał Premier League aż siedem razy – rekord w historii.
                      Znany z agresywnego stylu gry i niesamowitych serii punktowych. 
                      Nadal uważany za jednego z faworytów w każdym turnieju. Jeden z najbardziej dominujących graczy w historii.

"
    )
    div(
      style = "font-size: 20px; color: black; font-weight: bold;",
      HTML(tekst)
    )
  })
  output$liniowy <- renderPlotly({
    library(plotly)
    library(readxl)
    library(rvest)
    library(dplyr)
    library(stringr)
    koniczila <- read_excel("checkout.xlsx")
    suma <- koniczila[,1]
    
    suma$total <- rowSums(koniczila[,2:9])
    if(input$zawodnik != "Wszyscy"){
      suma$total <-  koniczila[[input$zawodnik]]
    }
    suma <- suma[1:21,]
    suma$Double <-factor(suma$Double, levels = c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","D13","D14","D15","D16","D17","D18","D19","D20","Bull"))
    wykres <- suma %>% ggplot(aes(x=Double, y=total, group=1))+geom_line(colour="#37A8E8")+geom_point(colour="#37A8E8")+
      labs(title="Liczba rzuconych doubli przez danego zawodnika",x = "Double", y = "Liczba doubli")+
      theme_minimal()
    wykres <- ggplotly(wykres)
    wykres
    
    
  })
  output$Plot <- renderPlotly({library(readxl)
    library(rvest)
    library(dplyr)
    library(stringr)
    library(plotly)
    
    sektory <- seq(1, 20)
    dart_order <- c(20, 1, 18, 4, 13, 6, 10, 15, 2, 17, 3, 19, 7, 16, 8, 11, 14, 9, 12, 5)
    kolumny <- colSums(koniczila[,2:9])
    
    labels <- paste("Sector", dart_order)
    labels2 <- paste("Double", dart_order)
    labels3 <- paste("Triple", dart_order)
    
    
    if (input$typ == "Procent"){
      koniczila[,2:9] <- sweep(koniczila[,2:9], 2, kolumny, FUN = "/")*100
      koniczila[,2:9] <- round(koniczila[,2:9],1)
      max_gracz <- colnames(koniczila[,2:9])[max.col(koniczila[,2:9], ties.method = "first")]
      max_wartosc <- apply(koniczila[,2:9], 1, max)
      pudzian <- paste0(max_gracz, ": ", round(max_wartosc, 1), "%")
    }
    if (input$typ == "Ilosc"){
      koniczila <- read_excel("checkout.xlsx")
      max_gracz <- colnames(koniczila[,2:9])[max.col(koniczila[,2:9], ties.method = "first")]
      max_wartosc <- apply(koniczila[,2:9], 1, max)
      pudzian <- paste0(max_gracz, ": ", max_wartosc)
    }
    pudzian[1:20] <- paste0 (labels2, "\n" ,pudzian[dart_order])
    pudzian[21] <- paste0 ("Bullseye", "\n", pudzian[21])
    
    if(input$zawodnik != "Wszyscy"){
      pudzian <-  koniczila[[input$zawodnik]]
      if (input$typ == "Procent"){
        pudzian[1:20] <- paste0(labels2, "\n",pudzian[dart_order], "%")
        pudzian[21] <- paste0("Bullseye","\n",pudzian[21], "%")
      }
      if (input$typ == "Ilosc"){
        pudzian[1:20] <- pudzian[dart_order]
      }
    }
    
    
    
    stworz_pierścień <- function(labels, kolory, dziura, nazwa) {
      plot_ly(
        type = 'pie',
        labels = labels,
        values = rep(1, length(labels)),
        marker = list(colors = kolory,
                      line = list(color = '#FFFFFF', width = 1)),
        textinfo = 'none',
        hoverinfo = 'none',
        hole = dziura,
        sort = FALSE, # żeby sektory były jak na tarczy
        direction = 'clockwise',
        rotation = -10,
        domain = list(x = c(0, 1), y = c(0, 1)),
        name = nazwa,
        hovertext = labels
      )
    }
    
    
    kolory_double <- ifelse(sektory %% 2 == 0, 'rgba(0,200,0,0.8)', 'rgba(200,0,0,0.8)')
    double_ring <- plot_ly(
      type = 'pie',
      labels = labels2,
      values = rep(1, length(labels2)),
      marker = list(colors = kolory_double,
                    line = list(color = '#FFFFFF', width = 1)),
      textinfo = 'none',
      hoverinfo = 'text',
      hole = 0.9,
      sort = FALSE,
      direction = 'clockwise',
      rotation = -10,
      domain = list(x = c(0, 1), y = c(0, 1)),
      name = "Double Ring",
      hovertext = pudzian[1:20]
    )
    
    
    
    kolory_single_outer <- ifelse(sektory %% 2 == 0, 'white', 'black')
    single_outer_ring <- stworz_pierścień(labels, kolory_single_outer, 0.55, 'Single Outer')
    
    kolory_triple <- ifelse(sektory %% 2 == 0, 'rgba(0,200,0,0.8)', 'rgba(200,0,0,0.8)')
    triple_ring <- stworz_pierścień(labels3, kolory_triple, 0.45, 'Triple Ring')
    
    kolory_single_inner <- ifelse(sektory %% 2 == 0, 'white', 'black')
    single_inner_ring <- stworz_pierścień(labels, kolory_single_inner, 0.10, 'Single Inner')
    single_inner_ring
    
    bull_labels <- c('Bull')
    bull_color <- c('rgba(0,200,0,0.9)')
    bull <- stworz_pierścień(bull_labels, bull_color, 0.05, "Ball")
    
    
    
    bulleye <- plot_ly(
      type = 'pie',
      labels = "BullsEye",
      values = 1,
      marker = list(colors = c("rgba(200,0,0,0.9)"),
                    line = list(color = '#FFFFFF', width = 1)),
      textinfo = 'none',
      hoverinfo = 'text',
      hole = 0,
      sort = FALSE,
      direction = 'clockwise',
      rotation = -10,
      domain = list(x = c(0, 1), y = c(0, 1)),
      name = "Eye",
      hovertext = c(pudzian[21])
    )
    
    angles <- seq(from = 90, by = -360/20, length.out = 20) * pi / 180
    r_labels <- 0.55
    
    
    x_labels <- 0.5+r_labels * cos(angles)
    y_labels <- 0.5+r_labels * sin(angles)
    
    xlab = c(0.49,  0.62,  0.73,  0.82,  0.89 , 0.9,  0.89,
             0.83,  0.75,  0.64 , 0.51 , 0.39  ,0.27 , 0.17,
             0.12, 0.09, 0.10,  0.155 , 0.26  ,0.36)
    
    ylab = c(1.04 , 1.02,  0.96,  0.84,  0.68,  0.5,  0.33,
             0.19,  0.04, -0.02, -0.04 ,-0.02,  0.04,  0.19,
             0.33,  0.5,  0.68,  0.82,  0.96,  1.02)
    
    annotations_list <- lapply(1:20, function(i) {
      list(
        x = xlab[i],
        y = ylab[i],
        text = dart_order[i],
        showarrow = FALSE,
        align = "center",  # Centruje tekst w poziomie
        valign = "middle", # Centruje tekst w pionie
        xanchor = "center", # Ustawienie punktu kotwiczenia w poziomie
        yanchor = "middle", 
        font = list(size = 20, color = 'black')
      )
    })
    
    tarcza <- subplot(bulleye, bull,single_inner_ring,triple_ring,
                      single_outer_ring, double_ring)
    tarcza %>% layout(annotations = annotations_list, showlegend = FALSE)
    
  })
  
  
  })
}