library(readxl)
library(rvest)
library(dplyr)
library(stringr)
library(plotly)

sektory <- seq(1, 20)
dart_order <- c(20, 1, 18, 4, 13, 6, 10, 15, 2, 17, 3, 19, 7, 16, 8, 11, 14, 9, 12, 5)
koniczila <- read_excel("checkout.xlsx")
kolumny <- colSums(koniczila[,2:9])
koniczila[,2:9] <- sweep(koniczila[,2:9], 2, kolumny, FUN = "/")*100
koniczila[,2:9] <- round(koniczila[,2:9],1)


labels <- paste("Sector", dart_order)
labels2 <- paste("Double", dart_order)
labels3 <- paste("Triple", dart_order)


max_gracz <- colnames(koniczila[,2:9])[max.col(koniczila[,2:9], ties.method = "first")]
max_wartosc <- apply(koniczila[,2:9], 1, max)
pudzian <- paste0(max_gracz, ": ", round(max_wartosc, 1), "%")
pudzian[1:20] <- paste0 (labels2, "\n" ,pudzian[dart_order])


stworz_pierścień <- function(labels, kolory, dziura, nazwa) {
  plot_ly(
    type = 'pie',
    labels = labels,
    values = rep(1, length(labels)),
    marker = list(colors = kolory,
                  line = list(color = '#FFFFFF', width = 1)),
    textinfo = 'none',
    hoverinfo = 'text',
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
  labels = "BullEye",
  values = rep(1, length(labels2)),
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
    align = "center",
    valign = "middle",
    xanchor = "center",
    yanchor = "middle", 
    font = list(size = 20, color = 'black')
  )
})

tarcza <- subplot(bulleye, bull,single_inner_ring,triple_ring,
                  single_outer_ring, double_ring)
tarcza %>% layout(annotations = annotations_list, showlegend = FALSE)
