library(ggrepel)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(tidyr)
library(usmap)

states <- us_map("states")

dane <- read.csv("dane.csv")

# dane do koordynatów zostały odczytane z states[[4]][[8]][[1]][[1]]
dc_coords <- matrix(c(
  1955479, -402055.2
), ncol = 2, byrow = TRUE)

dc_label_data <- data.frame(
  x = dc_coords[, 1],
  y = dc_coords[, 2],
  label = "Dystrykt Kolumbii"
)

dane <- dane %>%
  mutate(
    bin = case_when( Suma<0 ~ cut(Suma,
              breaks = c(-13,-10,-6,-2,-1,1),
              include.lowest = TRUE, right=FALSE),
              Suma>0 ~ cut(Suma,
                           breaks=c(-1,1,2,6,10,13), include.lowest=TRUE, left=FALSE))
  )

bins <- c(
  "(10,13]",
  "(6,10]",
  "(2,6]",
  "[-1,1]",
  "[-6,-2)",
  "[-10,-6)",
  "[-13,-10)"
)

bin_colors <- c(
  "(10,13]" ="#FF0000",
  "(6,10]" ="#FF4343",
  "(2,6]" ="#FF7C7C",
  "[-1,1]" ="#DEDEDE",
  "[-6,-2)" = "#639CFF",
  "[-10,-6)" = "#2954FF",
  "[-13,-10)" = "#0000ff"
)


 mapa <- plot_usmap(data = dane, values = "bin") +
  scale_fill_manual(values = bin_colors, name = "Różnica w liczbie wygranych 
wyborów według partii
(Republikanie-Demokraci)", guide = guide_legend(reverse = TRUE)) +
  labs(title = "Wyniki wyborów prezydenckich w USA w latach 1976-2024") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5,face="bold"))

mapa + geom_label_repel(
  data = dc_label_data,
  aes(x = x, y = y, label = label),
  size = 2, fill = bin_colors[bins==filter(dane, state=="district of columbia")$bin],fontface = "bold", color="white",
  nudge_y=-2)

