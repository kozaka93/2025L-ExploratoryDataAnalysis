install.packages("patchwork")
install.packages("grid")
install.packages("gridExtra")
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(readxl)
library(patchwork)
library(gridExtra)
library(grid)
library(stringr)

cars_registered <- read_excel("C:/Users/Paawel/Desktop/homeworks_data/StefanczykPawel/road_eqs_carhab$defaultview_page_spreadsheet.xlsx", sheet = "Sheet 1")
cars_registered <- cars_registered[10:40, c(1, 9, 10, 11)] %>%
                  rename(Kraj = `Data extracted on 03/04/2025 13:38:10 from [ESTAT]`,
                         Rok_2021 = ...9,
                         Rok_2022 = ...10,
                         Rok_2023 = ...11)
cars_registered$Kraj <- replace(cars_registered$Kraj, cars_registered$Kraj == "Czechia", "Czech Republic")
europe <- map_data("world", regions = cars_registered$Kraj)

eu <- ggplot() + geom_polygon(data = europe, aes(x = long, y = lat, group = group), fill = "lightgrey", col = "black") +
  coord_map("albers", 25, 50, xlim = c(-5, 35), ylim = c(30, 70))
eu

eu_registered <- europe %>% left_join(cars_registered, by = c("region" = "Kraj"))


p1 <- eu + geom_polygon(data = eu_registered, aes(x = long, y = lat, group = group, fill = as.numeric(Rok_2021))) + 
  labs(
    subtitle = "Rok 2021"
  ) + theme_void() + scale_fill_fermenter(palette = 8, trans="log10", direction = 1, name = "Liczba aut")


p2 <- eu + geom_polygon(data = eu_registered, aes(x = long, y = lat, group = group, fill = as.numeric(Rok_2022))) +
  labs(
    subtitle = "Rok 2022"
  ) + theme_void() + scale_fill_fermenter(palette = 8, trans="log10", direction = 1, name = "Liczba aut")

p3 <- eu + geom_polygon(data = eu_registered, aes(x = long, y = lat, group = group, fill = as.numeric(Rok_2023))) +
  labs(
    subtitle = "Rok 2023"
  ) + theme_void() + scale_fill_fermenter(palette = 8, trans="log10", direction = 1, name = "Liczba aut")


opis <- "Można zaobserowować, że na przestrzeni tych 3 lat liczba zarejsetrowanych aut znacząco wzrosła w Polsce, a zmalała w Islandii. W pozostałych krajach nie widać dużej różnicy"
opis <- str_wrap(opis, width = 65)
opis <- textGrob(opis, gp = gpar(fontsize = 10, lineheight = 1))

p1 + p2 + p3 + opis + plot_layout(ncol = 2, nrow = 2, heights = c(1, 0.5), widths = c(1, 1), guides = "collect") +
  plot_annotation(title = "Liczba zarejestrowanych aut w Europie")
