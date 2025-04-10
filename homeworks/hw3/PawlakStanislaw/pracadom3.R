library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(readxl)

dane_ue <- read_excel("C:/Users/MSI_GL72/Desktop/env_air_gge$defaultview_page_spreadsheet.xlsx", sheet = "Sheet 1")

colnames(dane_ue)[1] <- "Kraj" 

dane_ue <- dane_ue %>%
  mutate(Kraj = ifelse(Kraj == "Czechia", "Czech Republic", Kraj))

kraje_ue <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
              "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
              "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
              "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
              "Spain", "Sweden")

dane_ue_filtr <- dane_ue %>%
  filter(Kraj %in% kraje_ue)

dane_ue_filtr <- dane_ue_filtr %>% 
  select(where(~sum(!is.na(.)) > 0))

colnames(dane_ue_filtr) <- c("Kraj", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

dane_ue_filtr[, 2:11] <- sapply(dane_ue_filtr[, 2:11], as.numeric)

dane_ue_srednia <- dane_ue_filtr %>%
  mutate(Srednia_Emisja = rowMeans(select(., `2013`:`2022`), na.rm = TRUE))

swiat <- map_data("world")

kraje_centra <- swiat %>%
  filter(region %in% kraje_ue) %>%
  group_by(region) %>%
  summarise(long = mean(range(long)),
            lat = mean(range(lat)))

dane_mapa <- dane_ue_srednia %>%
  left_join(kraje_centra, by = c("Kraj" = "region"))

ggplot() +
  borders("world", regions = kraje_ue, fill = "gray90", colour = "gray60") +
  geom_point(data = dane_mapa, aes(x = long, y = lat, color = Srednia_Emisja), size = 4) +
  scale_color_gradient(low = "green", high = "red", name = "Średnia emisja") +
  theme_minimal() +
  labs(title = "Średnia emisja gazów cieplarnianych (2013–2022)",
       subtitle = "Kraje UE",
       x = "", y = "")


