# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Country_codes


library(tidyverse)
library(dplyr)
library(sf)
library(maps)
library(mapdata)
library(ggplot2)
library(mapproj)

data <- read.table(gzfile("estat_prc_rem_cs.tsv.gz"), header = TRUE, sep = "\t") %>% 
  separate(col = freq.lcstruct.p_adj.unit.geo.TIME_PERIOD,
           into = c("freq", "lcstruct", "p_adj", "unit", "country"),
           sep = "\\,",
           remove = FALSE) %>% 
  filter(lcstruct == "GREM" & p_adj == "RV") %>% 
  filter(country != "EU" & country != "EU27_2020" & country != "EU_V") %>% 
  select(-c(freq.lcstruct.p_adj.unit.geo.TIME_PERIOD, freq, lcstruct, p_adj, unit)) %>% 
  rename_with(~ gsub("^X", "", .), everything()) %>%
  rename_with(~ gsub(".S2$", "", .), everything()) %>% 
  select(-c('2022.S1', '2023.S1')) %>% 
  select(c('country', '2013', '2014', '2014', '2015', '2016'))
data$country <- recode(data$country,
                        "AT" = "Austria",
                        "BE" = "Belgium",
                        "BG" = "Bulgaria",
                        "CY" = "Cyprus",
                        "CZ" = "Czech Republic",
                        "DE" = "Germany",
                        "DK" = "Denmark",
                        "EE" = "Estonia",
                        "EL" = "Greece",
                        "ES" = "Spain",
                        "FI" = "Finland",
                        "FR" = "France",
                        "HR" = "Croatia",
                        "HU" = "Hungary",
                        "IE" = "Ireland",
                        "IT" = "Italy",
                        "LT" = "Lithuania",
                        "LU" = "Luxembourg",
                        "LV" = "Latvia",
                        "MT" = "Malta",
                        "NL" = "Netherlands",
                        "PL" = "Poland",
                        "PT" = "Portugal",
                        "RO" = "Romania",
                        "SE" = "Sweden",
                        "SI" = "Slovenia",
                        "SK" = "Slovakia") 

data <- data %>% mutate(`2013` = as.numeric(`2013`)) %>% 
  mutate(`2014` = as.numeric(`2014`)) %>%
  mutate(`Remuneration indexes` = rowMeans(select(., '2013', '2014', 
                                                  '2015', '2016'), na.rm = TRUE)) %>% 
  select(`country`, `Remuneration indexes`)

map_europe <- map_data("world") %>% 
  filter(region %in% c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", 
                       "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland", 
                       "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", 
                       "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", 
                       "Romania", "Sweden", "Slovenia", "Slovakia", "UK",
                       "Iceland", "Norway", "Albania",  "Andorra",  
                       "Belarus",  "Bosnia and Herzegovina",  "Kosovo",  "Liechtenstein",  
                       "North Macedonia",  "Moldova",  "Monaco",  "Montenegro",  
                       "San Marino",  "Serbia", "Switzerland",  "Ukraine",  "Vatican City", "Russia")) %>% 
  filter(is.na(subregion) | subregion != "Jan Mayen") %>% 
  filter(is.na(subregion) | subregion != "Svalbard") %>% 
  filter(region != "Russia" | (region == 'Russia' & subregion == 32))
data_map <- left_join(map_europe, data, by = c("region" = "country"))

wykres <- ggplot() +
  geom_polygon(data = data_map, aes(x = long, y = lat, group = group, fill = `Remuneration indexes`), 
               color = "white", linewidth = 0.3) +
  theme_minimal() +
  scale_fill_viridis_c(
    name = "Indeks wynagrodzeń",
    option = "turbo",
    trans = "log10", 
    breaks = c(98, 100, 102, 104, 106, 108, 110),
    labels = c(98, 100, 102, 104, 106, 108, 110),
    na.value = "grey80") +
  coord_map("lambert", parameters = c(45, 55), xlim = c(-10, 40), ylim = c(35, 70))+
  labs(title = "Ewolucja Wynagrodzeń Służby Publicznej w Europie w latach 2013-2016",
       subtitle = 
       "Roczna zmiana średniego wynagrodzenia netto urzędników centralnej administracji – wskaźnik 
       w ujęciu nominalnym i realnym. Wskaźnik wynagrodzeń mierzy roczną zmianę średniego 
       wynagrodzenia krajowych urzędników centralnej administracji.",
       x = 'Długość geograficzna', 
       y = 'Szerokość geograficzna') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14))+
  theme(
    plot.margin = margin(10, 20, 10, 20),
    axis.title = element_text(size = 12),  # Osie
    axis.text = element_text(size = 10),   # Etykiety osi
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
    )

wykres

png("mapa.png", width = 4000, height = 3000, res = 300)
print(wykres)
dev.off()
