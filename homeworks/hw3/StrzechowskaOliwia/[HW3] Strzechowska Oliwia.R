library(ggplot2)
library(dplyr)
library(countrycode)
library(tidyr)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(plotly)
library(rnaturalearthdata)

#wczytanie danych
df <- read.csv('/Users/oliwiastrzechowska/Desktop/estat_ten00118.csv', sep = ';')

#odseparowanie kolumn danych
df1 <- separate(df, col = 1, into = c('freq', 'product', 'currency', 'unit', 'ind', 'im', 'Country'))

#przefiltrowanie danych, nadanie państwom odpowiednich skrótów
df2 <- df1 %>%
  filter(im == "MSHH") %>%
  filter(X2024 != ":") %>%
  mutate(
    Country = case_when(
      Country == "EL" ~ "GR",      
      Country == "UK" ~ "GB",      
      Country == "EU27" ~ NA_character_,  
      TRUE ~ Country
    ),
    X2024 = as.numeric(str_extract(X2024, "\\d+\\.?\\d*")),
    iso_a3 = countrycode(Country, 'iso2c', 'iso3c')
  )

#ograniczenie rozważań do Europy
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(region_un == "Europe")

#połączenie mapy z danymi na temat cen gazu
map_data <- left_join(world, df2, by = c("iso_a3"))

map_data <- map_data %>%
  mutate(tooltip_text = paste0(name, "\nCena: ", round(X2024, 2), " EUR/GJ"))


#rysowanie mapy
p <- ggplot(map_data) +
  geom_sf(aes(fill = X2024, text = tooltip_text)) +
  scale_fill_distiller(palette = "Blues", direction = 1, na.value = "grey90", name = "Cena gazu (EUR/GJ)") +
  coord_sf(xlim = c(-25, 45), ylim = c(34, 72), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Cena gazu w Europie dla gospodarstw domowych (2024)",
    caption = "Źródło: eurostat"
  )

#konwersja do interaktywnej mapy (po najechaniu na dane państwo, dla którego dane były dostępne,
#pojawia się ramka z ceną w euro ilości gazu równoważnej gigadżulowi energii)
ggplotly(p, tooltip = "text")

