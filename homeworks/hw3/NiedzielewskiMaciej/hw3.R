# Aby stworzyć mapę całkowitej emisji co2 w poszczególnych państwach na świecie ładuję plik z danymi i 
# wybieram tylko dane o roku 2023

library(dplyr)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

data <- read.csv('~/laby_eksplo/annual-co2-emissions-per-country.csv')
data_2023 <- data %>% 
  filter(Year == 2023)

world <- ne_countries(scale = "medium", returnclass = "sf")
data_2023 <- data_2023 %>%
  rename(
    country_name = Entity,
    iso_code = Code,
    co2_emissions = Annual.CO..emissions
  ) %>% 
  mutate(co2_emissions = co2_emissions/1e6)
data_2023 <- data_2023 %>%
  mutate(
    iso_code = na_if(iso_code, ""))


# Łączę moje dane z world po kodzie iso
joined_data <- left_join(world, data_2023, by = c("iso_a3_eh" = "iso_code"))

# Jeśli nie ma kodu to próbuję po nazwie państwa
co2_name_match <- data_2023 %>% filter(is.na(iso_code))

joined_data <- joined_data %>%
  left_join(co2_name_match, by = c("name" = "country_name"), suffix = c("", "_by_name")) %>%
  mutate(
    co2_emissions = coalesce(co2_emissions, co2_emissions_by_name)
  )

# Dla większej czytelności wykresu używam skali logarytmicznej oraz zaokrąglam wartości emisji w legendzie

mapa <- ggplot(data = joined_data) +
  geom_sf(aes(fill = co2_emissions)) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log",
    na.value = "grey90",
    labels = scales::label_number(scale = 1, accuracy = 0.1)
  )+
  labs(
    title = "Całkowite emisje CO2 w państwach (2023)",
    fill = "Emisje (mln ton)",
    caption = "Źródło danycj: OurWorldinData.org"
  ) +
  theme_minimal()


