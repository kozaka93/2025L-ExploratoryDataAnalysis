library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
library(RColorBrewer)

rm(list = ls())


df <- read.csv("https://ourworldindata.org/grapher/annual-co2-emissions-per-country.csv?v=1&csvType=full&useColumnShortNames=true")

map_world <- map_data("world")

# Pokaże jak wyglądała emisja co2 100 lat temu. 50 lat temu i w 2023

# Najpierw trzeba odpowiedno przekształić niektóre nazwy krajów,
# usunąć nieinteresujące nas rzeczy, itp

df <- df %>% 
  rename(region = Entity)

map_regions <- unique(map_world$region)

co2_regions <- unique(df$region)

missing <- setdiff(map_regions, co2_regions)

# Widzimy ze 55 nazw jest różnych, wiec te większe panstwa poprawie recznie
# Najważniejsze poprawie ręcznie, a reszta jest tak mała że je pomine



df$region[df$region == "United States"] <- "USA"
df$region[df$region == "United Kingdom"] <- "UK"
df$region[df$region == "Czechia"] <- "Czech Republic"
df$region[df$region == "Korea, South"] <- "South Korea"
df$region[df$region == "Korea, North"] <- "North Korea"
df$region[df$region == "Myanmar"] <- "Burma"
df$region[df$region == "Eswatini"] <- "Swaziland"
df$region[df$region == "Ivory Coast"] <- "Cote d'Ivoire"
df$region[df$region == "Cabo Verde"] <- "Cape Verde"
df$region[df$region == "Timor"] <- "East Timor"
df$region[df$region == "Bahamas"] <- "The Bahamas"
df$region[df$region == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
df$region[df$region == "Congo"] <- "Republic of Congo"

# View(df)

# W 2023

df_2023 <- df %>% 
  filter(Year == 2023)

# Łączymy dane o emisji z danymi zawierającymi współrzędne geograficzne

map_joined <- map_world %>%
  left_join(df_2023, by = "region")

# Tak żeby napewno skala logarytmiczna zadziałała

map_joined_filtered <- map_joined %>%
  filter(emissions_total > 0)


# Wykres mapy świata

world_in_2023 <- map_joined_filtered %>% 
  ggplot() +
  geom_polygon(mapping = aes(x = long,
                             y = lat,
                             group = group,
                             fill = emissions_total))+
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.box.margin = margin(t = -10, b = 0),
    legend.key.width = unit(4, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)) +
  labs(title = "Emisja CO2",
       subtitle = "Rok 2023",
       fill = "Emisja CO2"
       ) +
  scale_fill_gradientn(
    colours = rev(brewer.pal(9, "RdYlGn")),
    trans = "log",
    breaks = c(1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10),
    labels = c("10 tys", "100 tys","1 mln", "10 mln", "100 mln", "1 mld", "10 mld")
  ) +
  coord_fixed(1.3)

world_in_2023



# Teraz pora na mape 50 lat temu

df_1973 <- df %>% 
  filter(Year == 1973)


map_joined <- map_world %>%
  left_join(df_1973, by = "region")

map_joined_filtered <- map_joined %>%
  filter(emissions_total > 0)


world_in_1973 <- map_joined_filtered %>% 
  ggplot() +
  geom_polygon(mapping = aes(x = long,
                             y = lat,
                             group = group,
                             fill = emissions_total))+
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.box.margin = margin(t = -10, b = 0),
    legend.key.width = unit(4, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)) +
  labs(title = "Emisja CO2",
       subtitle = "Rok 1973",
       fill = "Emisja CO2"
       ) +
  scale_fill_gradientn(
    colours = rev(brewer.pal(9, "RdYlGn")),
    trans = "log",
    breaks = c(1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10),
    labels = c("10 tys", "100 tys","1 mln", "10 mln", "100 mln", "1 mld", "10 mld")) +
  coord_fixed(1.3)

world_in_1973



# Na koniec czas na mapę 100 lat temu

df_1923 <- df %>% 
  filter(Year == 1923)

map_joined <- map_world %>%
  left_join(df_1923, by = "region")

map_joined_filtered <- map_joined %>%
  filter(emissions_total > 0)



world_in_1923 <- map_joined_filtered %>% 
  ggplot() +
  geom_polygon(mapping = aes(x = long,
                             y = lat,
                             group = group,
                             fill = emissions_total))+
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.box.margin = margin(t = -10, b = 0),
    legend.key.width = unit(4, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)) +
  labs(title = "Emisja CO2",
       subtitle = "Rok1923",
       fill = "Emisja CO2"
  ) +
  scale_fill_gradientn(
    colours = rev(brewer.pal(9, "RdYlGn")),
    trans = "log",
    breaks = c(1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10),
    labels = c("10 tys", "100 tys","1 mln", "10 mln", "100 mln", "1 mld", "10 mld")) +
  coord_fixed(1.3)

world_in_1923



# Patrząc na Chiny jak bardzo zeminiała sie emisja CO2
# na przestrzni 100 lat, chociaż to nie tyczy się wszytkich państw np. USA
