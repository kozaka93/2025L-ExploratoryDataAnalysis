library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)

###################################### WYKRES 1 (ilosc dni z pokrywą sniezną powyzej 5cm)
locations <- list(
  c(54.3722, 18.6389),  # Gdansk
  c(50.0647, 19.9450)   #Kraków
)

get_snowfall_data <- function(lat, lon) {
  api_url <- "https://archive-api.open-meteo.com/v1/archive"
  
  params <- list(
    latitude = lat,
    longitude = lon,
    hourly = 'snow_depth',
    start_date = "1950-01-01",
    end_date = "2024-12-31",
    timezone = "Europe/Warsaw",
  )
  
  response <- GET(url = api_url, query = params)
  if (status_code(response) == 200) {
    data <- fromJSON(rawToChar(response$content))
    df <- as.data.frame(data$hourly) %>% mutate(latitude = lat, longitude = lon)
    return(df)
  } else {
    print(paste("Błąd pobierania danych dla:", lat, lon))
    return(NULL)
  }
}

hourly = bind_rows(lapply(locations, function(loc) get_snowfall_data(loc[1], loc[2])))

coords = data.frame(city=c('Zakopane','Warszawa', 'Gdansk', 'Kraków'),
                    latitude = c(49.2992, 52.2298, 54.3722, 50.0647),
                    longitude = c(19.9496, 21.0122, 18.6389, 19.9450))

depth= hourly %>% inner_join(coords, by = c("latitude", "longitude"))
depth = depth %>% mutate(time = as.Date(time)) %>% 
  filter(city == "Kraków" | city == "Gdansk") %>% 
  group_by(time) %>% 
  mutate(hour = rep(0:23, length.out = n())) %>%
  ungroup()
  

depth = depth %>% mutate(snow_depth = snow_depth * 100) %>% 
  group_by(city, time) %>% 
  summarise(pokrywa = ifelse(any(snow_depth >= 5), 1, 0)) %>% 
  mutate(year = year(time)) %>% 
  group_by(city, year) %>% summarise(liczba_dni = sum(pokrywa, na.rm = T)) %>% 
  mutate(trend = rollmean(liczba_dni, k = 10, fill = NA, align = "right"))  


View(depth)


ggplot(depth, aes(x = year, y = liczba_dni, color = city, group = city)) +  
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_line(aes(y = trend), size = 1.2, color = "red") +  
  labs(title = "Ilość dni z pokrywą śnieżną conajmniej 5cm",
       x = "Rok",
       y = "Ilość dni",
       color = "Miasto") +
  theme_minimal() +
  facet_wrap(~ city, scales = "free_y") +  
  theme(strip.text = element_text(size = 14, face = "bold")) 


########################### WYKRES 2 (trend dla miast wojewódzkich i dla gdanska, krakowa, warszawy osobno) !!!

locations <- list(
  Warszawa = c(52.2298, 21.0122),
  Kraków = c(50.0647, 19.9450),
  Wrocław = c(51.1079, 17.0385),
  Poznań = c(52.4064, 16.9252),
  Szczecin = c(53.4285, 14.5528),
  Łódź = c(51.7592, 19.4550),
  Rzeszów = c(50.0413, 21.9990),
  Gdańsk = c(54.3520, 18.6466),
  Kielce = c(50.8661, 20.6286),
  Toruń = c(53.0138, 18.5984),
  Białystok = c(53.1325, 23.1688),
  Zielona_Góra = c(51.9356, 15.5062),
  Lublin = c(51.2465, 22.5684),
  Opole = c(50.2863, 18.6708),
  Olsztyn = c(53.7757, 20.4900),
  Katowice = c(50.2649, 19.0238)
)


get_snowfall_data <- function(lat, lon) {
  api_url <- "https://archive-api.open-meteo.com/v1/archive"
  
  params <- list(
    latitude = lat,
    longitude = lon,
    daily = "snowfall_sum",
    start_date = "1950-01-01",
    end_date = "2024-12-31",
    timezone = "Europe/Warsaw"
  )
  
  response <- GET(url = api_url, query = params)
  if (status_code(response) == 200) {
    data <- fromJSON(rawToChar(response$content))
    df <- as.data.frame(data$daily) %>% mutate(latitude = lat, longitude = lon)
    return(df)
  } else {
    print(paste("Błąd pobierania danych dla:", lat, lon))
    return(NULL)
  }
}


zakopane <- bind_rows(lapply(locations, function(loc) {
  Sys.sleep(60)  
  get_snowfall_data(loc[1], loc[2])
}))

coords <- data.frame(
  city = c('Warszawa', 'Kraków', 'Wrocław', 'Poznań', 'Szczecin', 'Łódź', 'Rzeszów', 'Gdańsk', 
           'Kielce', 'Toruń', 'Białystok', 'Zielona Góra', 'Lublin', 'Opole', 'Olsztyn', 'Katowice'),
  latitude = c(52.2298, 50.0647, 51.1079, 52.4064, 53.4285, 51.7592, 50.0413, 54.3520,
               50.8661, 53.0138, 53.1325, 51.9356, 51.2465, 50.2863, 53.7757, 50.2649),
  longitude = c(21.0122, 19.9450, 17.0385, 16.9252, 14.5528, 19.4550, 21.9990, 18.6466,
                20.6286, 18.5984, 23.1688, 15.5062, 22.5684, 18.6708, 20.4900, 19.0238))


zakopane = zakopane %>% inner_join(coords, by = c("latitude", "longitude"))
zakopane = zakopane %>% mutate(time = as.Date(time), month = month(time), year = year(time))


zakopane_trend = zakopane %>%
  group_by(year) %>%
  summarise(total_amount = sum(snowfall_sum, na.rm = TRUE)) %>%
  mutate(trend = rollmean(total_amount, k = 10, fill = NA, align = "right"))  

ggplot(zakopane_trend, aes(x = year, y = total_amount)) +  
  geom_line(size = 1, color = "#66B2FF") +  
  geom_point(size = 2, color = "#66B2FF") +  
  geom_line(aes(y = trend), size = 1.2, color = "red") +
  labs(title = "Skumulowana ilość śniegu w ciągu roku w miastach wojewódzkich",
       x = "Rok",
       y = "Skumulowane opady śniegu",
       color = "Miasto") +
  theme_minimal() 


#drugi wykres dla miast wyczególnionych 

zakopane_trend2 = zakopane %>%
  filter(city %in% c("Gdańsk", "Kraków", "Warszawa")) %>%
  group_by(year, city) %>%
  summarise(total_amount = sum(snowfall_sum, na.rm = TRUE), .groups = "drop") %>%
  group_by(city) %>%  
  mutate(trend = rollmean(total_amount, k = 10, fill = NA, align = "right"))

ggplot(zakopane_trend2, aes(x = year, y = total_amount, color = city, group = city)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 2, alpha = 0.3) +
  geom_line(aes(y = trend, group = city, color = city), size = 1.2) +  
  facet_grid(cols = vars(city), scales = "free_x") +
  scale_color_manual(values = c("Gdańsk" = "#4B0082", "Warszawa" = "#00008B", "Kraków" = "#00695C"))+
  labs(title = "Analiza trendu opadów śniegu", x = "Rok", y = "Suma opadów śniegu (cm)", color = 'miasto') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  theme(panel.spacing = unit(1.5, "lines"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = 'bold'), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, face = 'bold'),
        panel.background = element_rect(fill = "transparent", color = NA),  
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "grey70"),
        panel.grid.minor = element_line(color = "gray70")
        )  

ggsave("plot001.png", width = 12, height = 12 / 2.28356, units = "in", dpi = 300, bg = "transparent")


####################################### WYKRES 3 (rozklad dla miast wojewodzkich) !!!

pom1 = zakopane %>% group_by(city, year) %>% 
  summarise(suma_poprzednia = sum(snowfall_sum, na.rm = T)) %>% 
  filter(year %in% c(1960, 1961, 1962, 1963, 1964, 1965)) %>% group_by(city) %>% 
  summarise(`1960-1965` = mean(suma_poprzednia, na.rm = T))

pom2 = zakopane %>% group_by(city, year) %>% 
  summarise(suma_aktualna = sum(snowfall_sum, na.rm = T)) %>% 
  filter(year %in% c(2019, 2020, 2021, 2022, 2023, 2024)) %>% group_by(city) %>% 
  summarise(`2019-2024` = mean(suma_aktualna, na.rm = T))

pom1 %>% inner_join(pom2, by = 'city') %>%
  ggplot(aes(y = reorder(city, `1960-1965`))) +
  geom_segment(aes(x = `1960-1965`, xend = `2019-2024`, yend = city), color = "black", linewidth = 0.5) +
  geom_point(aes(x = `1960-1965`, color = "1960-1965"), size = 3.5) +
  geom_point(aes(x = `2019-2024`, color = "2019-2024"), size = 3.5) +
  scale_color_manual(values = c("1960-1965" = "blue", "2019-2024" = "red"),
                     name = "Okres") +
  labs(
    title = "Średnia suma opadów śniegu w miastach wojewódzkich",
    x = "Opady śniegu (cm)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 14, face = 'bold'),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = 'bold'), 
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.background = element_rect(fill = "transparent", color = NA),  
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "gray70")
  )

ggsave("plot002.png", dpi = 300, bg = "transparent")

####################################### WYKRES 4 (rozkład miesięczny i udzial dni snieznych w roku) !!!

#dla miast wyszczególnionych
  
zakopane %>%
  filter(year >= 2000 & city %in% c("Gdańsk", "Kraków", "Warszawa")) %>%
  group_by(year, month, city) %>%
  summarise(total_amount = sum(snowfall_sum, na.rm = TRUE)) %>%
  group_by(month, city) %>%
  summarise(total_snowfall = mean(total_amount)) %>%
  
  ggplot(aes(x = as.factor(month), y = total_snowfall, fill = city)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Gdańsk" = "#4B0082", "Warszawa" = "#00008B", "Kraków" = "#00695C")) + 
  labs(title = "Średnia suma opadów śniegu dla poszczególnych miesiący w latach 2000-2024",
     x = "Miesiąc",
     y = "Suma opadów śniegu (cm)",
     fill = "Miasto") +
  scale_x_discrete(labels = c("1" = "Sty", "2" = "Lut", "3" = "Mar", "4" = "Kwi", "5" = "Maj", "6" = "Cze", 
                              "7" = "Lip", "8" = "Sie", "9" = "Wrz", "10" = "Paź", "11" = "Lis", "12" = "Gru")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  theme(legend.title = element_text(size = 14, face = 'bold'),
        legend.text  = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = 'bold'),  
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, face = 'bold'),
        panel.background = element_rect(fill = "transparent", color = NA),  
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "grey70"),
        panel.grid.minor = element_line(color = "gray70")
        )  

ggsave("plot003.png", width = 12, height = 12 / 2.33356, units = "in", dpi = 300, bg = "transparent")



##nie bierzemy
zakopane %>%
  mutate(snowy_day = snowfall_sum > 0) %>%  # TRUE dla dni z opadami, FALSE dla bez opadów
  group_by(city, year) %>%
  summarise(procent_dni = sum(snowy_day) / n() * 100) %>%
  ggplot(aes(x = year, y = procent_dni, fill = city)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "Histogram udziału dni śnieżnych dla każdego roku",
       x = "Rok",
       y = "Procent dni z opadami śniegu (%)",
       fill = "Miasto") +
  theme_minimal()


########################################################## WYKRES 5 (zaleznosc od wysokosci)

get_elevation <- function(lat, lon) {
  url <- paste0("https://api.open-meteo.com/v1/elevation?latitude=", lat, "&longitude=", lon)
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  if (!is.null(data$elevation)) {
    return(data$elevation)  # Extract elevation directly
  } else {
    return(NA)  # Return NA if no data
  }
}

lat_min = 49
lat_max = 55
lon_min = 14
lon_max = 24

set.seed(123)
random_points <- data.frame(
  lat = runif(300, lat_min, lat_max),
  lon = runif(300, lon_min, lon_max)
)

random_points <- random_points %>%
  rowwise() %>%
  mutate(elevation = get_elevation(lat, lon)) %>%
  ungroup() 


get_snowfall <- function(lat, lon) {
  api_url <- "https://archive-api.open-meteo.com/v1/archive"
  
  params <- list(
    latitude = lat,
    longitude = lon,
    daily = "snowfall_sum",
    start_date = "2021-01-01",
    end_date = "2024-12-31",
    timezone = "Europe/Warsaw"
  )
  
  response <- GET(url = api_url, query = params)
  if (status_code(response) == 200) {
    data <- fromJSON(rawToChar(response$content))
    df <- as.data.frame(data$daily) %>% mutate(latitude = lat, longitude = lon)
    return(df)
  } else {
    print(paste("Błąd pobierania danych dla:", lat, lon))
    return(NULL)
  }
}


snowfall_data_list = lapply(1:nrow(random_points), function(i) {
  Sys.sleep(0.6) 
  get_snowfall(random_points$lat[i], random_points$lon[i])
})


snowfall_data = bind_rows(snowfall_data_list)


data = snowfall_data %>% inner_join(random_points, by = c('latitude' = 'lat', 'longitude' = 'lon'))

data = data %>%
  mutate(date = as.Date(time), month = month(date), year = year(date)) %>%
  group_by(latitude, longitude, year) %>%
  summarise(suma = sum(snowfall_sum), .groups = "drop") %>%
  group_by(latitude, longitude) %>%
  summarise(srednia = mean(suma), .groups = "drop") %>%
  ungroup() %>% 
  left_join(data %>% select(latitude, longitude, elevation) %>% 
  distinct(latitude, longitude, .keep_all = TRUE),by = c('latitude', 'longitude')) %>% 
  filter(elevation <= 1000)

View(data)

#wykres zaleznosci sniegu od wysokosci

  ggplot(data, aes(x = elevation, y = srednia)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Linia regresji
  labs(title = "Zależność sumy opadów śniegu od wysokości",
       x = "Wysokość n.p.m. (m)",
       y = "Średnia suma opadów śniegu (cm)") +
  theme_minimal()

 
  
  
  
  
