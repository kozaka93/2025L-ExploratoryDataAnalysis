
library(stringr)
library(stringi)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)

count_syllables <- function(name) {
  name <- tolower(name)
  name <- str_replace_all(name, c("í" = "i", "á" = "a", "é" = "e", "ó" = "o", "ú" = "u"))
  
  # Definicja wyjątków
  exceptions <- c("yelyzaveta" = 5, "anastasiya" = 5, "yelizaveta" = 5, "yevanhelina" = 5,
                  "mia" = 2, "lia" = 2, "pia" = 2)
  
  if (name %in% names(exceptions)) {
    return(exceptions[[name]])
  }
  
  vowels <- c("a", "e", "i", "o", "u", "y", "ą", "ę", "ó")
  vowel_count <- str_count(name, paste(vowels, collapse = "|"))
  
  vowel_groups <- c("ia", "ie", "io", "iu", "uo", "eu", "au")
  group_count <- str_count(name, paste(vowel_groups, collapse = "|"))
  
  syllables <- vowel_count - group_count
  
  return(max(1, syllables))
}

imiona_lata_2000_2019 <-read.csv("Imiona_nadane_wPolsce_w_latach_2000-2019.csv", stringsAsFactors = FALSE)
imiona_lata_2000_2019_zsumowane <- imiona_lata_2000_2019 %>%
  group_by(Imię, Płeć) %>%
  summarise(Liczba = sum(Liczba)) %>%
  arrange(desc(Liczba)) 

meskie_imie_piersze_2020 <- read.csv("2020_męskie_imię_pierwsze.csv" , stringsAsFactors = FALSE)
meskie_imie_piersze_2020_zsumowane <-   meskie_imie_piersze_2020 %>%
  select(Imię = IMIĘ_PIERWSZE, Płeć = PŁEĆ, Liczba = LICZBA_WYSTĄPIEŃ) %>%
  mutate(Płeć = "M") 


meskie_imie_piersze_2021 <- read.csv("2021_męskie_imię_pierwsze.csv", stringsAsFactors = FALSE)
meskie_imie_piersze_2021_zsumowane <- meskie_imie_piersze_2021 %>%
  select(Imię = IMIĘ_PIERWSZE, Płeć = PŁEĆ, Liczba = LICZBA_WYSTĄPIEŃ) %>%
  mutate(Płeć = "M")

meskie_imie_piersze_2022 <- read.csv("2022_męskie_imię_pierwsze.csv", stringsAsFactors = FALSE)
meskie_imie_piersze_2022_zsumowane <- meskie_imie_piersze_2022 %>%
  select(Imię = IMIĘ.PIERWSZE, Płeć = PŁEĆ, Liczba = LICZBA.WYSTĄPIEŃ) %>%
  mutate(Płeć = "M")


meskie_imie_piersze_2023 <- read.csv("2023_męskie_imię_pierwsze.csv", stringsAsFactors = FALSE)
meskie_imie_piersze_2023_zsumowane <- meskie_imie_piersze_2023 %>%
  select(Imię = IMIĘ_PIERWSZE, Płeć = PŁEĆ, Liczba = LICZBA_WYSTĄPIEŃ) %>%
  mutate(Płeć = "M")

meskie_imie_piersze_2024 <- read.csv("2024_męskie_imię_pierwsze.csv", stringsAsFactors = FALSE)
meskie_imie_piersze_2024_zsumowane <- meskie_imie_piersze_2024 %>%
  select(Imię = IMIĘ_PIERWSZE, Płeć = PŁEĆ, Liczba = LICZBA_WYSTĄPIEŃ) %>%
  mutate(Płeć = "M")

# Wczytanie i przekształcenie danych dla kobiet
damskie_imie_piersze_2020 <- read.csv("2020_żeńskie_imię_pierwsze.csv", stringsAsFactors = FALSE)
damskie_imie_piersze_2020_zsumowane <- damskie_imie_piersze_2020 %>%
  select(Imię = IMIĘ_PIERWSZE, Płeć = PŁEĆ, Liczba = LICZBA_WYSTĄPIEŃ) %>%
  mutate(Płeć = "K")

damskie_imie_piersze_2021 <- read.csv("2021_żeńskie_imię_pierwsze.csv", stringsAsFactors = FALSE)
damskie_imie_piersze_2021_zsumowane <- damskie_imie_piersze_2021 %>%
  select(Imię = IMIĘ_PIERWSZE, Płeć = PŁEĆ, Liczba = LICZBA_WYSTĄPIEŃ) %>%
  mutate(Płeć = "K")

damskie_imie_piersze_2022 <- read.csv("2022_żeńskie_imię_pierwsze.csv", stringsAsFactors = FALSE)
damskie_imie_piersze_2022_zsumowane <- damskie_imie_piersze_2022 %>%
  select(Imię = IMIĘ.PIERWSZE, Płeć = PŁEĆ, Liczba = LICZBA.WYSTĄPIEŃ) %>%
  mutate(Płeć = "K")

damskie_imie_piersze_2023 <- read.csv("2023_żeńskie_imię_pierwsze.csv", stringsAsFactors = FALSE)
damskie_imie_piersze_2023_zsumowane <- damskie_imie_piersze_2023 %>%
  select(Imię = IMIĘ_PIERWSZE, Płeć = PŁEĆ, Liczba = LICZBA_WYSTĄPIEŃ) %>%
  mutate(Płeć = "K")

damskie_imie_piersze_2024 <- read.csv("2024_żeńskie_imię_pierwsze.csv", stringsAsFactors = FALSE)
damskie_imie_piersze_2024_zsumowane <- damskie_imie_piersze_2024 %>%
  select(Imię = IMIĘ_PIERWSZE, Płeć = PŁEĆ, Liczba = LICZBA_WYSTĄPIEŃ) %>%
  mutate(Płeć = "K")

# Połączenie wszystkich tabel
wszystkie_imiona <- bind_rows(
  imiona_lata_2000_2019_zsumowane,
  meskie_imie_piersze_2020_zsumowane,
  meskie_imie_piersze_2021_zsumowane,
  meskie_imie_piersze_2022_zsumowane,
  meskie_imie_piersze_2023_zsumowane,
  meskie_imie_piersze_2024_zsumowane,
  damskie_imie_piersze_2020_zsumowane,
  damskie_imie_piersze_2021_zsumowane,
  damskie_imie_piersze_2022_zsumowane,
  damskie_imie_piersze_2023_zsumowane,
  damskie_imie_piersze_2024_zsumowane
)

# Sumowanie liczby wystąpień imion
wszystkie_imiona_zsumowane <- wszystkie_imiona %>%
  group_by(Imię, Płeć) %>%
  summarise(Liczba = sum(Liczba, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Liczba)) 

wszystkie_imiona_zsumowane$sylaby <- sapply(wszystkie_imiona_zsumowane$Imię, count_syllables)
write.csv(wszystkie_imiona_zsumowane, "wszystkie_imiona_zsumowane_sylaby.csv", row.names = FALSE)
wszystkie_imiona_zsumowane_sylaby<- read.csv("wszystkie_imiona_zsumowane_sylaby.csv")




# Średnia liczba sylab w imionach na przestrzeni lat
wszystkie_imiona_zsumowane_sylaby <- wszystkie_imiona_zsumowane_sylaby %>%
  mutate(Rok = case_when(
    Imię %in% meskie_imie_piersze_2020$IMIĘ_PIERWSZE ~ 2020,
    Imię %in% meskie_imie_piersze_2021$IMIĘ_PIERWSZE ~ 2021,
    Imię %in% meskie_imie_piersze_2022$IMIĘ.PIERWSZE ~ 2022,
    Imię %in% meskie_imie_piersze_2023$IMIĘ_PIERWSZE ~ 2023,
    Imię %in% meskie_imie_piersze_2024$IMIĘ_PIERWSZE ~ 2024,
    Imię %in% damskie_imie_piersze_2020$IMIĘ_PIERWSZE ~ 2020,
    Imię %in% damskie_imie_piersze_2021$IMIĘ_PIERWSZE ~ 2021,
    Imię %in% damskie_imie_piersze_2022$IMIĘ.PIERWSZE ~ 2022,
    Imię %in% damskie_imie_piersze_2023$IMIĘ_PIERWSZE ~ 2023,
    Imię %in% damskie_imie_piersze_2024$IMIĘ_PIERWSZE ~ 2024,
    TRUE ~ as.numeric(NA)
  )) %>%
  filter(!is.na(Rok))  

srednia_sylaby <- wszystkie_imiona_zsumowane_sylaby %>%
  group_by(Rok, Płeć) %>%
  summarise(SredniaSylaby = mean(sylaby, na.rm = TRUE), .groups = "drop")

ggplot(srednia_sylaby, aes(x = Rok, y = SredniaSylaby, color = Płeć)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Średnia liczba sylab w imionach na przestrzeni lat",
    x = "Rok",
    y = "Średnia liczba sylab",
    color = "Płeć"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2024, 2))  


# Czy liczba sylab wpływa na popularność imienia?
sylaby_w_latach <- wszystkie_imiona_zsumowane_sylaby %>%
  group_by(Rok, sylaby, Płeć) %>%
  summarise(Liczba = sum(Liczba, na.rm = TRUE), .groups = "drop")

sylaby_w_latach <- sylaby_w_latach %>%
  group_by(Rok, Płeć) %>%
  mutate(Liczba_norm = Liczba / max(Liczba, na.rm = TRUE))  # Skala 0-1 w każdym roku

heatplot <- ggplot(sylaby_w_latach, aes(x = Rok, y = sylaby, fill = Liczba_norm)) +
  geom_tile(color = "white") +
  facet_wrap(~Płeć, ncol = 1, scales = "free_y") +  
  scale_fill_gradientn(colors = c("seashell2", "seashell4", "palevioletred4", "deeppink1"), 
                       values = scales::rescale(c(0, 0.25, 0.75, 1)), 
                       name = "Popularność") +
  labs(
    title = "Najczęstsza liczba sylab w imionach na przestrzeni lat",
    x = "Rok",
    y = "Liczba sylab"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, color = "white", face = "bold"), 
    axis.text = element_text(size = 16, color = "white"),
    axis.title = element_text(size = 18, color = "white"),  
    axis.title.x = element_text(size = 18, color = "white"),  # Etykieta osi X
    axis.title.y = element_text(size = 18, color = "white"),  # Etykieta osi Y
    strip.text = element_text(size = 18, color = "white"),  
    legend.title = element_text(size = 18, color = "white", face = "bold"),
    legend.text = element_text(size = 16, color = "white"),
    panel.background = element_rect(fill = "black", color = NA),  
    plot.background = element_rect(fill = "black", color = NA)    
  )
heatplot



# Wykres dla kobiet (oryginalne kolory)
heatplot_k <- ggplot(sylaby_w_latach %>% filter(Płeć == "K"), 
                     aes(x = Rok, y = sylaby, fill = Liczba_norm)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("seashell2", "seashell4", "palevioletred4", "deeppink1"), 
                       values = scales::rescale(c(0, 0.25, 0.75, 1)), 
                       name = "Popularność") +
  labs(title = "Najczęstsza liczba sylab w imionach damskich", 
       x = "Rok", y = "Liczba sylab") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, color = "white", face = "bold"), 
    axis.text = element_text(size = 16, color = "white"),
    axis.title = element_text(size = 18, color = "white"),  
    strip.text = element_text(size = 18, color = "white"),  
    legend.title = element_text(size = 20, color = "white", face = "bold"),  
    legend.text = element_text(size = 18, color = "white"),  
    panel.background = element_rect(fill = "black", color = NA),  
    plot.background = element_rect(fill = "black", color = NA),
    legend.position = "right", 
    legend.key.height = unit(0.5, "cm"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )     
 
heatplot_k
# Wykres dla mężczyzn (zmienione kolory)
heatplot_m <- ggplot(sylaby_w_latach %>% filter(Płeć == "M"), 
                     aes(x = Rok, y = sylaby, fill = Liczba_norm)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("seashell2", "seashell4",  "cornflowerblue","navyblue"), 
                       values = scales::rescale(c(0, 0.25, 0.75, 1)), 
                       name = "Popularność") +
  labs(title = "Najczęstsza liczba sylab w imionach męskich", x = "Rok", y = "Liczba sylab") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, color = "white", face = "bold"), 
    axis.text = element_text(size = 16, color = "white"),
    axis.title = element_text(size = 18, color = "white"),  
    strip.text = element_text(size = 18, color = "white"),  
    legend.title = element_text(size = 18, color = "white", face = "bold"),
    legend.text = element_text(size = 16, color = "white"),
    panel.background = element_rect(fill = "black", color = NA),  
    plot.background = element_rect(fill = "black", color = NA),
    legend.position = "right",
    legend.key.height = unit(0.5, "cm"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()
  )

heatplot_m


ggsave("heatplot_kobiety.png", plot = heatplot_k, width = 8, height = 6, dpi = 300, bg = "transparent")

ggsave("heatplot_mezczyzni.png", plot = heatplot_m, width = 8, height = 6, dpi = 300, bg = "transparent")

ggsave("heatplot_kobiety.png", plot = heatplot_k + 
         theme(
           plot.background = element_blank(), 
           panel.background = element_blank(), 
           legend.position = "right",  
           legend.key.height = unit(0.5, "cm"),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()
         ), width = 8, height = 6, dpi = 300, bg = "transparent")

ggsave("heatplot_mezczyzni.png", plot = heatplot_m + 
         theme(
           plot.background = element_blank(), 
           panel.background = element_blank(), 
           legend.position = "right",  
           legend.key.height = unit(0.5, "cm"),
           panel.grid.major = element_blank(),  
           panel.grid.minor = element_blank()
         ), width = 8, height = 6, dpi = 300, bg = "transparent")







