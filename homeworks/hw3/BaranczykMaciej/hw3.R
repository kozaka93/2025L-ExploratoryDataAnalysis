library(ggplot2)
library(dplyr)
library(mapdata)
library(stringr)

#Źródło danych 
#https://data.worldbank.org/indicator/SP.DYN.TFRT.IN

#Pobieranie danych i wybieranie najnowszych danych, czyli roku 2022
df<-read.csv(file="API_SP.DYN.TFRT.IN_DS2_EN_csv_v2_26557.csv",sep=',')
df<-df %>% 
  select('Country.Name','X2022') %>% 
  rename('year_2022'='X2022')
w1 <- map_data("world")

#Szukanie różnic w zapisie nazw państw
diff <- setdiff(df$Country.Name, w1$region)
diff2<- setdiff(w1$region, df$Country.Name)

#Uzgadnianie zapisu nazwy państw
df <- df %>%
  mutate(Country.Name = recode(str_trim(Country.Name), 
                               "Antigua and Barbuda" = "Antigua",
                               "Bahamas, The" = "Bahamas",
                               "Brunei Darussalam" = "Brunei",
                               "Cote d'Ivoire" = "Ivory Coast",
                               "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                               "Congo, Rep." = "Republic of Congo",
                               "Cabo Verde" = "Cape Verde",
                               "Czechia" = "Czech Republic",
                               "Egypt, Arab Rep." = "Egypt",
                               "Micronesia, Fed. Sts." = "Micronesia",
                               "United Kingdom" = "UK",
                               "Gambia, The" = "Gambia",
                               "Iran, Islamic Rep." = "Iran",
                               "Kyrgyz Republic" = "Kyrgyzstan",
                               "St. Kitts and Nevis" = "Saint Kitts",
                               "Korea, Rep." = "South Korea",
                               "Lao PDR" = "Laos",
                               "St. Lucia" = "Saint Lucia",
                               "St. Martin (French part)" = "Saint Martin",
                               "Korea, Dem. People's Rep." = "North Korea",
                               "Slovak Republic" = "Slovakia",
                               "Eswatini" = "Swaziland",
                               "Syrian Arab Republic" = "Syria",
                               "Trinidad and Tobago" = "Tobago",
                               "Turkiye" = "Turkey",
                               "United States" = "USA",
                               "St. Vincent and the Grenadines" = "Saint Vincent",
                               "Venezuela, RB" = "Venezuela",
                               "British Virgin Islands" = "Virgin Islands",
                               "Viet Nam" = "Vietnam",
                               "Yemen, Rep." = "Yemen",
                               "Russian Federation" = "Russia"
  ))



df_merged<-inner_join(df,w1,by=join_by(Country.Name==region))

map <- ggplot() + 
  geom_polygon(data=df_merged, aes(x = long, y = lat, group = group, fill = year_2022), color = 'white',
    linewidth = 0.001) +
  theme_void() +
  scale_fill_viridis_c(guide = guide_colorbar(barwidth = 10, 
      barheight = 0.35)) +
  labs(fill = "Fertility Rate",
    title = "Fertility Rate by Country in 2022",
    subtitle = "Average number of children born to women \nover their lifetime",
    caption = "Source: World Bank") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5,size = 12),
    legend.title.position = "top",
    plot.caption = element_text(hjust = 0.5, size = 9)) +
  coord_fixed()

map
