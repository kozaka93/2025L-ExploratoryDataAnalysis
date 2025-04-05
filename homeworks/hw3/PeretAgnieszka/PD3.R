install.packages("SmarterPoland")
library(dplyr)
library(ggplot2)
library(maps)
library(tidyr)

df <- read.csv2("mrufki2.csv")

df2 <- df %>% 
  filter(Occurrence %in% c("Present","Likely")) %>% 
  group_by(TaxonName, Country) %>% 
  summarise(l_reg=n()) %>% 
  group_by(Country) %>% 
  summarise(n=n())
  

install.packages("countrycode")
library(countrycode)

df2$Country[df2$Country == "United Kingdom"] <- "UK"
df2$Country[df2$Country == "United States"] <- "USA"
df2$Country[df2$Country == "Czechia"] <- "Czech Republic"
df2$Country[df2$Country == "Congo"] <- "Republic of Congo"
df2$Country[df2$Country == "Côte d’Ivoire"] <- "Ivory Coast"
df2$Country[df2$Country == "Palestinian Territories"] <- " Palestine"
df2$Country[df2$Country == "Myanmar (Burma)"] <- "Myanmar"
df2$Country[df2$Country == "Byelarus"] <- "Belarus"

w1 <- map_data("world")
w1 <- left_join(w1, df2, by=c("region" = "Country"))

w1$n[w1$region == "South Sudan"] <- 59

world <- ggplot() + 
  geom_polygon(data = w1, aes(x = long, y = lat, group = group, fill = n)) +  
  coord_map("mollweide") +
  coord_fixed(1.3) +  
  scale_fill_fermenter(palette = "YlOrRd", trans = "log10", direction = 1) + 
  theme_void() +  
  labs(
    title = "Rozmieszczenie liczby gatunków mrowek na świecie", 
    fill = "Wskaźnik ilości gatunków mrówek", 
    subtitle = "Skala logarytmiczna w liczbie gatunków mrowek",  
    caption = "Źródło danych: www.antwiki.org"
  ) + 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(size = 12, hjust = 0.5),  
    plot.caption = element_text(size = 8, hjust = 1), 
    legend.position = "right"  
  )

world
