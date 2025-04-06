library(ggplot2)
library(dplyr)
library(SmarterPoland)
library(maps)
library(mapdata)

world <- map_data("world")
europe <- countries
roadf <- read.csv("fatalities.csv")

#dane z 2017-2021

roadf$Value[roadf$Value == ".."] <- 0
roadf$CountryName[roadf$CountryName == "Czechia"] <- "Czech Republic"
roadf$CountryName[roadf$CountryName == "United Kingdom"] <- "UK"
roadf$CountryName[roadf$CountryName == "Turkiye"] <- "Turkey"
roadf$CountryName[roadf$CountryName == "Republic of Moldova"] <- "Moldova"

result <- roadf %>% 
  select(CountryName,Value,PeriodCode) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  left_join(europe, by = c("CountryName" = "country")) %>% 
  filter(PeriodCode > 2018 & PeriodCode < 2022) %>% 
  group_by(CountryName) %>% 
  summarise(mean = round(mean(Value))) %>% 
  filter(mean != 0) %>% 
  mutate(bucket = case_when(mean < 100 ~ "(0,100)", 
                         mean >= 100 & mean < 300 ~ "[100,300)",
                         mean >= 300 & mean < 500 ~ "[300,500)",
                         mean >= 500 & mean < 700 ~ "[500,700)",
                         mean >= 700 ~ "[700,inf)"))

map_europe <- world %>%
  filter(long > -25, long < 45, lat > 33, lat < 72) %>% 
  filter(!region %in% c("Greenland")) %>% 
  left_join(result, by = c("region" = "CountryName"))

ggplot() +
  geom_polygon(data = map_europe, aes(x = long, y = lat, group = group, fill = bucket), color = "white", linewidth = 0.5) +
  coord_fixed() +
  theme_minimal() +
  scale_fill_manual(name = "liczba ofiar", values = c("#b2e0b7", "#80c88e", "#4da65f", "#2c8a37", "#1a6d23")) +
  labs(title = "Średnia roczna liczba ofiar śmiertelnych wypadków drogowych w Europie",
       subtitle = "dane z okresu 2019-2021") +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 13))

