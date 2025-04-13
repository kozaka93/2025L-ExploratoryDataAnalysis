library(ggplot)
library(tidyverse)
library(maps)
library(dplyr)
library(mapdata)

data <- readxl::read_xlsx("tps00198_page_spreadsheet.xlsx", sheet = "Sheet 1", skip = 6)

data <- data %>%
  select(!c(...3, ...5, ...7, ...9, ...11, ...13, ...15, ...17, ...19, ...21, ...23, ...25)) 

data <- data[-c(1, 3, 4, 53:63), ] %>%
  rename(region = TIME)

data1 <- data %>%
  pivot_longer(!region, names_to = "year", values_to = "value") %>%
  mutate(year = factor(year), value = as.numeric(value)) %>%
  pivot_wider(values_from = "value", names_from = "year") %>%
  mutate(ten_years_diff = as.numeric(data[, "2023"]$"2023") - as.numeric(data[, "2013"]$"2013"),
         region = case_when((region == "Türkiye") ~ "Turkey",
                            (region == "Kosovo*") ~ "Kosovo",
                            TRUE ~ region)) 
  

eu <- map_data('world') %>%
  filter(long < 180, long > -180)

data2 <- data1 %>%
  select(region, ten_years_diff) %>%
  full_join(eu)

ggplot() + 
  geom_polygon(data = data2, aes(x = long, y = lat, group = group, fill = ten_years_diff, color = "NA")) +
  coord_map(xlim = (c(-15, 40)), ylim = (c(36, 68)), "lambert", -15, 40) +
  scale_fill_gradient2(na.value = "grey", high = "#49006a", low = "#fff7f3", mid = "#f768a1", midpoint = 7) +
  scale_color_manual(values = 'grey', labels = 'brak danych') +
  guides(color = guide_legend(override.aes = list(fill = "grey"))) +
  labs(title = "Zmiana współczynnika obciążenia demograficznego",
       subtitle = "między 2013 a 2023 rokiem",
       fill = "różnica [p.p.]",
       x = element_blank(),
       y = element_blank(),
       color = element_blank()) +
  annotate("text",
           x = 27, y = 52,
           label = "Współczynnik obciążenia \ndemograficznego =\nliczba osób w wieku 65+\ndo liczby osób w wieku 15-65",
           hjust = 0,
           size = 2.5,
           color = "black") +
  annotate("text",
           x = -17.5, y = 34,
           label = "źródło: eurostat",
           hjust = 0,
           size = 2.1,
           color = "black") +
  theme_minimal()
