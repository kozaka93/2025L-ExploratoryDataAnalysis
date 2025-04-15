library(ggplot2)
library(dplyr)
library(sf)
library(tidytext)
library(ggrepel)

top_10_zenskie_combined <- read.csv("top_10_zenskie_combined.csv")
top_10_meskie_combined <- read.csv("top_10_meskie_combined.csv")
wojewodztwa_mapa <- st_read("A01_Granice_wojewodztw.shp")
top_1_2024_wojewodztwa_meskie <- read.csv("top_1_2024_wojewodztwa_meskie.csv")
top_1_2024_wojewodztwa_zenskie <- read.csv("top_1_2024_wojewodztwa_zenskie.csv")

pdf("wykresy.pdf", width = 12, height = 8)

# Wykres dla imion żeńskich

top_10_zenskie_combined <- top_10_zenskie_combined %>%
  mutate(GRUPA_LAT = ifelse(as.numeric(as.character(ROK)) < 2015, "2010-2014", "2020-2024"),
         ROK = as.factor(ROK))

# wersja dla top_5

top_5_zenskie_combined %>% mutate(LABEL_IMIE =  ifelse(((IMIĘ == "JULIA" & ROK == 2010) |
                                                          (IMIĘ == "LENA" & ROK == 2014) |
                                                          (IMIĘ == "ZUZANNA" & ROK == 2014) | 
                                                          (IMIĘ == "MAJA" & ROK == 2014) |
                                                          (IMIĘ == "WIKTORIA" & ROK == 2010) | 
                                                          (IMIĘ == "ZOFIA" & ROK == 2014) |  
                                                          (IMIĘ == "HANNA" & ROK == 2024) |
                                                          (IMIĘ == "LAURA" & ROK == 2024)), IMIĘ, "") ) -> top_5_label_zenskie

top_5_label_zenskie$ROK <- as.numeric(as.character(top_5_label_zenskie$ROK))

ggplot(top_5_label_zenskie, aes(x = ROK, y = WSPÓŁCZYNNIK_WYSTĘPOWANIA, 
                                color = IMIĘ, group = interaction(IMIĘ, GRUPA_LAT), label = IMIĘ)) +
  geom_line(linewidth = 1) +        
  geom_point(size = 3)  +
  labs(title = "Współczynnik wystąpień imion żeńskich (2010-2014, 2020-2024)",
       x = "ROK",
       y = "Współczynnik wystąpień") +
  theme_minimal() +
  geom_label_repel(aes(label = LABEL_IMIE), max.overlaps = 20) +
  theme(
    panel.grid.major = element_line(color = "black", size = 0.3),
    panel.grid.minor = element_line(color = "black", size = 0.2),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "none"
    ) +
  scale_color_manual( values = c("#FF9999", "#F26D6D", "#D1495B", "#E25C9C",
                                 "#B51856", "#F18DA2", "#C63D8F", "#8E3B60",
                                 "#F47C7C", "#FA8E63", "#E2725B", "#EDA4D2",
                                 "#C94C7C", "#A05DBE", "#F5B5C5", "#645141"))
                                 

# Wykres dla imion męskich

top_10_meskie_combined <- top_10_meskie_combined %>%
  mutate(GRUPA_LAT = ifelse(as.numeric(as.character(ROK)) < 2015, "2010-2014", "2020-2024"),
         ROK = as.factor(ROK))

# wersja dla top_5

top_5_meskie_combined %>% mutate(LABEL_IMIE =  ifelse(((IMIĘ == "JAKUB" & ROK == 2014) |
                                                         (IMIĘ == "JAN" & ROK == 2014) |
                                                         (IMIĘ == "KACPER" & ROK == 2014) | 
                                                         (IMIĘ == "FILIP" & ROK == 2014) |
                                                         (IMIĘ == "SZYMON" & ROK == 2010) | 
                                                         (IMIĘ == "MICHAŁ" & ROK == 2010) | 
                                                         (IMIĘ == "ANTONI" & ROK == 2020) | 
                                                         (IMIĘ == "FRANCISZEK" & ROK == 2023) | 
                                                         (IMIĘ == "NIKODEM" & ROK == 2024) |
                                                         (IMIĘ == "ALEKSANDER" & ROK == 2020) |
                                                         (IMIĘ == "LEON" & ROK == 2024)), IMIĘ, "") ) -> top_5_label_meskie


top_5_label_meskie$ROK <- as.numeric(as.character(top_5_label_meskie$ROK))
ggplot(top_5_label_meskie, aes(x = ROK, y = WSPÓŁCZYNNIK_WYSTĘPOWANIA, 
                        color = IMIĘ, group = interaction(IMIĘ, GRUPA_LAT), label = IMIĘ)) +
  geom_line(linewidth = 1) +        
  geom_point(size = 3)  +
  labs(title = "Współczynnik wystąpień imion męskich (2010-2014, 2020-2024)",
       x = "ROK",
       y = "Współczynnik wystąpień") +
  theme_minimal() +
  geom_label_repel(aes(label = LABEL_IMIE), max.overlaps = 20) +
  theme(
    panel.grid.major = element_line(color = "black", size = 0.3),
    panel.grid.minor = element_line(color = "black", size = 0.2),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("#50A6C2", "#185D70", "#2C5C99", "#3666DC", "#40798C",
                                "#6FAE9E", "#1D7F6D", "#2B9E8D", "#3FAD87", "#0E4C3C", "#82CAD5"))

# WYKRES MAPKA NAJPOPULARNIEJSZEGO IMIENIA DAMSKIEGO I MESKIEGO NA WOJEWODZTWO

# wykres dla imion męskich

wojewodztwa_mapa_joined_meskie <- left_join(wojewodztwa_mapa, 
                                            top_1_2024_wojewodztwa_meskie, 
                                            by = c("JPT_NAZWA_" = "WOJEWÓDZTWO"))

ggplot(data = wojewodztwa_mapa_joined_meskie) + 
  geom_sf(fill = "#50A6C2", color = "black") + 
  geom_sf_text(aes(label = paste0("2024: ", IMIĘ_PIERWSZE)), size = 4, color = "black", nudge_y = 0) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# wykres dla imion żeńskich

wojewodztwa_mapa_joined_zenskie <- left_join(wojewodztwa_mapa, 
                                             top_1_2024_wojewodztwa_zenskie, 
                                             by = c("JPT_NAZWA_" = "WOJEWÓDZTWO"))

ggplot(data = wojewodztwa_mapa_joined_zenskie) + 
  geom_sf(fill = "#FF9999", color = "black") + 
  geom_sf_text(aes(label = paste0("2024: ", IMIĘ_PIERWSZE)), size = 4, color = "black", nudge_y = 0) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


# install.packages("wordcloud")

library(wordcloud)

# wykres wordcloud

# dla imion męskich

imiona_meskie_ranking <- top_5_meskie_combined %>% 
  group_by(IMIĘ) %>%
  summarise(LATA_W_TOP_5 = n_distinct(ROK))

wykres_cloud_meskie <- imiona_meskie_ranking %>% 
  with(wordcloud(
    words = IMIĘ,
    freq = LATA_W_TOP_5,
    min.freq = 1,
    max.words = 20,
    scale = c(5, 1),
    colors = c("#3A7F97", "#1D5D73", "#135A6B", "#1B7D86", "#358D9D"),
    random.order = TRUE
  ))

# dla imion żeńskich

imiona_zenskie_ranking <- top_5_zenskie_combined %>% 
  group_by(IMIĘ) %>%
  summarise(LATA_W_TOP_5 = n_distinct(ROK))

wwwykres_cloud_zenskie <- imiona_zenskie_ranking %>% 
  with(wordcloud(
    words = IMIĘ,
    freq = LATA_W_TOP_5,
    min.freq = 1,
    max.words = 20,
    scale = c(5, 1),
    colors = c("#FF9999", "#FFCCCC", "#FFB3B3", "#FF8080", "#FF6666"),
    random.order = TRUE
  ))

dev.off()
