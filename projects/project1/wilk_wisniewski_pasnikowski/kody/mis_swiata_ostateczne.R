library(dplyr)
library(ggplot2)
library(scales)

setwd("D:/Studia/4 semest/Eksploracja/Projekt")
df <- read.csv("mistrzostwa_swiata1.csv", sep = ";")

df$Zapelnienie <- gsub(",", ".", df$Zapelnienie) 

df <- df %>% 
  select(Gospodarz, Gosc, Kraj, Ogladalnosc, Zapelnienie) %>% 
  mutate(Ogladalnosc=as.numeric(Ogladalnosc)) %>% 
  mutate(Zapelnienie=as.numeric(Zapelnienie))

df_gospodarz <- df %>%
  select(Gospodarz, Ogladalnosc, Zapelnienie) %>%
  rename(kraj = Gospodarz) 

df_gosc <- df %>%
  select(Gosc, Ogladalnosc, Zapelnienie) %>%
  rename(kraj = Gosc)

df_combined <- bind_rows(df_gospodarz, df_gosc)

top_countries <- df_combined %>% 
  group_by(kraj) %>% 
  summarise(srednia_ogl = mean(Ogladalnosc, na.rm = TRUE)) %>% 
  arrange(desc(srednia_ogl)) %>% 
  top_n(7, srednia_ogl) %>% 
  pull(kraj)

### wykres 1
df_combined %>% 
  filter(kraj %in% top_countries) %>% 
  ggplot(aes(x = reorder(kraj, Zapelnienie), y = Zapelnienie)) +
  geom_boxplot(fill = "#4d68f7", color = "black",lwd =0.7) +
  scale_y_continuous(expand=c(0,0), labels = percent_format())+
  labs(title = "Rozkład zapełnienia stadionów dla krajów z najlepszą frekwencją",
       x = "Kraje",
       y = "Procentowe zapełnienie") +
  theme_minimal(base_size = 16) +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#f6b713", color = NA),
    panel.grid.major = element_line(color = "#c29306"),
    panel.grid.minor = element_line(color = "#c29306"),
    axis.title.x = element_text(color = "black", size = 14),
    axis.title.y = element_text(color = "black", size = 14),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  )

### wykres 2
df %>%
  filter(Kraj %in% c("Polska", "Slowenia")) %>%
  ggplot(aes(x = Kraj, y = Zapelnienie, fill = Kraj)) +
  geom_violin(alpha = 1, color = "black", linewidth = 1.2) +  # Obrys czarny, wypełnienie kolorowe
  scale_fill_manual(values = c("Polska" = "#4d68f7", "Slowenia" = "#4d68f7")) +
  geom_jitter(width = 0.1, alpha = 1, color = "#3f17b0", size = 3) +
  scale_y_continuous(expand=c(0,0), labels = percent_format(), limits = c(0,1.05))+
  labs(title = "Rozkład zapełnienia stadionów na meczach w Polsce i Słowenii",
       x = "Kraj",
       y = "Procentowe zapełnienie stadionu") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(hjust = 1, size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title = element_text(size = 16, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 18),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "#ffedab", color = NA),
    panel.grid.major = element_line(color = "#c29306"),
    panel.grid.minor = element_line(color = "#c29306"),
    panel.grid = element_blank()
  )


### wykres 3
df_poland <- df %>%
  filter(Gospodarz=="Polska" | Gosc == "Polska") %>% 
  mutate(Zapelnienie= as.numeric(gsub(",", ".", Zapelnienie)))

zapelnienie_stats <- df_poland %>%
  summarise(
    srednia_zapelnienie = mean(Zapelnienie, na.rm = TRUE),
    max_zapelnienie = max(Zapelnienie, na.rm = TRUE),
    min_zapelnienie = min(Zapelnienie, na.rm = TRUE),
    mediana_zapelnienie = median(Zapelnienie, na.rm = TRUE)
  )

df_poland <- df_poland %>%
  mutate(Gosc = case_when(
    Gosc == "USA" & duplicated(Gosc) ~ paste("USA", cumsum(Gosc == "USA"), sep = ""),
    TRUE ~ Gosc
  ))

df_poland <- df_poland %>%
  mutate(Ordinal = 1:n())

avg_zapelnienie <- mean(df_poland$Zapelnienie, na.rm = TRUE)

ggplot(df_poland, aes(x = factor(Gosc, levels = unique(Gosc)), y = Zapelnienie)) +
  geom_point(color = "#4a13e7", size = 4, alpha = 1) +  
  geom_line(aes(group = 1), color = "#4a13e7", alpha = 1, size=1) +  
  geom_hline(yintercept = avg_zapelnienie, linetype = "dashed", color = "#3f17b0", size = 1) +  
  annotate("text", x = 1, y = avg_zapelnienie + 0.02, 
           label = paste("Średnie zapelnienie\nna meczach Polski:", round(avg_zapelnienie, 2)),
           color = "#3f17b0", size = 5, hjust = 0) +  
  scale_y_continuous(expand=c(0,0), labels = percent_format(), limits = c(0.8,1.01))+
  labs(title = "Zapelnienie stadionu na meczach reprezentacji Polski",
       x = "Przeciwnik",
       y = "Procentowe zapełnienie") +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 18),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ffedab", color = NA),
    panel.grid.major = element_line(color = alpha("#c29306",0.5)),
    panel.grid.minor = element_line(color = alpha("#c29306"),0.5)
  )

### Wykres 4

df_slowenia <- df %>%
  filter(Gospodarz=="Slowenia" | Gosc == "Slowenia") %>% 
  mutate(Zapelnienie= as.numeric(gsub(",", ".", Zapelnienie)))

df_slowenia <- df_slowenia %>%
  mutate(Gosc = case_when(
    Gosc == "Niemcy" & duplicated(Gosc) ~ paste("Niemcy", cumsum(Gosc == "Niemcy"), sep = ""),
    TRUE ~ Gosc
  ))


avg_zapelnienie_1 <- mean(df_slowenia$Zapelnienie, na.rm = TRUE)

slowenia_2 <- df_slowenia %>% 
  filter(Kraj=="Slowenia") 

avg_zapelnienie_2 <- mean(slowenia_2$Zapelnienie, na.rm = TRUE)

df_slowenia_2 <- df_slowenia %>%
  mutate(Kolor = case_when(
    row_number() > (n() - 2) ~ "Polska",  
    TRUE ~ "Słowenia"  
  ))


ggplot(df_slowenia_2, aes(x = factor(Gosc, levels = unique(Gosc)), y = Zapelnienie, color = Kolor)) +
  geom_line(aes(group = 1), color = "#4a13e7", alpha = 1, size = 1) +  
  geom_point(size = 4, alpha = 1) + 
  geom_hline(yintercept = avg_zapelnienie_1, linetype = "dashed", color = "#c29306", linewidth = 1) + 
  geom_hline(yintercept = avg_zapelnienie_2, linetype = "dashed", color = "#4d68f7", linewidth = 1) +
  scale_y_continuous(expand=c(0,0), labels = percent_format())+
  annotate("text", x = 2, y = avg_zapelnienie_1 - 0.07, 
           label = paste("Średnie zapelnienie\nna wszystkich meczach Słowenii:", round(avg_zapelnienie_1, 2)),
           color = "#c29306", size = 5, hjust = 0.1) +  
  annotate("text", x = 0.1, y = avg_zapelnienie_2 + 0.13, 
           label = paste("Średnie zapelnienie\nna meczach w Słowenii:", round(avg_zapelnienie_2, 2)),
           color = "#4d68f7", size = 5, hjust = 0) +  
  labs(title = "Zapełnienie stadionu na meczach reprezentacji Słowenii",
       x = "Przeciwnik",
       y = "Procentowe zapełnienie",
       color = "Miejsce meczu") +
  scale_color_manual(values = c("Polska" = "#c29306", "Słowenia" = "#3f17b0")) + 
  coord_cartesian(ylim = c(0.42, max(df_slowenia_2$Zapelnienie, na.rm = TRUE) + 0.1)) + 
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 18),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ffedab", color = NA),
    panel.grid.major = element_line(color = alpha("#c29306",0.5)),
    panel.grid.minor = element_line(color = alpha("#c29306"),0.5)
  )


