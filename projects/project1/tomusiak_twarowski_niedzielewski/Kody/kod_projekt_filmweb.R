install.packages("RColorBrewer")
install.packages("showtext")
install.packages("sysfonts")

#Potrzebne biblioteki

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(RColorBrewer)
library(showtext)
library(sysfonts)
library(stringr)

#Dodanie czcionek

font_add_google("Lato", "lato")
showtext_auto()
setwd("C:/Users/Admin/Desktop/Polibuda/GenerowaneWykresy")


# Wczytanie danych i obróbka

df_0 <- read.csv("C:/Users/Admin/Downloads/filmweb_0.csv")
box_df_0 <- read.csv('C:/Users/Admin/Downloads/boxoffice.csv')
genres_df_0 <- read.csv("C:/Users/Admin/Downloads/genres.csv")

df <- df_0 %>% distinct()
df$Year <- as.numeric(df$Year)
df$Score_critics <- as.numeric(gsub(",", ".", df$Score_critics))
df$Score_users <- as.numeric(df$Score_users)
df$Number_of_ratings_users <- as.numeric(df$Number_of_ratings_users)
df$Number_of_ratings_critics <- as.numeric(df$Number_of_ratings_critics)
df$Boxoffice <- as.numeric(df$Boxoffice)
df$Duration.min. <- as.numeric(df$Duration.min.)
df<- df%>%
  mutate(budget_numeric = as.numeric(gsub("[$[:space:]]", "", Budget))) 

box_df <- box_df_0 %>% distinct()
box_df <- box_df %>% distinct(Original_Title, .keep_all = TRUE)

genres_df <- genres_df_0 %>% distinct()
genres_df <- genres_df %>% distinct(Title, .keep_all = TRUE)

df <- df %>% 
  mutate(Original_Title = na_if(Original_Title, "N/A")) %>% 
  mutate(Original_Title = coalesce(Original_Title, Title))

# Połączenie df z gatunkami

df_genres <- df %>% left_join(genres_df, by = "Title")
df_genres <- distinct(df_genres)

df_polish_box <- box_df %>% left_join(df, by = "Original_Title")
df_polish_box <- df_polish_box %>% distinct(Original_Title, .keep_all = TRUE)
df_polish_box <- distinct(df_polish_box)
df_polish_box<- df_polish_box%>%
  mutate(budget_numeric = as.numeric(gsub("[$[:space:]]", "", Budget)))
df_polish_box<- df_polish_box%>%
  mutate(Title = if_else(is.na(Title), Original_Title, Title))


df_complete <- df_polish_box %>% left_join(genres_df, by = "Title")
df_complete <- df_complete %>%
  mutate(across((where(is.character)), ~ na_if(., "N/A"))) 
df_complete <- df_complete %>% 
  filter(!is.na(Genres))

# df_complete - wszytkie dane połączone (df, gatunki i polski box office)
# df_genres - df + gatunki
# df_polish_box - df + polski box office

# Wykres 1

violin <- df_genres %>% 
  select(Genres, Year, Score_users, Score_critics) %>% 
  filter(if_all(everything(), ~ !is.na(.))) %>% 
  filter(Year >= 2000) %>% 
  mutate(Genres = str_replace(Genres, "^Dramat_.*", "Dramat")) %>% 
  mutate(Genres = str_replace(Genres, "^Komedia_.*", "Komedia")) %>% 
  filter(Genres %in% c("Komedia", "Dramat", "Horror", "Animacja", "Akcja", "Dokument"))

plot1 <- ggplot(violin, aes(x = Genres, y = Score_users)) + 
  geom_violin(trim = FALSE, fill = "#F4978E",linewidth=0.7,width=1.1) +
  geom_boxplot(width = 0.25, fill = "#FFDE74",linewidth=0.7) +
  labs(
       x = "Gatunek",
       y = "Ocena Użytkowników") +
  theme_minimal(base_family = "lato") + 
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))+
  theme(axis.text.y = element_text(size=60),
    axis.text.x = element_text(size=60,angle = 45, hjust = 0.9),
        axis.title.x = element_text(size=60,margin = margin(t = 0)),
        axis.title.y = element_text(size=60,margin = margin(r = 10)))+
  scale_y_continuous(breaks = seq(1, 9, by = 1),limits = c(1.3, 9.1))

plot2 <- ggplot(violin, aes(x = Genres, y = Score_critics)) + 
  geom_violin(trim = FALSE, fill = "#CE4257",linewidth=0.7,width=1.5) +
  geom_boxplot(width = 0.12, fill = "#FFDE74",linewidth=0.7) +
  labs(
    x = "Gatunek",
    y = "Ocena Krytyków") +
  theme_minimal(base_family = "lato") + 
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))+
  theme(axis.text.y = element_text(size=60),
        axis.text.x = element_text(size=60,angle = 45, hjust = 0.9),
        axis.title.x = element_text(size=60,margin = margin(t = 0)),
        axis.title.y = element_text(size=60,margin = margin(r = 10)))+
  scale_y_continuous(breaks = seq(1, 9, by = 1),limits = c(1.3, 9.1))

plot1 <- plot1 + coord_cartesian(ylim = c(1.3, 9.1))
plot2 <- plot2 + coord_cartesian(ylim = c(1.3, 9.1))


combined_plot_aligned <- (plot1 + plot2 + plot_layout(guides = "collect")) & 
  theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave("combined_violin_plots.png", combined_plot_aligned, width = 30, height = 15, units="cm",bg="transparent")

#Wykres 2 (stacked bar plot)

selected_genres <- df_complete %>% 
  select(Genres, Year_BOXOFFICE, Gross_Total) %>% 
  filter(if_all(everything(), ~ !is.na(.))) %>% 
  mutate(Genres = str_extract(Genres, "^[^/]+")) %>%
  mutate(Genres = str_replace(Genres, "^Dramat_.*", "Dramat")) %>% 
  mutate(Genres = str_replace(Genres, "^Komedia_.*", "Komedia")) %>% 
  mutate(Gross_Total = as.numeric(str_remove_all(Gross_Total, "[\\$,]"))) %>% 
  filter(Year_BOXOFFICE >= 2000) %>% 
  group_by(Genres) %>%
  summarise(Total_Gross = sum(Gross_Total, na.rm = TRUE)) %>%
  mutate(Percentage = Total_Gross / sum(Total_Gross) * 100) %>% 
  arrange(desc(Percentage)) %>% 
  select(Genres) %>% 
  pull() %>%
  .[1:7]

Polski_box <- df_complete %>% 
  select(Genres, Year_BOXOFFICE, Gross_Total) %>% 
  filter(if_all(everything(), ~ !is.na(.))) %>% 
  mutate(Genres = str_extract(Genres, "^[^/]+")) %>%
  mutate(Genres = str_replace(Genres, "^Dramat_.*", "Dramat")) %>% 
  mutate(Genres = str_replace(Genres, "^Komedia_.*", "Komedia")) %>% 
  mutate(Gross_Total = as.numeric(str_remove_all(Gross_Total, "[\\$,]"))) %>% 
  filter(Year_BOXOFFICE >= 2005 & Year_BOXOFFICE <= 2025) %>% 
  mutate(Interval = case_when(
    Year_BOXOFFICE < 2010 ~ "2005-\n-2010",
    Year_BOXOFFICE < 2015 ~ "2010-\n-2015",
    Year_BOXOFFICE < 2020 ~ "2015-\n-2020",
    Year_BOXOFFICE <= 2025 ~ "2020-\n-2025"
  )) %>%
  mutate(Genres = ifelse(Genres %in% selected_genres, Genres, "Inne")) %>% 
  group_by(Interval) %>% 
  mutate(total_box_office = sum(Gross_Total, na.rm = TRUE)/1e6) %>% 
  group_by(Interval, total_box_office, Genres) %>%
  summarise(genre_gross = sum(Gross_Total, na.rm = TRUE)/1e6, .groups = "drop") %>%
  mutate(Percentage = genre_gross / total_box_office * 100) 

Polski_box$Genres <- factor(Polski_box$Genres, levels = c(selected_genres,"Inne"))

wykres2 <- ggplot(Polski_box, aes(x = Interval, y = Percentage, fill = Genres)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Okres",
    y = "Procent polskiego boxoffice (%)",
    fill = "Gatunek"
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    legend.position = "top",   # Pozycja legendy nad wykresem
    legend.justification = "right",  # Ustawienie legendy w poziomie
    legend.direction = "vertical", # Ustawienie legendy w poziomie (horyzontalnie)
  ) +
  theme(panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white", color = "#DAD7C4", size = 1),  # Tło legendy + obramowanie
    axis.text.x = element_text(size=60,angle = 0, hjust = 0.5,lineheight = 0.3,margin = margin(t = -20)),
    axis.text.y = element_text(size=60),
        axis.title.x = element_text(size=80,margin = margin(t = 10)),
        axis.title.y = element_text(size=80),
        legend.title = element_text(face="bold",size = 80),                
        legend.text = element_text(size = 60))+
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("#FC9F5B", "#DC143C", "#EC9FD4","#BBBB43","#F4978E","#FFDE74", "#7DCFB6","gray80"))

ggsave("wykres2.png", wykres2, width = 15, height = 30, units = "cm", bg = "transparent")


#Wykres 3


df_clean_1 <- df %>% 
  select(Year, Score_users, Score_critics, budget_numeric, Number_of_ratings_users, Number_of_ratings_critics) %>% 
  filter(if_all(everything(), ~ !is.na(.))) %>%
  filter(Number_of_ratings_users >= quantile(Number_of_ratings_users, 0.25, na.rm = TRUE) & Number_of_ratings_critics >= quantile(Number_of_ratings_critics, 0.25, na.rm = TRUE)) %>% 
  filter(!is.na(Year)) %>% 
  filter(!is.na(Score_users)) %>% 
  filter(!is.na(budget_numeric)) %>% 
  filter(Year >= 1995 & Year <= 2025) %>%
  mutate(Year_Category = case_when(
    Year >= 1995 & Year <= 2005 ~ "1995 - 2005",
    Year >= 2005 & Year <= 2015 ~ "2005 - 2015",
    Year >= 2015 & Year <= 2025 ~ "2015 - 2025",
  ),
  budget_million = budget_numeric / 1e6) %>% 
  filter(!is.na(Year_Category))

wykres3_p1_heatmap <- ggplot(df_clean_1, aes(x = budget_million, y = Score_users)) +
  geom_bin2d(bins = c(20, 50)) +  # Adjust 'bins' as needed
  scale_fill_gradientn(colors = c("khaki1", "#C78ED8", "#9673A6", "#644C86", "#402080"))+
  geom_smooth(aes(group = 1), method = "lm", color = "purple", se = FALSE, linewidth = 2) +
  facet_wrap(~ Year_Category) +
  labs(
    x = "Budżet (Miliony $)",
    y = "Ocena użytkowników"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(axis.title = element_text(face = "bold"))

df_tile <- df_clean_1 %>%
  mutate(
    x_bin = cut(budget_million, breaks = 20),
    y_bin = cut(Score_users, breaks = 50)
  ) %>%
  count(x_bin, y_bin, Year_Category, name = "count") %>%
  mutate(
    x = as.numeric(x_bin),
    y = as.numeric(y_bin)
  )

wykres3_p2_heatmap <- ggplot(df_clean_1, aes(x = budget_million, y = Score_critics)) +
  geom_bin2d(bins = c(20, 50)) +
  scale_fill_gradientn(colors = c("khaki1", "#C78ED8", "#9673A6", "#644C86", "#402080")) +
  geom_smooth(aes(group = 1), method = "lm", color = "purple", se = FALSE, linewidth = 2) +
  facet_wrap(~ Year_Category) +
  labs(
    x = "Budżet (Miliony $)",
    y = "Ocena krytyków"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(axis.title = element_text(face = "bold"))

ggplot(df_tile_complete, aes(x = x, y = y, fill = count)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("gray95", "khaki1", "#C78ED8", "#9673A6", "#644C86", "c"),
    limits = c(0, max(df_tile$count, na.rm = TRUE)),
    na.value = "gray95"
  ) +
  facet_wrap(~ Year_Category) +
  labs(
    x = "Budżet (bin)",
    y = "Ocena użytkowników (bin)",
    fill = "Liczba filmów"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(axis.title = element_text(face = "bold"))


y_range <- range(c(layer_scales(wykres3_p1_heatmap)$y$range$range, layer_scales(wykres3_p2_heatmap)$y$range$range))

# Apply the same y-axis range to both plots
wykres3_p1_heatmap <- wykres3_p1_heatmap + coord_cartesian(ylim = y_range)
wykres3_p2_heatmap <- wykres3_p2_heatmap + coord_cartesian(ylim = y_range)

ggsave("wykres_3_p1a.png", wykres3_p1_heatmap, bg = "transparent")
ggsave("wykres_3_p2a.png", wykres3_p2_heatmap, bg = "transparent")

# Wykres 4

df_budget <- df_polish_box %>% 
  mutate(Main_country_production = str_extract(Country_production, "^[^ ]+")) %>% 
  mutate(Main_country_production = case_when(
    Main_country_production == "Wielka" ~ "Wielka Brytania",
    Main_country_production == "Nowa" ~ "Nowa Zelandia",
    TRUE ~ Main_country_production  # Keep as-is if no match
  )) %>% 
  select(Title,Year_BOXOFFICE, budget_numeric, Gross_Total, Main_country_production) %>% 
  mutate(Gross_Total = as.numeric(str_remove_all(Gross_Total, "[\\$,]"))) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>% 
  filter(Year_BOXOFFICE >= 1995 & Year_BOXOFFICE <= 2025) %>%
  filter(!is.na(Year_BOXOFFICE)) %>% 
  filter(!is.na(Gross_Total)) %>% 
  filter(!is.na(budget_numeric)) %>% 
  mutate(Year_Category = case_when(
    Year_BOXOFFICE <= 2005 ~ "1995 - 2005",
    Year_BOXOFFICE <= 2015 ~ "2005 - 2015",
    Year_BOXOFFICE <= 2025 ~ "2015 - 2025",
  ),
  gross_million = Gross_Total / 1e6,
  budget_million = budget_numeric / 1e6) %>% 
  filter(!is.na(Year_Category)) %>% 
  mutate(USA_or_not = case_when(
    Main_country_production == "USA" ~ "USA",
    TRUE ~ "Poza USA"
  ))

najwieksze_fiaska <- df_polish_box %>% 
  mutate(Gross_Total = as.numeric(str_remove_all(Gross_Total, "[\\$,]"))) %>% 
  filter(Number_of_theatres!="-") %>% filter(Year_BOXOFFICE<=2024,as.numeric(Number_of_theatres)>=200) %>% 
  select(Title, budget_numeric, Gross_Total, Number_of_theatres) %>% 
  mutate(revenue = Gross_Total/budget_numeric) %>% 
  arrange(revenue) %>% 
  slice_head(n = 10)

top_titles <- top_titles %>%
  mutate(Title_wrapped = str_wrap(Title, width = 15))

wykres4 <- ggplot(df_budget, aes(x = budget_million, y = gross_million)) +
  geom_point(aes(color = USA_or_not), na.rm = TRUE,size=4.5,alpha=0.5) +
  scale_color_manual(values = c("USA" = "#8D86C9", "Poza USA" = "red")) +
  geom_smooth(aes(group = 1), method = "lm", color = "#402080", se = FALSE,linewidth=1.7) +
  geom_label(
    data = top_titles,
    aes(label = Title_wrapped),
    fill = "#F4978E",     
    color = "black",   
    label.size = 0.5,
    nudge_y = -0.75,
    size = 10,   
    hjust = 0.38,
    vjust = 0.8,
    alpha = 0.6,
    label.padding = unit(0.1, "lines"), 
    label.r = unit(0, "lines"),  
    lineheight = 0.3
  ) + 
  facet_wrap(~ Year_Category) +
  labs(
    color = "Kraj produkcji",
    x = "Budżet (Miliony $)",
    y = "Polski boxoffice (Miliony $)",
  ) +
  theme_minimal(base_family = "lato") +
  theme(panel.border = element_rect(color = "#DAD7C4", size = 1,fill="transparent"),
    legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = "#DAD7C4", size = 1),
        axis.text.x = element_text(size=40,angle = 90,vjust=0.5,lineheight = 0.3,margin = margin(t = 0)),
        axis.text.y = element_text(size=40),
        axis.title.x = element_text(size=60,margin = margin(t = 10)),
        axis.title.y = element_text(size=60,margin = margin(r = 2.5)),
        legend.title = element_text(face="bold",size = 60),                
        legend.text = element_text(size = 60),
        strip.text = element_text(size = 60, face = "bold", color = "black"),
        strip.background = element_rect(fill = "#DAD7C4", color = "black", size = 1))+
  scale_y_continuous(breaks = seq(0, 30, by = 5),limits = c(0, 33))+
  scale_x_continuous(breaks = seq(0, 350, by = 50),limits=c(0,355))+
  coord_cartesian(xlim = c(15, 358), ylim = c(1, 32.5))

ggsave("wykres4.png", wykres4, width=15,height=22.5,units="cm",bg = "transparent")

#Wykres 5

df_processed <- df %>%
  select(Duration.min., Year, Score_users, Score_critics) %>%
  filter(Year >= 1995) %>% 
  mutate(Duration_Group = cut(Duration.min., 
                              breaks = c(0, 60, 90, 120, Inf), 
                              labels = c("[0,60]", "[60,90]", "[90,120]", "120+"), 
                              include.lowest = TRUE,
                              right = FALSE),
         Time_Period = case_when(
           Year <= 2005 ~ "1995-2005",
           Year <= 2015 ~ "2005-2015",
           TRUE ~ "2015-2025"
         )) %>%
  na.omit()

mean_scores <- df_processed %>%
  group_by(Time_Period, Duration_Group) %>%
  summarise(Mean_Score = mean(Score_users, na.rm = TRUE),
            .groups = "drop")

mean_scores_crit <- df_processed %>%
  group_by(Time_Period, Duration_Group) %>%
  summarise(Mean_Score = mean(Score_critics, na.rm = TRUE),
            .groups = "drop")

wykres5_p1 <- ggplot(mean_scores, aes(x = Duration_Group, y = Mean_Score, color = Time_Period, group = Time_Period)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = c("orange", "red", "purple")) +
  labs(title = "Średnia ocena użytkowników względem długości \nfilmu przez lata",
       x = "Długość (minuty)",
       y = "Średnia ocena filmu",
       color = "Time Period") +
  theme_minimal() +
  theme(legend.position = "bottom")

wykres5_p2 <- ggplot(mean_scores_crit, aes(x = Duration_Group, y = Mean_Score, color = Time_Period, group = Time_Period)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = c("orange", "red", "purple")) +
  labs(
       x = "Długość (minuty)",
       y = "Średnia ocena filmu",
       color = "Time Period") +
  theme_minimal(base_family = "Arial") +
  theme(axis.title = element_text(face = "bold"))+
  theme(legend.position = "bottom")

y_range <- range(c(layer_scales(wykres5_p1)$y$range$range, layer_scales(wykres5_p2)$y$range$range))

wykres5_p1 <- wykres5_p1 + coord_cartesian(ylim = y_range)
wykres5_p2 <- wykres5_p2 + coord_cartesian(ylim = y_range)

ggsave("wykres5_p1.png", wykres5_p1, bg = "transparent")
ggsave("wykres5_p2.png", wykres5_p2, bg = "transparent")

# Wykres 6 

df_6 <- df %>% 
  mutate(Main_country_production = str_extract(Country_production, "^[^ ]+")) %>% 
  select(Boxoffice, Year, Main_country_production) %>% 
  filter(!is.na(Year)) %>% 
  filter(!is.na(Boxoffice)) %>% 
  filter(!is.na(Main_country_production)) %>% 
  filter(Year >= 1995 & Year <= 2025) %>%
  filter(Main_country_production == "Polska") %>% 
  mutate(
    Boxoffice = Boxoffice / 1e6,
    Okres = cut(Year, 
                             breaks = c(1995, 2000, 2005, 2010, 2015, 2020, 2026),  # Define the interval boundaries
                             labels = c("1995-2000", "2000-2005", "2005-2010", "2010-2015", "2015-2020", "2020-2025"),  # Labels for each group
                             include.lowest = TRUE,  
                             right = FALSE)) %>% 
  group_by(Okres) %>% 
  summarise(`Średni Boxoffice` = mean(Boxoffice))
  
wykres6 <- ggplot(data = df_6, aes(x = factor(Okres), y = `Średni Boxoffice`)) +
  geom_col(fill = "#8D86C9")+
  labs(
    x = "Okres",
    y = "Średni światowy boxoffice\nPolskich produkcji (mln zł)") +
  theme_minimal(base_family = "lato") +
  theme(
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size=60,angle = 0,lineheight = 0.1,margin = margin(t = 10)),
        axis.text.y = element_text(size=60),
        axis.title.x = element_text(size=80,margin = margin(t = 10)),
        axis.title.y = element_text(size=80,lineheight = 0.3))+scale_y_continuous(breaks=seq(0,11,by=1))+
  coord_cartesian(ylim = c(0.5, 11))


ggsave("wykres6.png", wykres6, width = 28, height=13, units="cm", bg = "transparent")

summary(df$Boxoffice)

df7 <- df %>% 
  mutate(Main_country_production = str_extract(Country_production, "^[^ ]+")) %>% 
  select(Boxoffice, Year, Main_country_production, Title) %>% 
  filter(!is.na(Year)) %>% 
  filter(!is.na(Boxoffice)) %>% 
  filter(!is.na(Main_country_production)) %>% 
  filter(Year >= 1995 & Year <= 2025) %>%
  filter(Main_country_production == "Polska") %>% 
  mutate(
    Boxoffice = Boxoffice / 1e6,
    Year_Category = cut(Year, 
                        breaks = c(1995, 2000, 2005, 2010, 2015, 2020, 2026),  # Define the interval boundaries
                        labels = c("[1995-2000]", "[2000-2005]", "[2005-2010]", "[2010-2015]", "[2015-2020]", "[2020-2025]"),  # Labels for each group
                        include.lowest = TRUE,  
                        right = FALSE)) 


top3_boxoffice <- df7 %>% 
  filter(Main_country_production == "Polska") %>% 
  group_by(Year_Category) %>% 
  arrange(desc(Boxoffice)) %>% 
  select(Title, Boxoffice)


# Wykres 7

new_data <- df %>% 
  mutate(Main_country_production = str_extract(Country_production, "^[^ ]+")) %>% 
  mutate(Main_country_production = ifelse(Main_country_production == "Polska", "Polska", "Reszta świata")) %>% 
  select(Main_country_production, Score_users, Score_critics) 

nowy_wykres <- ggplot(new_data, aes(x = Score_users, fill = Main_country_production)) +
  geom_density(alpha = 0.5,linewidth=1,color="gray60") +  # Ustawienie przezroczystości
  labs(
    x = "Oceny użytkowników",
    y = "Gęstość",
    fill = "Kraj produkcji"
  ) +
  theme_minimal(base_family = "lato") +
  theme(    legend.position = "top",
            legend.justification = "right", 
            legend.direction = "vertical", 
        legend.background = element_rect(fill = "white", color = "#DAD7C4", size = 1),  # Tło legendy + obramowanie
        axis.text.x = element_text(size=40,lineheight = 0.3,margin = margin(t = 0)),
        axis.text.y = element_text(size=40),
        axis.title.x = element_text(size=60,margin = margin(t = 2.5)),
        axis.title.y = element_text(size=60,margin = margin(r = 2.5)),
        legend.title = element_text(face="bold",size = 60),                
        legend.text = element_text(size = 60))+
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.1))+
  scale_x_continuous(breaks = seq(1, 9, by = 1))+
  coord_cartesian(xlim = c(1.3, 9.1), ylim = c(0, 0.525))

ggsave("nowy_wykres.png", units="cm",width = 20, height=10, nowy_wykres, bg = "transparent")
  
  
  
  





