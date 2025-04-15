install.packages("readxl", repos = "https://cloud.r-project.org/")
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("fmsb")
install.packages("lubridate")
install.packages("RColorBrewer")
install.packages("scales")
install.packages("tidyr")
library(dplyr)
library(readxl)
library(ggplot2)
library(fmsb)
library(lubridate)
library(RColorBrewer)
library(scales)
library(tidyr)


netflix <- read_excel("netflix.xlsx")
kaggle <- read.csv('kaggle.csv')

m_df <- netflix %>%
  left_join(kaggle, by = c("show_title" = "title"), relationship = "many-to-many") %>% 
  mutate(month = month(week, label = TRUE, abbr = FALSE))

miesiace <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec",
                 "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")


df_horror_01 <- m_df %>% 
  #filter(category == "Films") %>% 
  filter(grepl("Horror", genres, ignore.case = TRUE)) %>%
  group_by(month) %>%
  summarise(popularity = n())

radar_horror_01 <- df_horror_01 %>%
  arrange(month) %>%
  select(popularity) %>%
  t() %>%
  as.data.frame()

radar_horror_01 <- rbind(
  rep(max(df_horror_01$popularity, na.rm = TRUE), 12),
  rep(min(df_horror_01$popularity, na.rm = TRUE), 12),
  radar_horror_01)

colnames(radar_horror_01) <- miesiace

radar_horror_01 <- radar_horror_01[, rev(colnames(radar_horror_01))]

png("radar_horror_01.png", width = 800, height = 800, bg = "transparent")

radarchart(radar_horror_01, axistype = 1,
           pcol = "#c79f59EE", 
           pfcol = "#c79f5980", 
           plwd = 4,
           cglcol = "grey25", 
           cglty = 1, 
           axislabcol = "grey", 
           caxislabels = rep("", 5), 
           cglwd = 0.8,
           vlcex = 1.8)

title("Popularność horrorów w ciągu roku", cex.main = 2.8)

dev.off()


df_romance_01 <- m_df %>%
  filter(grepl("Romance", genres, ignore.case = TRUE)) %>%
  group_by(month) %>%
  summarise(popularity = sum(1/weekly_rank, na.rm = TRUE))

radar_romance_01 <- df_romance_01 %>%
  arrange(month) %>%
  select(popularity) %>%
  t() %>%
  as.data.frame()

radar_romance_01 <- rbind(
  rep(max(df_romance_01$popularity, na.rm = TRUE), 12),
  rep(min(df_romance_01$popularity, na.rm = TRUE), 12),
  radar_romance_01)

colnames(radar_romance_01) <- miesiace

radar_romance_01 <- radar_romance_01[, rev(colnames(radar_romance_01))]

png("radar_romance_01.png", width = 800, height = 800, bg = "transparent")

radarchart(radar_romance_01, axistype = 1,
           pcol = "#8a2b0dEE", 
           pfcol = "#8a2b0d80", 
           plwd = 4,
           cglcol = "gray25", 
           cglty = 1, 
           axislabcol = "gray", 
           caxislabels = rep("", 5), 
           cglwd = 0.8,
           vlcex = 1.8)

title("Popularność filmów romantycznych w ciągu roku", cex.main = 2.8)

dev.off()


df_comedy_01 <- m_df %>%
  filter(grepl("Comedy|Family", genres, ignore.case = TRUE)) %>%
  group_by(month) %>%
  summarise(popularity = n())

radar_comedy_01 <- df_comedy_01 %>%
  arrange(month) %>%
  select(popularity) %>%
  t() %>%
  as.data.frame()

radar_comedy_01 <- rbind(
  rep(max(df_comedy_01$popularity, na.rm = TRUE), 12),
  rep(min(df_comedy_01$popularity, na.rm = TRUE), 12),
  radar_comedy_01)

colnames(radar_comedy_01) <- miesiace

radar_comedy_01 <- radar_comedy_01[, rev(colnames(radar_comedy_01))]

png("radar_comedy_01.png", width = 800, height = 800, bg = "transparent")

radarchart(radar_comedy_01, axistype = 1,
           pcol = "#617983EE", 
           pfcol = "#61798380", 
           plwd = 4,
           cglcol = "gray25", 
           cglty = 1, 
           axislabcol = "gray", 
           caxislabels = rep("", 5), 
           cglwd = 0.8,
           vlcex = 1.8)

title("Popularność komedii i filmów familijnych w ciągu roku", cex.main = 2.8)

dev.off()
