#install.packages("dplyr")
#install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#install.packages("tidyverse") 
library(tidyverse)

df<-read.csv("C:/Users/marys/AppData/Local/Temp/a7a59a72-f09f-41de-856e-a331f5058627_projekt_2.zip.627/projekt_2/netflix_titles.csv")
#View(seriale)


df_2021 <- df %>% 
  filter(release_year == 2021)


df_categories <- df_2021 %>%
  separate_rows(listed_in, sep = ",\\s*")


category_counts <- df_categories %>%
  count(listed_in, sort = TRUE)

# wykres kołowy
ggplot(category_counts, aes(x = "", y = n, fill = listed_in)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Kategorie listed_in w roku 2021") +
  theme(legend.title = element_blank())

# wykres slupkowy

ggplot(category_counts, aes(x = reorder(listed_in, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Kategorie jakie zostały dodane na Netflix w roku 2021",
       x = "Kategoria", y = "Liczba wystąpień") +
  theme_minimal()