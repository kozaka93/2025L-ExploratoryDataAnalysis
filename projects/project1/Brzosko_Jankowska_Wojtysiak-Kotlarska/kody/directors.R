install.packages('dplyr')
install.packages('ggplot2')
install.packages('scales')
install.packages('packcircles')
library('dplyr')
library('ggplot2')

movies <- read.csv('Movies_2023_03_20.csv')
imdb <- read.csv('imdb.csv')

# Reżyserzy

# tu wersja z top 100

movies1 <- movies %>% 
  inner_join(imdb, by = c('Original_title' = 'Series_Title')) %>%
  select(Original_title, Number_of_votes, Director) %>%
  mutate(Number_of_votes = as.integer(Number_of_votes)) %>%
  arrange(desc(Number_of_votes)) %>%
  head(100)

directors1 <- movies1 %>%
  group_by(Director) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  filter(Count > 1)


# tu top 200

movies2 <- movies %>% 
  inner_join(imdb, by = c('Original_title' = 'Series_Title')) %>%
  select(Original_title, Number_of_votes, Director, Star1, Star2, Star3, Star4) %>%
  mutate(Number_of_votes = as.integer(Number_of_votes)) %>%
  arrange(desc(Number_of_votes)) %>%
  head(200)

directors2 <- movies2 %>%
  group_by(Director) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  filter(Count > 2) %>%
  mutate(Director = factor(Director))

# dyrektorzy według największej ilości głosów

directors <- movies %>%
  inner_join(imdb, by = c('Original_title' = 'Series_Title')) %>%
  select(Original_title, Number_of_votes, Director) %>%
  mutate(Number_of_votes = as.integer(Number_of_votes)) %>%
  group_by(Director) %>%
  summarize(popularity=sum(Number_of_votes)) %>%
  arrange(desc(popularity)) %>%
  head(10)


# wykres basic kolumnowy

library(scales)

plot <- ggplot(directors, aes(x = reorder(Director, -popularity), y = popularity)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 19),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  labs(x='Reżyserzy', y='Popularność') +
  geom_col(fill = "#afbbc0") +
  scale_y_continuous(labels = label_number())


png("directors_transparent.png", width = 600, height = 600, bg = "transparent")
print(plot)
dev.off()
