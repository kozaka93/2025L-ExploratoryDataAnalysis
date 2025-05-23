library(ggplot2)
library(dplyr)
library(plotly)

df <- read.csv("cats.csv")


# Dane do 1 eksperymentu
breed_gender <- df %>% 
  group_by(Breed,Gender) %>% 
  summarise(count = n())

breed_gender_2 <- breed_gender %>%
  mutate(BreedGender = paste(Breed, Gender, sep = "_"))


# Dane do 2 eksperymentu
colour <- df %>% 
  group_by(Breed, Fur_colour_dominant) %>% 
  summarise(count = n())


# Eksperyment 1:

# Wykres słupkowy
ggplot(breed_gender, aes(x = Breed, y = count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Liczba kotów według rasy i płci", 
       y = "Liczba kotów", 
       x = "Rasa")

# Wykres kołowy
ggplot(breed_gender_2, aes(x = "", y = count, fill = BreedGender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Udział kotów według rasy i płci", fill = "Rasa i płeć")

# Eksperyment 2: 
ggplot(colour, aes(x = Fur_colour_dominant, y = count, fill = Breed)) +
  geom_bar( stat = "identity",position = position_dodge2(preserve = "single", padding = 0.2)) +
  labs(
    title = "Liczba kotów według rasy i dominującego koloru futra",
    x = "Dominujący kolor futra",
    y = "Liczba kotów"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(colour, aes(x = Breed, y = Fur_colour_dominant, size = count)) +
  geom_point(alpha = 0.6, color = "purple") +
  labs(title = "Liczba kotów według rasy i dominującego koloru futra") +
  theme_minimal()
