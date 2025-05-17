library(dplyr)
library(ggplot2)

LOTR_data <- read.csv("C:/Users/Paawel/Desktop/homeworks_data/StefanczykPawel/lotr_tidy.csv")

LOTR_data_female <- LOTR_data %>% filter(Gender == "Female") %>%
        group_by(Race) %>% summarise(Words = sum(Words))
LOTR_data_male <- LOTR_data %>% filter(Gender == "Male") %>%
        group_by(Race) %>% summarise(Words = sum(Words))


plotfemale1 <- ggplot(LOTR_data_female, aes(x = "", y = Words, fill = Race)) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) + theme_void() + labs(title = "Liczba słów z filmów dla żeńskich postaci")
plotfemale1


plotmale1 <- ggplot(LOTR_data_male, aes(x = "", y = Words, fill = Race)) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) + theme_void() + labs(title = "Liczba słów z filmów dla męskich postaci ")
plotmale1

LOTR_data_grouped <- LOTR_data %>% group_by(Race, Gender) %>% summarise(Words = sum(Words), .groups = "drop")

plot3 <- ggplot(LOTR_data_grouped, aes(x = Race, y = Words)) + geom_col(position = "stack") + 
  facet_wrap(~Gender, scales = "free_y")
plot3

plot4 <- ggplot(LOTR_data_grouped, aes(x = Race, y = Words, fill = Race)) + geom_col(position = "stack") + 
  facet_wrap(~Gender, scales = "free_y")
plot4



