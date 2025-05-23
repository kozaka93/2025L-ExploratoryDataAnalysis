library(dplyr)
library(nycflights13)
library(ggplot2)

View(flights)
View(weather)
View(planes)
View(airports)
View(airlines)

#-----test 1------------------------------------------#
flights %>% left_join(airports, by = join_by(origin == faa)) %>% 
  group_by(name) %>% summarise(n = n()) %>% 
  mutate(procent = n / sum(n) * 100, label = paste0(round(procent, 1), "%")) %>%  
  ggplot(aes(x = "", y = n, fill = name)) +
  geom_col(width = 1, color = "white") + coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  theme_minimal() +
  labs(title = "Wykres liczby wylotów z lotnisk w Nowym Jorku", 
       fill = "Lotnisko") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
flights %>% group_by(origin) %>% summarise(n = n()) %>%
  left_join(airports, by = c("origin" = "faa")) %>%
  mutate(procent = n / sum(n) * 100, label = paste0(round(procent, 1), "%")) %>%
  ggplot(aes(x = reorder(name, -n), y = n)) + geom_col(fill = "steelblue") +
  geom_text(aes(label = label), vjust = -0.5, size = 4) +
  labs(title = "Wykres liczby wylotów z lotnisk w Nowym Jorku", 
       x = "Lotnisko", 
       y = "Liczba wylotów") +
  theme_minimal()

#-----test 2------------------------------------------#
delay_carrier <- flights %>% group_by(carrier) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE)) %>% 
  arrange(-mean) %>% head(5) %>%
  pull(carrier)
flights %>% left_join(weather) %>% 
  left_join(airlines) %>% 
  filter(carrier %in% delay_carrier) %>%
  mutate(visib_cubs = cut(visib, 
                          breaks = c(0, 2, 4, 6, 8, 10, 12),
                          include.lowest = TRUE,
                          right = FALSE)) %>% 
  group_by(name, visib_cubs) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE)) %>% 
  filter(!is.na(visib_cubs)) %>% 
  ggplot(aes(visib_cubs, name, fill = mean)) + geom_tile() +
  labs(title = "Wykres średniego opóźnienia w zależności od widoczności", 
       subtitle = "dla 5 najbardziej spóźniających się przewoźników", 
       y = "Widoczność", 
       x = "Średnie spóźnienie", 
       fill = "Przewoźnik") +
  theme_minimal()+
  theme(axis.text.y = element_text(angle = 30, hjust = 1))

  
flights %>% left_join(weather) %>% 
  left_join(airlines) %>%
  filter(carrier %in% delay_carrier) %>% 
  group_by(name, visib) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(visib, mean, color = name)) + geom_line() +
  labs(title = "Wykres średniego opóźnienia w zależności od widoczności", 
       subtitle = "dla 5 najbardziej spóźniających się przewoźników", 
       x = "Widoczność", 
       y = "Średnie spóźnienie", 
       color = "Przewoźnik") + 
  theme_minimal()
#-----test 3------------------------------------------#
flights_by_hour <- flights %>%
  mutate(hour = sched_dep_time %/% 100)

flights_by_hour %>%
  group_by(hour) %>%
  summarise(mean_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = mean_dep_delay)) +
  geom_col(fill = "slateblue4") +
  labs(
    title = "Średnie opóźnienie odlotów w zależności od godziny",
    x = "Godzina planowanego odlotu",
    y = "Średnie opóźnienie (minuty)"
  ) +
  theme_minimal()

flights_by_hour %>%
  filter(!is.na(dep_delay)) %>%
  ggplot(aes(x = factor(hour), y = dep_delay)) +
  geom_boxplot(fill = "slateblue4") +
  coord_cartesian(ylim = c(-20, 100)) +  # ograniczenie osi Y dla lepszej czytelności
  labs(
    title = "Rozkład opóźnień odlotów w zależności od godziny",
    x = "Godzina planowanego odlotu",
    y = "Opóźnienie (minuty)"
  ) +
  theme_minimal()

