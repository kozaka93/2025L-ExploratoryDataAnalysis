library(plotrix)

#Dane
values <- c(27, 37, 6, 22, 8)
labels <- c(2019, 2020, 2021, 2022, 2023)
data <- data.frame(labels, values)


# Wykres kołowy
pk <- pie3D(values, labels = labels,
  main = "Wykres kołowy 3D",
  theta = 0.6,
  start = pi,
  shade = 0.5
)


#Wykres liniowy
pl <- ggplot(data, aes(x = labels, y = values)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(size = 3, color = "darkgreen")+
  scale_y_continuous(breaks = seq(0, 40, by = 5))+
  theme_minimal() +
  labs(title = "Wykres liniowy", x = "Nazwa", y = "Wartość")

