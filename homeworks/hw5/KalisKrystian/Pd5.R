library(dplyr)
library(ggplot2)

data <- read.csv("Data.csv", 
                 sep = ";",
                 header = TRUE,   
                 stringsAsFactors = FALSE,  
                 fileEncoding = "UTF-8") 

df <- data %>% filter(Country == "Poland", 
                      Variable == "Total passengers - Passenger-km  (millions)") %>%
              select(Year, Value) 

df$Value <- as.numeric(df$Value)

#wykres slupkowy 
ggplot(df, aes(x = as.factor(Year), y = Value)) +
  geom_col(width = 0.7, fill = "#c6dbef") +
  theme_minimal() +
  labs(title = "Liczba pasażerów transportu publicznego w milionach w Polsce", 
       subtitle = "Dane z lat 2010-2016",
       y = "Liczba pasażerów", 
       x = "Rok") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(size = 12)) +
  scale_y_continuous(expand = c(0,0))

#wykres kolowy
ggplot(df, aes(x = "", y = Value, fill = as.factor(Year))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Liczba pasażerów transportu publicznego w milionach w Polsce", 
       subtitle = "Dane z lat 2010-2016",
       fill = "Rok") +
  scale_fill_manual(values = c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#084594"))+
  theme( plot.title = element_text(size = 12))
