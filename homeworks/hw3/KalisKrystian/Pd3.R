library(ggplot2)
library(dplyr)
library(maps)
library(tidyr)

world_map <- map_data("world") %>% 
  filter(long <= 180)

data <- read.csv("data.csv", 
                 header = TRUE, 
                 sep = ";")

df <- data %>% select(Location, Period, Dim1, Value) %>% 
  mutate(Value = substr(data$Value, 1, 4))

typeof(df[4,4])
df[,4] <- as.numeric(df[,4])
df <- df %>% group_by(Location) %>% summarise(Value = sum(Value)/8)

max(df$Value, na.rm = TRUE)

missing1 <- setdiff(df$Location, world_map$region)
missing2 <- setdiff(world_map$region, df$Location)
missing1
missing2
df[28,1] <- "Brunei"
df[199,1] <- "Palestine"

df2 <- world_map %>% 
  left_join(df, join_by(region == Location)) %>% 
  mutate(Procent = factor((case_when(is.na(Value) ~ "brak danych",
                          Value < 10 ~ "[0, 10) %",
                          Value >= 10 & Value < 20 ~ "[10, 20) %",
                          Value >= 20 & Value < 30 ~ "[20, 30) %",
                          Value >= 30 & Value < 40 ~ "[30, 40) %",
                          Value >= 40 ~ "40+ %")),
                          ordered = TRUE,
                          levels = c("brak danych", 
                                     "[0, 10) %", 
                                     "[10, 20) %", 
                                     "[20, 30) %", 
                                     "[30, 40) %",
                                     "40+ %")))

mapa <- ggplot() +
  geom_polygon(data = df2, aes(x = long, y = lat, group = group, fill = Procent),color = "black") +
  coord_map("mollweide") +
  expand_limits(x = c(-190,190)) +
  labs(title = "Procent osób otyłych wśród dorosłych") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("grey", "yellow", "gold", "orange", "red", "darkred"))+ 
  theme(legend.title = element_blank(), legend.position = "Right") +
  theme_minimal() +   theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank())

mapa
