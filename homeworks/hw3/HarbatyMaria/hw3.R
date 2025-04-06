install.packages("readxl")  
install.packages("rnaturalearth")
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)

df <- read_excel("Global Wellbeing Initiative Dataset.xlsx")

colnames(df)

# Będę robiłą mapkę gdzie będzie pokazane w jakim kraju jaki procent osób
# odpowiedziało w ankiecie czy powiedzieli, że są thriving w życiu.

df1 <- df %>% 
  filter(...3 == "15-24") %>% 
  select(...1, `Thriving Index`) %>% 
  mutate(`Thriving Index` = as.numeric(`Thriving Index`),
         `Thriving Index` = round(`Thriving Index`, 2),
         `Thriving Index` = `Thriving Index`* 100)
 
# Teraz zaczynam tworzyć mapę

world <- ne_countries(returnclass = "sf")

world_data <- world %>%
  left_join(df1, by = c("name" = "...1"))

ggplot(data = world_data) +
  geom_sf(aes(fill = `Thriving Index`), color = "white", size = 0.1) + # Fill by index
  scale_fill_viridis_c(option = "F", na.value = "gray90") +   # Gradient color scale
  theme_minimal() +
  labs(title = "In which country people of age from 15 to 24 years are the most thriving?",
       subtitle = "The data is based on a global survey in which people aged 15 to 24 were asked whether 
they feel they are thriving in life.",
       fill = "Percentage of respondents feeling they are thriving in life") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  coord_sf(datum = NA) 

df1[which.max(df1$`Thriving Index`),]

# Widzimy, że najaśniejszy kolor jest w Holandii a następnie w Finlandii

