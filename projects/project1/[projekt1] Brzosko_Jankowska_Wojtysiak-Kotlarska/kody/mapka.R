install.packages("dplyr")
install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")
install.packages("sf")

library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(sf)

#import kina z pliku kina.csv(read.csv mi nie działało tylko wybierałam 
#ten plik z tego folderu i import i tam trzeba było wybrać semicolon jako ten odzielający)
kina <- read.csv("kina.csv", sep = ";", encoding = "UTF-8")

kina1 <- kina %>% 
  mutate(audience = audience.in.indoor.cinemas.per.1000.population.2023..person.,
         region = Name) %>%
  select(region, audience)

kina1$region <- tolower(kina1$region)
kina1 <- kina1 %>%
  mutate(audience_cat = cut(audience,
                            breaks = c(800, 950, 1100, 1250, 1450, 1750),
                            labels = c("800–950", "951–1100", "1101–1250", "1251–1450", "1451–1750")))


granice <- st_read("A01_Granice_wojewodztw.shp", quiet = TRUE)

# ggplot(data = granice) + geom_sf()

mapka <- granice %>% 
  left_join(kina1, by = join_by(JPT_NAZWA_ == region)) %>% 
  ggplot() + 
  geom_sf(aes(fill = audience_cat), color="white") + 
  theme_void() +
  #scale_fill_gradient(low = "#e2d1b2", high = "#8a2b0d") + 
  scale_fill_manual(values = c(
    "800–950" = "#e8cfc6",
    "951–1100" = "#dca28c",
    "1101–1250" = "#cd6e57",
    "1251–1450" = "#b4423d",
    "1451–1750" = "#6d2710"
  )) +
  labs(title = "",
       fill = "Liczba widzów w kinach\nna 1000 mieszkańców") +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))


png("mapka.png", width = 700, height = 700, bg = "transparent")
print(mapka)
dev.off()
