library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(readxl)
library(patchwork)

# Dane pobrałem ze strony "https://wonder.cdc.gov/". Przedstawiają one dane na temat ilości zgonów
# spowodowanych prez zatrucia lub przedawkowania związanymi z syntetycznymi opioidami (głównie fentanyl)
# z podziałem na stany.


#pobranie i obróbka danych
info <- read_excel("C:/Users/48690/OneDrive/Pulpit/KosiarskiPiotr/info.xlsx")
info23 <- info %>% 
  filter(`Year Code` == 2023) %>% 
  mutate(dera = Deaths*1000/Population, region = tolower(`Residence State`)) %>% 
  select(c(region, dera))


info19 <- info %>% 
  filter(`Year Code` == 2019) %>% 
  mutate(dera = Deaths*1000/Population, region = tolower(`Residence State`)) %>% 
  select(c(region, dera))

#pobranie mapy usa z podzialem na stany i obróbka
states23 <- map_data("state")
states23 <- states23 %>% 
  left_join(info23)

states19 <- map_data("state")
states19 <- states19 %>% 
  left_join(info19)

#rysowanie mapy
p1 <- ggplot(data = states23) +
  geom_polygon(aes(x = long, y = lat, fill = dera, group = group), color = "white") +
  coord_map("albers", 25, 50) +
  theme(legend.position = "right")+
  labs(subtitle = "w 2023 roku", fill = "Współczynnik \n śmiertelności")+
  scale_fill_gradient(low =  "green" , high = "black", limits = c(0,0.7))


p2 <- ggplot(data = states19) +
  geom_polygon(aes(x = long, y = lat, fill = dera, group = group), color = "white") +
  coord_map("albers", 25, 50) +
  theme(legend.position = "right")+
  labs(subtitle = "w 2019 roku", fill = "Współczynnik \n śmiertelności")+
  scale_fill_gradient(low =  "green" , high = "black",limits = c(0,0.7))


p2 + p1 + plot_layout(guides = "collect")+
  plot_annotation(title ="Liczba śmierci z przedawkowania fentanylu na tysiąc mieszkańców",
                  theme = theme(plot.title = element_text(hjust = 0.3)))

#OPIS
# Widać znaczący wzrost śmierci z powodu zatrucia lub przedawkowania opioidami syntetycznymi.
# W ciągu 4 lat współczynnik śmiertelności wzrósł w dużej częsci stanów niemal dwukrotnie.

# Dodatkowe wyjaśnienia
# W tytule użyłem terminu „fentanyl” zamiast „opioidy syntetyczne”, ponieważ chciałem, aby mapa była bardziej zrozumiała.
# Termin „opioidy syntetyczne” może być mało zrozumiały dla większości osób,
# podczas gdy fentanyl jest główną przyczyną zgonów w tej kategorii i jest bardziej rozpoznawalny.
# Jednakże mapa przedstawia zgony z powodu zatrucia lub przedawkowania wszystkimi opioidami syntetycznymi

# Pobrane dane dotyczą 50 stanów + district of columbia, który jest stanem federalnym
# i z pewnych względów jest osobnym przypadkiem, jednakże nie jest on stanem i nie
# zmienia on za wiele, dlatego został on pominięty na mapie
# alaske też pominąłem, żeby mapa była czytelniejsza


