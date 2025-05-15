install.packages("PogromcyDanych")
library(PogromcyDanych)

auta <- auta2012

auta <- auta %>% 
  group_by(Marka) %>% 
  summarise(ilosc = n())

auta <- auta %>% 
  arrange(-ilosc) %>% 
  head(10)

#pierwszy wykres
sum(auta$ilosc)

ggplot(auta, aes(x = "", y = ilosc, fill = Marka)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Udział poszczególnych marek samochodów wystawionych do sprzedaży",
  subtitle = "2012 rok (Łącznie 137180 samochodów)")


#drugi wykres

ggplot(auta, aes(x = Marka, y = ilosc)) +
  geom_col(fill = "blue4") +  # Dodajemy stat="identity" dla słupkowego wykresu
  theme_minimal()+
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 9, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 9))+
  geom_text(aes(label = ilosc), vjust = -0.3, size = 3) + 
  labs(
    title = "Udział poszczególnych marek samochodów wystawionych do sprzedaży",
    subtitle = "2012 rok (Łącznie 137180 samochodów)",
    x = "Marka", 
    y = "Ilość"   
  )



