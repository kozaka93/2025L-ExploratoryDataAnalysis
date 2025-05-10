#Wczytywanie oryginalnego wykresu
install.packages("jpeg", "ggplot2")
library(jpeg)
library(ggplot2)
img <- readJPEG("wykres.jpeg")
plot(1:2, type="n", axes=FALSE, xlab="", ylab="", frame.plot=FALSE)
rasterImage(img, 1, 1, 2, 2)

#Link do strony źródłowej: 
#https://www.fxmag.pl/energetyka/prognozy-cen-wegla-zakladaja-kolejne-spadki-ile-bedziemy-placic-pod-koniec-roku-za-tone-kwiecien-2025

#Wizualizacja posiada kilka wad:
#1) Osie 0X oraz 0Y nie są podpisane
#2) Na wykresie nie ma tytułu
#3) Skała na osi 0X jest mało czytelna - dni dla danych miesięcy nie są takie
# same, nie wszystkie dni w danym miesiącu są podpisane, sposób zapisu jest mało 
#czytleny i nietypowy 
#4) Siatka na wykresie jest zbyt rzadka co przy małych różnicach wartości jak w tym
#przypadku utrudnia odczytywanie wartości

#Wczytanie danych
ceny_wegla <- read.csv("coal_prices_daily.csv", sep=';')

#Formatowanie daty
ceny_wegla$Date <- as.Date(ceny_wegla$Date, format = "%d.%m.%Y")
  
#Utworzenie poprawionego wykresu
ggplot(ceny_wegla, aes(x = Date, y = Price.USD.T)) +
  geom_line(color = "blue", size = 1.5) +
  labs(title = "Ceny węgla Newcatlse na przełomie marca i kwietnia w 2025 roku", 
       x = "Data", 
       y = "Cena węgla (USD/tone)") +
  scale_x_date(date_breaks = "3 day", date_labels = "%d-%m") +
  scale_y_continuous(breaks = seq(93, 105, by = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Dlaczego wykres ten jest lepszy od oryginału?
# Wykres ten jest lepszy od oryginału, ponieważ ma dokładniejszą siatkę, dzięki 
#czemu łatwiej jest odczytywac z niego wartości. Ponadto jest na nim uwzględnione
#więcej dat dzięki czemu jest on dokładnieszy. Również jego jakość polepszyło dodanie
#opisów obu osi oraz tytułu wykresu, a także forma zapisu dat jest czytelniejsza 
# i mniej myląca. 


