library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

#link do danych i oryginalnego wykresu : https://dashboard.stat.gov.pl/ -> wynagrodzenia -> przeciętne
#miesieczne nominalne wynagrodzenie brutto w sektorze przedsiębiorstw(analogiczny okres roku poprzedniego = 100)

df <- read_excel("C:/Users/Paawel/Desktop/homeworks_data/StefanczykPawel/sredniewynagrodzenie.xlsx")
df1 <- df %>% pivot_longer(cols = -Rok,
                           names_to = "Miesiac",
                           values_to = "Wartosc")
df1$Wartosc <- as.numeric(gsub(",", ".", df1$Wartosc))
df1$Miesiac <- factor(df1$Miesiac, levels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"))

p1 <- ggplot(df1, aes(x = Miesiac, y = Wartosc, group = Rok, col = as.factor(Rok))) + geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(100, 120, by = 4), limits = c(100, 120)) +
  xlab(NULL) + ylab(NULL) + 
  scale_color_manual(values = c(
    "2019" = "#d0e6f5",
    "2020" = "#a6d4f2",
    "2021" = "#7dc1f0",
    "2022" = "#55aeee",
    "2023" = "#2b9cec",
    "2024" = "#008ae9",
    "2025" = "#0066b3"
  ))
p1 #wykres ze strony
#Gdy popatrzymy na oryginalny wykres to zobaczymy, że:
#(i) kolory linii są do siebie bardzo podobne przez co czasem można mieć problem ze śledzeniem odpowiedniego wykresu,
#(ii) obie osie nie mają oznaczeń przez co nie wiadomo co jest na osi Ox i Oy

#Zatem aby wykres był dobry wystarczy dodać nazwy na osiach oraz zmienić skalowanie osi Oy.

p2 <- ggplot(df1, aes(x = Miesiac, y = Wartosc, group = Rok, col = as.factor(Rok))) + geom_line(linewidth = 1) + 
  scale_y_continuous(breaks = seq(100, 120, by = 4), limits = c(100, 120)) + 
  labs(
    title = "przeciętne miesieczne nominalne wynagrodzenie brutto w sektorze przedsiębiorstw(analogiczny okres roku poprzedniego = 100)",
    color = "ROK"
  )
p2















