###########################################
###     WSTĘP DO EKSPLORACJI DANYCH     ###
###           LABORATORIUM 6            ###
########################################### 

library(ggplot2)
library(dplyr)
library(SmarterPoland)


## Zadanie 1
# Zadania są zamieszczone w pliku .pdf w folderze lab6.
# Dane potrzebne do odtworzenia wizualizacji wczytujemy następująco:

df <- read.csv(file = "https://raw.githubusercontent.com/kozaka93/2025L-ExploratoryDataAnalysis/main/labs/lab06/house_data.csv", 
               encoding = "UTF-8")




## Zadanie 2
# Posługując się danymi z *Zadania 1* należy odwzorować poniższy wykres.






## patchwork

install.packages("patchwork")
install.packages("grid")
install.packages("gridExtra")


library(patchwork)



## Zadanie 3
# Przygotuj tabelę z podsumowaniem ile nieruchomości znajduje się dla poszczególnych kodów pocztowych i lat z wykresu.








## Zadanie 4
# Utwórz nową zmienną `is_renovated`, która będzie przyjmować wartość TRUE jeżeli była renowacja i FALSE gdy jej nie było. 
# Przygotuj wykres ukazujący rozkład piwerzchni mieszkanel dla domów w podziale na liczbę pięter i status renowacji.





## Zadanie 5 - stworzyć wykres gęstości brzegowych:
# a) wykres punktowy dwóch wskaźników + kolor
# b) dodać po lewej rozkład zmiennej death.rate
# c) dodać na dole rozkład zmiennej birth.rate






## ggrepel
# https://ggrepel.slowkow.com/articles/examples.html

install.packages("ggrepel")

library(ggrepel)




## Zadanie 6
# Narysuj wykres punktowy zależności między wskaźnikiem urodzeń a wskaźnikiem śmierci 
# oraz podpisz punkty o najniższym i najwyższym wskaźniku śmiertelności (nazwą kraju).




