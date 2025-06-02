# PD 

# Będę przeprowadzała eksperyment ukazujacy eskperyment ukazujący
# problem z zaburzeniem percepcji danych ze wzlędu na używanie wykresów 3D

library(ggplot2)
library(dplyr)
library(plotly)
# Najpierw zrobię ramkę danych z danymi dotyczącymi ocen wśród uczniów 

df <- data.frame(
  Przedmiot = c("Matematyka", "Język polski", "Angielski", "Fizyka"),
  Plec = c("C","C","C","C","D","D","D","D"),
  Srednia_ocena = c(3.6, 3.8, 3.9, 4.0,4.2, 4.1, 4.5, 3.9)
) 


# Tutaj mamy chłopców na czerwono a dziewczynki na niebiesko

ggplot(df, aes(x = Przedmiot, y = Srednia_ocena, fill = Plec)) +
  geom_col(position = position_dodge())+
  scale_fill_manual(name = "Płeć",
                    values = c("C" = "red", "D" = "lightblue"),
                    labels = c("C" = "Chłopcy", "D" = "Dziewczęta")) + 
  theme_minimal()+
  labs(title = "Porównywanie średniej ocen uczniów z jednej klasy w podziale 
na płeć oraz przedmiot",
       y = "Średnia ocena")+
  coord_cartesian(ylim = c(0,5)) +
  theme(
    plot.title = element_text(size = 15)
  )

# Tutaj dziewczynki na czerwono

ggplot(df, aes(x = Przedmiot, y = Srednia_ocena, fill = Plec)) +
  geom_col(position = position_dodge())+
  scale_fill_manual(name = "Płeć",
                    values = c("C" = "lightblue", "D" = "red"),
                    labels = c("C" = "Chłopcy", "D" = "Dziewczęta")) + 
  theme_minimal()+
  labs(title = "Porównywanie średniej ocen uczniów z jednej klasy w podziale 
na płeć oraz przedmiot",
       y = "Średnia ocena")+
  coord_cartesian(ylim = c(0,5))+
  theme(
    plot.title = element_text(size = 15)
  )


# Chcę pokazac że czerwony nie jest dobrym kolorem na wykres ze względu na to że 
# zbyt przyciąga uwagę przez co różnice są nieprawdziwe zmniejszone lub zwiększone

# Tutaj robię wykres gdzie kolory są zbliżone intensywnością

ggplot(df, aes(x = Przedmiot, y = Srednia_ocena, fill = Plec)) +
  geom_col(position = position_dodge())+
  scale_fill_manual(name = "Płeć",
                    values = c("C" = "lightpink", "D" = "lightblue"),
                    labels = c("C" = "Chłopcy", "D" = "Dziewczęta")) + 
  theme_minimal()+
  labs(title = "Porównywanie średniej ocen uczniów z jednej klasy w podziale 
na płeć oraz przedmiot",
       y = "Średnia ocena")+
  coord_cartesian(ylim = c(0,5))

# Tutaj robię wykres z inną podziałką co może wprowadzić w błąd
ggplot(df, aes(x = Przedmiot, y = Srednia_ocena, fill = Plec)) +
  geom_col(position = position_dodge())+
  scale_fill_manual(name = "Płeć",
                    values = c("C" = "lightpink", "D" = "lightblue"),
                    labels = c("C" = "Chłopcy", "D" = "Dziewczęta")) + 
  theme_minimal()+
  labs(title = "Porównywanie średniej ocen uczniów z jednej klasy 
w podziale na płeć oraz przedmiot",
       y = "Średnia ocena") +
  coord_cartesian(ylim = c(3.5, 4.65))



  

  
