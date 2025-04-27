library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)
dane <- read.csv("wykres_do_poprawy.csv")

# Dane zostały zaczerpnięte ze strony https://dashboard.stat.gov.pl/

# Na oryginalnym wykresie skala osi Y jest rozłożona od 0 do 8000, mimo że wszystkie
# wartości są w przedziale (6100,6600), co powoduje, że różnice między nimi nie są
# widoczne, wszystkie linie nakładają się na siebie. Drugim problemem jest to, że
# skala kolorów zawiera kolory zbyt podobne do siebie, co utrudnia ich rozpoznanie,
# oraz nie sugeruje ona kolejności danych lat. Nie posiada on również podpisanych osi

dane <- dane %>%
  pivot_longer(cols = -id_rok, names_to  = "Miesiąc", values_to = "Zatrudnienie") %>%
  rename(rok = id_rok) %>%
  mutate(Miesiąc = factor(Miesiąc,levels = c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII"),ordered = TRUE),
    Zatrudnienie = as.numeric(str_replace(Zatrudnienie, ",", ".")))

ggplot(dane,aes(x=Miesiąc,y=Zatrudnienie,group=rok,color=as.factor(rok)))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs(title="Przeciętne zatrudnienie w sektorze przedsiębiorstw (tys. osób)",color="Rok")+
  theme_minimal(base_size = 14)+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  ylim(6100,6600)+
  scale_color_viridis_d(option = "plasma")

# Wygenerowany wykres jest lepszy, ponieważ zawiera podpisane osie, co przekłada się
# na szybsze zrozumienie tego co jest przedstawione i w jaki sposób. Została zmieniona
# skala osi Y co powoduje, że widoczne są różnice w trendach. Skala kolorów, została
# tak dobrana, żeby kolory nie były sobie podobne i nie myliły się między latami, ale
# również żeby sugerowały kolejność chronologiczną danych lini (starsze lata są
# przedstawione za pomocą ciemniejszych kolorów)