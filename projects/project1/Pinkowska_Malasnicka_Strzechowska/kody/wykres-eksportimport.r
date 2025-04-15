install.packages('ggplot2')
library(ggplot2)
library(RColorBrewer)
install.packages("egg")
library(ggrepel)
library(grid)
library(egg)

#Wczytujemy dane
eksport <- read.csv("C:/Users/julia/Downloads/Eksport.csv", sep=";", header=TRUE, stringsAsFactors=FALSE)


#Formatujemy dane
eksport$Kraj <- as.character(eksport$Kraj)  # Konwersja nazw krajów na tekst
eksport$Wartość <- as.numeric(gsub(" ", "", eksport$Wartość))  # Usunięcie spacji i konwersja na liczbę
eksport <- eksport[-1, ]
eksport<- eksport %>%
  select(c(Kraj, Wartość))

#EKSPORT

#Wybieramy 10 największych eksporterów
eksport <- eksport[order(-eksport$Wartość), ]  # Sortowanie malejąco
top10 <- eksport[1:10, ]                      # Wybór 10 największych producentów
inne <- sum(eksport$Wartość[11:nrow(eksport)])  # Suma pozostałych wartości


# Dodajemy nowy wiersz "Inne"
top10 <- rbind(top10, data.frame(Kraj="Inne", Wartość=inne))

# Dodajemy procenty
top10$procent <- round(top10$Wartość / sum(top10$Wartość) * 100, 1)


paleta <- colorRampPalette(c(brewer.pal(4, "Greens"), brewer.pal(5, "Blues")))(length(top10$Kraj))
# Tworzymy wykres kołowy
ggplot(top10, aes(x="", y=Wartość, fill=Kraj)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar(theta="y") +
  theme_void() + 
  labs(title="10 największych eksporterów jajek") +
  scale_fill_brewer(palette="") +  
  geom_text(aes(label = ifelse(procent > 2, paste0(procent, "%"), "")), 
            position = position_stack(vjust=0.5), size=3) + scale_fill_manual(values = paleta)


paleta <- c("#CC2200","#D0B26C", "#FF4422", "#FF7700", "#FFA500", 
            "#FFC832", "#FFFF00", "#C68642", "#8B7513", "#D2B48C")


ggplot(top10, aes(x = 1, y = Wartość, fill = Kraj)) +  
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = paleta, guide = FALSE) +  
  geom_text(aes(
    label = ifelse(procent > 2, paste0(Kraj, "\n", procent, "%"), ""),
    x = 1.4
  ), 
  position = position_stack(vjust = 0.5), 
  size = 3, color = "black",
  angle = 0, fontface = "bold") + 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_blank(), 
    axis.title = element_blank() 
  )

#IMPORT
import <- read.csv("C:/Users/julia/Documents/import.csv", sep=";", header=TRUE, stringsAsFactors=FALSE)


import$Kraj <- as.character(import$Kraj)  # Konwersja nazw krajów na tekst
import$Wartość <- as.numeric(gsub(" ", "", import$Wartość))  # Usunięcie spacji i konwersja na liczbę
import <- import[-1, ]
import<- import %>%
  select(c(Kraj, Wartość))

#Wybieramy 10 największych eksporterów
import <- import[order(-import$Wartość), ] # Sortowanie malejąco
top10_i <- import[1:10, ] # Wybór 10 największych producentów
inne_2 <- sum(import$Wartość[11:nrow(import)])# Suma pozostałych wartości

# Dodajemy nowy wiersz "Inne"
top10_i <- rbind(top10_i, data.frame(Kraj="Inne", Wartość=inne_2))

# Dodajemy procenty
top10_i$procent <- round(top10_i$Wartość / sum(top10_i$Wartość) * 100, 1)

paleta_1 <- colorRampPalette(c(brewer.pal(4, "Reds"), brewer.pal(5, "YlOrBr")))(length(top10$Kraj))
# Tworzymy wykres kołowy
ggplot(top10_i, aes(x="", y=Wartość, fill=Kraj)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar(theta="y") +
  theme_void() + 
  labs(title="10 największych importerów jajek") +
  geom_text(aes(label = ifelse(procent > 2, paste0(procent, "%"), "")), 
            position = position_stack(vjust=0.5), size=3) + scale_fill_manual(values = paleta_1)


paleta <- c("#FF0000", "#CC2200", "#FF4422", "#FF7700", "#FFA500", 
            "#FFC832", "#FFFF00", "#FFFACD", "#C68642", "#8B4513", "#D2B48C") 

paleta <- c("#CC2200","#D0B26C", "#FF4422", "#FF7700","#FFFACD", "#FFA500", 
            "#FFC832", "#FFFF00", "#C68642", "#8B7513", "#D2B48C")

ggplot(top10_i, aes(x = 1, y = Wartość, fill = Kraj)) +  
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = paleta, guide = FALSE) +
  geom_text(aes(
    label = ifelse(procent > 2, paste0(Kraj, "\n", procent, "%"), ""),
    x = 1.4
  ), 
  position = position_stack(vjust = 0.5), 
  size = 3, color = "black",
  angle = 0, fontface = "bold") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_blank(),
    axis.title = element_blank()
  )



