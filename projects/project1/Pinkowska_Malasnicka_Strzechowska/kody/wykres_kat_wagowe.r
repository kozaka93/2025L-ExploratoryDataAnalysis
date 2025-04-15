library(ggplot2)
library(dplyr)
install.packages('openxlsx')
library(openxlsx)


df <- read.xlsx('/Users/oliwiastrzechowska/jaja4.xlsx')

names(df) <- c("towar", "cena", "klasa")

ggplot(df_summary, aes(x = towar, y = srednia_cena, fill = towar)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9, color = "black", size = 0.3) +  # Czarna ramka dla słupków
  facet_wrap(~klasa) +  
  theme_minimal() +  
  labs(
    title = "Średnia cena jaj według systemu chowu i klasy wagowej", 
    x = "System chowu", 
    y = "Średnia cena (PLN)",
    fill = "System chowu"
  ) +
  scale_fill_manual(
    values = c(
      "klatkowy" = "#F76D5E",   
      "ściółkowy" = "#FFE066",  
      "wolny wybieg" = "#FDCB6E", 
      "ekologiczny" = "#F9844A"  
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.position = "right",
    panel.spacing = unit(1, "cm"),  
  )
