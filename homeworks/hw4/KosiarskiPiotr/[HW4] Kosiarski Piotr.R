# Wykres pochodzi z Fox News Radio w 2011 roku
# Skala wykresu zaczyna się od 8% przez co zmiany sprawiają wrażenie dużo większych, niż realnie są.
# Dodatkowo wartość w listopadzie mimo, że jest inna niż w październiku, na wykresie ma tę samą wartość


data <- data.frame(
  Month = factor(c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov"),
                 levels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")),
  ile = c(9, 8.9, 8.8, 9, 9.1, 9.2, 9.1, 9.1, 9.1, 9, 8.6)
)

ggplot(data, aes(x=Month, y=ile, group=1))+geom_point(color = "red")+ geom_line(color = "red")+
  geom_area(fill = "red", alpha = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", ile)), vjust = -1, color = "yellow")+
  coord_cartesian(ylim = c(0, 10))+
  scale_y_continuous(breaks = seq(0, 10, by = 2),
                     labels = scales::percent_format(scale = 1) )+
  theme_dark()+
  labs(title="UNEMPLOYMENT RATE", subtitle = "UNDER PRESIDENT OBAMA", y = NULL, x = NULL)+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "yellow"),
        plot.subtitle = element_text(face = "bold", size = 12, hjust = 0.5, color = "white"),
        axis.text.y = element_text(color = "white", size = 10, face = "bold"),
        axis.text.x = element_text(color = "white", size = 10, face = "bold"),
        panel.border = element_rect(fill = NA),
        plot.background = element_rect(fill = "grey30"),
        panel.background = element_rect(fill = "grey30"))
  
# Chciałem zachować motyw kolorystyczny wykresu jakby co

# Mój wykres jest lepszy, ponieważ skala jest zachowana i zmiany danych są przedstawiony w sposób proporcjonalny.
# Wartość w listopadzie jest prawidłowo odzwierciedlona na wykresie 


  
