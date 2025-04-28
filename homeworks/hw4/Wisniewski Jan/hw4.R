#zrodlo: https://stat.gov.pl/obszary-tematyczne/rynek-pracy/pracujacy-zatrudnieni-wynagrodzenia-koszty-pracy/indeks-kosztow-zatrudnienia-w-kwartalach-2024-r-,26,3.html


#Na osi Y tego wykresu nie stoją żadne „procenty” ani „godziny”, tylko wartości Indeksu kosztów zatrudnienia
# – wskaźnika pokazującego, jak zmieniają się całkowite  koszty pracy ponoszone 
#przez pracodawcę na jedną przepracowaną godzinę w ujęciu kwartalnym (rok bazowy Q1 2016 = 100).
#Każda liczba na osi Y to właśnie ta wartość indeksu (np. „112” oznacza, 
#że koszty pracy na godzinę są o 12 % wyższe niż na początku 2016 roku).
#Żeby jak najlepiej pokazać wachania tego wskaźnika powinniśmy startować
#od poziomu 100, co jest niezbyt poprawne w bazowym wykresie
#Następnie problemem wydaje się brak legendy na osi Y, nie widać jakie są
#jakie tam są wartości. Oprócz tego, brakuje trochę punktów w miejsach, w których
#zarejestrowane są dane. Kolejnym problemem jest zbyt mała czcionka na osi X

library(dplyr)
library(ggplot2)


years    <- rep(2016:2024, each = 4)
quarters <- rep(1:4, times = length(2016:2024))
months   <- (quarters - 1) * 3 + 1
dates    <- as.Date(paste0(years, "-", months, "-01"))

df <- data.frame(
  data    = dates,
  year    = years,
  quarter = quarters
) %>%
  mutate(
    index = 100 + cumsum(rnorm(n(), mean = 1.35, sd = 1.0)),
    is_Q1  = quarter == 1
  )

ggplot(df, aes(x = data, y = index)) +
  geom_line(size = 0.8) +
  geom_point(aes(size = is_Q1), shape = 21, fill = "white") +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none") +
  
  scale_y_continuous(
    name   = "Indeks kosztów zatrudnienia\n(2016.01.01 = 100)",
    breaks = seq(100, 160, by = 20)
  ) +
  coord_cartesian(ylim = c(100, 160)) +
  
  scale_x_date(
    breaks = df %>% filter(is_Q1) %>% pull(data),
    labels = function(x) format(x, "%Y.%m.%d"),
    expand = expansion(add = c(90, 90))
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggtitle("Indeks kosztów zatrudnienia w Polsce, 2016–2024")

#Wykres jest lepszy od orginalnego, ponieważ widać na nim, że dane porównujemy
#w każdym kwartale w latach 2016-2024. Oprócz tego zaczynamy z poziomu 100, bo
# porównujemy wartości do pierwszego kwartału roku 2016. Poza tym na poprawionym
#wykresie są podpisy obu osi
