
# Źródło: https://www.iata.org/en/publications/economics/chart-week/chart-of-the-week-25-april-2025/

# Wady wykresu:
# 1. Głównym problemem wykresu są dwie osie y. Jedna odnosi się do wykresów słupkowych przedstawiających liczbę pasażerów,
# a druga do wzrostu procentowego przedstawionego jako wykres punktowy. Nie jest wyraźnie zaznaczone
# które dane do czego się odnoszą, osie nie sa podpisane, co może utrudnić odbiorcy poprawne odczytanie wykresu.
# 2. Kropki przedstawiające wzrost procentowy liczby pasażerów są słabo widoczne i trudne do odczytania.
# 3. Brak siatki utrudnia odczytanie jakichkolwiek wartości, szczególnie z wykresu punktowego.
# 4. Pełne nazwy państw na osi x nie mieszczą się, szczególnie jeśli patrzymy na wykres na węższym ekranie np. na telefonie.
# 5. Brak tytułów osi

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

data <- read.csv('data.csv') 

#dane odcztyane z wykresu, procent wzrostu wyliczony na podstawie tych danych:

data <- data %>%
  mutate(growth_procent = ((Passengers_2024_mln - Passengers_2014_mln)/Passengers_2014_mln) * 100)

# Ustawienie kolejności krajów
ordering <- data %>%
  arrange(desc(Passengers_2024_mln)) %>%
  pull(Country)

df_long <- data %>%
  pivot_longer(cols = c(Passengers_2014_mln, Passengers_2024_mln), names_to = "Year", values_to = "Passengers_mln") %>%
  mutate(Year = recode(Year, "Passengers_2014_mln" = "2014", "Passengers_2024_mln" = "2024"),
          Country = factor(Country, levels = ordering))

# Wykresy
plot1 <- ggplot(df_long, aes(x = Country, y = Passengers_mln, fill = Year)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Numbers of passengers to ASEAN countries in 2014 and 2024",
       x = "Country", y = "Number of passengers (mln)") +
  scale_fill_manual(values = c("2014" = "lightblue1", "2024" = "lightcoral")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100)) +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

plot2 <- ggplot(data, aes(x = factor(Country, levels = ordering), y = growth_procent)) +
  geom_point(size=5, colour = "darkgreen") +
  geom_text(aes(label = round(growth_procent, 1)), vjust = -1, size = 5) +
  labs(title = "2024 growth from 2014",
       x = "Country", y = "Percent") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 150)) +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))


png("plot.png", width = 1600, height = 600)

plot1 + plot2

dev.off()

# Rozwiązania wcześniej opisanych problemów:
# 1. Utworzyłam dwa osobne wykresy: jeden obrazujący liczbę pasażerów w latach 2014 i 2024, a drugi procentowy wzrtost,
#   co znacznie ułatwia ich odczytanie i rozwiązuje problem dwóch osi y.
# 2. Wartości procentowe na wykresie punktowym są podpisane, dzięki czemu odczytanie wartości procentowych jest proste.
# 3. Na obu wykresach dodałam siatke, która ułatwia odczytanie wartości.
# 4. Nazwy krajów są napisane pod kątem 45 stopni, dzięki czemu nie nachodzą na siebie.
# 5. Podpisy osi zostały dodane
# Ponadto zmieniłam kolory czytelniejsze i bardziej przyjemne dla oka niż np. intensywny czerwony (kolor kropek na oryginalnym wykresie).
# 
# Dane dot. liczby pasażerów odczytałam z wykresu i na ich podstawie obliczyłam procent wzrostu.
# Do dokładnych danych nie było dostępu, a wartości na wykresie były zaokrąglone,
# dlatego niektóre wartości na moim wykresie mogą się nieco różnić od oryginalnego. 