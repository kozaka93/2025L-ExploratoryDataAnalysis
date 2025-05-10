
# Wizualizacja źródłowa: https://obserwatorgospodarczy.pl/2025/04/18/zle-wiesci-w-handlu/
# Wady: 
# 1) Przedstawienie wielu mało różniących się wartości na wykresie kołowym
# 2) Porównanie danych dla dwóch kategorii poprzez postawienie dwóch wykresów kołowych obok 
#    siebie - nie widać gołym okiem różnic pomiędzy zmianami, przez co zmiany odczytujemy 
#    koniec końców przez czytanie wartości.


library(dplyr)
library(ggplot2)
library(tidyr)

data <- readxl::read_xlsx("obroty_towarowe_handlu_zagranicznego_ogolem_i_wedlug_krajow_w_styczniu__lutym_2025_r..xlsx",
                          range = "A28:I41")
data <- data %>%
  filter(!is.na(data[ ,'2025']))

data <- data %>%
  mutate("panstwo" = stringr::str_replace(data$Wyszczególnienie, "^\\d+\\.\\s*", ""))

data <- data %>%
  select(!c(1:7))

colnames(data) <- c("dane2024", "dane2025", "panstwo")

data_other <- data %>% 
  summarise(dane2024 = 100 - sum(as.numeric(dane2024)), dane2025 = 100 - sum(as.numeric(dane2025))) %>%
  mutate(panstwo = "Pozostałe kraje")

data <- data %>%
  rbind(data_other)

data <- data %>%
  mutate(dane2024 = as.numeric(dane2024),
         dane2025 = as.numeric(dane2025)) %>%
  pivot_longer(!panstwo, names_to = "rok", values_to = "wartosc") %>%
  mutate(panstwo = forcats::fct_reorder(panstwo, -wartosc))

data %>%
  ggplot(aes(y = panstwo, x = wartosc, fill = rok)) +
  geom_col(position = "dodge") +
  labs(y = "państwo",
       x = "udział procentowy",
       title = "Struktura eksportu Polski w styczniu i lutym",
       subtitle = "w roku 2024 i 2025",
       fill = "Rok") +
  scale_fill_manual(values = c("red", "blue"), labels = c("2024", "2025")) +
  xlim(0, 35) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  theme_minimal()


# Przygotowany wykres jest lepszy, ponieważ lepiej widać małe zmiany pomiędzy udziałem
# danych krajów w strukturze eksportu. Ponadto oczywistym jest, że cała struktura eksportu
# to 100%, więc zabieg wizualny w postaci wykresu kołowego jest niepotrzebny. 
# Na mojej wizualizacji można też całkiem dobrze odczytać dane bez ich podpisywania, 
# co usprawiedliwia stworzenie wizualizacji a nie po prostu tabelki z danymi.