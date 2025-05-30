library(dplyr)
library(tidyr)
library(stringr)
library(treemap)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(tidyverse)

df_raw <- read.csv("https://raw.githubusercontent.com/mini-pw/2021Z-DataVisualizationTechniques/master/labs/data/Pokemon.csv")[,-1]

df <- df_raw %>%
  filter(Type.1 %in% c("Fire", "Water", "Grass", "Poison", "Electric")) %>%
  mutate(Type = factor(Type.1, levels = c("Fire", "Water", "Grass", "Poison", "Electric")))

percentages <- df %>%
  group_by(Type) %>%
  summarise(Percentage = n() / nrow(df) * 100) %>%
  ungroup()

types <- unique(percentages$Type)
type_colors <- setNames(brewer.pal(length(types), "Set1"), types)

ggplot(percentages, aes(x = Type, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Pokemon Types", x = NULL, y = "Percentage", fill = "Type") +
  scale_y_continuous(breaks = seq(0, 100, by = 5), 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = type_colors) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16))

treemap(df,
        index = "Type",
        vSize = "Total",
        vColor = "Type",
        palette = type_colors,
        draw = TRUE,
        title = "Treemap of Pokemon Types",
        border.col = "white",
        border.lwd = 2,
        fontsize.labels = 12,
        fontsize.title = 16)

# results <- read_excel("ankieta.xlsx")
results <- data.frame(
  treemap_1 = c(18, 10, 12.5, 35, 18, 32.5, 16.66, 17, 11, 24, 10, 22, 28, 30, 18, 25),
  treemap_2 = c(18, 10, 10, 40, 20, 27, 12.5, 15, 14, 20, 14, 20, 27, 29, 19, 20),
  bar_1 = c(16, 17, 16, 17, 16, 17, 17, 16, 17, 17, 16, 15, 16, 16, 16, 16),
  bar_2 = c(12, 12.8, 12, 19, 12, 13, 14, 12.3, 12.5, 13, 11, 11, 13, 12, 12, 12),
  uwaga = c(2, 1, 1, 2, 2, 2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1),
  czytelnosc = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2))

results <- results %>% 
  mutate(treemap_1_diff = abs(treemap_1 - percentages$Percentage[filter(percentages, Type == "Poison")$Type]),
         treemap_2_diff = abs(treemap_2 - percentages$Percentage[filter(percentages, Type == "Fire")$Type]),
         bar_1_diff = abs(bar_1 - percentages$Percentage[filter(percentages, Type == "Grass")$Type]),
         bar_2_diff = abs(bar_2 - percentages$Percentage[filter(percentages, Type == "Electric")$Type]))

results_long <- results %>%
  select(treemap_1_diff, treemap_2_diff, bar_1_diff, bar_2_diff) %>%
  pivot_longer(
    cols = everything(),
    names_to = "source",
    values_to = "diff"
  ) %>%
  mutate(
    plot_type = ifelse(str_detect(source, "treemap"), "Treemap", "Wykres słupkowy")
  )

ggplot(results_long, aes(x = plot_type, y = diff)) +
  geom_boxplot(width = 0.5, fill = "#ff4d6d") +
  labs(
    title = "Porównanie dokładności odczytu danych",
    x = "Typ wykresu",
    y = "Błąd odczytu (wartość bezwzględna)"
  ) +
  theme_minimal()

results_long_c <- results %>%
  select(uwaga, czytelnosc) %>%
  pivot_longer(cols = everything(), names_to = "kryterium", values_to = "wybor") %>%
  mutate(
    wybor = factor(wybor, levels = c(1, 2), labels = c("Treemap", "Wykres słupkowy")),
    kryterium = recode(kryterium,
                       uwaga = "Przyciąganie uwagi",
                       czytelnosc = "Czytelność"))

results_percent <- results_long_c %>%
  count(kryterium, wybor) %>%
  group_by(kryterium) %>%
  mutate(procent = 100 * n / sum(n)) %>%
  ungroup()

results_final <- results_percent %>%
  complete(kryterium, wybor, fill = list(n = 0, procent = 0))

ggplot(results_final, aes(x = kryterium, y = procent, fill = wybor)) +
  geom_col(position = "dodge") +
  labs(
    title = "Procentowy wybór preferowanego wykresu według kryteriów",
    x = "Kryterium",
    y = "Procent odpowiedzi",
    fill = "Preferowany wykres") +
  scale_fill_manual(values = c("Treemap" = "#a4133c", "Wykres słupkowy" = "#ff8fa3")) +
  scale_y_continuous(breaks = seq(0, 100, 10), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal()
