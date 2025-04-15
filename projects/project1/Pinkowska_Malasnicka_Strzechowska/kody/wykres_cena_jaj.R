library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(lubridate)

jaja <- read_excel("JAJA-projekt.xlsx")
jaja <- as.data.frame(jaja)

pkb <- read_excel("pkb2-projekt.xlsx")
pkb <- as.data.frame(pkb)

pkb_long <- pkb %>% 
  pivot_longer(!rok, names_to = "kwartal", values_to = "pkb") %>%
  arrange(rok)

pkb_long$kwartal <- as.numeric(pkb_long$kwartal)

colnames(jaja)[1] <- "rok"

jaja_long <- jaja %>% 
  pivot_longer(!rok, names_to = "miesiac", values_to = "srednia_cena_jaj") %>% 
  arrange(rok)

miesiace_map <- c("styczeń"=1, "luty"=2, "marzec"=3, "kwiecień"=4, "maj"=5, "czerwiec"=6,
                  "lipiec"=7, "sierpień"=8, "wrzesień"=9, "październik"=10, "listopad"=11, "grudzień"=12)
jaja_long$miesiac <- as.numeric(miesiace_map[jaja_long$miesiac])
jaja_long$data <- as.Date(paste(jaja_long$rok, jaja_long$miesiac, "01", sep = "-"))
jaja_long$kwartal <- as.numeric(case_when(jaja_long$miesiac < 4 ~ 1,
                                          jaja_long$miesiac < 7 ~ 2,
                                          jaja_long$miesiac < 10 ~ 3,
                                          TRUE ~ 4))

jaja_bez_2024 <- jaja_long %>% 
  filter(rok < 2024)

jaja_pkb <- jaja_bez_2024 %>%
  left_join(pkb_long, by = c("rok", "kwartal")) %>%
  mutate(cena_do_pkb_procent = srednia_cena_jaj / pkb * 100)


ggplot(jaja_pkb, aes(x = data, y = cena_do_pkb_procent)) +
  geom_line(size = 1.5) +
  labs(title = "Stosunek ceny skupu jaj do PKB kwartalnego w Polsce w latach 2004 - 2023",
       x = "Data",
       y = "Cena jaj / PKB * 100%") +
  theme_minimal() +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"))
        