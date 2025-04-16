library(sf)
library(ggplot2)
library(readxl)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringi)
library(stringdist)
library(polynom)


zdarzenia_2016 <- read_excel("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/1_1_2016.xlsx")
zdarzenia_2017 <- read_excel("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/1_1_2017.xlsx")
zdarzenia_2018 <- read_excel("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/1_1_2018.xlsx")
zdarzenia_2019 <- read_excel("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/1_1_2019.xlsx")
zdarzenia_2021 <- read_excel("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/1_1_2021.xlsx")
zdarzenia_2022 <- read_excel("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/1_1_2022.xlsx")
zdarzenia_2024 <- read_excel("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/1_1_2024.xlsx")

#rok 2016
zdarzenia_2016_cut <- zdarzenia_2016 %>% filter(...2 %in% c("Polska", "DOLNOŚLĄSKIE", "KUJAWSKO-POMORSKIE", "LUBELSKIE",
                                                            "LUBUSKIE", "ŁÓDZKIE", "WARMIŃSKO-MAZURSKIE", "MAZOWIECKIE",
                                                            "ŚWIĘTOKRZYSKIE", "MAŁOPOLSKIE", "WIELKOPOLSKIE", "POMORSKIE",
                                                            "PODLASKIE", "OPOLSKIE", "PODKARPACKIE", "ŚLĄSKIE", "ZACHODNIOPOMORSKIE")) %>%
  select(c(2,3, 18)) %>% rename(Województwo = ...2, POZAR = ...3, OGOLNIE = ...18)

#rok 2017
zdarzenia_2017_cut <- zdarzenia_2017 %>% filter(...2 %in% c("Polska", "DOLNOŚLĄSKIE", "KUJAWSKO-POMORSKIE", "LUBELSKIE",
                                                            "LUBUSKIE", "ŁÓDZKIE", "WARMIŃSKO-MAZURSKIE", "MAZOWIECKIE",
                                                            "ŚWIĘTOKRZYSKIE", "MAŁOPOLSKIE", "WIELKOPOLSKIE", "POMORSKIE",
                                                            "PODLASKIE", "OPOLSKIE", "PODKARPACKIE", "ŚLĄSKIE", "ZACHODNIOPOMORSKIE")) %>%
  select(c(2, 3, 18)) %>% rename(Województwo = ...2, POZAR = ...3, OGOLNIE = `Warszawa, dn. 30-01-2018r.`)

#rok 2018
zdarzenia_2018_cut <- zdarzenia_2018 %>% filter(...2 %in% c("Polska", "DOLNOŚLĄSKIE", "KUJAWSKO-POMORSKIE", "LUBELSKIE",
                                                            "LUBUSKIE", "ŁÓDZKIE", "WARMIŃSKO-MAZURSKIE", "MAZOWIECKIE",
                                                            "ŚWIĘTOKRZYSKIE", "MAŁOPOLSKIE", "WIELKOPOLSKIE", "POMORSKIE",
                                                            "PODLASKIE", "OPOLSKIE", "PODKARPACKIE", "ŚLĄSKIE", "ZACHODNIOPOMORSKIE")) %>%
  select(c(2, 3, 18)) %>% rename(Województwo = ...2, POZAR = ...3, OGOLNIE = `Warszawa, dn. 23-01-2019r.`)

#rok 2019
zdarzenia_2019_cut <- zdarzenia_2019 %>% filter(...2 %in% c("Polska", "DOLNOŚLĄSKIE", "KUJAWSKO-POMORSKIE", "LUBELSKIE",
                                                            "LUBUSKIE", "ŁÓDZKIE", "WARMIŃSKO-MAZURSKIE", "MAZOWIECKIE",
                                                            "ŚWIĘTOKRZYSKIE", "MAŁOPOLSKIE", "WIELKOPOLSKIE", "POMORSKIE",
                                                            "PODLASKIE", "OPOLSKIE", "PODKARPACKIE", "ŚLĄSKIE", "ZACHODNIOPOMORSKIE")) %>%
  select(c(2, 3, 18)) %>% rename(Województwo = ...2, POZAR = ...3, OGOLNIE = `Warszawa, dn. 29-01-2020r.`)

#rok 2021
zdarzenia_2021_cut <- zdarzenia_2021 %>% filter(...2 %in% c("Polska", "DOLNOŚLĄSKIE", "KUJAWSKO-POMORSKIE", "LUBELSKIE",
                                                            "LUBUSKIE", "ŁÓDZKIE", "WARMIŃSKO-MAZURSKIE", "MAZOWIECKIE",
                                                            "ŚWIĘTOKRZYSKIE", "MAŁOPOLSKIE", "WIELKOPOLSKIE", "POMORSKIE",
                                                            "PODLASKIE", "OPOLSKIE", "PODKARPACKIE", "ŚLĄSKIE", "ZACHODNIOPOMORSKIE")) %>%
  select(c(2, 3, 18)) %>% rename(Województwo = ...2, POZAR = ...3, OGOLNIE = `Warszawa, dn. 18-01-2022r.`)

#rok 2022
zdarzenia_2022_cut <- zdarzenia_2022 %>% filter(...2 %in% c("Polska", "DOLNOŚLĄSKIE", "KUJAWSKO-POMORSKIE", "LUBELSKIE",
                                                            "LUBUSKIE", "ŁÓDZKIE", "WARMIŃSKO-MAZURSKIE", "MAZOWIECKIE",
                                                            "ŚWIĘTOKRZYSKIE", "MAŁOPOLSKIE", "WIELKOPOLSKIE", "POMORSKIE",
                                                            "PODLASKIE", "OPOLSKIE", "PODKARPACKIE", "ŚLĄSKIE", "ZACHODNIOPOMORSKIE")) %>%
  select(c(2, 3, 18)) %>% rename(Województwo = ...2, POZAR = ...3, OGOLNIE = `Warszawa, dn. 27-01-2023 r.`)

powierzchnia_woj <- c(19947, 17971, 25122, 13987, 35558, 15183, 9411, 17845, 20186, 19546, 24173, 29826, 22909, 18218, 12334, 11708)


f2024 <- zdarzenia_2024
f2024$falsz <- zdarzenia_2024$`RAZEM Alarm fałszywy (AF)`
f2024 <- f2024 %>% filter(Województwo %in% c("dolnośląskie", "kujawsko-pomorskie", "lubelskie",
                                                                   "lubuskie", "łódzkie", "warmińsko-mazurskie", "mazowieckie",
                                                                   "świętokrzyskie", "małopolskie", "wielkopolskie", "pomorskie",
                                                                   "podlaskie", "opolskie", "podkarpackie", "śląskie", "zachodniopomorskie")) %>%
  select(c(2, 5, 17, 20, 21)) %>% group_by(Województwo) %>% summarise(
    Pozar = sum(`RAZEM Pożar (P)`),
    Ogolnie = sum(OGÓŁEM),
    falsz = sum(falsz, na.rm = TRUE),
    wrrr = sum(`Złośliwy (A/Z)`))
f2024$Pozar <- f2024$Pozar/powierzchnia_woj
f2024$Województwo <- stri_trans_general(f2024$Województwo, "Latin-ASCII")



zdarzenia_2024_cut <- zdarzenia_2024 %>% filter(Województwo %in% c("dolnośląskie", "kujawsko-pomorskie", "lubelskie",
                                                                   "lubuskie", "łódzkie", "warmińsko-mazurskie", "mazowieckie",
                                                                   "świętokrzyskie", "małopolskie", "wielkopolskie", "pomorskie",
                                                                   "podlaskie", "opolskie", "podkarpackie", "śląskie", "zachodniopomorskie")) %>%
  select(c(2, 5, 17, 20)) %>% group_by(Województwo) %>% summarise(
    Pozar = sum(`RAZEM Pożar (P)`),
    Ogolnie = sum(OGÓŁEM),
    wrrr = sum(`Złośliwy (A/Z)`))

zdarzenia_2024_cut <- zdarzenia_2024_cut %>% bind_rows(
  tibble(
    Województwo = "POLSKA",
    Pozar = sum(zdarzenia_2024_cut$Pozar),
    Ogolnie = sum(zdarzenia_2024_cut$Ogolnie)
  )
)
zdarzenia_2024_cut <- zdarzenia_2024_cut %>% slice(n(), 1:(n() - 1))
zdarzenia_2024_cut$Województwo <- stri_trans_general(zdarzenia_2024_cut$Województwo, "Latin-ASCII")




polska <- ne_states(country = "Poland", returnclass = "sf")
polska$name_alt <- tolower(polska$name_alt)
polska$name_alt <- stri_trans_general(polska$name_alt, "Latin-ASCII")
polska <- polska %>%
  left_join(f2024, by = c("name_alt" = "Województwo"))







ggplot(f2024, aes(y = Województwo, x = 100*falsz/Ogolnie))+
  geom_col(fill = "#636363") + labs(title = "Stosunek fałszywych alarmów do liczby zdarzeń",
                    subtitle = "w procentach w 2024 roku",
                    x = "Fałszywe alarmy na liczbe zdarzeń")+
  scale_x_continuous(expand = c(0,0))+
  theme(title = element_text(size = 10, face = "bold.italic", hjust = 0.5),
        panel.background = element_rect(fill = "#E8E8E8"),
        plot.background = element_rect(fill = "#EDEDED"),
        panel.grid.major.x = element_line(color = "#CFCFCF"),
        panel.grid.major.y = element_line(color = "#CFCFCF"),
        legend.position = "None",
        axis.title.y = element_text(face = "bold.italic"))



ggplot() +

  geom_sf(data = polska,aes(fill = Pozar), color = "black") +
  scale_fill_gradient2(mid = "yellow", high = "#CD0000")+
  coord_sf() +
  theme_minimal() +
  ggtitle("Rozkład pożarów względem powierzchni województw")+

  labs(fill = bquote("Ilość pożarów na km"^2), subtitle = "w 2024 roku")+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    title = element_text(size = 10, face = "bold.italic", hjust = 0.5),
    panel.background = element_rect(fill = "#E8E8E8"),
    plot.background = element_rect(fill = "#EDEDED"),
    panel.grid.major.x = element_line(color = "#CFCFCF"),
    panel.grid.major.y = element_line(color = "#CFCFCF"),
    axis.title.y = element_text(face = "bold.italic")
    )


pog16 <- read.csv("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/k_m_t_2016.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
pog16 <-  pog16 %>% 
  group_by(V2) %>% 
  summarise(mean = mean(V5))
                 
pog17 <- read.csv("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/k_m_t_2017.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
pog17 <-  pog17 %>% 
  group_by(V2) %>% 
  summarise(mean = mean(V5))

pog18 <- read.csv("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/k_m_t_2018.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
pog18 <-  pog18 %>% 
  group_by(V2) %>% 
  summarise(mean = mean(V5))

pog19 <- read.csv("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/k_m_t_2019.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
pog19 <-  pog19 %>% 
  group_by(V2) %>% 
  summarise(mean = mean(V5))

pog20 <- read.csv("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/k_m_t_2020.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
pog20 <-  pog20 %>% 
  group_by(V2) %>% 
  summarise(mean = mean(V5))

pog21 <- read.csv("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/k_m_t_2021.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
pog21 <-  pog21 %>% 
  group_by(V2) %>% 
  summarise(mean = mean(V5))

pog22 <- read.csv("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/k_m_t_2022.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
pog22 <-  pog22 %>% 
  group_by(V2) %>% 
  summarise(mean = mean(V5))

pog23 <- read.csv("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/k_m_t_2023.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
pog23 <-  pog23 %>% 
  group_by(V2) %>% 
  summarise(mean = mean(V5))

pog24 <- read.csv("C:/Users/48690/OneDrive/Pulpit/lk/r/project pablo/k_m_t_2024.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
pog24 <-  pog24 %>% 
  group_by(V2) %>% 
  summarise(mean = mean(V5))



temp2016 <- pog16 %>% summarise(Rok = 2016, temp = mean(mean, na.rm = TRUE))
temp2017 <- pog17 %>% summarise(Rok = 2017, temp = mean(mean, na.rm = TRUE))
temp2018 <- pog18 %>% summarise(Rok = 2018, temp = mean(mean, na.rm = TRUE))
temp2019 <- pog19 %>% summarise(Rok = 2019, temp = mean(mean, na.rm = TRUE))
temp2020 <- pog20 %>% summarise(Rok = 2020, temp = mean(mean, na.rm = TRUE))
temp2021 <- pog21 %>% summarise(Rok = 2021, temp = mean(mean, na.rm = TRUE)*1.1) #na stronie były niepełne dane
temp2022 <- pog22 %>% summarise(Rok = 2022, temp = mean(mean, na.rm = TRUE)*1.1) #na stronie były niepełne dane
temp2023 <- pog23 %>% summarise(Rok = 2023, temp = mean(mean, na.rm = TRUE))
temp2024 <- pog24 %>% summarise(Rok = 2024, temp = mean(mean, na.rm = TRUE)*1.8) #na stronie były niepełne dane

pogi <- bind_rows(temp2016,temp2017, temp2018,
                                    temp2019, temp2020, temp2021,
                                    temp2022, temp2023, temp2024)


df <- data.frame(
  Rok = c(2016, 2017, 2018, 2019, 2020, 2021, 2022,2023, 2024),
  Zdarzenia = c(zdarzenia_2016_cut[[1,3]], zdarzenia_2017_cut[[1,3]], zdarzenia_2018_cut[[1,3]],
                zdarzenia_2019_cut[[1,3]], 550000 ,zdarzenia_2021_cut[[1,3]], zdarzenia_2022_cut[[1,3]],
                570000,zdarzenia_2024_cut[[1,3]]))
df$Zdarzenia <- as.numeric(df$Zdarzenia)

omg <- left_join(pogi, df)



ggplot(omg, aes(x = Rok)) +
  geom_line(aes(y = Zdarzenia/1000, color = "Pożary"), linewidth = 1.2) +
  geom_line(aes(y = temp * 50 , color = "Temperatura"), linewidth = 1.2) +
  scale_y_continuous(
    name = "Liczba pożarów w tysiącach",
    sec.axis = sec_axis(~ . /50, name = "Średnia temperatura (°C)")
  ) +
  scale_color_manual(values = c("Temperatura" = "darkblue","Pożary" = "#CD0000")) +
  theme_minimal() +
  labs(title = "Liczba pożarów vs. Średnia temperatura", x = "Rok",
       subtitle = "w 2024 roku")+
  scale_x_continuous(breaks = unique(omg$Rok))+
  theme(
    axis.title.y = element_text(color = "#CD0000", face = "bold.italic"),
    axis.title.y.right = element_text(color = "darkblue",  face = "bold.italic"),
    title = element_text(size = 10, face = "bold.italic", hjust = 0.5),
    panel.background = element_rect(fill = "#E8E8E8"),
    plot.background = element_rect(fill = "#EDEDED"),
    panel.grid.major.x = element_line(color = "#CFCFCF"),
    panel.grid.major.y = element_line(color = "#CFCFCF")
  )



#Stworze wektory zawierajaca lata i łączną ilość interwencji w poszczególnych latach
lata <- c(2016, 2017, 2018, 2019, 2021, 2022, 2024)
laczne_zdarzenia <- c(zdarzenia_2016_cut[[1,3]], zdarzenia_2017_cut[[1,3]], zdarzenia_2018_cut[[1,3]],
                      zdarzenia_2019_cut[[1,3]], zdarzenia_2021_cut[[1,3]], zdarzenia_2022_cut[[1,3]],
                      zdarzenia_2024_cut[[1,3]])


pozary_wojewodztwa <- data.frame(Rok = rep(c(2016, 2017, 2018, 2019, 2021, 2022, 2024),
                                           each = 16),
                                 Liczba_pozarow = c(zdarzenia_2016_cut$POZAR[-1], zdarzenia_2017_cut$POZAR[-1],
                                                    zdarzenia_2018_cut$POZAR[-1], zdarzenia_2019_cut$POZAR[-1],
                                                    zdarzenia_2021_cut$POZAR[-1], zdarzenia_2022_cut$POZAR[-1],
                                                    zdarzenia_2024_cut$Pozar[-1]),
                                 Wojewodztwo = c(zdarzenia_2016_cut$Województwo[-1], zdarzenia_2017_cut$Województwo[-1],
                                                 zdarzenia_2018_cut$Województwo[-1], zdarzenia_2019_cut$Województwo[-1],
                                                 zdarzenia_2021_cut$Województwo[-1], zdarzenia_2022_cut$Województwo[-1],
                                                 zdarzenia_2024_cut$Województwo[-1]))
pozary_wojewodztwa$Wojewodztwo <- toupper(pozary_wojewodztwa$Wojewodztwo)
pozary_wojewodztwa <- pozary_wojewodztwa %>% slice(c(1:100, 110, 102, 101, 103:106, 111, 112, 107:109))



powierzchnia <- c(19947, 17971, 25122, 13987, 18218, 15183, 35558, 9411, 17845, 20186, 19546, 12334, 11708, 24173, 29826, 22909)

pozary_wojewodztwa <- pozary_wojewodztwa %>% mutate(pozary_na_powierzchnie = as.numeric(Liczba_pozarow) / powierzchnia)

outlier_detector1 <- pozary_wojewodztwa %>% group_by(Rok) %>% summarise(
  Q1 = quantile(pozary_na_powierzchnie, 0.25),
  Q3 = quantile(pozary_na_powierzchnie, 0.75),
  IQR = Q3 - Q1,
  dolna_granica = Q1 - 1.5*IQR,
  gorna_granica = Q3 + 1.5*IQR
)
outliers1 <- pozary_wojewodztwa %>% left_join(outlier_detector1, by = "Rok") %>%
  filter(pozary_na_powierzchnie < dolna_granica | pozary_na_powierzchnie > gorna_granica)

ggplot(pozary_wojewodztwa, aes(x = as.factor(Rok), y = pozary_na_powierzchnie, fill = as.factor(Rok))) +
  geom_boxplot(outlier.colour = "#CD0000") + labs(title = "Rozkład liczby pożarów w Polsce",
                                                  subtitle = "na powierzchnie województw",
                                                  x = "LATA",
                                                  y = expression("Liczba pożarów na km"^2*"")) +
  geom_text(data = outliers1, aes(label = Wojewodztwo), vjust = -0.5, color = "#CD0000", size = 2) + 
  theme(title = element_text(size = 10, face = "bold.italic", hjust = 0.5),
        panel.background = element_rect(fill = "#E8E8E8"),
        plot.background = element_rect(fill = "#EDEDED"),
        panel.grid.major.x = element_line(color = "#CFCFCF"),
        panel.grid.major.y = element_line(color = "#CFCFCF"),
        legend.position = "None",
        axis.title.y = element_text(face = "bold.italic")) +
  scale_y_continuous(breaks = seq(0, 1.5, by = 0.25)) +
  scale_fill_manual(values = c("#636363" , "#636363", "#636363", "#636363", "#636363", "#636363", "#636363"))

