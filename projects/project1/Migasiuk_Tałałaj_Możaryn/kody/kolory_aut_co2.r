library("dplyr")
library("ggplot2")
library("ggridges")
library("forcats")
#####
# WYKRES 1 - KOLORY AUT(Na Plakacie)

color_mapping <- c(
  "black"   = "black",     
  "gray"    = "darkgrey",      
  "silver"  = "gray82",  
  "white"   = "snow",
  "blue"    = "blue",      
  "red"     = "red2",       
  "green"   = "green4",     
  "yellow"  = "yellow",
  "golden"  = "gold",      
  "beige"   = "wheat",     
  "brown"   = "saddlebrown",
  "violet"  = "purple"
)

etykiety_kolorow <- c(
  "black"   = "Czarny",     
  "gray"    = "Szary",      
  "silver"  = "Srebrny",  
  "white"   = "Biały",
  "blue"    = "Niebieski",      
  "red"     = "Czerwony",       
  "green"   = "Zielony",     
  "yellow"  = "Żółty",
  "golden"  = "Złoty",      
  "beige"   = "Beżowy",     
  "brown"   = "Brązowy",
  "violet"  = "Fioletowy"
)

df <- df %>% 
  filter(!is.na(Production_year), !is.na(Colour), Colour != "other")

df <- df %>%
  mutate(grupa = case_when(
    Production_year < 1990 ~ "przed 1990",
    between(Production_year, 1990, 2020) ~ as.character(Production_year),
    Production_year > 2020 ~ "po 2020"
  )) %>%
  mutate(grupa = factor(grupa,
                        levels = c("przed 1990",
                                   as.character(1990:2020),
                                   "po 2020"))) %>%
  mutate(Colour = factor(Colour,
                         levels = c("black", "silver", "white", "gray",
                                    "blue", "red", "green", "yellow",
                                    "golden", "beige", "brown", "violet")))

df_pie <- df %>%
  group_by(grupa, Colour) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(grupa) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()  

wykres_1 <- ggplot(df_pie, aes(x = grupa, y = percentage, fill = Colour)) +
  geom_bar(stat = "identity", width = 0.95, color = "white") +
  scale_fill_manual(values = color_mapping,
                    labels = etykiety_kolorow) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  scale_x_discrete(expand = c(0.01, 0)) +
  labs(
    title = "Rozkład kolorów samochodów według roku produkcji",
    x = "Rok produkcji",
    y = "Procent",
    fill = "Kolor"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    text = element_text(color = "black", face = "bold"),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "right"
  )

print(wykres_1)




#####
# WYKRES 2 - rozkład ceny i przebiegu dla roznych przedziałow czasowych

df <- read.csv("/Users/pawelmozaryn/Desktop/4 semestr/wstep do explo/projekt1/Car_sale_ads.csv")

library(patchwork)
df$Price <- ifelse(df$Currency == "EUR", df$Price / 4.6, df$Price)
df$Currency <- "PLN"


# Assuming 'df' is your initial data frame
dane <- df %>% 
  filter(Price < quantile(Price, 0.8, na.rm = TRUE), 
         Mileage_km < quantile(Mileage_km, 0.8, na.rm = TRUE), 
         Production_year >= 1990 & Production_year <= 2021,
         !is.na(Price), !is.na(Mileage_km), !is.na(Production_year))  # Remove NA rows

# Add age category column with split [2010, 2021]
dane <- dane %>% 
  mutate(wiek = case_when(
    Production_year < 2000 ~ "[1990, 2000)",
    Production_year < 2010 ~ "[2000, 2010)",
    Production_year < 2015 ~ "[2010, 2015)",
    Production_year <= 2021 ~ "[2015, 2021]",
    TRUE ~ "Other"  # Catch-all for unexpected cases (shouldn't occur with filter)
  ))

# Check for NA in wiek (should be 0)
print("Rows with NA in wiek:")
print(sum(is.na(dane$wiek)))

# Add index column
dane$Index <- 1:nrow(dane)

# Sample 700 rows
probka <- sample(dane$Index, 1000)

# Scatter plot
wykres_punkt <- dane %>% 
  filter(Index %in% probka) %>% 
  ggplot(aes(x = Price, y = Mileage_km, color = wiek)) + 
  geom_point() + 
  theme(legend.position = "none")

# Density plot for Price
gestosc1 <- dane %>% 
  filter(Index %in% probka) %>% 
  ggplot(aes(x = Price, color = wiek)) + 
  geom_density(alpha = 0.7, linewidth = 1.5) + 
  theme(axis.title.x = element_blank())

# Density plot for Mileage_km
gestosc2 <- dane %>% 
  filter(Index %in% probka, !is.na(Mileage_km)) %>%  # Ensure no NA in Mileage_km
  ggplot(aes(x = Mileage_km, color = wiek)) + 
  geom_density(alpha = 0.7, linewidth = 1.5) + 
  theme(axis.title.y = element_blank()) + 
  coord_flip()

# Combine plots
wykres_2 <- gestosc1 + guide_area() + wykres_punkt + gestosc2 + 
  plot_layout(heights = c(0.3, 0.7), widths = c(0.8, 0.2), guides = "collect") + 
  plot_annotation(
    title = "Relationship between price and mileage by production year intervals",
    subtitle = "consider a random sample of 1000 observations drawn from 200 000 observations"
  ) + scale_fill_discrete(name = "production year interval")
#####

# Wykres 3 - trendy wśród typów benzyny

df <- read.csv("Car_sale_ads.csv")

df <- df %>% filter(Fuel_type != "Hydrogen" & Fuel_type != "Ethanol")

# Data transformation with proper year group labels
df <- df %>%
  mutate(grupa = case_when(
    is.na(Production_year) ~ NA_character_,  # Handle NA values if any
    Production_year < 1990 ~ "przed 1990",
    Production_year >= 2020 ~ "po 2020",
    TRUE ~ paste0("[", floor(Production_year / 5) * 5, ", ", floor(Production_year / 5) * 5 + 5, ")")
  )) %>%
  mutate(grupa = factor(grupa, levels = c("przed 1990", "[1990, 1995)", "[1995, 2000)", "[2000, 2005)",
                                          "[2005, 2010)", "[2010, 2015)", "[2015, 2020)", "po 2020")))

# Summarize data and calculate percentages
df_wykres <- df %>%
  group_by(grupa, Fuel_type, .drop = FALSE) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(grupa) %>%
  mutate(percentage = count / sum(count) * 100, na.rm = TRUE)

# Create pie chart with faceting
wykres_3 <- ggplot(df_pie, aes(x = grupa, y = percentage, fill = Fuel_type)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = c("Diesel"="darkgrey",
                               "Gasoline" = "darkblue",
                               "Hybrid" = "lightgreen",
                               "Electric" = "lightblue",
                               "Gasoline + LPG" = "red",
                               "Gasoline + CNG" = "saddlebrown" 
  ))+
  theme_void() +
  labs(title = "Car Fuel Types by Production Year Group") + theme(axis.text.x = element_text(hjust = 0.5, angle = 45), axis.text.y.left = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("przed 1990", "1990-1994", "1995-1999", "2000-2004", 
                              "2005-2009", "2010-2014", "2015-2019", "2020+")) +
  theme(legend.position = "bottom") 
#####

# wykres 4 - emisje CO2 a lata produkcji (Na Plakacie)

df <- read.csv("Car_sale_ads.csv")

df <- df %>%
  mutate(grupa = case_when(
    is.na(Production_year) ~ NA_character_,
    Production_year < 1990 ~ "przed 1990",
    Production_year >= 2020 ~ "po 2020",
    TRUE ~ paste0("[", floor(Production_year / 5) * 5, ", ", floor(Production_year / 5) * 5 + 5, ")")
  )) %>%
  mutate(grupa = factor(grupa, levels = c("przed 1990", "[1990, 1995)", "[1995, 2000)", "[2000, 2005)",
                                          "[2005, 2010)", "[2010, 2015)", "[2015, 2020)", "po 2020")))

# Definicja nowych kolorów dla wybranych przedziałów produkcyjnych
nowe_kolory <- c(
  "[2000, 2005)" = "brown4",   # jasnoniebieski
  "[2005, 2010)" = "forestgreen",   # ciemnozielony
  "[2010, 2015)" = "blue2",        # pomarańczowy
  "[2015, 2020)" = "red2"            # czerwony
)

# Agregacja danych do wykresu:
# Obliczenie liczby obserwacji i procentowego udziału emisji CO2 w każdej grupie
df_gestosc <- df %>% 
  filter(!is.na(CO2_emissions), grupa %in% c("[2000, 2005)", "[2005, 2010)","[2010, 2015)", "[2015, 2020)")) %>%
  filter(CO2_emissions < quantile(CO2_emissions, 0.95), CO2_emissions >= 25)

# Tworzenie wykresu gęstości emisji CO2
wykres_4 <- ggplot(df_gestosc, aes(x = CO2_emissions, fill = grupa)) + 
  geom_density(linewidth = 1.5, adjust = 2, alpha = 0.7) +
  labs(
    title = "Emisja CO2 według grup roku produkcji",
    subtitle = "Przedziały 5-letnie",
    x = "Emisja CO2 [g/km]",
    y = "Gęstość",
    fill = "Grupa roku produkcji"
  ) +
  scale_fill_manual(values = nowe_kolory) +
  theme_minimal() +
  theme(text = element_text(face= "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

print(wykres_4)
ggsave("wykres.png", plot = wykres_4, bg = "transparent", width = 10, height = 6, dpi = 300)
