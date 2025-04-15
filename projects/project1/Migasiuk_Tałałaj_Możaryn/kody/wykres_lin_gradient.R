library("dplyr")
library("ggplot2")
library("ggridges")
library("forcats")

df <- read.csv("Car_sale_ads.csv")
cars_pln <- df %>% filter(Currency == "PLN")


cars_pln$Year_Group <- cut(cars_pln$Production_year,
                     breaks = c(1915, 1998, 2004, 2009, 2015, 2021),
                     labels = c("1915-1998", "1999-2004", "2005-2009", "2010-2015", "2016-2021"),
                     include.lowest = TRUE)
# # 
# ggplot(cars_pln, aes(x = Year_Group, y = Price)) +
#   geom_violin(fill = "skyblue", alpha = 0.6) +
#   labs(title = "Violin Plot of Price grouped by Production Year",
#        x = "Production Year Group",
#        y = "Price") +
#   theme_minimal() +
#   ylim(0, 1.5e5)
# 
# unique(cars_pln$Type)
# 
# 
# 
# ggplot(cars_pln, aes(x = Year_Group, y = Price)) +
#   geom_violin(fill = "skyblue", alpha = 0.6) +
#   stat_summary(fun = median, geom = "crossbar", width = 0.5, fatten = 2, color = "red") +
#   labs(title = "Violin Plot of Price grouped by Production Year",
#        x = "Production Year Group",
#        y = "Price") +
#   theme_minimal() +
#   ylim(0, 2e5)

# ggplot(cars_pln, aes(x = Year_Group, y = Mileage_km)) +
#   geom_violin(fill = "skyblue", alpha = 0.6) +  # Wykres skrzypcowy
#   stat_summary(fun = median, geom = "crossbar", width = 0.5, fatten = 2, color = "red") +  # Mediana jako pozioma linia
#   labs(title = "Violin Plot of Mileage grouped by Production Year",
#        x = "Production Year Group",
#        y = "Mileage") +
#   theme_minimal() +
#   ylim(0, 5e5)

# names(cars_pln)


new_cars <- cars_pln %>% filter(Production_year >= 1999)

top_brands <- new_cars %>%
  count(Vehicle_brand, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(Vehicle_brand)


cars_top_brands <- new_cars %>%
  filter(Vehicle_brand %in% top_brands)


# ggplot(cars_top_brands, aes(x = Vehicle_brand, y = Mileage_km, fill = Vehicle_brand)) +
#   geom_violin() +
#   theme_minimal() +
#   labs(title = "Violin plot of Mileage by Car Brand (all cars included)",
#        x = "Car Brand",
#        y = "Mileage") +
#   theme(legend.position = "none") +
#   stat_summary(fun = median, geom = "point", color = "black", size = 3, shape = 16) +
#   ylim(0, 5e5)

# top_median_mileage <- cars_top_brands %>%
#   group_by(Vehicle_brand) %>%
#   summarise(median_mileage = median(Mileage_km, na.rm = TRUE)) %>%
#   arrange(desc(median_mileage))
# 
# print(top_median_mileage)


#to dla samochodow ponizej 50k jako potencjalne na pierwszy samochod

 cars_affordable <- new_cars %>%
  filter(Price <= 50000)
# 
# top_affordable_brands <- cars_affordable %>%
#   count(Vehicle_brand, sort = TRUE) %>%
#   top_n(5, n) %>%
#   pull(Vehicle_brand)
# 
 cars_top_affordable_brands <- cars_affordable %>%
  filter(Vehicle_brand %in% top_affordable_brands)
# 
# ggplot(cars_top_affordable_brands, aes(x = Vehicle_brand, y = Mileage_km, fill = Vehicle_brand)) +
#   geom_violin() +
#   theme_minimal() +
#   labs(title = "Violin plot of Mileage by Car Brand (price below 50k)",
#        x = "Car Brand",
#        y = "Mileage") +
#   theme(legend.position = "none") +
#   stat_summary(fun = median, geom = "point", color = "black", size = 3, shape = 16) +
#   ylim(0, 5e5)

#MEGA SLABY WYKRES DO TEGO

# ggplot(cars_top_affordable_brands, aes(x = Price, y = Power_HP)) +
#   geom_density_2d(aes(color = ..level..), size = 1) +
#   scale_color_viridis_c() +
#   labs(title = "Density Contour Plot of Price vs Power",
#        x = "Price",
#        y = "Horsepower (HP)") +
#   theme_minimal()

cars_top_affordable_brands$Power_Category <- cut(
  cars_top_affordable_brands$Power_HP,
  breaks = c(0, 100, 200, 300, 500, Inf),
  labels = c("0-100 HP", "100-200 HP", "200-300 HP", "300-500 HP", "Wrong")
)

cars_top_affordable_brands$Power_Category[cars_top_affordable_brands$Power_Category == "Wrong"] <- NA

# ggplot(cars_top_affordable_brands, aes(x = Price, fill = Power_Category)) +
#   geom_density(alpha = 0.5) +
#   scale_fill_viridis_d() +
#   labs(title = "Density Plot of Price Across Different Horsepower Levels",
#        x = "Price ",
#        y = "Density",
#        fill = "Horsepower Category") +
#   theme_minimal()

# ggplot(na.omit(cars_top_affordable_brands), aes(x = Price, fill = Power_Category)) +
#   geom_density(alpha = 0.5) +
#   scale_fill_viridis_d() +
#   labs(title = "Density Plot of Price (<50k) Across Different Horsepower Levels",
#        x = "Price",
#        y = "Density",
#        fill = "Horsepower Category") +
#   theme_minimal()

# ggplot(cars_pln, aes(x = Displacement_cm3, fill = Year_Group)) +
#   geom_density(alpha = 0.5) +
#   scale_fill_viridis_d() +
#   labs(title = "Density Plot of Displacement (cm3) Across Different Age Groups",
#        x = "Displacement (cm3)",
#        y = "Density",
#        fill = "Car Year Group") +
#   theme_minimal() +
#   xlim(0,3500)

# ggplot(
#   cars_pln %>% filter(Year_Group %in% c("1915-1998", "2016-2021")), 
#   aes(x = Displacement_cm3, fill = Year_Group)
# ) +
#   geom_density(alpha = 0.5) +
#   scale_fill_viridis_d() +
#   labs(title = "Density Plot of Displacement (cm3) for Selected Year Groups",
#        x = "Displacement (cm3)",
#        y = "Density",
#        fill = "Car Year Group") +
#   theme_minimal() +
#   xlim(0, 3500) +
#   facet_wrap(~Year_Group, scales = "fixed")
# 
# cars_pln %>% group_by(Year_Group) %>% summarise(count = n())
# 
# unique(cars_pln$Type)


###### Jest na plakacie


wykresik1 <-ggplot(cars_pln, aes(x = Displacement_cm3/1000, y = fct_rev(Year_Group), fill = Year_Group)) +
  geom_density_ridges(alpha = 0.5, scale = 1.2) +
  scale_fill_manual(values = c("orange", "darkorange", "chocolate","saddlebrown", "brown4")) +
  labs(
    title = "Litraż samochodów a ich wiek",
    x = "Pojemność silnika (litry)",
    y = "Liczność",
    fill = "Grupa rocznikowa"
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 18))+
  xlim(0, 3.5)





# cars_pln$Power_Category <- cut(
#   cars_pln$Power_HP,
#   breaks = c(0, 100, 200, 300, 500, Inf),
#   labels = c("0-100 HP", "100-200 HP", "200-300 HP", "300-500 HP", "Wrong")
# )
# 
# cars_pln$Power_Category[cars_pln$Power_Category == "Wrong"] <- NA
# 
# ggplot(na.omit(cars_pln), aes(x = Price, fill = Power_Category)) +
#   geom_density(alpha = 0.5) +
#   scale_fill_viridis_d() +
#   labs(title = "Density Plot of Price Across Different Horsepower Levels",
#        x = "Price",
#        y = "Density",
#        fill = "Horsepower Category") +
#   theme_minimal() +
#   xlim(0, 100000)

############

transmission_percentages <- new_cars %>%
  filter(Transmission %in% c("Manual", "Automatic")) %>%
  group_by(Production_year, Transmission) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Production_year) %>%
  mutate(percentage = count / sum(count) * 100)

transmission_percentages <- transmission_percentages %>%
  mutate(Transmission = recode(Transmission,
                               "Manual" = "Manualna",
                               "Automatic" = "Automatyczna"))

transmission_percentages <- transmission_percentages %>%
  mutate(Transmission = factor(Transmission, levels = c("Manualna", "Automatyczna")))
# Wykres
wykresik2 <-ggplot(transmission_percentages, aes(x = Production_year, y = percentage, fill = Transmission)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Manualna" = "blue3", "Automatyczna" = "red3")) +  
  theme_minimal() +
  labs(  title = "Typ skrzyni biegów a rok produkcji",
         subtitle = "Manualna czy automatyczna? Udział procentowy według roku",
         x = "Rok produkcji",
         y = "Procent",
         fill = "Typ skrzyni biegów") +
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(labels = scales::percent_format(scale = 100), expand = c(0, 0)) +
  theme(text = element_text(face = "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 15, face = "plain")) 

##########
library(ggplot2)

###### Jest na plakacie

wykresik2 <- ggplot(transmission_percentages, aes(x = Production_year, y = percentage, color = Transmission, group = Transmission)) +
  geom_line(size = 1.8) +
  geom_point(size = 3.2) +
  scale_color_manual(values = c("Manualna" = "blue3", "Automatyczna" = "red3")) +
  theme_minimal() +
  labs(
    title = "Typ skrzyni biegów a rok produkcji",
    subtitle = "Manualna czy automatyczna? Udział procentowy według roku",
    x = "Rok produkcji",
    y = "Procent",
    color = "Typ skrzyni biegów"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),limits = c(0, 100), expand = c(0, 0)) +
  theme(
    text = element_text(face = "bold"),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15, face = "plain")
  )
print(wykresik2)

# Same automaty
automatic_percentages <- transmission_percentages %>%
  filter(Transmission == "Automatyczna")
# Tworzenie wykresu


ggplot(automatic_percentages, aes(x = Production_year, y = percentage, fill = Transmission)) +
  geom_bar(stat = "identity", position = "stack") +  # Zmieniamy na 'stack' zamiast 'fill'
  scale_fill_manual(values = c("Automatyczna" = "#ff7f0e")) +  # Kolor pomarańczowy
  theme_minimal() +
  labs(title = "Udział skrzyń automatycznych według roku produkcji",
       x = "Rok produkcji",
       y = "Procent") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 0), limits = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

unique(df$Fuel_type)

#########################

# Calculate the percentages for each Fuel_type by Production_year
fuel_percentages <- new_cars %>%
  filter(Fuel_type %in% c("Gasoline", "Gasoline + LPG", "Diesel", "Electric", "Hybrid")) %>%
  group_by(Production_year, Fuel_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Production_year) %>%
  mutate(percentage = count / sum(count) * 100)

# Wykres
ggplot(fuel_percentages, aes(x = Production_year, y = percentage, fill = Fuel_type)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked bar chart
  scale_fill_manual(values = c(
    "Gasoline" = "#1f77b4",
    "Gasoline + LPG" = "#ff7f0e",
    "Diesel" = "#2ca02c",
    "Electric" = "purple",
    "Hybrid" = "red"
  )) +
  theme_minimal() +
  labs(title = "Udział rodzajów paliwa według roku produkcji",
       x = "Rok produkcji",
       y = "Procent",
       fill = "Typ paliwa") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(fuel_percentages, aes(x = Production_year, y = percentage, color = Fuel_type, group = Fuel_type)) +
  geom_line(size = 1) +  # Line graph instead of bar chart
  scale_color_manual(values = c(
    "Gasoline" = "#1f77b4",
    "Gasoline + LPG" = "#ff7f0e",
    "Diesel" = "#2ca02c",
    "Electric" = "purple",
    "Hybrid" = "red"
  )) +
  theme_minimal() +
  labs(title = "Udział rodzajów paliwa według roku produkcji",
       x = "Rok produkcji",
       y = "Procent",
       color = "Typ paliwa") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 0), limits = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

