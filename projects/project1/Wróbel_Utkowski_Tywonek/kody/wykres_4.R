library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(patchwork)
library(ggpubr)

birth_village <- read.csv("kody/urodzenia_wies.csv", sep = ";")
birth_city <- read.csv("kody/urodzenia_miasto.csv", sep = ";")

birth_city <- birth_city[1:16,]

earnings <- read.csv("kody/wynagrodzenie_wojew.csv", sep = ";")


birth_village_long <- birth_village %>% 
  pivot_longer(!NAZWA ,names_to = "rok", values_to = "urodzenia")

birth_village_long$urodzenia <- gsub(" ", "", birth_village_long$urodzenia)
birth_village_long$urodzenia <- as.integer(birth_village_long$urodzenia)
birth_village_long$rok <- gsub("X", "", birth_village_long$rok)
birth_village_long$rok <- as.integer(birth_village_long$rok)
names(birth_village_long)[1] <- "wojewodztwo"


birth_city_long <- birth_city %>% 
  pivot_longer(!NAZWA ,names_to = "rok", values_to = "urodzenia")

birth_city_long$urodzenia <- as.integer(birth_city_long$urodzenia)
birth_city_long$rok <- gsub("X", "", birth_city_long$rok)
birth_city_long$rok <- as.integer(birth_city_long$rok)
names(birth_city_long)[1] <- "wojewodztwo"

earnings_long <- earnings %>% 
  pivot_longer(!NAZWA, names_to = "rok", values_to = "pensja")


earnings_long$pensja <- gsub(",", ".", earnings_long$pensja)
earnings_long$pensja <- as.double(earnings_long$pensja)

earnings_long$rok <- gsub("X", "", earnings_long$rok)
names(earnings_long)[1] <- "wojewodztwo"

birth <- full_join(birth_city_long, birth_village_long, by=join_by(wojewodztwo, rok))

tmp1 <- birth_city_long %>% 
  group_by(rok) %>% 
  summarise(suma = sum(urodzenia))
birth <- birth_village_long %>% 
  group_by(rok) %>% 
  summarise(suma = sum(urodzenia))
birth['suma2'] <- tmp1$suma
birth$suma <- birth$suma+birth$suma2

bvtmp<-birth_village_long %>% 
  group_by(rok) %>% 
  summarise(suma = sum(urodzenia))

bctmp <-birth_city_long %>% 
  group_by(rok) %>% 
  summarise(suma = sum(urodzenia))
ertmp <- earnings_long %>% 
  group_by(rok) %>% 
  summarise(mediana = median(pensja))

df1<- data.frame(
  rok = birth$rok,
  wies = bvtmp$suma,
  pensja = ertmp$mediana
)


df2<-data.frame(
  rok = birth$rok,
  miasto = bctmp$suma,
  pensja = ertmp$mediana
)

df3 <- data.frame(
  rok = birth$rok,
  ogolem = birth$suma,
  pensja = ertmp$mediana
)

scale_factor <- max(df1$wies) / max(df1$pensja)

m1<- ggplot(df1, aes(x = rok)) +
  geom_bar(aes(y = wies, fill = "Urodzenia"), stat = "identity", position = "dodge") +
  geom_line(aes(y = pensja * scale_factor, color = "Przychód"), linewidth = 1.3) +
  geom_point(aes(y = pensja * scale_factor, color = "Przychód"), size = 2.2) +
  scale_y_continuous(
    name = "Liczba urodzeń",
    sec.axis = sec_axis(~ . / scale_factor, name = "Przychód  [zł]")) +
  scale_x_continuous(breaks = seq(min(df1$rok), max(df1$rok), by = 3), 
                     guide = guide_axis(angle = 30))+
  scale_fill_manual(values = c("Urodzenia" = "#c6dbef")) +
  scale_color_manual(values = c("Przychód" = "#084594")) +
  theme_minimal()+
  labs(title = "Na obszarach wiejskich")+
  theme(legend.position = "none")


m2 <- ggplot(df2, aes(x = rok)) +
  geom_bar(aes(y =miasto, fill = "Urodzenia"), stat = "identity", position = "dodge") +
  geom_line(aes(y = pensja * scale_factor, color = "Przychód"), linewidth = 1.3) +
  geom_point(aes(y = pensja * scale_factor, color = "Przychód"), size = 2.2) +
  scale_y_continuous(
    name = "Liczba urodzeń",
    sec.axis = sec_axis(~ . / scale_factor, name = "Przychód  [zł]")) +
  scale_x_continuous(breaks = seq(min(df1$rok), max(df1$rok), by = 3), 
                     guide = guide_axis(angle = 30))+
  scale_fill_manual(values = c("Urodzenia" = "#c6dbef")) +
  scale_color_manual(values = c("Przychód" = "#084594")) +
  theme_minimal()+
  labs(title = "Na obszarach miejskich")+
  theme(legend.position = "none")


m3<-ggplot(df3, aes(x = rok)) +
  geom_bar(aes(y =ogolem, fill = "Urodzenia"), stat = "identity", position = "dodge") +
  geom_line(aes(y = pensja * scale_factor, color = "Przychód"), linewidth = 1.3) +
  geom_point(aes(y = pensja * scale_factor, color = "Przychód"), size = 2.2) +
  scale_y_continuous(
    name = "Liczba urodzeń",
    sec.axis = sec_axis(~ . / scale_factor, name = "Przychód  [zł]")) +
  scale_x_continuous(breaks = seq(min(df1$rok), max(df1$rok), by = 2), 
                     guide = guide_axis(angle = 30))+
  scale_fill_manual(values = c("Urodzenia" = "#c6dbef")) +
  scale_color_manual(values = c("Przychód" = "#084594")) +
  theme_minimal()+
  labs(title = "Ogółem",
       fill = "",
       color = "")

legend <- get_legend(
  m3 + 
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.title = element_blank())
)

final_plot <- 
  (m1 + m2) / m3 & 
  theme(legend.position = "none") 


wrap_plots(
  plot_spacer(),
  wrap_elements(full = legend),
  final_plot,
  heights = c(0.05, 0.1, 1)
) +
  plot_annotation(
    title = "Liczba urodzeń a mediana dochodu",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5)
    )
  )

