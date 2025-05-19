library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)
library(RColorBrewer)
install.packages("plotrix")
library(plotrix)

dane<- read_csv("Documents/wstep/customer_support_tickets.csv")
dane<-dane %>%  rename(product="Product Purchased", wiek="Customer Age") 
#------------------------------------kolowy1
df2<-dane %>% mutate(age2=case_when(
  wiek < 10 ~ "0-9 lat",
  wiek < 20 ~ "10-19 lat",
  wiek < 30 ~ "20-29 lat",
  wiek < 40 ~ "30-39 lat",
  wiek < 50 ~ "40-49 lat",
  wiek < 60 ~ "50-59 lat",
  wiek < 70 ~ "60-69 lat",
  wiek < 80 ~ "70-79 lat",
  wiek < 90 ~ "80-89 lat",
  TRUE ~ "90+"
)) %>%  group_by(age2) %>% summarise(count=n()) %>% 
  mutate(procent = round(100 * count / sum(count), 1),
         label = paste0(procent, "%"),
         ypos = cumsum(count) - 0.5 * count)

ggplot(df2, aes(x = "", y = procent, fill = age2)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Analiza wpływu wieku na liczbę złożonych reklamacji", fill="Wiek reklamującego") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size=3)+
  theme_void()+
  scale_fill_manual(values = colors)
#----------------------------pierscien
df3 <- df2 %>%
  mutate(
    ymax = cumsum(count),
    ymin = c(0, head(ymax, n = -1)),
    label_pos = (ymax + ymin) / 2)

ggplot(df3, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = age2)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4.5)) +  
  theme_void() +
  geom_text(aes(x = 4.3, y = label_pos, label = paste0(round(procent), "%")), size = 5) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Analiza wpływu wieku na liczbę złożonych reklamacji", fill="Wiek reklamującego")
#------------------------------kolowy2
colors <- brewer.pal(7, "Set3")
par(mar = c(4, 4, 2, 4)) 
pie(df2$procent, col=colors, labels=paste0(df2$procent, "%"),
    labeldist=0.5, radius=0.8,
    main="Analiza wpływu wieku na liczbę złożonych reklamacji")
legend("topright", inset=c(-0.5,0), legend = df2$age2, fill = colors, title="Przedział wiekowy", xpd=TRUE, bty="n")
#------------------------------kolowy3d
par(mar = c(4, 4, 2, 4)) 
pie3D(df2$procent,
      labels=paste0(df2$procent, "%"),
      explode = 0.1,              
      col = colors,
      main = "Analiza wpływu wieku na liczbę złożonych reklamacji",
      labelcex = 0.8)
legend("topright", inset=c(-0.8,0), legend = df2$age2, fill = colors, title="Przedział wiekowy", xpd=TRUE, bty="n")
#------------------------------slupkowy
ggplot(df2, aes(x = age2, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Analiza wpływu wieku na liczbę złożonych reklamacji", y = "Liczba reklamacji", x = "Przedział wiekowy") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1,margin = margin(r = 0)))
#------------------------------slupkowykolorowy
ggplot(df2, aes(x = age2, y = count, fill=age2)) +
  geom_bar(stat = "identity") +
  labs(title = "Analiza wpływu wieku na liczbę złożonych reklamacji", y = "Liczba reklamacji", fill="Przedział wiekowy") +
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(),        
        axis.ticks.x = element_blank())+
  scale_fill_brewer(palette = "Set3")
