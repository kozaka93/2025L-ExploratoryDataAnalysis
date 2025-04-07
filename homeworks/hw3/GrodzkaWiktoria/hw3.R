library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(readr)
library(scales) 

#żródła:
#https://www.numbeo.com/cost-of-living/in/New-York
#https://www.kaggle.com/datasets/myrios/cost-of-living-index-by-country-by-number-2024?resource=download

#-----------------------mapa Europy
df<-map_data("world")
eu <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", 
        "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", 
        "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Georgia", 
        "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", 
        "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", 
        "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", 
        "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", 
        "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "UK")
df<- df %>% filter(long>=-25 & long <= 80 & lat<=100 & lat>=30) %>% 
  mutate(region_eu=case_when(
    region=="Vatican" ~ "Italy",
    region %in% eu ~ region,
    TRUE ~ "non_eu"))

ggplot(data = df) +
  geom_polygon(aes(x = long, y = lat, fill=region_eu, group = group), color = "white") +
  theme(legend.position = "None")+
  xlim(-25, 63)+
  ylim(30,72)+
  coord_fixed(1.3)

#------------base----------------------
base<-ggplot(data = df, mapping = aes(x = long, y = lat, group = group, fill=region_eu), color = "white")+
  coord_fixed(1.3) +
  geom_polygon(color = "white", fill = "gray")

#---------------------wykres kosztów życia w Europie--------------------
koszty <- read.csv("Documents/wstep/koszty_zycia.csv", header = TRUE, sep = ",")

#setdiff(eu,unique(koszty$Country))

koszty<-koszty %>%
  rename(region=Country)%>% mutate(region=case_when(
    region=="Bosnia And Herzegovina"~"Bosnia and Herzegovina",
    region=="Kosovo (Disputed Territory)"~"Kosovo",
    region=="Monaco"~"France",
    region=="San Marino"~"Italy",
    region=="Andorra"~"Spain",
    region=="United Kingdom"~"UK",
    region=="Liechtenstein"~"Switzerland",
    TRUE ~ region)) %>% mutate(region_eu=case_when(
      region %in% eu ~ region,
      TRUE ~ "non_eu"), cost=case_when(
        region_eu=="non_eu"~0,
        TRUE~ Cost.of.Living.Index*66.468)) %>%
  select(region_eu,cost) %>% distinct()


merged <- merge(df, koszty, by = "region_eu")%>% 
  filter(region_eu != "non_eu")
merged <- merged[order(merged$group, merged$order), ]

base + geom_polygon(data=merged,aes(group=group,fill = cost))+
  theme_void() +
  labs(title = "Średnie miesięczne koszty życia w krajach europejskich w 2024 roku.",
       subtitle = "Na jedną osobe nie uwzględniając wynajmu mieszkania.",
       fill = "Koszt [zł]")+
  scale_fill_distiller(
    palette = "Reds",
    direction = 1,  # 1 = jasny → ciemny, -1 = odwrotnie
    na.value = "gray90",
    labels = label_number(scale = 0.001, suffix = " tys")
  )     










