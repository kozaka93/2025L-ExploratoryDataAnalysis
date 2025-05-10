library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
#źródło danych: https://news.gallup.com/poll/659630/americans-economic-financial-expectations-sink-april.aspx

data <- read_csv("~/Desktop/GrodzkaWiktoria/data.csv")

nazwy_kolumn <- strsplit(names(data), "%")[[1]] %>% str_trim()
colnames(data)<-"col"
dane <- data %>%
  separate(col,into = nazwy_kolumn, sep = ";") %>%
  mutate(across(c(`Excellent/Good;`, `Only fair;`, `Poor`),~ as.numeric(.))) %>% rename(date=`The Gallup Economic Confidence Index summarizes Americans' assessments of current economic conditions;`,Good=`Excellent/Good;`,`Only fair`=`Only fair;`)

dane_dod<- dane %>%
  pivot_longer(cols = c(Good, `Only fair`, `Poor`),
               names_to = "Rate",
               values_to = "Percent") %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))
#---------wykres główny-------
# To co chciałabym zmienić w orginalnym wykresie to przede wszystkim nieodpowiedni typ linii szkicujących wykres.
# Moim zdaniemm linie przerywane zaburzają przejrzystość analizy, co więcej uważam, że linie powinny być w różnych odcieniach
# tego samego koloru aby prawidłowo oddawać skalę zadowolenia Amerykanów.
# Dodałabym również punkty, które wyraźnie oddadzą zmiany w ocenie sytuacji finansowej.
# Ponadto dodałabym znak procent przy wartościach na osi Y.
ggplot(dane_dod, aes(x = date, y = Percent, color = Rate, group = Rate)) +
  geom_line() +  
  geom_point() + scale_x_date(breaks = seq(min(dane_dod$date), max(dane_dod$date), by = "5 years"), 
  date_labels = "%Y") +
  labs(
    x = "Date", 
    y = "Percent", 
    color = "Rate",
    title = "Americans' Current Financial Situation, 2001-2025",  
    subtitle = "How would you rate your financial situation today — as excellent, good, only fair or poor?" 
  )+scale_y_continuous(labels = function(x) paste0(x, "%"))+
  theme_minimal() + theme(
    plot.title = element_text(family = "Arial", size = 16, face = "bold",color="#858785"),  
    plot.subtitle = element_text(family = "Arial", size = 12, face = "italic",color="#858785"),
    axis.title.x = element_text(family = "Arial", size = 12, face = "italic",color = "#858785"),  
    axis.title.y = element_text(family = "Arial", size = 12, face = "italic",color = "#858785"),  
    axis.text.x = element_text(color = "#858785"),   
    axis.text.y = element_text(color = "#858785"),   
    legend.title = element_text(family = "Arial", size = 12, face = "italic",color = "#858785"),  
    legend.text = element_text(family = "Arial", size = 10, face = "italic",color = "#858785"),
    plot.background = element_rect(fill = "#eff8f0", color=NA),  
    panel.background = element_rect(fill = "#eff8f0", color=NA),
    panel.border = element_blank())+
  scale_color_manual(values = c("Good" = "#006400", "Only fair" = "#4C9A2A", "Poor" = "#6DBE45"))

# Powyższy wykres jest czytelniejszy niż orginał, a dobrane kolory i typy linii sugerują w jaki sposób powinien być interpretowany.
#----------------------------
dane_1<- dane%>% 
  mutate(year = str_sub(date, -4)) %>%
  group_by(year) %>% 
  summarise(Good=mean(Good), `Only fair`=mean(`Only fair`),Poor=mean(Poor))

dane_2<- dane_1 %>%
  pivot_longer(cols = c(Good, `Only fair`, Poor),
               names_to = "Rate",
               values_to = "Percent")
#-----------wykres dodatkowy
# Dodatkowo jeżeli chcielibyśmy pokazać zmianę struktury zadowolenia w poszczegolnych latach to możemy powyższy wykres
# przedstawić w formie słupkowej. Bedzie ona odpowiednio reprezentowała udział procentów w całości.
ggplot(dane_2, aes(x = factor(year), y = Percent, fill = Rate)) +
  geom_col(position = "fill") +  scale_x_discrete(breaks = seq(min(dane_2$year), max(dane_2$year), by = 5))+
  labs(
    title = "Oceny sytuacji ekonomicznej w poszczególnych latach",
    x = "Rok",
    y = "Procent odpowiedzi",
    fill = "Kategoria"
  ) +
  theme_minimal()+ scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
  labs(
    x = "Date", 
    y = "Percent", 
    fill = "Rate",
    title = "Americans' Current Financial Situation, 2001-2025",  
    subtitle = "How would you rate your financial situation today — as excellent, good, only fair or poor?" 
  )+theme_minimal() + theme(
    plot.title = element_text(family = "Arial", size = 16, face = "bold",color="#858785"),  
    plot.subtitle = element_text(family = "Arial", size = 12, face = "italic",color="#858785"),
    axis.title.x = element_text(family = "Arial", size = 12, face = "italic",color = "#858785"),  
    axis.title.y = element_text(family = "Arial", size = 12, face = "italic",color = "#858785"),  
    axis.text.x = element_text(color = "#858785"),   
    axis.text.y = element_text(color = "#858785"),   
    legend.title = element_text(family = "Arial", size = 12, face = "italic",color = "#858785"),  
    legend.text = element_text(family = "Arial", size = 10, face = "italic",color = "#858785"),
    plot.background = element_rect(fill = "#eff8f0", color=NA),  
    panel.background = element_rect(fill = "#eff8f0", color=NA),
    panel.border = element_blank())+
  scale_fill_manual(values = c("Good" = "#006400", "Only fair" = "#4C9A2A", "Poor" = "#6DBE45"))

