library(ggplot2)
library(sqldf)
library(dplyr)
library(readr)
library(scales) 
library(tidyr)


#Houses <- read_csv("~/Documents/wstep/projekt_1/Houses.csv")
# #-------------------------------------------------jedynka---------------------------------------
# df<-Houses %>% filter(city=='Warszawa',year>=2020 & year<=2023) %>% mutate(price=round(price,0),MiNI=round((((cos(latitude*pi/180)*(longitude-21.00694))^2+(latitude-52.22194)^2)^0.5*111.32),3))
# #MiNI to odległość od MiNI w km w linii prostej
# 
# df1<-df %>% filter(sq<=40)
# 
# 
# ggplot(data = df1, mapping = aes(x = sq, y=price, color=MiNI)) + geom_point() + scale_color_distiller(palette="Blues", direction=-1) + ylim(0,1000000) +
#   scale_y_continuous(labels = label_number(scale = 0.001, suffix=" tys")) + 
#   labs(
#   title = "Rynek mieszkaniowy w latach 2020-2023",
#   subtitle = "Rozważamy mieszkania atrakcyjne dla studentów Wydziału MiNI",
#   x = "Wielkość mieszkania [m^2]",
#   y = "Cena mieszkania [zł]",
#   color="Odległość od Wydziału MiNI [km]")
# #-------------------------------------------------czwórka---------------------------------------
# df2<-Houses %>% 
#   filter(sq<=200 & year>=2020 & year<=2023) %>%
#   mutate(przedzial=case_when(
#   sq<=10 ~ '(0,10]',
#   sq<=20 ~ '(10,20]',
#   sq<=30 ~ '(20,30]',
#   sq<=40 ~ '(30,40]',
#   sq<=50 ~ '(40,50]',
#   sq<=60 ~ '(50,60]',
#   sq<=70 ~ '(60,70]',
#   sq<=80 ~ '(70,80]',
#   sq<=90 ~ '(80,90]',
#   sq<=100 ~ '(90,100]',
#   sq<=110 ~ '(100,110]',
#   sq<=120 ~ '(110,120]',
#   sq<=130 ~ '(120,130]',
#   sq<=140 ~ '(130,140]',
#   sq<=150 ~ '(140,150]',
#   sq<=160 ~ '(150,160]',
#   sq<=170 ~ '(160,170]',
#   sq<=180 ~ '(170,180]',
#   sq<=190 ~ '(180,190]',
#   sq<=200 ~ '(190,200]'), cena_za_m2=price/sq) %>%
#   group_by(przedzial) %>%
#   summarise(śr_cena_za_m2=mean(cena_za_m2), mean_cena=mean(price)) %>%
#   mutate(przedzial=forcats::fct_reorder(przedzial,śr_cena_za_m2)) 
# 
#   ggplot(df2,aes(y=przedzial, x=śr_cena_za_m2, fill=mean_cena)) +
#   geom_col() +
#   scale_x_continuous(expand=c(0,0)) + 
#   labs(
#     title = "Rynek mieszkaniowy w latach 2020-2023",
#     subtitle = "Rozważamy cene za m^2 w zależności od metrażu",
#     x = "Średnia cena za m^2 [zł]",
#     y = "Przedział metrażu [m^2]", fill="Cena mieszkania [zł]") +
#   scale_x_continuous(labels = label_number(scale = 0.001, suffix=" tys"), expand=c(0,0)) +
#   scale_fill_continuous(labels = label_number(scale = 0.001, suffix=" tys"))
  
  
  
#!!!!!!----------------w Warszawie----------------------------------------------------
df2W<-Houses %>% 
  filter(sq<=200 & year>=1960 & year<=2023 & city=='Warszawa'& sq>10) %>%
  mutate(przedzial=case_when(
    sq<=20 ~ '(10,20]',
    sq<=30 ~ '(20,30]',
    sq<=40 ~ '(30,40]',
    sq<=50 ~ '(40,50]',
    sq<=60 ~ '(50,60]',
    sq<=70 ~ '(60,70]',
    sq<=80 ~ '(70,80]',
    sq<=90 ~ '(80,90]',
    sq<=100 ~ '(90,100]',
    sq<=110 ~ '(100,110]',
    sq<=120 ~ '(110,120]',
    sq<=130 ~ '(120,130]',
    sq<=140 ~ '(130,140]',
    sq<=150 ~ '(140,150]',
    sq<=160 ~ '(150,160]',
    sq<=170 ~ '(160,170]',
    sq<=180 ~ '(170,180]',
    sq<=190 ~ '(180,190]',
    sq<=200 ~ '(190,200]'), cena_za_m2=price/sq) %>%
  group_by(przedzial) %>%
  summarise(śr_cena_za_m2=mean(cena_za_m2), mean_cena=mean(price)) %>%
  mutate(przedzial=forcats::fct_reorder(przedzial,śr_cena_za_m2))
df2W<-df2W %>%
  mutate(dolna_granica = as.numeric(gsub("[^0-9]", "", sub(",.*", "", przedzial)))) %>%
  arrange(dolna_granica) %>%
  mutate(przedzial = factor(przedzial, levels = przedzial))

  ggplot(df2W,aes(y=przedzial, x=śr_cena_za_m2, fill=mean_cena)) +
  geom_col() + 
  scale_x_continuous(expand=c(0,0), labels = label_number(scale = 0.001, suffix=" tys")) + 
  labs(
    title = "Rynek mieszkaniowy w Warszawie w latach 1960-2023",
    subtitle = expression("Rozważamy cene za"~m^2~"w zależności od metrażu"),
    x = expression("Średnia cena za"~m^2~"[zł]"),
    y = expression("Przedział metrażu ["~m^2~"]"), fill="Cena mieszkania [zł]")+ 
    scale_fill_gradient2(
      mid = "red",   # Kolor dla najmniejszych wartości (np. ciepły)
      high = "navy",   # Kolor dla średnich wartości (np. pomarańczowy)
      low ="lightgoldenrod1",  # Kolor dla największych wartości (np. zimny)
      midpoint = 2000000  # Ustawienie punktu środkowego dla gradientu
    ,labels = label_number(scale = 0.000001, suffix=" mln"))
#   scale_fill_gradient2(
#     mid = "#F1A340",   # Kolor dla najmniejszych wartości (np. ciepły)
#     high = "#DC143C",   # Kolor dla średnich wartości (np. pomarańczowy)
#     low = "#62B3E2",  # Kolor dla największych wartości (np. zimny)
#     midpoint = 2000000  # Ustawienie punktu środkowego dla gradientu
#     ,labels = label_number(scale = 0.000001, suffix=" mln"))
# #------------wykres pensja
# wiek <- 23:70
# zarobki_roczne <- numeric(length(wiek_values))
# zarobki_roczne[1] <- 91963.68  
#   
# for (i in 2:length(wiek)) {
#   if (wiek_values[i] == 26) {
#     zarobki_roczne[i] <- 98653.56}
#   else {
#       zarobki_roczne[i] <- zarobki_roczne[i - 1] * 1.05}}
#   
# df2 <- data.frame(wiek = wiek, zarobki_roczne = zarobki_roczne)
  
#-----------------------------budżet miniowiczów funkcja--------------------------------

oblicz_zarobki <- function(start_brutto = 10000, wiek_start = 23, wiek_koniec = 50, podwyzka = 0.05) {
  # Składki ZUS
  skladka_emerytalna <- 0.0976
  skladka_rentowa <- 0.015
  skladka_chorobowa <- 0.0245
  skladka_zdrowotna <- 0.09
  
  # Progi podatkowe
  kwota_wolna <- 30000
  prog_podatkowy <- 120000
  ulga_dla_mlodych <- 85528
  
  # Stawki PIT
  PIT_12 <- 0.12
  PIT_32 <- 0.32
  
  pensja_brutto <- start_brutto
  zarobki <- data.frame(Wiek = integer(), Brutto = numeric(), Netto = numeric())
  
  for (wiek in wiek_start:wiek_koniec) {
    roczne_brutto <- pensja_brutto * 12
    
    # Obliczenie składek ZUS
    skladki_ZUS <- roczne_brutto * (skladka_emerytalna + skladka_rentowa + skladka_chorobowa)
    podstawa_podatku <- roczne_brutto - skladki_ZUS
    
    # Obliczenie podatku
    if (wiek < 26 & podstawa_podatku <= ulga_dla_mlodych) {
      podatek <- 0  # Ulga dla młodych
    } else {
      do_opodatkowania <- max(0, podstawa_podatku - kwota_wolna)
      
      if (do_opodatkowania <= prog_podatkowy) {
        podatek <- do_opodatkowania * PIT_12
      } else {
        podatek <- (prog_podatkowy * PIT_12) + ((do_opodatkowania - prog_podatkowy) * PIT_32)
      }
    }
    
    # Obliczenie składki zdrowotnej
    skladka_zdrowotna_kwota <- podstawa_podatku * skladka_zdrowotna
    
    # Obliczenie pensji netto
    pensja_netto <- roczne_brutto - skladki_ZUS - podatek - skladka_zdrowotna_kwota
    
    # Zapis do tabeli
    zarobki <- rbind(zarobki, data.frame(Wiek = wiek, Brutto = roczne_brutto, Netto = pensja_netto))
    
    # Wzrost pensji o 5%
    pensja_brutto <- pensja_brutto * (1 + podwyzka)
  }
  
  return(zarobki)
}



#df3 %>% ggplot(aes(x=Wiek,y=Brutto, fill=Netto))+scale_y_continuous(labels = label_number(scale = 0.001, suffix=" tys"))
# #--------------------wykres zarobkow rocznych-----------------------------------
# df3<-oblicz_zarobki(10000,23,50,0.05) %>% mutate(Pensja_N=Netto/12,Pensja_B=Brutto/12)%>% mutate(pr10=Netto/10,pr15=Netto*0.15, pr20=Netto/5)
# ggplot(df3, aes(x = Wiek)) +
#   # Warstwa 1: Brutto (cały słupek)
#   geom_col(aes(y = Brutto), fill = "gray", alpha = 0.7) +
#   # Warstwa 2: Netto (część słupka na czerwono)
#   geom_col(aes(y = Netto), fill = "purple", alpha = 0.8) +
#   scale_y_continuous(labels = label_number(scale = 0.001, suffix = " tys")) +
#   labs(x = "Wiek [lata]",
#        y = "Zarobki roczne [zł]") 
# 
# #--------------------wykres pensji proba oszcz----------------------------------------------
# df3<-oblicz_zarobki(10000,23,50,0.05) %>% mutate(Pensja_N=Netto/12,Pensja_B=Brutto/12)%>% mutate(pr10=Pensja_N*0.9,pr15=Pensja_N*0.85, pr20=Pensja_N*0.8)
# ggplot(df3, aes(x = Wiek)) +
#   # Warstwa 1: Brutto (cały słupek)
#   geom_col(aes(y = Pensja_B),fill= "gray", alpha = 0.7) +
#   # Warstwa 2: Netto (część słupka na czerwono)
#   geom_col(aes(y = Pensja_N), fill = "purple", alpha = 0.8) +
#   geom_col(aes(y = pr10), fill = "blue", alpha = 0.7) +
#   # Warstwa 2: Netto (część słupka na czerwono)
#   geom_col(aes(y =pr15), fill = "orange", alpha = 0.7) +
#   geom_col(aes(y = pr20), fill ="green", alpha = 0.7)+
#   scale_y_continuous(labels = label_number(scale = 0.001, suffix = " tys")) +
#   labs(x = "Wiek [lata]",
#        y = "Średnia pensja miesięczna [zł]",fill="Dochód")+
#   theme_minimal()+guides(fill = guide_legend(
#     title = "Dochód",
#     labels = c("Pensja Brutto", "Pensja Netto", "Przedział 10%", "Przedział 15%", "Przedział 20%"),
#     override.aes = list(
#       fill = c("gray", "purple", "blue", "orange", "green") # Przypisanie kolorów do legendy
#     )
#   ))

#!!!!-----------------pensja na reke z oszcz------------------

df3<-oblicz_zarobki(10000,23,50,0.05) %>% mutate(Pensja_N=Netto/12,Pensja_B=Brutto/12)%>% mutate(pr10=Pensja_N*0.9,pr15=Pensja_N*0.85, pr20=Pensja_N*0.8)
df3_long <- df3 %>% gather(key = "kategoria", value = "pensja", Pensja_B, Pensja_N, pr10, pr15, pr20)


ggplot(df3_long, aes(x = Wiek, y = pensja, color = kategoria, group = kategoria)) +   
  geom_line(size = 1.5) +  # Używamy geom_line() do rysowania linii
  geom_point(size=1.7)+
  scale_y_continuous(labels = label_number(scale = 0.001, suffix = " tys")) + 
  expand_limits(y = 0) + 
  labs(
    x = "Wiek [lata]",             
    y = "Kwota [zł]",
    color="Dochód"
  ) +   
  scale_color_manual(  # Używamy scale_color_manual do kolorowania linii
    values = c(
      "Pensja_B" = "gray", 
      "Pensja_N" = "yellow", 
      "pr10" = "sienna2", 
      "pr15" = "brown2", 
      "pr20" = "mediumorchid4"
    ), labels=c("Brutto", 
                "Netto",
                "Po odłożeniu 10%", 
                "Po odłożeniu 15%", 
                "Po odłożeniu 20%")
  )+theme_minimal()

# #--------------------wykres oszczedzania----------------------------------------------
# df4<- df3 %>% mutate(pr10=Netto/10,pr15=Netto*0.15, pr20=Netto/5)
# sum10=0
# sum15=0
# sum20=0
# cumsum10=c()
# cumsum15=c()
# cumsum20=c()
# for (i in 1:48){
#   i=as.numeric(i)
#   sum10=sum10+df4$pr10[i]
#   append(cumsum10,sum10)
#   
#   sum15=sum15+df4$pr15[i]
#   append(cumsum15,sum15)
#   
#   sum20=sum20+df4$pr20[i]
#   append(cumsum20,sum20)
# }
#   
# df4<- df4 %>% mutate(cumsum10=cumsum10,cumsum15=cumsum15,cumsum20=cumsum20)
# 
# ggplot(df4, aes(x = Wiek)) +
#   # Warstwa 1: Brutto (cały słupek)
#   geom_col(aes(y = pr20), fill = "blue", alpha = 0.7) +
#   # Warstwa 2: Netto (część słupka na czerwono)
#   geom_col(aes(y =pr15), fill = "purple", alpha = 0.7) +
#   geom_col(aes(y = pr10), fill ="green", alpha = 0.7) +
#   scale_y_continuous(labels = label_number(scale = 0.001, suffix = " tys")) +
#   labs(x = "Wiek [lata]",
#        y = "Oszczędności [zł]") 
