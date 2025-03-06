# Ładowanie pakietów
# install.packeges("dplyr")
library(dplyr)


# Ładowanie danych
df <- read.csv("data.csv")
#df<-data

########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowe, binarna/nominalna
# Age - ilościowe, zliczenia
# Occupation - jakościowe, nominalna
# Sleep.Duration - ilościowe, zliczenia
# Quality.of.Sleep - jakościowe, uporządkowane
# Physical.Activity.Level - uporządkowane
# BMI.Category - jakościowe, uporządkowane
# Sleep.Disorder - jakościowe,nominalna


########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(`BMI Category`,`Heart Rate`) %>% 
  group_by(`BMI Category`) %>% 
  summarise(mean_hr=mean(`Heart Rate`)) %>%
  arrange(-mean_hr) %>% 
  head(1) %>% 
  select(`BMI Category`)

########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(`Occupation`,`Sleep Duration`,`Stress Level`) %>% 
  filter(`Stress Level`<7) %>% 
  group_by(`Occupation`) %>% 
  summarise(mean_sd=mean(`Sleep Duration`)) %>% 
  arrange(-mean_sd) %>% 
  head(1) %>% 
  select(`Occupation`)

########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(`Occupation`,`Sleep Disorder`) %>% 
  group_by(`Occupation`) %>%
  summarise(total = n(), disorder = sum(`Sleep Disorder` != "None")) %>%
  mutate(d_t=(disorder/total)*100) %>% 
  select(`Occupation`,d_t) %>% 
  arrange(-d_t)
#Odp.: Osoby z zaburzeniem snu najczęściej pracują w Sales Representative, potem Salesperson, a potem Nurse.

########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df %>% 
  select(`Occupation`,`Gender`,`BMI Category`) %>% 
  filter(`BMI Category`=='Obese') %>% 
  group_by(`Occupation`,`Gender`) %>% 
  summarise(n=n()) %>% 
  mutate(o_f=ifelse(`Gender`== 'Female',`n`,0), o_m=ifelse(`Gender`== 'Male',`n`,0)) %>% 
  select(-Gender,-n) %>% 
  group_by(`Occupation`) %>% 
  summarise(number_of_f=sum(o_f),number_of_m=sum(o_m)) %>% 
  filter(number_of_f<number_of_m) %>% 
  select(`Occupation`)

########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
df1<-df %>% 
  group_by(`Occupation`) %>% 
  mutate(n=n()) %>% 
  filter(n>20) %>% 
  separate(`Blood Pressure`, into = c('skurcz', 'rozkurcz'), sep = "/") %>% 
  mutate(skurcz=as.numeric(skurcz),rozkurcz=as.numeric(rozkurcz)) %>% 
  mutate(delta=skurcz-rozkurcz) %>% 
  group_by(`Occupation`) %>% 
  summarise(sr_delta=mean(delta)) %>% 
  arrange(-sr_delta)

df1[2,1]

df2<-df %>% 
  select(`Sleep Duration`,`Quality of Sleep`) %>% 
  group_by(`Sleep Duration`) %>% 
  summarise(sr_czasu=mean(`Quality of Sleep`), med_czasu=median(`Quality of Sleep`)) %>% 
  View()

########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
df3<-df %>% 
  select(`Occupation`, `Physical Activity Level`,`Age`, `Heart Rate`) %>% 
  group_by(`Occupation`) %>% 
  summarise(sr_akt=mean(`Physical Activity Level`)) %>% 
  arrange(-sr_akt) %>% 
  head(3)

a<-df3[[1]]

df4<-df %>% 
  select(`Occupation`,`Age`, `Heart Rate`) %>% 
  mutate(Age=as.numeric(Age)) %>% 
  mutate(char=ifelse(Age>=50,1,0)) %>% 
  group_by(char) %>% 
  summarise(sr_total=mean(`Heart Rate`)) 

df5<-df %>% 
  filter(`Occupation`==a[1] | `Occupation`==a[2] | `Occupation`==a[3]) %>%
  mutate(Age=as.numeric(Age)) %>% 
  mutate(char=ifelse(Age>=50,1,0)) %>% 
  group_by(char) %>% 
  summarise(sr_3=mean(`Heart Rate`))

r<-abs(df4[[2]]-df5[[2]])             #r[1] to różnica dla Age<50, a r[2] dla Age>=50

########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
df6<-df %>% 
  select(`Occupation`,`Age`, `Gender`,`Stress Level`) %>% 
  mutate(przedzial=case_when(
    Age>=90 ~ "[90,100)",
    Age>=80 ~ "[80,90)",
    Age>=70 ~ "[70,80)",
    Age>=60 ~ "[60,70)",
    Age>=50 ~ "[50,60)",
    Age>=40 ~ "[40,50)",
    Age>=30 ~ "[30,40)",
    Age>=20 ~ "[20,30)",
    Age>=10 ~ "[10,20)",
    Age>=0 ~ "[0,10)")) %>% 
  group_by(przedzial,Gender,Occupation) %>% 
  summarise(count=n()) %>% 
  group_by(przedzial, Gender) %>%
  slice_max(n=1,count)
df7<-df %>% 
  select(`Occupation`,`Age`, `Gender`,`Stress Level`) %>% 
  mutate(przedzial=case_when(
    Age>=90 ~ "[90,100)",
    Age>=80 ~ "[80,90)",
    Age>=70 ~ "[70,80)",
    Age>=60 ~ "[60,70)",
    Age>=50 ~ "[50,60)",
    Age>=40 ~ "[40,50)",
    Age>=30 ~ "[30,40)",
    Age>=20 ~ "[20,30)",
    Age>=10 ~ "[10,20)",
    Age>=0 ~ "[0,10)")) %>% 
  group_by(przedzial) %>% 
  summarise(med=median(`Stress Level`), mean=mean(`Stress Level`),odchylenie=sd(`Stress Level`))
