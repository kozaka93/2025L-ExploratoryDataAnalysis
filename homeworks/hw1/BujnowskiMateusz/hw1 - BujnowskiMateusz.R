# Ładowanie pakietów
library(dplyr)
library(tidyr)


# Ładowanie danych
df <- read.csv("data.csv")


########### Zad 1 (1pkt) ###########
# Określ jakiego typu są poniższe zmienne w zbiorze danych.

# Rozwiązanie: ------------------------------------------------------------

# Gender - jakościowe, binarne
# Age - ilościowe, ilorazowa
# Occupation - jakościowe, nominalna
# Sleep.Duration - ilościowa, ilorazowa
# Quality.of.Sleep - jakościowa, uporządkowana
# Physical.Activity.Level - ilościowa, ilorazowa
# BMI.Category - jakościowa, uporządkowana
# Sleep.Disorder - jakościowa, nominalna




########### Zad 2 (0.5pkt) ###########
# Jakie mają średie tętno ludzie w różnych kategoriach BMI? Dla jakiej kategorii BMI średnie tętno jest najwyższe?

# Rozwiązanie: ------------------------------------------------------------
# lista średniego tętna dla każdej kategorii BMI
df %>% group_by(BMI.Category) %>% # grupuję po BMI i wyliczam średnie tętno dla każdego rodzaju BMI
  summarise(srednie_tetno = mean(Heart.Rate, na.rm = TRUE)) %>% # sortuję malejąco
  arrange(-srednie_tetno)
# kategoria BMI, dla której średnie tętno jest najwyższe
df %>% group_by(BMI.Category) %>% # grupuję po BMI i wyliczam średnie tętno dla każdego rodzaju BMI
  summarise(srednie_tetno = mean(Heart.Rate, na.rm = TRUE)) %>% # sortuję malejąco
  arrange(-srednie_tetno) %>% head(1) # biorę najwyższe tętno




########### Zad 3 (0.5pkt) ###########
# W jakim zawodzie ludzie średnio najwięcej śpią jeżeli ich poziom stresu jest poniżej 7?

# Rozwiązanie: ------------------------------------------------------------
# zawód, w którym ludzie średnio najwięcej śpią mając stres poniżej 7
df %>% filter(Stress.Level < 7) %>% # filtruję po poziomie stresu, następnie grupuję po zawodzie i wyliczam średni sen
  group_by(Occupation) %>% summarise(sredni_sen_pow_7 = mean(Sleep.Duration, na.rm = TRUE)) %>% 
  arrange(-sredni_sen_pow_7) %>% head(1) # sortuję malejąco i biorę pierwszy wiersz




########### Zad 4 (0.5pkt) ###########
# W jakich zawodach najcześćiej pracują osoby z dowolnym zaburzeniem snu? 

# Rozwiązanie: ------------------------------------------------------------
# zawody, w których najczęściej pracują osoby z zaburzeniami snu, od najliczniejsze grupy
df %>% filter(Sleep.Disorder != "None") %>% # filtruję, żeby były tylko osoby z zaburzeniami snu
  count(Occupation, sort = TRUE) %>% # zliczam ilość wystąpień w każdym zawodzie i sortuję malejąco
  head(5) # wybieram 5 zawodów, w któych najwięcej jest osób z zaburzeniami snu



########### Zad 5 (0.5pkt) ###########
# W jakich zawodach jest więcej otyłych (Obese) mężczyzn niż otyłych kobiet?

# Rozwiązanie: ------------------------------------------------------------
df %>% filter(BMI.Category == "Obese") %>% # wybieram tylko osoby otyłe
  group_by(Occupation, Gender) %>% summarise(ilosc = n()) %>% # grupuję po zawodzie i płci oraz zliczam ilość wystąpień
  pivot_wider(names_from = Gender, values_from = ilosc, , values_fill = 0) %>% # zamieniam na tabelę szeroką
  filter(Male > Female) %>% select(Occupation) # wybieram zawody, w których jest więcej otyłych mężczyzn niż kobiet




########### Zad 6 (1 pkt) ###########
# Znajdź 3 zawody dla których w naszym zbiorze mamy więcej niż 20 przykładów oraz 
# różnica między ciśnieniem skurczowym i rozkurczowym jest średnio największa.
# Następnie dla zawodu, który jest drugi pod względem średniej różnicy i ma ponad 20 przykładów
# podaj średnią i medianę czasu snu dla różnych jakości snu.

# Rozwiązanie: ------------------------------------------------------------
# wypisuję trzy zawody
df %>% group_by(Occupation) %>% filter(n() > 20) %>% # grupuję po zawodzie i filtruję, żeby było go co najmniej 20
  mutate(skurczowe = as.numeric(sub("/.*", "", Blood.Pressure)), 
         rozkurczowe = as.numeric(sub(".*/", "", Blood.Pressure)),
         roznica = skurczowe - rozkurczowe) %>%  # robię osobne kolumny dla podzielonego ciśnienia i wyliczam różnicę
  summarise(srednia_roznica = mean(roznica)) %>% # wyliczam średnią różnicę po zawodzie
  arrange(-srednia_roznica) %>% head(3) # sortuję malejąco i wybieram trzy najwyższe
# znajduję nazwę zawodu, który jest drugi pod względem różnicy
zawod <- df %>% group_by(Occupation) %>% filter(n() > 20) %>% # grupuję po zawodzie i filtruję, żeby było go co najmniej 20
  mutate(skurczowe = as.numeric(sub("/.*", "", Blood.Pressure)), 
         rozkurczowe = as.numeric(sub(".*/", "", Blood.Pressure)),
         roznica = skurczowe - rozkurczowe) %>%  # robię osobne kolumny dla podzielonego ciśnienia i wyliczam różnicę
  summarise(srednia_roznica = mean(roznica)) %>% # wyliczam średnią różnicę po zawodzie
  arrange(-srednia_roznica) %>% slice(2) %>% select(Occupation) %>% as.character() # sortuję malejąco i drugi najwyższy zawód
# wyliczamy dla tego zawodu średnią i medianę snu dla różnych jakości snu
df %>% filter(Occupation == zawod) %>% group_by(Quality.of.Sleep) %>% # grupuję po jakości snu tylko dla tego zawodu
  summarise(srednia_czas = mean(Sleep.Duration), mediana_czasu = median(Sleep.Duration)) # wyliczam medianę i śreni czas snu dla różnych jakości




########### Zad 7 (1 pkt) ###########
# Znajdź 3 zawody, w których ludzie średnio najbardziej się ruszają. Jaka jest różnica
# między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując dane
# względem osób które mają co najmniej 50 lat (grupa 1) i poniżej 50 lat (grupa 2). 

# Rozwiązanie: ------------------------------------------------------------
# trzy zawody, w których ludzie średnio najwięcej się ruszają
df %>% group_by(Occupation) %>% # grupuję po zawodzie i obliczam średnią ruchu dla zawodu
  summarise(srednia_aktywnosc = mean(Physical.Activity.Level)) %>% 
  arrange(-srednia_aktywnosc) %>% head(3) %>% select(Occupation) # sortuję malejąco i wybieram 3 pierwsze

# obliczam różnicę między średnim tętnem z wszystkich danych a średnim tętnem w tych zawodach grupując po wieku 50 lat
zawody <- df %>% group_by(Occupation) %>% # grupuję po zawodzie i obliczam średnią ruchu dla zawodu
  summarise(srednia_aktywnosc = mean(Physical.Activity.Level)) %>% 
  arrange(-srednia_aktywnosc) %>% head(3) %>% select(Occupation) # sortuję malejąco i wybieram 3 pierwsze

# Nie wiem, która interpretacja polecenia jest poprawna. W obu wyliczone mam średnie tętno dla 
# tych trzech zawodów w podzieleniu na dwie grupy wiekowe.
# W pierwszej interpretacji odejmuję od śrendiego tętna wszystkich danych również w
# podziale na dwie grupy wiekowe.
calkowite_srenie_tetno <- df %>% 
  mutate(podzial_wieku = ifelse(Age >= 50, "Grupa 1", "Grupa 2")) %>% # grupuję osoby powyżej i poniżej 50 lat
  group_by(podzial_wieku) %>% summarise(srednie_tetno1 = mean(Heart.Rate)) # obliczam średnie tętno dla wszystkich danych
df %>% filter(Occupation %in% zawody$Occupation) %>% # filtruję dane, żeby były tylko zawody wcześniej wyznaczone
  mutate(podzial_wieku = ifelse(Age >= 50, "Grupa 1", "Grupa 2")) %>% # grupuję osoby powyżej i poniżej 50 lat
  group_by(podzial_wieku) %>% summarise(srednie_tetno = mean(Heart.Rate)) %>% # obliczam średnie tętno po grupach
  left_join(calkowite_srenie_tetno, by = "podzial_wieku") %>%  # łączę z danymi z całego zbioru
  mutate(roznica = srednie_tetno1 - srednie_tetno) %>%  # obliczam różnicę między średnimi tętnami
  select(podzial_wieku, roznica) # wybieram kolumny z wynikami

# W drugiej interpretacji odejmuję od średniego tętna dla wszystkich danych bez podziału
# na dwie grupy wiekowe.
df %>% filter(Occupation %in% zawody$Occupation) %>% # filtruję dane, żeby były tylko zawody wcześniej wyznaczone
  mutate(podzial_wieku = ifelse(Age >= 50, "Grupa 1", "Grupa 2")) %>% # grupuję osoby powyżej i poniżej 50 lat
  group_by(podzial_wieku) %>% summarise(srednie_tetno = mean(Heart.Rate)) %>% # obliczam średnie tętno po grupach
  mutate(roznica = mean(df$Heart.Rate) - srednie_tetno) %>%  # obliczam różnicę między średnimi tętnami
  select(podzial_wieku, roznica) # wybieram kolumny z wynikami




########### Zad 8 (1 pkt) ###########
# Pogrupuj ludzi względem wieku przypisując ich do kubełków wieku co 10 (czyli [0,10), [10, 20) etc.).
# Następnie podaj w każdej z tych grup najbardziej popularny zawód dla mężczyzn i kobiet
# Dodatkowo wyświetl średnią, medianę i odchylenie standardowe dla poziomu stresu w tak utworzonych grupach.

# Rozwiązanie: ------------------------------------------------------------
# Tutaj również nie mam pewności, czy dobrze zinterpretowałem polecenie.
# W pierwszym rozwiązaniu całość polecenia jest wykonana na raz.Dzielę ludzi na podgrupy wiekowe,
# a następnie znajduję najpopularniejszy zawó dla meżczyzn i kobiet i od razu
# wyliczam średnią, medianę i odchylenie standardowe dla podgrup wiekowych z podziałęm na mężczyzn i kobiety.
df %>% mutate(grupa_wiekowa = case_when( # dzielę na kategorie wiekowe
  Age < 10  ~ "[0,10)", 
  Age < 20  ~ "[10,20)", 
  Age < 30  ~ "[20,30)", 
  Age < 40  ~ "[30,40)", 
  Age < 50  ~ "[40,50)", 
  Age < 60  ~ "[50,60)", 
  Age < 70  ~ "[60,70)", 
  Age < 80  ~ "[70,80)", 
  Age < 90  ~ "[80,90)", 
  TRUE      ~ "[90,100)")) %>%
  group_by(grupa_wiekowa, Gender) %>% # grupuję po dodanej grupie wiekowej i płci
  summarise(
    najlepszy_zawod = names(which.max(table(Occupation))), # wyliczam i zwracam nazwę najczęstszego zawodu
    srednia = mean(Stress.Level, na.rm = TRUE), # obliczam średni poziom stresu
    mediana = median(Stress.Level, na.rm = TRUE), # obliczam medianę poziomu stresu
    odchylenie_standardowe = sd(Stress.Level, na.rm = TRUE)) # obliczam odchylenie standardowe poziomu stresu

# W drugim rozwiązaniu są dwie ramki danych. Pierwsza część jest taka sama, ale w drugiej
# częśći wyliczona średnia, mediana i odchylenie standardowe są obliczone tylko dla podgrup
# wiekowych, bez podziału w nich na mężczyzn i kobiety.
df %>% mutate(grupa_wiekowa = case_when( # dzielę na kategorie wiekowe
  Age < 10  ~ "[0,10)", 
  Age < 20  ~ "[10,20)", 
  Age < 30  ~ "[20,30)", 
  Age < 40  ~ "[30,40)", 
  Age < 50  ~ "[40,50)", 
  Age < 60  ~ "[50,60)", 
  Age < 70  ~ "[60,70)", 
  Age < 80  ~ "[70,80)", 
  Age < 90  ~ "[80,90)", 
  TRUE      ~ "[90,100)")) %>%
  group_by(grupa_wiekowa, Gender) %>% # grupuję po dodanej grupie wiekowej i płci
  summarise(najlepszy_zawod = names(which.max(table(Occupation)))) # wyliczam i zwracam nazwę najczęstszego zawodu
df %>% mutate(grupa_wiekowa = case_when( # dzielę na kategorie wiekowe
  Age < 10  ~ "[0,10)", 
  Age < 20  ~ "[10,20)", 
  Age < 30  ~ "[20,30)", 
  Age < 40  ~ "[30,40)", 
  Age < 50  ~ "[40,50)", 
  Age < 60  ~ "[50,60)", 
  Age < 70  ~ "[60,70)", 
  Age < 80  ~ "[70,80)", 
  Age < 90  ~ "[80,90)", 
  TRUE      ~ "[90,100)")) %>%
  group_by(grupa_wiekowa) %>% # grupuję po dodanej grupie wiekowej
  summarise(
    srednia = mean(Stress.Level, na.rm = TRUE), # obliczam średni poziom stresu
    mediana = median(Stress.Level, na.rm = TRUE), # obliczam medianę poziomu stresu
    odchylenie_standardowe = sd(Stress.Level, na.rm = TRUE)) # obliczam odchylenie standardowe poziomu stresu
