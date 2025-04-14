library(xml2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(tidyr)
library(zoo)


# xml_to_csv <- function(xml_file, csv_file) {
#   # Wczytanie pliku XML
#   doc <- read_xml(xml_file)
#   
#   # Definicja przestrzeni nazw Excela w XML
#   ns <- c(ss = "urn:schemas-microsoft-com:office:spreadsheet")
#   
#   # Znalezienie wszystkich wierszy (Row) przy użyciu przestrzeni nazw
#   rows <- xml_find_all(doc, ".//ss:Row", ns)
#   
#   # Lista, do której będziemy zapisywać dane każdego wiersza
#   rows_list <- list()
#   
#   # Iteracja przez wszystkie wiersze
#   for(i in seq_along(rows)) {
#     # Dla danego wiersza pobieramy wszystkie komórki (Data)
#     cells <- xml_find_all(rows[[i]], ".//ss:Data", ns)
#     # Pobieramy tekst z komórek; jeśli komórka jest pusta, zapisujemy pusty ciąg
#     row_data <- if(length(cells) > 0) xml_text(cells) else ""
#     rows_list[[i]] <- row_data
#   }
#   
#   # Ustalamy maksymalną długość wiersza, aby wyrównać długości
#   max_length <- max(sapply(rows_list, length))
#   
#   # Uzupełniamy brakujące elementy pustymi ciągami, aby wszystkie wiersze miały tę samą długość
#   rows_list <- lapply(rows_list, function(x) {
#     length(x) <- max_length
#     x[is.na(x)] <- ""
#     x
#   })
#   
#   # Konwersja listy do macierzy
#   mat <- do.call(rbind, rows_list)
#   
#   # Zapis macierzy do pliku CSV, używając przecinków jako separatorów, bez zapisywania numerów wierszy
#   write.csv(mat, file = csv_file, row.names = FALSE, fileEncoding = "UTF-8")
#   cat("Plik CSV został zapisany jako:", csv_file, "\n")
# }
# xml_to_csv("NFZ_o_zdrowiu_-_Depresja_Dane_za_lata_2013-2023.xml", "NFZ_o_zdrowiu_-_Depresja_Dane_za_lata_2013-2023.csv")


przetworz_dane <- function(df) {
  # Wyszukaj, gdzie kończy się spis treści (pierwszy wiersz, w którym wartość się powtarza)
  unikalne <- unique(df[[1]])
  spis_tresci <- unikalne[1:(which(duplicated(df[[1]]))[1] - 1)]
  
  # Utwórz ramkę danych "spis_tresci" z trzema kolumnami
  spis_tresci <- data.frame(
    Nazwa = spis_tresci, 
    Opis = rep(NA, length(spis_tresci)), 
    Dodatkowe = rep(NA, length(spis_tresci)), 
    stringsAsFactors = FALSE
  )
  
  # Znajdź wszystkie wiersze, w których znajduje się "Powrót do spisu treści"
  indeksy_powrotu <- which(df[[1]] == "Powrót do spisu treści")
  
  # Początkowy indeks bloku danych
  start <- length(spis_tresci$Nazwa) + 2
  
  for(i in seq_along(indeksy_powrotu)) {
    # Dla pierwszego bloku korygujemy indeks – bo w pierwszym przypadku obie informacje (nazwa i nagłówek) są o jeden wiersz za nisko
    if(i == 1) {
      current_start <- start - 1
    } else {
      current_start <- start
    }
    
    # Wiersz zawierający nazwę tabeli – pozostaje bez zmian
    nazwa_tabeli <- df[current_start, 1]
    
    # Ustalamy początkowo kandydat na ostatni wiersz danych
    end_candidate <- indeksy_powrotu[i] - 2
    
    # Cofamy się w górę – jeśli w kolumnie 2 w danym wierszu jest pusta wartość,
    # przyjmujemy wiersz wyżej jako faktyczny koniec danych
    while(end_candidate > (current_start + 1) && 
          (is.na(df[end_candidate, 2]) || df[end_candidate, 2] == "")) {
      end_candidate <- end_candidate - 1
    }
    end <- end_candidate
    
    # Pobieramy tabelę danych: od wiersza bezpośrednio po nazwie (current_start + 1) do 'end'
    tabela <- df[(current_start + 1):end, , drop = FALSE]
    
    # --- Usuwanie pustych kolumn ---
    # Sprawdzamy każdą kolumnę – jeśli poza pierwszym wierszem (nagłówkiem)
    # wszystkie komórki są puste (NA lub ""), kolumna zostanie usunięta.
    if(nrow(tabela) >= 2) {
      tabela <- tabela[, !sapply(tabela, function(col) {
        all(is.na(col[-1]) | col[-1] == "")
      }), drop = FALSE]
    } else {
      # Jeśli tabela ma tylko jeden wiersz (tylko nagłówek), usuwamy wszystkie kolumny.
      tabela <- tabela[, FALSE, drop = FALSE]
    }
    # --- Koniec usuwania pustych kolumn ---
    
    # Ustawiamy pierwszy wiersz jako nagłówek
    if(nrow(tabela) >= 1) {
      colnames(tabela) <- as.character(tabela[1, ])
      tabela <- tabela[-1, , drop = FALSE]  # usuwamy wiersz z nagłówkiem
      rownames(tabela) <- NULL            # resetujemy numerację wierszy
    }
    
    # Przypisujemy całą ramkę danych do zmiennej o nazwie odpowiadającej tabeli
    assign(nazwa_tabeli, tabela, envir = .GlobalEnv)
    
    # Wiersz opisowy – trafia do spisu treści (kolumna Opis) pobrany z kolumny 1
    opis <- df[end + 1, 1]
    
    # Dodatkowe informacje: pobieramy z kolumny 1 dla kolejnych wierszy, jeśli występują
    if((end + 2) <= (indeksy_powrotu[i] - 1)) {
      dodatkowe <- df[(end + 2):(indeksy_powrotu[i] - 1), 1]
      # Jeśli jest więcej niż jeden wiersz, łączymy je separatorem ";;"
      if(length(dodatkowe) > 1) {
        dodatkowe <- paste(dodatkowe, collapse = ";;")
      } else {
        dodatkowe <- dodatkowe[1]
      }
    } else {
      dodatkowe <- NA
    }
    
    # Aktualizujemy spis treści – wpisujemy opis i dodatkowe informacje do pierwszego wolnego wiersza
    wiersz_do_opisu <- which(is.na(spis_tresci[["Opis"]]))[1]
    spis_tresci[wiersz_do_opisu, "Opis"] <- opis
    spis_tresci[wiersz_do_opisu, "Dodatkowe"] <- dodatkowe
    
    # Przejście do kolejnego bloku danych
    start <- indeksy_powrotu[i] + 1
  }
  
  # Zapisujemy spis treści jako osobną ramkę danych
  assign("spis_tresci", spis_tresci, envir = .GlobalEnv)
}
df <- read.csv("NFZ_o_zdrowiu_-_Depresja_Dane_za_lata_2013-2023.csv", header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
przetworz_dane(df)
rm(df)



###############################################################################
# OGÓLNE ######################################################################
###############################################################################
rm(`Tabela 2.1: Struktura wieku i płci pacjentów, którym udzielono świadczenia z rozpoznaniem głównym lub współistniejącym depresji—F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 (2013–2023)`)
rm(`Tabela 2.4: Liczba pacjentów, którym udzielono świadczenia z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) wg form opieki (2013–2023)`)
rm(`Tabela 2.7: Świadczenia udzielone z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) w poradniach psychiatrycznych (2013–2023)`)
rm(`Tabela 2.8: Struktura porad lekarskich udzielonych w poradniach psychiatrycznych z rozpoznaniem głównym depresji—F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10 (2013–2023)`)
rm(`Tabela 2.9: Odsetek pacjentów korzystających z rozpoznaniem głównym (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) z poszczególnych form leczenia (2013–2023)`)
rm(`Wykres 1.2: Liczba osób chorych na depresję (F32, F33, F34.1 wg ICD-10) w Polsce (2000-2019) jako odsetek ludności (lewy wykres) i w wartościach bezwzględnych (prawy wykres)`)
rm(`Wykres 2.1: Liczba osób, którym udzielono świadczenia z rozpoznaniem depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, głównym lub współistniejącym) wg grup wiekowych oraz płci (2023)`)
rm(`Wykres 2.2: Odsetek osób, którym udzielono świadczenia z rozpoznaniem depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, głównym lub współistniejącym) wg województwa zamieszkania wśród ludności województwa (2023)`)
rm(`Wykres 2.3: Liczba pacjentów, którym udzielono świadczenia z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) wg najczęstszych rodzajów świadczeń—niepełnoletni (2013–2023)`)
rm(`Wykres 2.4: Liczba pacjentów, którym udzielono świadczenia z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) wg najczęstszych rodzajów świadczeń—dorośli (2013–2023)`)
rm(`Wykres 2.5: Zmiana procentowa liczby świadczeń w POZ, SOR/IP/ZRM i świadczeń w POZ z rozpoznaniem głównym Z76 i wystawioną receptą na leki przeciwdepresyjne z powodu depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) w porównaniu do 2013 r.`)
rm(`Wykres 2.6: Zmiana procentowa liczby świadczeń w poradniach psychiatrycznych oraz osobodni na psychiatrycznych oddziałach szpitalnych, oddziałach dziennych oraz ZLŚ w odniesieniu do 2013 r. udzielonych  z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10)`)
rm(`Wykres 2.7: Liczba i udział pacjentów, którym udzielono świadczenie w POZ, opiece psychiatrycznej i leczeniu uzależnień lub pilotażu CZP z rozpoznaniem głównym depresji—F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10 (2023)`)
rm(`Wykres 2.8: Kombinacje korzystania z form opieki w latach 2013–2023 przez pacjentów, którym w 2023 r. udzielono świadczenia z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10)`)




###############################################################################
# KRAJE EUROPEJSKIE ###########################################################
###############################################################################
rm(`Wykres 1.1a: Odsetek osób chorych na depresję (F32, F33, F34.1 wg ICD-10) w wybranych krajach europejskich w roku 2014 i 2019`)
rm(`Wykres 1.3a: Odsetek osób chorych na depresję (F32, F33, F34.1 wg ICD-10) w wybranych krajach europejskich – kobiety (2019)`)
rm(`Wykres 1.3b: Odsetek osób chorych na depresję (F32, F33, F34.1 wg ICD-10) w wybranych krajach europejskich – mężczyźni (2019)`)




###############################################################################
# RECEPTY I LEKI (+ REFUNDACJE) ###############################################
###############################################################################
rm(`Wykres 2.12: Liczba pacjentów (w tys.) poniżej 18 r.ż., którzy zrealizowali receptę na refundowane leki przeciwdepresyjne (2013–2023)`)
rm(`Wykres 2.14: Odsetek pacjentów zamieszkałych w miastach i gminach miejskich wśród pacjentów, którzy w 2023 r. zrealizowali co najmniej jedną receptę na refundowany lek przeciwdepresyjny`)
rm(`Wykres 2.15: Odsetek osób, które w 2023 roku zrealizowały co najmniej jedną receptę na refundowany lek przeciwdepresyjny w stosunku do ludności województwa`)
rm(`Wykres 2.22: Recepty na refundowane leki przeciwdepresyjne a świadczenia udzielone z powodu zaburzeń psychicznych (F00-F99)`)
rm(`Wykres 2.23a: Struktura wieku pacjentów realizujących recepty na leki przeciwdepresyjne— leki refundowane i nierefundowane (2019-2023)`)
rm(`Wykres 2.23b: Struktura płci pacjentów realizujących recepty na leki przeciwdepresyjne— leki refundowane i nierefundowane (2019-2023)`)
rm(`Tabela 2.13: Struktura wieku i płci pacjentów realizujących recepty na refundowane leki przeciwdepresyjne (2013–2023)`)
rm(`Tabela 2.14: Realizacja recept na refundowane leki przeciwdepresyjne (2013–2023)`)
rm(`Tabela 2.15: Liczba pacjentów (w tys.) realizujących recepty na refundowane leki przeciwdepresyjne wg wyszczególnionych substancji (2013–2023)`)
rm(`Tabela 2.16: Wartość refundacji (w mln zł) leków przeciwdepresyjnych wg substancji czynnych (2013–2023)`)
rm(`Tabela 2.17: Wartość dopłat pacjentów (w mln) do refundowanych leków przeciwdepresyjnych wg substancji czynnych (2013–2023)`)
rm(`Tabela 2.18: Miejsce wystawiania recept na refundowane leki przeciwdepresyjne (2023)`)
rm(`Tabela 2.21a: Realizacja recept na leki przeciwdepresyjne (refundowane i nierefundowane przez NFZ) wg dziesięciu najczęściej występujących substancji czynnych w danym roku (2019-2023)`)
rm(`Tabela 2.21: Informacje o realizacji recept na leki przeciwdepresyjne (refundowane i nierefundowane przez NFZ) (2019-2023)`)
rm(`Tabela 2.20: Świadczenia, w ramach których wystawiono receptę na refundowane leki przeciwdepresyjne i dla których sprawozdano rozpoznanie główne z grupy: Zaburzenia psychiczne i zaburzenia zachowania (F00-F99) (2023)`)
rm(`Tabela 2.19: Świadczenia, w ramach których wystawiono receptę na refundowane leki przeciwdepresyjne (2023)`)
rm(`Tabela 2.24: Wskaźnik proporcji pokrycia dni (PDC, ang. proportion of days covered) lekami refundowanymi z sertralinum wg grup wiekowych`)
rm(`Wykres 2.11: Liczba pacjentów (w tys.) realizujących recepty na refundowane leki przeciwdepresyjne (2013–2023)`)
rm(`Wykres 2.13: Struktura wieku pacjentów realizujących recepty na refundowane leki przeciwdepresyjne (2013–2023)`)
rm(`Wykres 2.16: Wartość refundacji oraz dopłat pacjentów dla refundowanych leków przeciwdepresyjnych (2013–2023)`)
rm(`Wykres 2.17: Wartość średniej refundacji oraz średnich dopłat pacjentów do refundowanych leków przeciwdepresyjnych w przeliczeniu na pacjenta (2013–2023)`)
rm(`Wykres 2.18: Liczba pacjentów (w tys.) realizująca recepty na refundowane leki przeciwdepresyjne według wyszczególnionych substancji czynnych (2013–2023)`)
rm(`Wykres 2.19: Liczba pacjentów (w tys.) poniżej 18 r.ż., którzy zrealizowali recepty na refundowane leki przeciwdepresyjne według wyszczególnionych substancji czynnych (2013–2023)`)
rm(`Wykres 2.20: Wartość refundacji (w mln zł) leków przeciwdepresyjnych w podziale na substancje czynne (2013–2023)`)
rm(`Wykres 2.21: Liczba DDD (w mln) dla refundowanych leków przeciwdepresyjnych w podziale na substancje czynne (2013–2023)`)
rm(`Wykres 2.24a: Liczba opakowań na osobę ze względu na grupy wiekowe dla leków przeciwdepresyjnych — leki refundowane i nierefundowane (2019-2023)`)
rm(`Wykres 2.24b: Liczba opakowań na osobę ze względu na płeć dla leków przeciwdepresyjnych — leki refundowane i nirefundowane (2019-2023)`)
rm(`Wykres 2.25: Struktura liczby opakowań wg poziomów odpłatności pacjenta dla leków przeciwdepresyjnych—leki refundowane i nierefundowane (2019-2023)`)
rm(`Wykres 2.26: Rozkład wieku pacjentów wybranych do analizy ciągłości farmakoterapii refundowanymi lekami przeciwdepresyjnym na przykładzie sertralinum – pacjenci rozpoczynający terapię substancją sertralinum w 2022 r.`)
rm(`Wykres 2.29: Wskaźnik proporcji pokrycia dni (PDC, ang. proportion of days covered) lekami refundowanymi z sertralinum wg grup wiekowych`)




###############################################################################
# LECZENIE ####################################################################
###############################################################################
rm(`Wykres 2.27: Długość trwania farmakoterapii refundowanymi lekami przeciwdepresyjnymi zawierającymi substancję czynną sertralinum wg grup wiekowych`)
rm(`Wykres 2.28: Odsetki pacjentów, których farmakoterapia lekami refundowanymi z substancją sertralinum trwała co najmniej 180 dni według województw`)
rm(`Tabela 2.22: Mediana długości leczenia lekiem refundowanym z substancją czynną sertralinum (czas pomiędzy datą realizacji pierwszej recepty a datą zakończenia terapii) oraz odsetek osób, dla których długość leczenia wynosiła co najmniej 180 dni wg grup wiekowych`)
rm(`Tabela 2.23: Długość trwania farmakoterapii refundowanymi lekami przeciwdepresyjnymi zawierającymi substancję czynną sertralinum wg grup wiekowych`)




###############################################################################
# ORZECZENIA I ZWOLNIENIA #####################################################
###############################################################################
rm(`Wykres 3.1: Liczba zwolnień (w tys.) z tytułu choroby własnej z powodu dużej depresji—F32, F33 wg ICD-10 (2013–2023)`)
rm(`Wykres 3.6: Liczba ponownych orzeczeń o niezdolności do pracy z powodu dużej depresji (F32, F33 wg ICD-10) wystawionych dla celów rentowych wg płci (2013–2023)`)
rm(`Wykres 3.8: Liczba orzeczeń z rozpoznaniem dużej depresji (F32, F33 wg ICD-10) wg kategorii stopnia niezdolności do pracy—mężczyźni (2013–2023)`)
rm(`Wykres 3.5: Odsetek orzeczeń z rozpoznaniem dużej depresji (F32, F33 wg ICD-10) wg kategorii stopnia niezdolności do pracy—mężczyźni (2013–2023)`)
rm(`Wykres 3.7: Liczba orzeczeń z rozpoznaniem dużej depresji (F32, F33 wg ICD-10) wg kategorii stopnia niezdolności do pracy—kobiety (2013–2023)`)
rm(`Wykres 3.4: Odsetek orzeczeń z rozpoznaniem dużej depresji (F32, F33 wg ICD-10) wg kategorii stopnia niezdolności do pracy—kobiety (2013–2023)`)
rm(`Wykres 3.3: Średnia długość zwolnienia (w dniach) z tytułu choroby własnej z powodu depresji—F32, F33 wg ICD-10 (2013–2023)`)
rm(`Wykres 3.2: Liczba dni (w mln) zwolnień lekarskich z tytułu choroby własnej z powodu dużej depresji—F32, F33 wg ICD-10 (2013–2023)`)
rm(`Tabela 3.1: Liczba wystawionych orzeczeń z powodu ciężkiej depresji (F32, F33 wg ICD-10) w celach rentowych wg stopnia niezdolności do pracy (2013–2023)`)




###############################################################################
# WARTOŚĆI REFUNDACJI LECZENIA ################################################
###############################################################################
rm(`Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`)
rm(`Tabela 2.11: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) w poradniach psychiatrycznych w podziale na rodzaj porady (2013–2023)`)
rm(`Tabela 2.12: Wartość refundacji porad lekarskich udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) w poradniach psychiatrycznych w podziale na rodzaj porady (2013–2023)`)
rm(`Wykres 2.9: Procentowa zmiana łącznej wartości refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) w stosunku do 2013 r. wg form opieki`)
rm(`Wykres 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) w przeliczeniu na pacjenta (2013–2023)`)




###############################################################################
###############################################################################
###############################################################################
# ŚWIADCZENIA #################################################################
###############################################################################
rm(`Tabela 2.2: Liczba pacjentów, którym udzielono świadczenia z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) wg rodzajów świadczeń`)
rm(`Tabela 2.3: Pacjenci, którym udzielono świadczenia w podstawowej opiece zdrowotnej w związku z depresją—F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10 (2013–2023)`)
rm(`Tabela 2.5: Świadczenia udzielone w podstawowej opiece zdrowotnej w związku z depresją— F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10 (2013–2023)`)
rm(`Tabela 2.6: Świadczenia udzielone  z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) wg form opieki (2013–2023)`)




###############################################################################
###############################################################################
###############################################################################
# KRAJE EUROPEJSKIE ###########################################################
###############################################################################
rm(`Wykres 1.4: Udział DALY (utracone lata życia z powodu choroby skorygowane niesprawnością) z powodu depresji (F32, F33, F34.1 wg ICD-10) wśród DALY z powodu wszystkich chorób w wybranych krajach europejskich (2019)`)
rm(`Wykres 1.1b: Standaryzowany wiekiem odsetek osób chorych na depresję (F32, F33, F34.1 wg ICD-10) w wybranych krajach europejskich w roku 2014 i 2019`)







###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

wykres3_0 <- ggplot(`Wykres 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10) w przeliczeniu na pacjenta (2013–2023)`, 
       aes(x = as.factor(Rok), 
           y = as.numeric(`Wartość refundacji świadczeń w przeliczeniu na 1 pacjenta (zł)`))) + 
  geom_col(width = 0.8, fill = "darkorange") + labs(title = "Wartość refundacji świadczeń na 1 pacjenta w latach 2013–2023",
                               x = "Rok",
                               y = "Wartość refundacji (zł)") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.04)), breaks = seq(0, 1700, by = 200)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")+
  theme(
    plot.margin = margin(10, 20, 10, 20),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title = element_text(size = 25),  # Osie
    axis.text = element_text(size = 15),   # Etykiety osi
    legend.text = element_text(size = 18), # Opisy legendy
    plot.title = element_text(size = 30), # Tytuł wykresu
    legend.title = element_text(size = 20)) + # Tytuł legendy
  theme(
    plot.background = element_rect(fill = "#111f51", color = NA),  # Tło całego wykresu na czarne
    legend.background = element_rect(fill = "#111f51", color = NA), # Tło wokół legendy
    panel.background = element_rect(fill = "gray80", color = NA),
    axis.line = element_line(color = "white"),   # Białe osie
    axis.ticks = element_line(color = "white"),  # Białe znaczniki osi
    axis.text = element_text(color = "white"),   # Kolor etykiet osi
    axis.title = element_text(color = "white"),  # Kolor tytułów osi
    legend.text = element_text(color = "white"), # Kolor tekstu legendy
    legend.title = element_text(color = "white"), # Kolor tytułu legendy
    plot.title = element_text(color = "white"))
  
png("wykres3_0.png", width = 5000, height = 3000, res = 300, bg = "transparent")
print(wykres3_0)
dev.off() 



dane3_1 <- data.frame(
  Rok = as.numeric(`Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`$Rok),
  `Łącznie` = as.numeric(`Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`$`Wartość refundacji świadczeń: łącznie (mln zł)`),
  `Szpitalne oddziały psychiatryczne` = as.numeric(`Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`$`Wartość refundacji świadczeń: oddziały psychiatryczne (szpitalne) (mln zł)`),
  `Poradnie psychologiczne, psychiatryczne i leczenia uzależnień` = as.numeric(`Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`$`Wartość refundacji świadczeń: poradnie psychologiczne, psychiatryczne i leczenia uzależnień (mln zł)`),
  `Oddziały dzienne` = as.numeric(`Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`$`Wartość refundacji świadczeń: oddziały dzienne (mln zł)`))

wykres3_1 <- dane3_1 %>% 
  pivot_longer(cols = -Rok, names_to = "Kategoria", values_to = "Wartość") %>% 
  ggplot(aes(x = Rok, y = Wartość, fill = Kategoria, color = Kategoria)) +
  geom_area(colour="white")  +
  labs(title = "Refundacja świadczeń (w mln zł) w latach 2013-2023",
       x = "Rok", y = "Wartość (w mln zł)") + 
  scale_fill_manual(values = c(`Łącznie` = "orange", 
                               `Szpitalne.oddziały.psychiatryczne` = "purple", 
                               `Poradnie.psychologiczne..psychiatryczne.i.leczenia.uzależnień` = "red", 
                               `Oddziały.dzienne` = "green"),
                    labels = c(`Łącznie` = "Łącznie", 
                               `Szpitalne.oddziały.psychiatryczne` = "Szpitalne oddziały psychiatryczne", 
                               `Poradnie.psychologiczne..psychiatryczne.i.leczenia.uzależnień` = "Poradnie psychol., psychiatr. i leczenia uzależnień", 
                               `Oddziały.dzienne` = "Oddziały dzienne")) +
  scale_color_manual(values = c(`Łącznie` = "orange", 
                                `Szpitalne.oddziały.psychiatryczne` = "purple", 
                                `Poradnie.psychologiczne..psychiatryczne.i.leczenia.uzależnień` = "red", 
                                `Oddziały.dzienne` = "green"),
                     labels = c(`Łącznie` = "Łącznie", 
                                `Szpitalne.oddziały.psychiatryczne` = "Szpitalne oddziały psychiatryczne", 
                                `Poradnie.psychologiczne..psychiatryczne.i.leczenia.uzależnień` = "Poradnie psychol., psychiat. i leczenia uzależnień", 
                                `Oddziały.dzienne` = "Oddziały dzienne")) + 
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(
    panel.grid.major.x = element_line(color = "white", size = 1),
    panel.grid.major.y = element_line(color = "white", size = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)), breaks = seq(0, 900, by = 100)) + 
  scale_x_continuous(breaks = seq(2013, 2023, by = 1),
                     labels = as.character(seq(2013, 2023, by = 1)),
                     expand = c(0, 0)) +
  guides(
    fill  = guide_legend(nrow = 3, byrow = TRUE),
    color = guide_legend(nrow = 3, byrow = TRUE)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")+
  theme(
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title = element_text(size = 28),  # Osie
    axis.text = element_text(size = 25),   # Etykiety osi
    legend.text = element_text(size = 25), # Opisy legendy
    plot.title = element_text(size = 30), # Tytuł wykresu
    legend.title = element_text(size = 26)) + # Tytuł legendy
  theme(
    plot.background = element_rect(fill = "#111f51", color = NA),  # Tło całego wykresu na czarne
    legend.background = element_rect(fill = "#111f51", color = NA), # Tło wokół legendy
    panel.background = element_rect(fill = "gray8", color = NA),
    axis.line = element_line(color = "white"),   # Białe osie
    axis.ticks = element_line(color = "white"),  # Białe znaczniki osi
    axis.text = element_text(color = "white"),   # Kolor etykiet osi
    axis.title = element_text(color = "white"),  # Kolor tytułów osi
    legend.text = element_text(color = "white"), # Kolor tekstu legendy
    legend.title = element_text(color = "white"), # Kolor tytułu legendy
    plot.title = element_text(color = "white"),
    panel.border = element_rect(color = "white", fill = NA, size = 1))
wykres3_1

ggsave("wykres3_1.jpg", wykres3_1,
       width = 5000 / 300, height = 3000 / 300, dpi = 300, bg = "white", device = "jpeg")


PKB <- read_xlsx("pkb_gus.xlsx")
dane3_2 <- data.frame(
  Rok = as.numeric(`Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`$Rok),
  `Łącznie2` = as.numeric(`Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`$`Wartość refundacji świadczeń: łącznie (mln zł)`),
  `Szpitalne oddziały psychiatryczne2` = as.numeric(`Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`$`Wartość refundacji świadczeń: oddziały psychiatryczne (szpitalne) (mln zł)`),
  `Poradnie psychologiczne, psychiatryczne i leczenia uzależnień2` = as.numeric(`Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`$`Wartość refundacji świadczeń: poradnie psychologiczne, psychiatryczne i leczenia uzależnień (mln zł)`),
  `Oddziały dzienne2` = as.numeric(`Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`$`Wartość refundacji świadczeń: oddziały dzienne (mln zł)`)
) %>% left_join(PKB, by = "Rok") %>%
  mutate(
    `Łącznie` = `Łącznie2` / `PKB (w mln zł)` * 100,
   `Szpitalne oddziały psychiatryczne` = `Szpitalne.oddziały.psychiatryczne2` / `PKB (w mln zł)` * 100,
    `Poradnie psychologiczne, psychiatryczne i leczenia uzależnień` = `Poradnie.psychologiczne..psychiatryczne.i.leczenia.uzależnień2` / `PKB (w mln zł)` * 100,
    `Oddziały dzienne` = `Oddziały.dzienne2` / `PKB (w mln zł)` * 100) %>% 
  select(c('Rok', 'Łącznie', 'Szpitalne oddziały psychiatryczne', 'Poradnie psychologiczne, psychiatryczne i leczenia uzależnień', 'Oddziały dzienne'))

wykres3_2 <- dane3_2 %>% 
  pivot_longer(cols = -Rok, names_to = "Kategoria", values_to = "Wartość") %>% 
  ggplot(aes(x = Rok, y = Wartość, fill = Kategoria, color = Kategoria)) +
  geom_area(alpha = 0.5, colour="black")  +
  labs(title = "Wskaźnik PKB (w %) dla refundacji świadczeń w latach 2013-2023",
       x = "Rok", y = "Wskaźnik PKB (w %)") + 
  scale_fill_manual(values = c(`Łącznie` = "orange", 
                               `Szpitalne oddziały psychiatryczne` = "purple", 
                               `Poradnie psychologiczne, psychiatryczne i leczenia uzależnień` = "red", 
                               `Oddziały dzienne` = "green")) +
  scale_color_manual(values = c(`Łącznie` = "orange", 
                                `Szpitalne oddziały psychiatryczne` = "purple", 
                                `Poradnie psychologiczne, psychiatryczne i leczenia uzależnień` = "red", 
                                `Oddziały dzienne` = "green")) + theme_bw() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 0.03, by = 0.002)) + 
  scale_x_continuous(breaks = seq(2013, 2023, by = 1),
                     labels = as.character(seq(2013, 2023, by = 1)),
                     expand = c(0, 0)) +
  guides(
    fill  = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")+
  theme(
    plot.margin = margin(10, 20, 10, 20),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title = element_text(size = 25),  # Osie
    axis.text = element_text(size = 20),   # Etykiety osi
    legend.text = element_text(size = 20), # Opisy legendy
    plot.title = element_text(size = 30), # Tytuł wykresu
    legend.title = element_text(size = 22)) + # Tytuł legendy
  theme(
    plot.background = element_rect(fill = "#111f51", color = NA),  # Tło całego wykresu na czarne
    legend.background = element_rect(fill = "#111f51", color = NA), # Tło wokół legendy
    panel.background = element_rect(fill = "gray8", color = NA),
    axis.line = element_line(color = "white"),   # Białe osie
    axis.ticks = element_line(color = "white"),  # Białe znaczniki osi
    axis.text = element_text(color = "white"),   # Kolor etykiet osi
    axis.title = element_text(color = "white"),  # Kolor tytułów osi
    legend.text = element_text(color = "white"), # Kolor tekstu legendy
    legend.title = element_text(color = "white"), # Kolor tytułu legendy
    plot.title = element_text(color = "white"),
    panel.border = element_rect(color = "white", fill = NA, size = 1))
wykres3_2

png("wykres3_2.png", width = 5000, height = 3000, res = 300, bg = "transparent")
print(wykres3_2)
dev.off() 

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
df <- read.csv("liczba_osob_wiek_plec.csv")
df4 <- read.csv("dzieci_specjalisci.csv")
df4$liczba.specjalistow.zgłoszonych.do.umów <- na.approx(df4$liczba.specjalistow.zgłoszonych.do.umów)
df$Liczba.pacjentów <- (df$Liczba.pacjentów / 1000)
df_wide <- df %>%
  pivot_wider(names_from = Płeć, values_from = Liczba.pacjentów)

wykres2_6 <- ggplot(df_wide) +
  geom_segment( aes(x=Grupa.wiekowa, xend=Grupa.wiekowa, y=Mężczyźni, yend=Kobiety), color="gray80",size=2.5) +
  geom_point( aes(x=Grupa.wiekowa, y=Mężczyźni, color="Mężczyźni"), size=7 ) +
  geom_point( aes(x=Grupa.wiekowa, y=Kobiety, color="Kobiety"), size=7 ) +
  scale_color_manual(name = "Płeć", values=c("Mężczyźni"="green","Kobiety"="red"))+
  coord_flip()+
  xlab("Grupa wiekowa") +
  ylab("Liczba pacjentów ( w tys.)") +
  ggtitle("Liczba osób, którym udzielono świadczenia z rozpoznaniem depresji (2023)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(
    panel.grid.major.x = element_line(color = "white", size = 1),
    panel.grid.major.y = element_line(color = "white", size = 0.1)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")+
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0, 120, by = 10),
                     limits = c(0, 120))+
  theme(
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title = element_text(size = 28),  # Osie
    axis.text = element_text(size = 25),   # Etykiety osi
    legend.text = element_text(size = 25), # Opisy legendy
    plot.title = element_text(size = 30), # Tytuł wykresu
    legend.title = element_text(size = 26)) + # Tytuł legendy
  theme(
    plot.background = element_rect(fill = "#111f51", color = NA),  # Tło całego wykresu na czarne
    legend.background = element_rect(fill = "#111f51", color = NA), # Tło wokół legendy
    panel.background = element_rect(fill = "gray8", color = NA),
    axis.line = element_line(color = "white"),   # Białe osie
    axis.ticks = element_line(color = "white"),  # Białe znaczniki osi
    axis.text = element_text(color = "white"),   # Kolor etykiet osi
    axis.title = element_text(color = "white"),  # Kolor tytułów osi
    legend.text = element_text(color = "white"), # Kolor tekstu legendy
    legend.title = element_text(color = "white"), # Kolor tytułu legendy
    plot.title = element_text(color = "white"),
    panel.border = element_rect(color = "white", fill = NA, size = 1))
wykres2_6

ggsave("wykres2_6.jpg", wykres2_6,
       width = 5000 / 300, height = 3000 / 300, dpi = 300, bg = "white", device = "jpeg")


wykres2_7 <- df4 %>% 
  mutate(liczba.dzieci.na.jednego.specjaliste = ifelse(is.na(liczba.dzieci.na.jednego.specjaliste),liczba.dzieci/liczba.specjalistow.zgłoszonych.do.umów,liczba.dzieci.na.jednego.specjaliste)) %>% 
  ggplot(aes(x=Rok,y=liczba.dzieci.na.jednego.specjaliste)) +
  geom_point(color="red",size=4.5)+
  geom_line(color="red",size=3.5) +
  labs(title="Liczba dzieci przypadających na jednego specjaliste zgłoszonego do umów NFZ", y="Liczba dzieci")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")+
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")+
  guides(
    fill  = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     breaks = seq(0, 70, by = 10),
                     labels = seq(0, 70, by = 10),
                     limits = c(0, 70)) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1),
                     labels = seq(2013, 2023, by = 1))+
  theme(
    panel.grid.major.x = element_line(color = "white", size = 0.5),
    panel.grid.major.y = element_line(color = "white", size = 0.5)) +
  theme(
    plot.margin = margin(20, 70, 20, 40),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title = element_text(size = 28),  # Osie
    axis.text = element_text(size = 25),   # Etykiety osi
    legend.text = element_text(size = 25), # Opisy legendy
    plot.title = element_text(size = 30), # Tytuł wykresu
    legend.title = element_text(size = 26)) + # Tytuł legendy
  theme(
    plot.background = element_rect(fill = "#111f51", color = NA),  # Tło całego wykresu na czarne
    legend.background = element_rect(fill = "#111f51", color = NA), # Tło wokół legendy
    panel.background = element_rect(fill = "gray8", color = NA),
    axis.line = element_line(color = "white"),   # Białe osie
    axis.ticks = element_line(color = "white"),  # Białe znaczniki osi
    axis.text = element_text(color = "white"),   # Kolor etykiet osi
    axis.title = element_text(color = "white"),  # Kolor tytułów osi
    legend.text = element_text(color = "white"), # Kolor tekstu legendy
    legend.title = element_text(color = "white"), # Kolor tytułu legendy
    plot.title = element_text(color = "white"),
    panel.border = element_rect(color = "white", fill = NA, size = 1))
wykres2_7

ggsave("wykres2_7.jpg", wykres2_7,
       width = 5100 / 300, height = 3000 / 300, dpi = 300, bg = "white", device = "jpeg")


###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
dane1_1 <- data.frame(
  Rok = as.numeric(`Wykres 3.8: Liczba orzeczeń z rozpoznaniem dużej depresji (F32, F33 wg ICD-10) wg kategorii stopnia niezdolności do pracy—mężczyźni (2013–2023)`$Rok),
  `Stopień niezdolności` = as.factor(`Wykres 3.8: Liczba orzeczeń z rozpoznaniem dużej depresji (F32, F33 wg ICD-10) wg kategorii stopnia niezdolności do pracy—mężczyźni (2013–2023)`$`Stopień niezdolności`),
  `Liczba orzeczeń` = as.numeric(`Wykres 3.8: Liczba orzeczeń z rozpoznaniem dużej depresji (F32, F33 wg ICD-10) wg kategorii stopnia niezdolności do pracy—mężczyźni (2013–2023)`$`Liczba orzeczeń`))%>% 
  mutate(Płeć = 'Mężczyźni') %>% filter(`Stopień.niezdolności`!= "Niezdolność do samodzielnej egzystencji")
dane1_2 <- data.frame(
  Rok = as.numeric(`Wykres 3.7: Liczba orzeczeń z rozpoznaniem dużej depresji (F32, F33 wg ICD-10) wg kategorii stopnia niezdolności do pracy—kobiety (2013–2023)`$Rok),
  `Stopień niezdolności` = as.factor(`Wykres 3.7: Liczba orzeczeń z rozpoznaniem dużej depresji (F32, F33 wg ICD-10) wg kategorii stopnia niezdolności do pracy—kobiety (2013–2023)`$`Stopień niezdolności`),
  `Liczba orzeczeń` = as.numeric(`Wykres 3.7: Liczba orzeczeń z rozpoznaniem dużej depresji (F32, F33 wg ICD-10) wg kategorii stopnia niezdolności do pracy—kobiety (2013–2023)`$`Liczba orzeczeń`))%>% 
  mutate(Płeć = 'Kobiety') %>% filter(`Stopień.niezdolności`!= "Niezdolność do samodzielnej egzystencji")

wykres_line <- bind_rows(dane1_1, dane1_2) %>% 
  ggplot(aes(x = Rok, 
             y = `Liczba.orzeczeń`, 
             group = interaction(Płeć, `Stopień.niezdolności`), 
             color = interaction(Płeć, `Stopień.niezdolności`))) +
  geom_point(size = 3) +
  geom_line(size = 3) +
  labs(title = "Liczba orzeczeń o depresji wg stopnia niezdolności do pracy (2013-2023)",
       x = "Rok",
       y = "Liczba orzeczeń",
       color = "Płeć\nStopień niezdolności") +
  scale_color_manual(values = c("orange", "red", "green", "turquoise"))+
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")+
  guides(
    fill  = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0, 4800, by = 600),
                     limits = c(0, 4800)) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1),
                     labels = seq(2013, 2023, by = 1))+
  theme(
  panel.grid.major.x = element_line(color = "white", size = 0.1),
  panel.grid.major.y = element_line(color = "white", size = 1)) +
  scale_color_manual(
    values = c("orange", "red", "green", "turquoise"),
    labels = c(
      "Kobieta\nCałkowita niezdolność do pracy",
      "Mężczyzna\nCałkowita niezdolność do pracy",
      "Kobieta\nCzęściowa niezdolność do pracy",
      "Mężczyzna\nCzęściowa niezdolność do pracy"
    )
  )+
  theme(
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title = element_text(size = 28),  # Osie
    axis.text = element_text(size = 25),   # Etykiety osi
    legend.text = element_text(size = 25), # Opisy legendy
    plot.title = element_text(size = 30), # Tytuł wykresu
    legend.title = element_text(size = 26)) + # Tytuł legendy
  theme(
    plot.background = element_rect(fill = "#111f51", color = NA),  # Tło całego wykresu na czarne
    legend.background = element_rect(fill = "#111f51", color = NA), # Tło wokół legendy
    panel.background = element_rect(fill = "gray8", color = NA),
    axis.line = element_line(color = "white"),   # Białe osie
    axis.ticks = element_line(color = "white"),  # Białe znaczniki osi
    axis.text = element_text(color = "white"),   # Kolor etykiet osi
    axis.title = element_text(color = "white"),  # Kolor tytułów osi
    legend.text = element_text(color = "white"), # Kolor tekstu legendy
    legend.title = element_text(color = "white"), # Kolor tytułu legendy
    plot.title = element_text(color = "white"),
    panel.border = element_rect(color = "white", fill = NA, size = 1))
wykres_line

ggsave("wykres_liniowy.jpg", wykres_line,
       width = 5000 / 300, height = 3000 / 300, dpi = 300, bg = "white", device = "jpeg")



data1 <- `Tabela 2.22: Mediana długości leczenia lekiem refundowanym z substancją czynną sertralinum (czas pomiędzy datą realizacji pierwszej recepty a datą zakończenia terapii) oraz odsetek osób, dla których długość leczenia wynosiła co najmniej 180 dni wg grup wiekowych` %>% 
  select(`Grupa wiekowa`,`Liczba osób (tys.)`,`Mediana długości leczenia`) %>% 
  mutate(across(c("Liczba osób (tys.)","Mediana długości leczenia"), as.numeric)) %>% 
  rename(grupa_wiekowa = `Grupa wiekowa`,
         liczba_osob = `Liczba osób (tys.)`,
         mediana = `Mediana długości leczenia`)
data1 <- data1 %>%
  bind_rows(data1 %>% slice(c(7,8)) %>% summarise(across(c(liczba_osob,mediana), mean))) %>% 
  slice(-c(7,8,9))
data1[7,1] <- "75 +"
s <- max(data1$mediana)/max(data1$liczba_osob)

wykres1_2 <- data1 %>% 
  ggplot(aes(x = grupa_wiekowa)) +
  geom_col(aes(y = liczba_osob, fill = mediana), size = 1, width = 0.5) +
  scale_x_discrete(expand = c(0.075,0.075)) +
  scale_fill_gradient(name = "Mediana długości kuracji (w dniach)", low = "yellow", high = "red") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(
    panel.grid.major.y = element_line(color = "white", size = 1)) +
  labs(title = "Dane dla substancji czynnej SERTRALINY", x = "Grupa wiekowa") +
  theme(legend.position = "bottom", axis.text = element_text(size = 10), plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0, 60, by = 10),
                     limits = c(0, 60),
                     name = "Liczba pacjentów, którym została przypisana (w tys.)") +
  theme(
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title = element_text(size = 28),  # Osie
    axis.text = element_text(size = 25),   # Etykiety osi
    legend.text = element_text(size = 25), # Opisy legendy
    plot.title = element_text(size = 30), # Tytuł wykresu
    legend.title = element_text(size = 26)) + # Tytuł legendy
  theme(
    plot.background = element_rect(fill = "#111f51", color = NA),  # Tło całego wykresu na czarne
    legend.background = element_rect(fill = "#111f51", color = NA), # Tło wokół legendy
    panel.background = element_rect(fill = "gray8", color = NA),
    axis.line = element_line(color = "white"),   # Białe osie
    axis.ticks = element_line(color = "white"),  # Białe znaczniki osi
    axis.text = element_text(color = "white"),   # Kolor etykiet osi
    axis.title = element_text(color = "white"),  # Kolor tytułów osi
    legend.text = element_text(color = "white"), # Kolor tekstu legendy
    legend.title = element_text(color = "white"), # Kolor tytułu legendy
    plot.title = element_text(color = "white"),
    panel.border = element_rect(color = "white", fill = NA, size = 1)) +
  guides(fill = guide_colorbar(
    barwidth = unit(7, "cm"),
    barheight = unit(1, "cm"),
    ticks.colour = "white",
    ticks.linewidth = 1.1,
  )) + theme(legend.title = element_text(vjust = 0.8))
wykres1_2

ggsave("wykres1_2.jpg", wykres1_2,
       width = 5000 / 300, height = 3300 / 300, dpi = 300, bg = "white", device = "jpeg")
