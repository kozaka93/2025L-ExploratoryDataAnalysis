# Analiza dotycząca książek

## 📦 Opis projektu

Aplikacja Shiny służy do interaktywnej analizy danych o książkach. Umożliwia badanie rozkładu ocen i liczby stron w zależności od gatunku, analizę związku ceny z oceną, a także eksplorację popularności gatunków i języków.

## 🛠️ Technologie i biblioteki

Projekt korzysta z następujących pakietów R:

-   **shiny**: framework do budowy interaktywnych aplikacji webowych
-   **shinydashboard**: szablon dashboardu
-   **dplyr**: manipulacja danymi
-   **tidyr**: przekształcanie danych
-   **purrr**: operacje na listach i wektorach
-   **stringr**: operacje na ciągach znaków
-   **lubridate**: przetwarzanie dat
-   **ggplot2**: tworzenie statycznych wykresów
-   **plotly**: tworzenie interaktywnych wykresów
-   **RColorBrewer**: palety kolorów
-   **DT**: renderowanie tabel DataTable

## ▶️ Uruchamianie aplikacji

1.  Upewnij się, że masz zainstalowane wszystkie wymagane biblioteki. W R uruchom:

    ``` r
    install.packages(c(
      "shiny", "shinydashboard", "dplyr", "tidyr", "purrr",
      "stringr", "lubridate", "ggplot2", "plotly",
      "RColorBrewer", "DT"
    ))
    ```

2.  Pobierz repozytorium lub skopiuj pliki projektu do lokalnego katalogu.

3.  Upewnij się, że w katalogu znajdują się pliki CSV:

    -   `sprzatniete.csv` (dane ocen i gatunków)
    -   `Books_Data_Clean.csv` (dane o sprzedażach)
    -   `books.csv`
    -   `bestsellers with categories.csv`

4.  W RStudio przejdź do folderu z aplikacją i uruchom:

    ``` r
    shiny::runApp()
    ```

5.  Aplikacja otworzy się w przeglądarce pod adresem lokalnym (np. `http://127.0.0.1:xxxx`).

## 📁 Struktura plików

```         
/ (root)
│
├── app.R                      # główny skrypt aplikacji Shiny
├── sprzatniete.csv            # dane o książkach (oceny, gatunki)
├── Books_Data_Clean.csv       # dane o sprzedażach książek
├── books.csv                  # dodatkowy zestaw danych
├── bestsellers with categories.csv # dane bestsellerów z kategoriami
└── README.md                  # ten plik
```

## 📝 Opis zakładek aplikacji

1.  **Gatunki książek**

    -   Rozkład ocen i liczby stron wg wybranych gatunków (wykresy violin i boxplot).
    -   Top 5 najwyżej ocenianych książek w wybranych gatunkach.
    -   Krótki opis obserwacji.

2.  **Data publikacji**

    -   Filtrowanie po okresach publikacji i kategorii (fiction/nonfiction).
    -   Zależność ceny od oceny (punktowy + trend).
    -   Top książki z danego okresu.
    -   Opis wyników.

3.  **Popularność i języki**

    -   Rozkład ocen wg kraju (języka) i gatunku.
    -   Ilość wystąpień najpopularniejszych gatunków w wybranym języku.
    -   Ranking najpopularniejszych gatunków ogółem (histogram).
    -   Opisy wniosków.

## ✍️ Autor

Maria Harbaty, Kamil Twarowski, Michał Tałałaj\
Data: czerwiec 2025
