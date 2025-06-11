# Spotify Analyzer - raport
### proj. 2, gr. 14 - Marek Makowski, Kacper Lewicki, Mikołaj Paśnikowski
Do naszego projektu użyliśmy danych z API Spotify'a o najpopularniejszych utworach z lat 2010-2023:
https://www.kaggle.com/datasets/irynatokarchuk/top-streamed-spotify-songs-by-year-2010-2023?resource=download

Nasza aplikacja składa się z dwóch głównych części:
## 1. Wykres rozrzutu utworów
Dla pięciu zmiennych z API Spotify'a:

 - danceability (taneczność)
 - energy (energia)
 - valence (pozytywność/wesołość)
 - tempo (BPM)
 - popularity (popularność)

aplikacja generuje wykres (scatter plot) dwóch wybranych zmiennych. Do niego następnie można dołączyć linię regresji, mediany lub średniej. Do tych dwóch ostatnich opcji możemy dopasować poziom wygładzenia (ilość podprzedziałów, na których mierzona jest statystyka).
Utwory możemy sortować jeszcze według gatunku oraz ograniczając ich zakres popularności (zmienna z API, jak wyżej).

*Screenshot:*
(https://drive.google.com/file/d/1hYIYCueAqm9QD0swqxuCcm1pu1fRdr3O/view?usp=sharing)


## 2. Udział N% najpopularniejszych artystów
 W tej części znajduje się jeden wykres, który dla podanego procenta najpopularniejszych artystów przedstawia ich łączny procentowy udział we wszystkich odtworzeniach (streamach) w danym roku dla lat 2010-2023.
 
 *Tutaj warto zaznaczyć, że dane, którymi się posługujemy już są danymi ograniczonymi do najpopularniejszych utworów/artystów

 *Screenshot:*
(https://drive.google.com/file/d/1F5AqAH4oQGS3qyVIXGx4GVxemK--WOAU/view?usp=sharing)																							
