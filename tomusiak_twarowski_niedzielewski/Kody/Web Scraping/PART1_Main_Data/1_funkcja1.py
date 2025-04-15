## PART1_Main_Data
# Krok 1:
# W tej części chcemy uzyskać bazę linków odnoszących się do poszczególnyhch filmów na filmweb.pl,
# w ten sposób najłatwiej będzie uzyskać pożądane przez nas dane. Korzystamy z BAZY FILMÓW, 
# sortujemy po popularności (malejąco), w ten sposób uzyskamy 10 000 najpopularniejszych filmów na filmwebie.
# 
# Korzystamy z dwóch bibliotek: Selenium i BeautifulSoup.
# 
# Pierwsza pomaga w pozyskiwaniu danych ze stron "dynamicznych",
# tzn. takich, które zmieniają zawartość w zależności od interakcji z użytkownikiem. Umożliwia ona również zczytanie skryptu
# html, który nie jest początkowo widoczny dla drugiej biblioteki, o której zaraz.
#
# Druga odpowiada za właściwe zczytanie danych, które nas interesuje. W tym kroku zczytujemy tylko linki, ale w kolejnych krokach
# będzie naszym podstawowym narzędziem pracy.




from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
import time
import requests
from bs4 import BeautifulSoup
from selenium.webdriver.chrome.options import Options

options = Options()
options.add_argument('--headless=new')
chr = Service(executable_path="chromedriver.exe")
driver = webdriver.Chrome(service=chr,options=options)


# selenium_get: wykorzystuje Selenium do obsługi stron dynamicznych. Tworzy bota, który wchodzi na stronę, scrolluje do pewnego momentu,
# a następnie zczytuje skrypt html, który później zwraca.

def selenium_get(url):

    options = Options()
    options.add_argument('--headless=new')
    chr = Service(executable_path="chromedriver.exe")
    driver = webdriver.Chrome(service=chr,options=options)

    if not isinstance(url,str):
        raise Exception
    
    driver.get(url)
    time.sleep(3)
    driver.execute_script('window.scrollTo(0, 1000)')
    time.sleep(3)
    driver.execute_script('window.scrollTo(0, 1300)')
    time.sleep(3)
    html = driver.execute_script("return document.getElementsByTagName('html')[0].innerHTML")
    driver.quit()
    return html

# soup_get_links: mając skrypt danej strony interesuje nas jej zawartość, w tym przypadku tylko linki do danych filmów. 
# Funkcja zczytuje pożądane przez nas dane i zwraca ich listę.

def soup_get_links(html):
    soup = BeautifulSoup(html,'html.parser')
    all_links = [a.get('href') for a in soup.find_all('a') if a.get('href')]
    film_links = [link for link in all_links if link.startswith('/film/')]
    film_links = film_links[0:30]
    unique_links = set(film_links)
    unique_links = list(unique_links)
    return unique_links

# get_all_linksl: powyższe dwie funkcje to narzędzia, które zajmują się obróbką pojedynczej strony.
# Z użyciem pętli przechodzimy ze strony na stronę i zbieramy dane z każdej z nich. 
# Następnie 'sklejamy' je w listę, a na koniec zwracamy.

def get_all_links():
    list = []
    str = ''
    i=1
    for i in range(1,1001):
        str = f'https://www.filmweb.pl/search#/films?page={i}'
        html = selenium_get(str)
        list_i = soup_get_links(html)
        list.append(list_i)
    return list

# main: uzyskujemy końcowy efekt w postaci pliku tekstowego z linkami, który wykorzystamy w kroku kolejnym.

def main(listlinks):

    flat_links = [item for sublist in listlinks for item in sublist]

    with open('list_of_links.txt', 'w') as file:
        for link in flat_links:
            file.write(link + '\n')

if __name__ == '__main__':
    main(get_all_links())