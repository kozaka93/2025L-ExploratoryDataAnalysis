from bs4 import BeautifulSoup
import requests
import pandas as pd
import pycountry
import re
import time

countries = [country.name for country in pycountry.countries]
sorted_countries = sorted(countries, key=len, reverse=True)
pattern = '|'.join(re.escape(country) for country in sorted_countries)

index_ = ['Title','Country','Rating']
df_imdb = pd.DataFrame(columns=index_)

def soup_imdb_single(url):
    headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36"
    }
    response = requests.get(url,headers=headers)
    soup = BeautifulSoup(response.text, 'html.parser')
    return soup
    
def country(soup):
        try:           
            section = soup.find('section',{'data-testid': 'Details'})
            list_elements = section.find_all('div',class_='ipc-metadata-list-item__content-container')
            text = list_elements[1].text
            matches = re.findall(pattern, text)
            countries_string = '/'.join(matches)
            return countries_string
        except AttributeError:
            return None
def rating(soup):
     try:
          rating = soup.find('span',class_ = 'sc-d541859f-1 imUuxf').text
          return rating
     except AttributeError:
          return None
def title(soup):
     try:
          title = soup.find('span',class_ ='hero__primary-text')
          return title.text
     except AttributeError:
          return None
with open('links_imdb','r') as file:
     links = [line.strip() for line in file]

def main():
     for i in range(len(links)):
        if i%100 == 0:
             time_start = time.time()
        soup = soup_imdb_single(links[i])
        data_row = [title(soup),country(soup), rating(soup)]
        df_imdb.loc[len(df_imdb)] = data_row
        if i%100 == 99:
            time_end = time.time()
            elapsed_time = time_end-time_start
            print('Operation Successful! '+f'Elapsed time: {elapsed_time:.4f} seconds')
main()
df_imdb.to_csv('imdb.csv', index=False, na_rep= 'N/A' )
