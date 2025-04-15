# PART1_Main_Data
# krok 3:
# To jest plik czysto testowy. Musimy sprawdzić czy na ułożenie wszystkich stron jest takie same, czy zawierają taki sam rodzaj danych, oraz
# w jaki sposób właściwie je zebrać. Każda funkcja odpowiada za pozyskanie innego rodzaju danych. 
# Kolejny krok to plik o nazwie 'dataf'.


import pickle
import numpy as np
from bs4 import BeautifulSoup
import re

with open(r"C:\Users\tomus\Desktop\Filmweb_Project\PART1_Main_Data\my_html_list.pkl", "rb") as f:
    html_list = pickle.load(f)

def try_title():
    n = len(html_list)
    for i in range(n):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            title = soup_temp.find('h1', class_='filmCoverSection__title')
            print(title.text)
        except AttributeError:
            print("None")
#ok
def try_year():
    n = len(html_list)
    for i in range(500):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            year_tag = soup_temp.find('div', class_ = 'filmCoverSection__year')
            (year_tag.text)
        except AttributeError:
            try:
                year_tag = soup_temp.find('h2', class_ = 'filmCoverSection__year')
                (year_tag.text)
            except AttributeError:
                print(None)
#ok
def try_duration():
    n = len(html_list)
    for i in range(n):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            year_tag = soup_temp.find('div',class_ = 'filmCoverSection__duration')
            duration = year_tag.get('data-duration')
            print(duration)
        except AttributeError:
            print("None")
#ok
def try_original_title():
    n = len(html_list)
    for i in range(n):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            ot_tag = soup_temp.find('h2', class_='filmCoverSection__originalTitle')
            original_title = ''.join(ot_tag.find_all(string=True, recursive=False)).strip()
            print(original_title)
        except AttributeError:
            print("None")
#ok
def try_rating():
    n = len(html_list)
    for i in range(20):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            rating_tag = soup_temp.find('div', class_ ='filmRating filmRating--filmRate filmRating--hasPanel')
            score = (rating_tag.get('data-rate'))
            score = np.round(float(score),4)
            print(score)
        except AttributeError:
            print("None")
        try:
            number_of_ratings = (rating_tag.get('data-count'))
            print(number_of_ratings)
        except AttributeError:
            print('None')
#ok
def try_critic():
    n = len(html_list)
    for i in range(20):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            critic_tag = soup_temp.find('div',class_ = 'filmRating filmRating--filmCritic')
            rating = critic_tag.find('span','filmRating__rateValue')
            print(rating.text)
        except AttributeError:
            print("None")
        try:
            number_critic = critic_tag.find('span',class_='filmRating__count')
            number_critic.text.split()[0]
        except AttributeError:
            print('None')
#ok
def try_director():
    n = len(html_list)
    for i in range(n):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            director_tag = soup_temp.find('span',class_ ='filmInfo__info cloneToCast cloneToOtherInfo')
            directors = director_tag.find_all('a')
            k = len(directors)
            l = ['']*(k)
            for i in range(k):
                l[i] = directors[i].get('title')
                print(l[i])
        except AttributeError:
            print('None')
#ok
def try_production():
    n = len(html_list)
    for i in range(n):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            prod_tag = soup_temp.find('span',class_ ='filmInfo__info filmInfo__info--productionCountry')
            countries = prod_tag.find_all('a')
            k = len(countries)
            l = ['']*(k)
            for i in range(k):
                l[i] = countries[i].text
                print(l[i])
        except AttributeError:
            print('None')
#ok
def try_worldwide_premiere():
    n = len(html_list)
    for i in range(100):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            premiera_s = soup_temp.find('span',class_= 'block')
            prem = premiera_s.get('content')
            if prem == None:
                premiera_s = soup_temp.find_all('span',class_ = 'block')
                prem = premiera_s[1].get('content')
                print(prem)
            else:
                print(prem)
        except AttributeError:
            print(None)
#ok
def try_pol_premiere():
    n = len(html_list)
    for i in range(100):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            premiera_s = soup_temp.find('span',class_= 'block premiereCountry')
            prem = premiera_s.get('content')
            print(prem)
        except AttributeError:
            print('None')
#ok
def try_boxoffice():
    n = len(html_list)
    for i in range(100):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
           bo = soup_temp.find('span',class_ ='filmInfo__info filmInfo__info--column' )
           text = bo.text
           match = re.search(r'\$\d[\d\s]*', text)
           if match:
            first_number = match.group().replace("$", "").replace(" ", "")
        except AttributeError:
            print('None')
#ok
def try_budget():
    n = len(html_list)
    for i in range(50):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            budget_tag = soup_temp.find_all('span', class_='filmInfo__info')
            for line in budget_tag:
                budget_value = line.find('span', {'data-i18n': 'film:info.gross.label'})
                if budget_value == None:
                     continue
                else:
                     if 'na' in budget_value.text:
                        continue
                     else:
                        print(budget_value.text)
        except AttributeError:
            print("None")
#ok
def try_distribution():
    n = len(html_list)
    for i in range(100):
        url_temp = html_list[i]
        soup_temp = BeautifulSoup(url_temp,'html.parser')
        try:
            dystr_tag = soup_temp.find('div',class_ ='filmInfo__group filmInfo__group--distributors')
            dystr_tag = dystr_tag.find('span',class_ ='filmInfo__info')
            dystr_tag = dystr_tag.text
            print(dystr_tag)
        except AttributeError:
            print("None")
#ok

if __name__ == '__main__':
    try_distribution()