# PART1_Main_Data
# krok 4:
# Każda funkcja odpowiada za właściwe zebranie danych określonego rodzaju. Najważniejsza jest funkcja 'main', która tworzy finalny dataset.
# Funkcję main wywołamy w innym pliku, który będzie zwieńczeniem całej pracy.



import pandas as pd
from bs4 import BeautifulSoup
import pickle
import re
import numpy as np

with open(r"C:\Users\tomus\Desktop\Filmweb_Project\PART1_Main_Data\my_html_list.pkl", "rb") as f:
    html_list = pickle.load(f)
n = len(html_list)
def title(soup):
        try:
            title = soup.find('h1', class_='filmCoverSection__title')
            return(title.text)
        except AttributeError:
            return None

def year(soup):
        try:
            year_tag = soup.find('div', class_ = 'filmCoverSection__year')
            return year_tag.text
        except AttributeError:
            try:
                year_tag = soup.find('h2', class_ = 'filmCoverSection__year')
                return year_tag.text
            except AttributeError:
                return None

def duration(soup):
        try:
            year_tag = soup.find('div',class_ = 'filmCoverSection__duration')
            duration = year_tag.get('data-duration')
            return duration
        except AttributeError:
            return None
        
def original_title(soup):
        try:
            ot_tag = soup.find('h2', class_='filmCoverSection__originalTitle')
            original_title = ''.join(ot_tag.find_all(string=True, recursive=False)).strip()
            return original_title
        except AttributeError:
            return None

def rating(soup):
        try:
            rating_tag = soup.find('div', class_ ='filmRating filmRating--filmRate filmRating--hasPanel')
            score = (rating_tag.get('data-rate'))
            score = np.round(float(score),4)
            score = float(score)
        except AttributeError:
            score = None
        try:
            number_of_ratings = int(rating_tag.get('data-count'))
        except AttributeError:
            number_of_ratings = None
        return score, number_of_ratings

def critic(soup):
        try:
            critic_tag = soup.find('div',class_ = 'filmRating filmRating--filmCritic')
            rating = critic_tag.find('span','filmRating__rateValue')
            rating = rating.text
        except AttributeError:
            rating = None
        try:
            number_critic = critic_tag.find('span',class_='filmRating__count')
            nc = number_critic.text.split()[0]
        except AttributeError:
            nc = None
        return rating, nc

def director(soup):
        try:
            director_tag = soup.find('span',class_ ='filmInfo__info cloneToCast cloneToOtherInfo')
            directors = director_tag.find_all('a')
            k = len(directors)
            l = ['']*(k)
            str_ = ''
            for i in range(k):
                l[i] = directors[i].get('title')
                str_ += f'{l[i]} '
            return str_
        except AttributeError:
            return None

def production(soup):
        try:
            prod_tag = soup.find('span',class_ ='filmInfo__info filmInfo__info--productionCountry')
            countries = prod_tag.find_all('a')
            k = len(countries)
            l = ['']*(k)
            str_ = ''
            for i in range(k):
                    l[i] = countries[i].text
                    str_ += f'{l[i]}'
                    str_ += ' ' 

            return str_
        except AttributeError:
            return None

def worldwide_premiere(soup):
        try:
            premiera_s = soup.find('span',class_= 'block')
            prem = premiera_s.get('content')
            if prem == None:
                premiera_s = soup.find_all('span',class_ = 'block')
                prem = premiera_s[1].get('content')
                return prem
            else:
                return prem
        except AttributeError:
            return None

def poland_premiere(soup):
        try:
            premiera_s = soup.find('span',class_= 'block premiereCountry')
            prem = premiera_s.get('content')
            return prem
        except AttributeError:
            return None

def boxoffice(soup):
        try:
           bo = soup.find('span',class_ ='filmInfo__info filmInfo__info--column' )
           text = bo.text
           match = re.search(r'\$\d[\d\s]*', text)
           if match:
            first_number = match.group().replace("$", "").replace(" ", "")
            return first_number
        except AttributeError:
            return None

def budget(soup):
        try:
            budget_tag = soup.find_all('span', class_='filmInfo__info')
            for line in budget_tag:
                budget_value = line.find('span', {'data-i18n': 'film:info.gross.label'})
                if budget_value == None:
                     continue
                else:
                     if 'na' in budget_value.text:
                        continue
                     else:
                        if 'w' in budget_value.text:
                             continue
                        else:
                             return budget_value.text
        except AttributeError:
            return None

def distribution(soup):
        try:
            dystr_tag = soup.find('div',class_ ='filmInfo__group filmInfo__group--distributors')
            dystr_tag = dystr_tag.find('span',class_ ='filmInfo__info')
            dystr_tag = dystr_tag.text
            return dystr_tag
        except AttributeError:
            return None

def create_List(soup):
     row = [title(soup),year(soup),duration(soup),original_title(soup), 
            rating(soup)[0],rating(soup)[1], critic(soup)[0],critic(soup)[1], director(soup),production(soup),
            worldwide_premiere(soup), poland_premiere(soup), boxoffice(soup), 
            budget(soup), distribution(soup)]
     n = len(row)
     return row
     #for i in range(n):
      #    print(row[i])
def main(df):
    for i in range(n):
          url_temp = html_list[i]
          soup_temp = BeautifulSoup(url_temp,'html.parser')
          row = create_List(soup_temp)
          df.loc[len(df)] = row
if __name__ =='__main__':
    pass





