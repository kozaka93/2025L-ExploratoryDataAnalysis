import pandas as pd
from bs4 import BeautifulSoup
import pickle

index_ = ['Title','Genres']
def unmarge(string):
    target = ' ' 
    if string.count(target) == 3:
        occurrence = 0
        new_s = ''
        for char in string:
            if char == target:
                occurrence += 1
                if occurrence == 2:
                    new_s += '_'
                else:
                    new_s += char
            else:
                new_s += char
        return new_s
    else:
        return string

def title(soup):
        try:
            title = soup.find('h1', class_='filmCoverSection__title')
            return(title.text)
        except AttributeError:
            return None
def genres(soup):
     containers = soup.find_all('span',{'itemprop': 'genre'})
     list_genres = [unmarge(containers[i].text) for i in range(len(containers))]
     return list_genres
df_genres = pd.DataFrame(columns=index_)

with open(r"C:\Users\tomus\Desktop\Filmweb_Project\PART1\my_html_list.pkl", "rb") as f:
    html_list = pickle.load(f)

n = len(html_list)

def main_genres(df):
    for i in range(n):
          url_temp = html_list[i]
          soup_temp = BeautifulSoup(url_temp,'html.parser')
          genres_ = genres(soup_temp)
          string = ''
          for i in range(len(genres_)):
               string += f'{genres_[i]}'
               if i != len(genres_)-1:
                    string += '/'
               else:
                continue
          string_pure = string.replace(' ', '')
          row = [title(soup_temp),string_pure]
          df.loc[len(df)] = row
          if i % 100 == 0:
              print('Success!')
if __name__ == '__main__':
     main_genres(df_genres)
     df_genres.to_csv('genres.csv', index=False, na_rep='N/A')



