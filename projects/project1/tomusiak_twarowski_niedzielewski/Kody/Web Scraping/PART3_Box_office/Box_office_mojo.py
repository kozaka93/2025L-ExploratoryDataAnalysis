from bs4 import BeautifulSoup
import requests
import pandas as pd

index_ = ['Year_BOXOFFICE','Original_Title','Gross_yearly','Gross_Total','Number_of_theatres']
df = pd.DataFrame(columns=index_)

def get_title(line):
    title = line.find('a',class_ ='a-link-normal')
    title_raw = title.text
    return title_raw
def get_gross(line):
    gross = line.find('td',class_ ='a-text-right mojo-field-type-money mojo-estimatable')
    return gross.text
def get_gross_total(line):
    gross = line.find_all('td',class_ ='a-text-right mojo-field-type-money mojo-estimatable')
    gross_total = gross[1]
    return gross_total.text
def get_theatres(line):
    theatre = line.find('td',class_ ='a-text-right mojo-field-type-positive_integer')
    theatre_new = theatre.text
    return theatre_new

def main():
    year = 2025
    i = 1
    while year != 2001:
        url_temp = requests.get(f'https://www.boxofficemojo.com/year/{year}/?area=PL&ref_=bo_yl_table_{i}')
        soup_temp = BeautifulSoup(url_temp.text,'html.parser')
        containers = soup_temp.find_all('tr')
        n = len(containers)
        for j in range(1,n):
            row = [year,get_title(containers[j]),get_gross(containers[j]),
                   get_gross_total(containers[j]),get_theatres(containers[j])]
            df.loc[len(df)] = row
        i += 1
        year -= 1
main()
df.to_csv('boxoffice.csv', index=False, na_rep='N/A')