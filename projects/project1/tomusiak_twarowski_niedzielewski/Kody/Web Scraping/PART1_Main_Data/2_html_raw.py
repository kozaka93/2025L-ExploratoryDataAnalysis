# PART1_Main_Data
# krok 2:
# Teraz wykorzystujemy listę linków, które uzyskaliśmy w kroku wcześniejszym. Z ich wykorzystaniem ściągamy skrypt html
# dla każdej pojedynczej strony. Łącznie zczytujemy 10 000 stron. Wykorzystujemy dwie bibilioteki: pickle, oraz requests.
#
# Requests zczytuje skrypt html, ale UWAGA ze stron, które nie są dynamiczne. Następnie wszystkie skrypty kompilujemy w listę,
# a następnie z wykorzystaniem biblioteki pickle, zapisujemy, aby mieć do niej dostęp w kolejnych krokach.



import pickle
import requests

str_main = 'https://www.filmweb.pl/'
links_full = []
with open(r'C:\Users\tomus\Desktop\Filmweb_Project\PART1_Main_Data\list_of_links.txt', 'r') as file:
    for line in file:
        url = str_main + line.strip()
        links_full.append(url)

html_list = []
n = len(links_full)
for i in range(n):
    url_temp = links_full[i]
    page_temp = requests.get(url_temp)
    raw_html = page_temp.text  # Get raw HTML text
    html_list.append(raw_html)
    print(f'{i}')

with open("my_html_list.pkl", "wb") as f:
    pickle.dump(html_list, f)
